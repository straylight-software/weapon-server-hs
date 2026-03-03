{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- | Centralized structured logging with Katip

All logging goes to a file by default. Stdout logging is ONLY enabled
when explicitly requested (e.g., --verbose flag, non-TUI serve mode).

This ensures TUI mode never has stdout corrupted by log output.

Log files are stored in: ~/.local/share/weapon/logs/weapon.log
-}
module Log (
    -- * Configuration
    LogConfig (..),
    defaultLogConfig,
    
    -- * Logger
    Logger,
    newLoggerWithConfig,
    closeLogger,
    withLoggerConfig,
    
    -- * Legacy constructors (for backwards compatibility during migration)
    newLogger,
    newLoggerWithLevel,
    newNullLogger,
    withLogger,
    withLoggerLevel,
    withNullLogger,

    -- * Logging functions
    logInfo,
    logWarn,
    logError,
    logDebug,
    logMsg,

    -- * Context
    withNS,
) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Text (Text)
import Katip hiding (logMsg)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hClose, openFile, stdout)

-- | Logging configuration
data LogConfig = LogConfig
    { lcAppName :: Text
    -- ^ Application name (appears in log namespace)
    , lcLevel :: Severity
    -- ^ Minimum severity level to log
    , lcStdout :: Bool
    -- ^ Whether to also log to stdout (default: False)
    , lcFile :: Bool
    -- ^ Whether to log to file (default: True)
    , lcLogDir :: Maybe FilePath
    -- ^ Override log directory (default: ~/.local/share/weapon/logs/)
    }
    deriving (Show, Eq)

-- | Default configuration: file logging only, no stdout
defaultLogConfig :: Text -> LogConfig
defaultLogConfig appName =
    LogConfig
        { lcAppName = appName
        , lcLevel = InfoS
        , lcStdout = False
        , lcFile = True
        , lcLogDir = Nothing
        }

-- | Logger containing Katip state
data Logger = Logger
    { lgEnv :: LogEnv
    , lgContext :: LogContexts
    , lgNamespace :: Namespace
    , lgFileHandle :: Maybe Handle
    -- ^ File handle to close on shutdown (if file logging enabled)
    }



-- | Create a new logger with explicit configuration
newLoggerWithConfig :: LogConfig -> IO Logger
newLoggerWithConfig LogConfig{..} = do
    le <- initLogEnv (Namespace [lcAppName]) "production"
    
    -- Add file scribe if enabled
    (le', fileHandle) <- if lcFile
        then do
            logDir <- case lcLogDir of
                Just d -> pure d
                Nothing -> do
                    xdgData <- getXdgDirectory XdgData "weapon"
                    pure (xdgData </> "logs")
            createDirectoryIfMissing True logDir
            let logFile = logDir </> "weapon.log"
            h <- openFile logFile AppendMode
            fileScribe <- mkHandleScribeWithFormatter bracketFormat ColorIfTerminal h (permitItem lcLevel) V2
            le'' <- registerScribe "file" fileScribe defaultScribeSettings le
            pure (le'', Just h)
        else
            pure (le, Nothing)
    
    -- Add stdout scribe if enabled
    le'' <- if lcStdout
        then do
            stdoutScribe <- mkHandleScribeWithFormatter bracketFormat ColorIfTerminal stdout (permitItem lcLevel) V2
            registerScribe "stdout" stdoutScribe defaultScribeSettings le'
        else
            pure le'
    
    pure Logger
        { lgEnv = le''
        , lgContext = mempty
        , lgNamespace = Namespace [lcAppName]
        , lgFileHandle = fileHandle
        }

-- | Close the logger and any open file handles
closeLogger :: Logger -> IO ()
closeLogger lg = do
    void (closeScribes (lgEnv lg))
    case lgFileHandle lg of
        Just h -> hClose h
        Nothing -> pure ()

-- | Bracket for logger lifecycle with config
withLoggerConfig :: LogConfig -> (Logger -> IO a) -> IO a
withLoggerConfig config = bracket (newLoggerWithConfig config) closeLogger

--------------------------------------------------------------------------------
-- Legacy constructors (for backwards compatibility)
-- These should be migrated to use LogConfig eventually
--------------------------------------------------------------------------------

-- | Create a new logger (stdout only, for backwards compatibility)
-- DEPRECATED: Use newLoggerWithConfig with explicit config
newLogger :: Text -> IO Logger
newLogger appName = newLoggerWithConfig config
  where
    config = (defaultLogConfig appName)
        { lcStdout = True  -- Legacy behavior: stdout
        , lcFile = False   -- Legacy behavior: no file
        , lcLevel = DebugS
        }

-- | Create a new logger with a minimum severity level (stdout only)
-- DEPRECATED: Use newLoggerWithConfig with explicit config
newLoggerWithLevel :: Text -> Severity -> IO Logger
newLoggerWithLevel appName level = newLoggerWithConfig config
  where
    config = (defaultLogConfig appName)
        { lcStdout = True  -- Legacy behavior: stdout
        , lcFile = False   -- Legacy behavior: no file
        , lcLevel = level
        }

-- | Create a null logger that discards all output (for TUI mode)
-- This creates a logger with no scribes - all messages are discarded
newNullLogger :: Text -> IO Logger
newNullLogger appName = newLoggerWithConfig config
  where
    config = (defaultLogConfig appName)
        { lcStdout = False
        , lcFile = False  -- Null logger: no output anywhere
        }

-- | Bracket for logger lifecycle (stdout only)
-- DEPRECATED: Use withLoggerConfig
withLogger :: Text -> (Logger -> IO a) -> IO a
withLogger appName = bracket (newLogger appName) closeLogger

-- | Bracket for logger lifecycle with minimum severity level (stdout only)
-- DEPRECATED: Use withLoggerConfig
withLoggerLevel :: Text -> Severity -> (Logger -> IO a) -> IO a
withLoggerLevel appName level = bracket (newLoggerWithLevel appName level) closeLogger

-- | Bracket for null logger lifecycle (no output)
-- DEPRECATED: Use withLoggerConfig with lcFile=False, lcStdout=False
withNullLogger :: Text -> (Logger -> IO a) -> IO a
withNullLogger appName = bracket (newNullLogger appName) closeLogger

--------------------------------------------------------------------------------
-- Logging functions
--------------------------------------------------------------------------------

-- | Log at INFO level with payload
logInfo :: (LogItem a) => Logger -> Text -> a -> IO ()
logInfo lg = runLog lg InfoS

-- | Log at WARNING level with payload
logWarn :: (LogItem a) => Logger -> Text -> a -> IO ()
logWarn lg = runLog lg WarningS

-- | Log at ERROR level with payload
logError :: (LogItem a) => Logger -> Text -> a -> IO ()
logError lg = runLog lg ErrorS

-- | Log at DEBUG level with payload
logDebug :: (LogItem a) => Logger -> Text -> a -> IO ()
logDebug lg = runLog lg DebugS

-- | Simple message logging (no structured payload)
logMsg :: Logger -> Severity -> Text -> IO ()
logMsg lg sev msg =
    runKatipContextT (lgEnv lg) (lgContext lg) (lgNamespace lg) $
        logLocM sev (logStr msg)

-- | Internal: run a log action with payload
runLog :: (LogItem a) => Logger -> Severity -> Text -> a -> IO ()
runLog lg sev msg payload =
    runKatipContextT (lgEnv lg) (lgContext lg <> liftPayload payload) (lgNamespace lg) $
        logLocM sev (logStr msg)

--------------------------------------------------------------------------------
-- Context management
--------------------------------------------------------------------------------

-- | Add namespace context for a block of logging
withNS :: Logger -> Text -> Logger
withNS lg ns = lg{lgNamespace = lgNamespace lg <> Namespace [ns]}
