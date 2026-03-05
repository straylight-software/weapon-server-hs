{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Server runner with explicit backend selection

No silent fallback - if the requested backend is unavailable, fail loudly.
Port binding also fails immediately unless explicit retry is requested.
-}
module Server.Run (
    runServer,
    runServerWithCleanup,
    ServerSettings (..),
    ServerBackend (..),
    defaultServerSettings,
    checkBackendAvailable,
) where

import Control.Concurrent.Async (AsyncCancelled (..))
import Control.Exception (IOException, SomeException, catch, fromException, throwIO, try)
import Data.Text qualified as T
import Evring.Wai.MultiCore qualified as Evring
import GHC.Conc qualified
import Log qualified
import Network.Socket (Family (..), SockAddr (..), SocketType (..), close, connect, socket)
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import System.IoUring.URing qualified as URing

-- | Server backend choice
data ServerBackend = BackendIoUring | BackendWarp
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Server settings - all options must be explicit
data ServerSettings = ServerSettings
    { serverBackend :: !ServerBackend
    -- ^ Which backend to use (no fallback)
    , serverPort :: !Int
    -- ^ Port to bind to
    , serverPortRetry :: !Int
    -- ^ Number of ports to try if busy (0 = fail immediately)
    , serverCores :: !(Maybe Int)
    -- ^ Number of cores (Nothing = all available, only for io_uring)
    , serverLogger :: !Log.Logger
    }

defaultServerSettings :: Log.Logger -> ServerSettings
defaultServerSettings logger =
    ServerSettings
        { serverBackend = BackendIoUring
        , serverPort = 4096
        , serverPortRetry = 0 -- Fail immediately by default
        , serverCores = Nothing -- Use all available
        , serverLogger = logger
        }

{- | Check if a backend is available on this system
Returns Left with error message if unavailable, Right () if available
-}
checkBackendAvailable :: ServerSettings -> IO (Either String ())
checkBackendAvailable settings@ServerSettings{..} = case serverBackend of
    BackendWarp -> pure (Right ()) -- Warp is always available
    BackendIoUring -> do
        result <- probeIoUring settings
        case result of
            Right () -> pure (Right ())
            Left err -> pure (Left $ "io_uring unavailable: " <> err)

{- | Run the server with the configured backend
Fails if the requested backend is unavailable (no fallback)
-}
runServer :: ServerSettings -> Application -> IO ()
runServer settings app = runServerWithCleanup settings app (pure ())

{- | Run the server with cleanup action on shutdown
Fails if the requested backend is unavailable (no fallback)
-}
runServerWithCleanup :: ServerSettings -> Application -> IO () -> IO ()
runServerWithCleanup settings@ServerSettings{..} app cleanup = do
    let lg = Log.withNS serverLogger "server"

    -- Check backend availability first - fail if not available
    available <- checkBackendAvailable settings
    case available of
        Left err -> do
            Log.logError lg (T.pack err) ()
            throwIO $ userError err
        Right () -> pure ()

    case serverBackend of
        BackendIoUring -> do
            Log.logInfo lg "Starting with io_uring backend" ()
            result <- try $ runWithEvring settings app cleanup
            case result of
                Right () -> pure ()
                Left (e :: SomeException)
                    -- Async cancellation is normal shutdown
                    | Just AsyncCancelled <- fromException e -> pure ()
                    | otherwise -> do
                        Log.logError lg ("io_uring failed: " <> T.pack (show e)) ()
                        throwIO e -- Re-throw, no fallback
        BackendWarp -> do
            Log.logInfo lg "Starting with warp backend" ()
            runWithWarp settings app

{- | Probe io_uring availability by trying to init rings for all cores
Returns Left with error message on failure, Right () on success
-}
probeIoUring :: ServerSettings -> IO (Either String ())
probeIoUring ServerSettings{..} = do
    numCores <- maybe GHC.Conc.getNumCapabilities pure serverCores
    result <- try $ do
        rings <- mapM (\_ -> URing.initURing 0 4096 8192) [1 .. numCores]
        mapM_ URing.closeURing rings
    case result of
        Right () -> pure (Right ())
        Left (e :: SomeException) -> pure (Left $ show e)

-- | Run with evring (io_uring)
runWithEvring :: ServerSettings -> Application -> IO () -> IO ()
runWithEvring ServerSettings{..} app cleanup = do
    let evringSettings =
            (Evring.defaultServerSettings serverLogger)
                { Evring.serverPort = serverPort
                , Evring.serverPortRetry = serverPortRetry
                , Evring.serverCores = serverCores
                }
    Evring.runServerMultiCoreWithCleanup evringSettings app cleanup

-- | Run with warp
runWithWarp :: ServerSettings -> Application -> IO ()
runWithWarp ServerSettings{..} app = do
    let lg = Log.withNS serverLogger "warp"
    -- Find a port - fails immediately if serverPortRetry is 0 and port is busy
    port <- findPort lg serverPort serverPortRetry
    Log.logInfo lg ("Starting on port " <> T.pack (show port)) ()
    let warpSettings =
            Warp.setPort port
                . Warp.setHost "127.0.0.1"
                $ Warp.defaultSettings
    Warp.runSettings warpSettings app

{- | Find a usable port
If portRetry is 0, fail immediately if the port is busy
Otherwise, try up to portRetry additional ports
-}
findPort :: Log.Logger -> Int -> Int -> IO Int
findPort lg port retriesLeft = do
    inUse <- isPortInUse port
    if inUse
        then
            if retriesLeft > 0
                then do
                    Log.logWarn lg ("Port " <> T.pack (show port) <> " is busy, trying " <> T.pack (show (port + 1))) ()
                    findPort lg (port + 1) (retriesLeft - 1)
                else do
                    let msg = "Port " ++ show port ++ " is in use (use --port-retry N to try additional ports)"
                    Log.logError lg (T.pack msg) ()
                    ioError $ userError msg
        else pure port

-- | Check if a port is in use by attempting a connection
isPortInUse :: Int -> IO Bool
isPortInUse port =
    catch
        ( do
            sock <- socket AF_INET Stream 0
            connect sock (SockAddrInet (fromIntegral port) 0x0100007f) -- 127.0.0.1
            close sock
            pure True -- Connection succeeded, something is listening
        )
        (\(_ :: IOException) -> pure False) -- Connection failed, port is free
