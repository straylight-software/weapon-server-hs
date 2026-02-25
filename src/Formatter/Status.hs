{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Formatter.Status
Description : Formatter availability detection and status reporting
Stability   : stable

This module provides functionality for detecting which code formatters are
available on the system and reporting their status. It supports both built-in
formatters (prettier, black, gofmt, rustfmt) and user-configured formatters.

= Architecture

The module separates pure configuration logic from IO-based executable detection:

* __Pure functions__: 'formattersFor', 'applyConfig', 'baseFormatters',
  'formatterInfoToStatus', 'mkFormatterInfo' - these can be tested without IO
* __IO functions__: 'statusFor', 'statusForConfig', 'checkExecutables' -
  these perform actual filesystem/PATH lookups

= Usage

@
import Formatter.Status
import qualified Config.Config as Config

main :: IO ()
main = do
    dhallCache <- Config.newDhallCache
    exeCache <- newExeCache
    statuses <- statusFor dhallCache exeCache "."
    mapM_ print statuses
@

= Performance

Executable lookups are cached via 'ExeCache' to avoid repeated PATH searches.
The cache is thread-safe and should be shared across requests.
-}
module Formatter.Status (
    -- * Core Types
    FormatterStatus (..),
    FormatterInfo (..),

    -- * IO-based Status Checking
    statusFor,
    statusForConfig,
    checkExecutables,

    -- * Pure Configuration Logic
    formattersFor,
    applyConfig,
    baseFormatters,
    formatterInfoToStatus,
    mkFormatterInfo,

    -- * Executable cache (re-exported from Util.ExeCache)
    ExeCache,
    newExeCache,
) where

import Config.Config qualified as Config
import Config.Types qualified as CT
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import Util.ExeCache (ExeCache, findExecutableCached, newExeCache)

-- ════════════════════════════════════════════════════════════════════════════
-- Types
-- ════════════════════════════════════════════════════════════════════════════

{- | Internal representation of a formatter's configuration.

This type captures the static information about a formatter:
its name, which file extensions it handles, and the executable
name to search for in PATH.

This is an internal type that gets converted to 'FormatterStatus'
after checking whether the executable is available.
-}
data FormatterInfo = FormatterInfo
    { fiName :: Text
    -- ^ Human-readable name of the formatter (e.g., "prettier", "black")
    , fiExtensions :: [Text]
    -- ^ File extensions this formatter handles (e.g., [".js", ".ts"])
    , fiExeName :: String
    -- ^ Name of the executable to search for in PATH
    }
    deriving (Show, Eq)

{- | Status of a formatter including whether it's available on the system.

This is the public-facing type returned by status queries, containing
all information needed to display formatter availability to users.
-}
data FormatterStatus = FormatterStatus
    { fsName :: Text
    -- ^ Human-readable name of the formatter
    , fsExtensions :: [Text]
    -- ^ File extensions this formatter handles
    , fsEnabled :: Bool
    -- ^ Whether the formatter executable was found in PATH
    }
    deriving (Show, Eq, Generic)

instance ToJSON FormatterStatus where
    toJSON status =
        object
            [ "name" .= fsName status
            , "extensions" .= fsExtensions status
            , "enabled" .= fsEnabled status
            ]

-- ════════════════════════════════════════════════════════════════════════════
-- IO-based Status Checking
-- ════════════════════════════════════════════════════════════════════════════

{- | Get formatter status for a directory, loading config and checking executables.

This is the main entry point for formatter status queries. It:

1. Loads the Dhall configuration for the given directory
2. Determines which formatters are configured
3. Checks which formatter executables are available in PATH

@
statuses <- statusFor dhallCache exeCache "/path/to/project"
-- Returns [FormatterStatus] with enabled=True for available formatters
@
-}
statusFor :: Config.DhallCache -> ExeCache -> FilePath -> IO [FormatterStatus]
statusFor dhallCache exeCache dir = do
    cfg <- Config.load dhallCache dir
    statusForConfig exeCache dir cfg

{- | Get formatter status for an already-loaded configuration.

Use this when you already have a 'CT.Config' value and want to avoid
re-parsing the configuration. The directory parameter is currently unused
but reserved for future use (e.g., project-local formatter detection).

@
cfg <- Config.load dhallCache "."
statuses <- statusForConfig exeCache "." cfg
@
-}
statusForConfig :: ExeCache -> FilePath -> CT.Config -> IO [FormatterStatus]
statusForConfig exeCache _dir cfg = checkExecutables exeCache (formattersFor cfg)

{- | Check which formatter executables are available in PATH.

This is the IO boundary - it takes a pure list of 'FormatterInfo' and
produces 'FormatterStatus' by checking the filesystem. The results
are cached in 'ExeCache' to avoid repeated PATH lookups.

@
let infos = [mkFormatterInfo "myformat" [".ext"] "myformat-bin"]
statuses <- checkExecutables exeCache infos
@
-}
checkExecutables :: ExeCache -> [FormatterInfo] -> IO [FormatterStatus]
checkExecutables exeCache = mapM (toStatus exeCache)

{- | Convert a single FormatterInfo to FormatterStatus by checking PATH.

Internal helper that performs the actual executable lookup.
-}
toStatus :: ExeCache -> FormatterInfo -> IO FormatterStatus
toStatus exeCache info = do
    enabled <- isJust <$> findExecutableCached exeCache (fiExeName info)
    pure $ formatterInfoToStatus info enabled

-- ════════════════════════════════════════════════════════════════════════════
-- Pure Configuration Logic
-- ════════════════════════════════════════════════════════════════════════════

{- | Get the list of formatters for a given configuration.

Applies user configuration to the base formatters list. If no formatter
configuration is specified, returns 'baseFormatters'. If formatters are
disabled, returns an empty list.

This is a pure function that can be tested without IO.

@
-- With default config, returns base formatters
formattersFor defaultConfig == baseFormatters

-- With formatters disabled
formattersFor (defaultConfig { cfgFormatter = Just FormatterDisabled }) == []
@
-}
formattersFor :: CT.Config -> [FormatterInfo]
formattersFor cfg =
    case CT.cfgFormatter cfg of
        Nothing -> baseFormatters
        Just fmtCfg -> applyConfig fmtCfg baseFormatters

{- | Apply a formatter configuration to a list of formatter infos.

* 'FormatterDisabled' - Returns empty list, disabling all formatters
* 'FormatterEnabled' - Returns the input list (enabled map controls
  which formatters are active, but we check executables regardless)

This is a pure function separated from IO for testability.
-}
applyConfig :: CT.FormatterConfig -> [FormatterInfo] -> [FormatterInfo]
applyConfig CT.FormatterDisabled _infos = []
applyConfig (CT.FormatterEnabled _enabledMap) infos = infos

{- | Convert a FormatterInfo to a FormatterStatus given an enabled flag.

This is a pure conversion function that constructs the public-facing
'FormatterStatus' from the internal 'FormatterInfo' and a boolean
indicating whether the executable was found.

@
formatterInfoToStatus (mkFormatterInfo "black" [".py"] "black") True
-- FormatterStatus { fsName = "black", fsExtensions = [".py"], fsEnabled = True }
@
-}
formatterInfoToStatus :: FormatterInfo -> Bool -> FormatterStatus
formatterInfoToStatus info enabled =
    FormatterStatus
        { fsName = fiName info
        , fsExtensions = fiExtensions info
        , fsEnabled = enabled
        }

{- | Smart constructor for creating a FormatterInfo.

Use this instead of the record constructor for better forward compatibility.

@
mkFormatterInfo "prettier" [".js", ".ts"] "prettier"
@
-}
mkFormatterInfo :: Text -> [Text] -> String -> FormatterInfo
mkFormatterInfo name extensions exeName =
    FormatterInfo
        { fiName = name
        , fiExtensions = extensions
        , fiExeName = exeName
        }

{- | The built-in formatters supported out of the box.

These formatters are available by default if their executables are found
in PATH. Users can disable them via configuration.

Currently supported:

* __prettier__ - JavaScript, TypeScript, JSX, TSX, JSON, CSS, HTML, Markdown
* __black__ - Python
* __gofmt__ - Go
* __rustfmt__ - Rust
-}
baseFormatters :: [FormatterInfo]
baseFormatters =
    [ mkFormatterInfo "prettier" prettierExtensions "prettier"
    , mkFormatterInfo "black" [".py"] "black"
    , mkFormatterInfo "gofmt" [".go"] "gofmt"
    , mkFormatterInfo "rustfmt" [".rs"] "rustfmt"
    ]
  where
    -- Prettier handles a wide variety of web-related file types
    prettierExtensions :: [Text]
    prettierExtensions = [".js", ".ts", ".jsx", ".tsx", ".json", ".css", ".html", ".md"]
