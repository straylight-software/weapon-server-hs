{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Config.Dhall
Description : Dhall configuration loading with caching

This module handles loading and caching Dhall configuration files.

= Failure Mode

Parse errors are FATAL - if a config file exists but fails to parse,
the application will fail with a descriptive error. This is intentional:
silent fallback to defaults masks configuration mistakes.

Missing files are allowed - if a config file doesn't exist, it's simply
skipped in the layering process.

= Caching Strategy

The module uses 'MVar'-based caching to avoid re-parsing Dhall files
on every call. The cache stores:

1. Parsed defaults (loaded once on first access)
2. File contents by path (loaded once per unique path)

The cache is thread-safe and should be initialized once per application
via 'newDhallCache' and stored in 'AppState'.

= Configuration Layering

Configuration is loaded in layers:

1. Built-in Haskell defaults ('Config.Types.defaultConfig')
2. Global config (@~\/.config\/weapon\/weapon.dhall@)
3. Project config (@\<project\>\/weapon.dhall@)

Each layer overrides the previous using 'Config.Merge.mergeConfigs'.

= Usage

@
cache <- newDhallCache
config <- loadConfigCached cache "\/path\/to\/project"
@
-}
module Config.Dhall (
    -- * Cache
    DhallCache,
    newDhallCache,

    -- * Loading (all cached)
    loadConfigCached,
    loadConfigFromFileCached,

    -- * Loading (uncached, internal use)
    loadDefaults,
    loadConfigFromFile,

    -- * Paths
    globalConfigPath,
    projectConfigPath,

    -- * Merging (re-exported from Config.Merge)
    mergeConfigs,

    -- * Errors
    ConfigError (..),
) where

import Config.Merge (mergeConfigs)
import Config.Types
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (Exception, SomeException, displayException, throwIO, try)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import Dhall.Core (throws)
import Dhall.Import (load)
import Dhall.JSON (dhallToJSON)
import Dhall.Parser (exprFromText)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

-- ════════════════════════════════════════════════════════════════════════════
--                                                                       Errors
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration loading errors
data ConfigError
    = -- | File exists but failed to parse (path, error message)
      ConfigParseError FilePath String
    deriving (Show, Eq)

instance Exception ConfigError

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Path Functions
-- ════════════════════════════════════════════════════════════════════════════

{- | Get the path to the global configuration file.

Returns @~\/.config\/weapon\/weapon.dhall@.

This is an IO action because it needs to resolve the home directory.
-}
globalConfigPath :: IO FilePath
globalConfigPath = do
    home <- getHomeDirectory
    pure $ home </> ".config" </> "weapon" </> "weapon.dhall"

{- | Get the path to a project-local configuration file.

Returns @\<dir\>\/weapon.dhall@.

This is a pure function - no IO required.
-}
projectConfigPath :: FilePath -> FilePath
projectConfigPath dir = dir </> "weapon.dhall"

-- ════════════════════════════════════════════════════════════════════════════
-- Dhall Cache
-- ════════════════════════════════════════════════════════════════════════════

{- | Cache for parsed Dhall configurations.
Stores both the defaults and any loaded config files.
Thread-safe via MVar.
-}
data DhallCache = DhallCache
    { dcDefaults :: MVar (Maybe Config)
    -- ^ Cached defaults (Nothing = not yet loaded)
    , dcFiles :: MVar (Map FilePath (Maybe Config))
    -- ^ Cached config files by path
    }

-- | Create a new empty Dhall cache
newDhallCache :: IO DhallCache
newDhallCache = do
    defaults <- newMVar Nothing
    files <- newMVar Map.empty
    pure $ DhallCache defaults files

-- ════════════════════════════════════════════════════════════════════════════
-- Cached Loading Functions
-- ════════════════════════════════════════════════════════════════════════════

{- | Load defaults from Dhall file (cached).

This function caches the result - the Dhall file is parsed only once
per cache instance, regardless of how many times it's called.

FAILS if the defaults file is missing or fails to parse.
-}
loadDefaultsCached :: DhallCache -> IO Config
loadDefaultsCached cache = modifyMVar (dcDefaults cache) $ \case
    Just cfg -> pure (Just cfg, cfg)
    Nothing -> do
        cfg <- loadDefaults
        pure (Just cfg, cfg)

{- | Load config from a specific Dhall file (cached).

Results are cached by filepath - each file is parsed only once per cache.
Returns Nothing if the file doesn't exist.

FAILS if the file exists but fails to parse.
-}
loadConfigFromFileCached :: DhallCache -> FilePath -> IO (Maybe Config)
loadConfigFromFileCached cache path = modifyMVar (dcFiles cache) $ \files ->
    case Map.lookup path files of
        Just cfg -> pure (files, cfg)
        Nothing -> do
            cfg <- loadConfigFromFile path
            pure (Map.insert path cfg files, cfg)

{- | Load full config (global + project + defaults) using cache.
FAILS on any parse error. Missing config files are allowed.
-}
loadConfigCached :: DhallCache -> FilePath -> IO Config
loadConfigCached cache projectDir = do
    -- Load built-in defaults (cached, FAILS if missing or invalid)
    defaults <- loadDefaultsCached cache

    -- Load global config (cached, FAILS on parse error)
    globalPath <- globalConfigPath
    globalCfg <- loadConfigFromFileCached cache globalPath

    -- Load project config (cached, FAILS on parse error)
    let projectPath = projectConfigPath projectDir
    projectCfg <- loadConfigFromFileCached cache projectPath

    -- Merge: defaults <- global <- project
    let withGlobal = maybe defaults (mergeConfigs defaults) globalCfg
    let final = maybe withGlobal (mergeConfigs withGlobal) projectCfg

    pure final

-- ════════════════════════════════════════════════════════════════════════════
-- Uncached Loading Functions
-- ════════════════════════════════════════════════════════════════════════════

{- | Return the built-in default configuration.

Previously this loaded from @dhall/Defaults.dhall@, but that caused issues
because the Dhall schema field names don't match the Haskell record field
names (Haskell uses prefixes like @kb@, @sc@, @tui@ while Dhall uses
unprefixed snake_case). Rather than fix ~20 Dhall type files, we now
use the Haskell 'defaultConfig' directly.

User config files (@~\/.config\/weapon\/weapon.dhall@ and project
@weapon.dhall@) are still loaded and merged on top of these defaults.

This function is used internally by 'loadDefaultsCached'.
-}
loadDefaults :: IO Config
loadDefaults = pure defaultConfig

{- | Load config from a specific Dhall file (uncached).

Returns 'Nothing' if the file doesn't exist.

FAILS with 'ConfigParseError' if the file exists but fails to parse.

This function is used internally by 'loadConfigFromFileCached'.
-}
loadConfigFromFile :: FilePath -> IO (Maybe Config)
loadConfigFromFile path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else Just <$> parseConfigFileStrict path

-- ════════════════════════════════════════════════════════════════════════════
-- Internal Helpers
-- ════════════════════════════════════════════════════════════════════════════

{- | Parse a config file that is known to exist.

Uses a two-step process:
1. Parse and evaluate the Dhall expression
2. Convert to JSON and parse with FromJSON (which allows partial records)

This allows user configs to only specify the fields they want to override,
rather than requiring a full Config record.

FAILS with 'ConfigParseError' if parsing fails.
No silent fallback to defaults.
-}
parseConfigFileStrict :: FilePath -> IO Config
parseConfigFileStrict path = do
    result <- try parseViaJson :: IO (Either SomeException Config)
    case result of
        Left err -> throwIO $ ConfigParseError path (displayException err)
        Right cfg -> pure cfg
  where
    parseViaJson :: IO Config
    parseViaJson = do
        -- Read the file
        content <- TIO.readFile path
        -- Parse Dhall expression
        expr <- throws (exprFromText path content)
        -- Resolve imports and normalize
        resolved <- load expr
        -- Convert to JSON
        json <- throws (dhallToJSON resolved)

        -- Parse JSON with our FromJSON instance (allows missing fields)
        case Aeson.fromJSON json of
            Aeson.Error msg -> fail $ "JSON parse error: " ++ msg ++ " for JSON: " ++ show json
            Aeson.Success cfg -> pure cfg
