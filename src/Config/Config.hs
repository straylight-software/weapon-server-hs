{- |
Module      : Config.Config
Description : Dhall configuration loading and management

Primary configuration module for weapon server. Uses Dhall as the
configuration format.

= Overview

All loading functions require a 'DhallCache' for performance. The cache
avoids re-parsing Dhall files on every call.

= Quick Start

@
cache <- newDhallCache
config <- load cache "\/path\/to\/project"
@

= Configuration Layering

Configuration is loaded in layers, with later layers overriding earlier:

1. Built-in defaults ('defaultConfig')
2. Global config (~\/.config\/weapon\/weapon.dhall)
3. Project config (project\/weapon.dhall)

See "Config.Types" for type definitions and "Config.Merge" for merge semantics.
-}
module Config.Config (
    -- * Types
    Config.Types.Config (..),

    -- * Cache

    -- | The cache stores parsed Dhall configurations to avoid re-parsing.
    Dhall.DhallCache,
    Dhall.newDhallCache,

    -- * Loading (cached)

    -- | All loading functions use the cache for performance.
    load,
    loadFile,

    -- * Path Functions
    globalConfigPath,
    projectConfigPath,

    -- * Defaults
    defaultConfig,

    -- * Merging
    mergeConfig,
) where

import Config.Dhall qualified as Dhall
import Config.Merge qualified as Merge
import Config.Types

{- | Get the global configuration file path.

Returns @~\/.config\/weapon\/weapon.dhall@.
-}
globalConfigPath :: IO FilePath
globalConfigPath = Dhall.globalConfigPath

{- | Get the project-local configuration file path.

Returns @\<projectDir\>\/weapon.dhall@.
-}
projectConfigPath :: FilePath -> FilePath
projectConfigPath = Dhall.projectConfigPath

{- | Load configuration from a specific Dhall file.

Results are cached by filepath - each file is parsed only once.
Returns 'Nothing' if the file doesn't exist or fails to parse.

@
cache <- newDhallCache
mConfig <- loadFile cache "\/path\/to\/config.dhall"
@
-}
loadFile :: Dhall.DhallCache -> FilePath -> IO (Maybe Config)
loadFile = Dhall.loadConfigFromFileCached

{- | Load full configuration with all layers merged.

Loads and merges: defaults <- global <- project.

This is the primary function for loading configuration.

@
cache <- newDhallCache
config <- load cache "\/path\/to\/project"
@
-}
load :: Dhall.DhallCache -> FilePath -> IO Config
load = Dhall.loadConfigCached

{- | Merge two configurations, with the second overriding the first.

See "Config.Merge" for detailed merge semantics.

@
let merged = mergeConfig baseConfig overrideConfig
@
-}
mergeConfig :: Config -> Config -> Config
mergeConfig = Merge.mergeConfigs
