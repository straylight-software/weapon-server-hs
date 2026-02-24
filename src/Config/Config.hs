{- | Config module - Dhall configuration loading and management
Uses Dhall as the only configuration format.

All loading functions require a DhallCache for performance.
-}
module Config.Config (
    -- * Types
    Config.Types.Config (..),

    -- * Cache
    Dhall.DhallCache,
    Dhall.newDhallCache,

    -- * Operations (all cached)
    load,
    loadFile,
    globalConfigPath,
    projectConfigPath,

    -- * Defaults
    defaultConfig,

    -- * Merging
    mergeConfig,
) where

import Config.Dhall qualified as Dhall
import Config.Types

-- | Get global config path (Dhall)
globalConfigPath :: IO FilePath
globalConfigPath = Dhall.globalConfigPath

-- | Get project config path (Dhall)
projectConfigPath :: FilePath -> FilePath
projectConfigPath = Dhall.projectConfigPath

-- | Load config from a Dhall file (cached)
loadFile :: Dhall.DhallCache -> FilePath -> IO (Maybe Config)
loadFile = Dhall.loadConfigFromFileCached

-- | Load config (defaults + global + project) (cached)
load :: Dhall.DhallCache -> FilePath -> IO Config
load = Dhall.loadConfigCached

-- | Merge two configs (second overrides first)
mergeConfig :: Config -> Config -> Config
mergeConfig = Dhall.mergeConfigs
