{- | Config module - Dhall configuration loading and management
Uses Dhall as the only configuration format
-}
module Config.Config (
    -- * Types
    Config.Types.Config (..),

    -- * Operations
    get,
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

-- | Load config from a Dhall file
loadFile :: FilePath -> IO (Maybe Config)
loadFile = Dhall.loadConfigFromFile

-- | Load config (defaults + global + project)
load :: FilePath -> IO Config
load = Dhall.loadConfig

-- | Merge two configs (second overrides first)
mergeConfig :: Config -> Config -> Config
mergeConfig = Dhall.mergeConfigs

-- | Get config for current project
get :: FilePath -> IO Config
get = load
