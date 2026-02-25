{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.ConfigDhallProps
Description : Property tests for Config.Dhall module

Property-based tests for Dhall configuration loading and caching.
-}
module Property.ConfigDhallProps where

import Config.Dhall
import Config.Types
import Data.List qualified as L
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.FilePath (pathSeparator, takeFileName, (</>))
import Test.Tasty
import Test.Tasty.Hedgehog

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Generators
-- ════════════════════════════════════════════════════════════════════════════

genDirPath :: Gen FilePath
genDirPath = do
    segments <- Gen.list (Range.linear 1 5) genPathSegment
    -- Use pathSeparator from System.FilePath for the root
    pure $ [pathSeparator] <> joinPaths segments
  where
    genPathSegment = Gen.string (Range.linear 1 20) Gen.alphaNum
    -- Safe path joining for finite test lists
    joinPaths [] = ""
    joinPaths [x] = x
    joinPaths (x : xs) = x </> joinPaths xs

-- ════════════════════════════════════════════════════════════════════════════
--                                                    Path Function Properties
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: projectConfigPath appends weapon.dhall to directory
prop_projectConfigPathAppendsWeaponDhall :: Property
prop_projectConfigPathAppendsWeaponDhall = property $ do
    dir <- forAll genDirPath
    let result = projectConfigPath dir
    -- Result should end with weapon.dhall
    assert $ takeFileName result == "weapon.dhall"
    -- Result should start with the directory
    assert $ dir `L.isPrefixOf` result

-- | Property: projectConfigPath is deterministic
prop_projectConfigPathDeterministic :: Property
prop_projectConfigPathDeterministic = property $ do
    dir <- forAll genDirPath
    projectConfigPath dir === projectConfigPath dir

-- | Property: projectConfigPath handles root directory
prop_projectConfigPathRoot :: Property
prop_projectConfigPathRoot = property $ do
    projectConfigPath "/" === "/weapon.dhall"

-- | Property: projectConfigPath handles relative path
prop_projectConfigPathRelative :: Property
prop_projectConfigPathRelative = property $ do
    projectConfigPath "." === "./weapon.dhall"

-- | Property: defaultsPath is constant
prop_defaultsPathConstant :: Property
prop_defaultsPathConstant = property $ do
    defaultsPath === "dhall/Defaults.dhall"

-- ════════════════════════════════════════════════════════════════════════════
--                                                      Default Config Properties
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: defaultConfig has keybinds populated
prop_defaultConfigHasKeybinds :: Property
prop_defaultConfigHasKeybinds = property $ do
    -- Critical: app_exit must be present for Ctrl+C to work
    case kbAppExit (cfgKeybinds defaultConfig) of
        Just exit -> assert $ exit /= ""
        Nothing -> failure

-- | Property: defaultConfig has sensible server defaults
prop_defaultConfigServerDefaults :: Property
prop_defaultConfigServerDefaults = property $ do
    let server = cfgServer defaultConfig
    scPort server === Just 4096
    scHostname server === Just "localhost"
    scCors server === Just True

-- | Property: defaultConfig has INFO log level
prop_defaultConfigLogLevel :: Property
prop_defaultConfigLogLevel = property $ do
    cfgLogLevel defaultConfig === Just INFO

-- | Property: defaultConfig theme is set
prop_defaultConfigTheme :: Property
prop_defaultConfigTheme = property $ do
    cfgTheme defaultConfig === Just "ono-sendai"

-- | Property: defaultConfig instrumentation is disabled
prop_defaultConfigInstrumentation :: Property
prop_defaultConfigInstrumentation = property $ do
    cfgInstrumentation defaultConfig === Just False

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Test Tree
-- ════════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Config.Dhall Property Tests"
        [ testGroup
            "path functions"
            [ testProperty "projectConfigPath appends weapon.dhall" prop_projectConfigPathAppendsWeaponDhall
            , testProperty "projectConfigPath deterministic" prop_projectConfigPathDeterministic
            , testProperty "projectConfigPath root" prop_projectConfigPathRoot
            , testProperty "projectConfigPath relative" prop_projectConfigPathRelative
            , testProperty "defaultsPath constant" prop_defaultsPathConstant
            ]
        , testGroup
            "defaultConfig"
            [ testProperty "has keybinds" prop_defaultConfigHasKeybinds
            , testProperty "server defaults" prop_defaultConfigServerDefaults
            , testProperty "log level" prop_defaultConfigLogLevel
            , testProperty "theme" prop_defaultConfigTheme
            , testProperty "instrumentation disabled" prop_defaultConfigInstrumentation
            ]
        ]
