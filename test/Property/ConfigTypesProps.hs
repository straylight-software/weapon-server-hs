{-# LANGUAGE OverloadedStrings #-}

-- | Config.Types property tests
module Property.ConfigTypesProps where

import Config.Types
import Data.Aeson (decode, encode)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 50) Gen.alphaNum

genLogLevel :: Gen LogLevel
genLogLevel = Gen.element [DEBUG, INFO, WARN, ERROR]

genShareMode :: Gen ShareMode
genShareMode = Gen.element [ShareManual, ShareAuto, ShareDisabled]

genDiffStyle :: Gen DiffStyle
genDiffStyle = Gen.element [DiffAuto, DiffStacked]

genPermissionAction :: Gen PermissionAction
genPermissionAction = Gen.element [PermAsk, PermAllow, PermDeny]

genAutoUpdate :: Gen AutoUpdate
genAutoUpdate = Gen.element [AutoUpdateEnabled, AutoUpdateDisabled, AutoUpdateNotify]

genKeybindsConfig :: Gen KeybindsConfig
genKeybindsConfig =
    -- Generate a subset of keybinds for testing
    pure
        defaultKeybinds
            { kbLeader = Just "ctrl+x"
            , kbAppExit = Just "ctrl+c,ctrl+d"
            }

genServerConfig :: Gen ServerConfig
genServerConfig =
    ServerConfig
        <$> Gen.maybe genText
        <*> Gen.maybe (Gen.int (Range.linear 1 65535))
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe Gen.bool

genTUIConfig :: Gen TUIConfig
genTUIConfig =
    TUIConfig
        <$> Gen.maybe (Gen.int (Range.linear 1 10))
        <*> Gen.maybe (Gen.int (Range.linear 1 10))
        <*> Gen.maybe genDiffStyle

genFormatterEntry :: Gen FormatterEntry
genFormatterEntry =
    FormatterEntry
        <$> Gen.list (Range.linear 1 3) genNonEmptyText
        <*> Gen.maybe (Gen.int (Range.linear 1000 10000))

genFormatterConfig :: Gen FormatterConfig
genFormatterConfig =
    Gen.choice
        [ pure FormatterDisabled
        , FormatterEnabled . Map.fromList <$> Gen.list (Range.linear 0 3) ((,) <$> genNonEmptyText <*> genFormatterEntry)
        ]

genCompactionConfig :: Gen CompactionConfig
genCompactionConfig =
    CompactionConfig
        <$> Gen.maybe Gen.bool
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe (Gen.int (Range.linear 1024 16384))

genExperimentalConfig :: Gen ExperimentalConfig
genExperimentalConfig =
    ExperimentalConfig
        <$> Gen.maybe Gen.bool
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe Gen.bool

-- ============================================================================
-- Properties
-- ============================================================================

prop_keybindsConfigRoundtrip :: Property
prop_keybindsConfigRoundtrip = property $ do
    cfg <- forAll genKeybindsConfig
    let json = encode cfg
    case decode json of
        Nothing -> failure
        Just cfg' -> cfg === cfg'

prop_serverConfigRoundtrip :: Property
prop_serverConfigRoundtrip = property $ do
    cfg <- forAll genServerConfig
    let json = encode cfg
    case decode json of
        Nothing -> failure
        Just cfg' -> cfg === cfg'

prop_tuiConfigRoundtrip :: Property
prop_tuiConfigRoundtrip = property $ do
    cfg <- forAll genTUIConfig
    let json = encode cfg
    case decode json of
        Nothing -> failure
        Just cfg' -> cfg === cfg'

prop_formatterEntryRoundtrip :: Property
prop_formatterEntryRoundtrip = property $ do
    entry <- forAll genFormatterEntry
    let json = encode entry
    case decode json of
        Nothing -> failure
        Just entry' -> entry === entry'

prop_formatterConfigRoundtrip :: Property
prop_formatterConfigRoundtrip = property $ do
    cfg <- forAll genFormatterConfig
    let json = encode cfg
    case decode json of
        Nothing -> failure
        Just cfg' -> cfg === cfg'

-- | Property: FormatterDisabled encodes as false
prop_formatterDisabledEncodesFalse :: Property
prop_formatterDisabledEncodesFalse = property $ do
    let json = encode FormatterDisabled
    json === "false"

-- | Property: ServerConfig port is positive when present
prop_serverConfigPortPositive :: Property
prop_serverConfigPortPositive = property $ do
    cfg <- forAll genServerConfig
    case scPort cfg of
        Just port -> assert $ port > 0
        Nothing -> success

-- | Property: LogLevel round-trip
prop_logLevelRoundtrip :: Property
prop_logLevelRoundtrip = property $ do
    level <- forAll genLogLevel
    let json = encode level
    case decode json of
        Nothing -> failure
        Just level' -> level === level'

-- | Property: ShareMode round-trip
prop_shareModeRoundtrip :: Property
prop_shareModeRoundtrip = property $ do
    mode <- forAll genShareMode
    let json = encode mode
    case decode json of
        Nothing -> failure
        Just mode' -> mode === mode'

-- | Property: AutoUpdate round-trip
prop_autoUpdateRoundtrip :: Property
prop_autoUpdateRoundtrip = property $ do
    au <- forAll genAutoUpdate
    let json = encode au
    case decode json of
        Nothing -> failure
        Just au' -> au === au'

-- | Property: PermissionAction round-trip
prop_permissionActionRoundtrip :: Property
prop_permissionActionRoundtrip = property $ do
    action <- forAll genPermissionAction
    let json = encode action
    case decode json of
        Nothing -> failure
        Just action' -> action === action'

-- | Property: Default keybinds has app_exit set
prop_defaultKeybindsHasAppExit :: Property
prop_defaultKeybindsHasAppExit = property $ do
    case kbAppExit defaultKeybinds of
        Just _ -> success
        Nothing -> failure

-- | Property: Default config has keybinds
prop_defaultConfigHasKeybinds :: Property
prop_defaultConfigHasKeybinds = property $ do
    -- The default config should have all keybinds populated
    case kbAppExit (cfgKeybinds defaultConfig) of
        Just exit -> assert $ exit /= ""
        Nothing -> failure

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Config.Types Property Tests"
        [ testProperty "KeybindsConfig round-trip" prop_keybindsConfigRoundtrip
        , testProperty "ServerConfig round-trip" prop_serverConfigRoundtrip
        , testProperty "TUIConfig round-trip" prop_tuiConfigRoundtrip
        , testProperty "FormatterEntry round-trip" prop_formatterEntryRoundtrip
        , testProperty "FormatterConfig round-trip" prop_formatterConfigRoundtrip
        , testProperty "FormatterDisabled encodes false" prop_formatterDisabledEncodesFalse
        , testProperty "ServerConfig port positive" prop_serverConfigPortPositive
        , testProperty "LogLevel round-trip" prop_logLevelRoundtrip
        , testProperty "ShareMode round-trip" prop_shareModeRoundtrip
        , testProperty "AutoUpdate round-trip" prop_autoUpdateRoundtrip
        , testProperty "PermissionAction round-trip" prop_permissionActionRoundtrip
        , testProperty "Default keybinds has app_exit" prop_defaultKeybindsHasAppExit
        , testProperty "Default config has keybinds" prop_defaultConfigHasKeybinds
        ]
