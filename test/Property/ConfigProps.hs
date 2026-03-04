{-# LANGUAGE OverloadedStrings #-}

-- | Config property tests
module Property.ConfigProps where

import Config.Config (mergeConfig)
import Config.Types
import Data.Aeson (decode, encode)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- | Property: merging twice with same config is idempotent
prop_mergeIdempotent :: Property
prop_mergeIdempotent = property $ do
    base <- forAll genConfig
    override <- forAll genConfig
    let merged1 = mergeConfig base override
    let merged2 = mergeConfig merged1 override
    merged1 === merged2

-- | Property: config JSON round-trip preserves values
prop_configJsonRoundtrip :: Property
prop_configJsonRoundtrip = property $ do
    cfg <- forAll genConfig
    let json = encode cfg
    case decode json of
        Nothing -> failure
        Just cfg' -> cfg === cfg'

prop_formatterDisabledJson :: Property
prop_formatterDisabledJson = property $ do
    let cfg = defaultConfig{cfgFormatter = Just FormatterDisabled}
    let json = encode cfg
    case decode json of
        Nothing -> failure
        Just cfg' -> cfgFormatter cfg' === Just FormatterDisabled

-- | Property: config merge is associative
prop_configMergeAssociative :: Property
prop_configMergeAssociative = property $ do
    cfg1 <- forAll genConfig
    cfg2 <- forAll genConfig
    cfg3 <- forAll genConfig
    let left = mergeConfig (mergeConfig cfg1 cfg2) cfg3
    let right = mergeConfig cfg1 (mergeConfig cfg2 cfg3)
    left === right

-- | Property: config update is idempotent (updating twice same as once)
prop_configUpdateIdempotent :: Property
prop_configUpdateIdempotent = property $ do
    base <- forAll genConfig
    update <- forAll genConfig
    let once = mergeConfig base update
    let twice = mergeConfig once update
    once === twice

-- | Property: mergeConfig with Right values overrides Left values
prop_mergeRightOverridesLeft :: Property
prop_mergeRightOverridesLeft = property $ do
    theme1 <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    theme2 <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let cfg1 = defaultConfig{cfgTheme = Just theme1}
    let cfg2 = defaultConfig{cfgTheme = Just theme2}
    let merged = mergeConfig cfg1 cfg2
    cfgTheme merged === Just theme2

-- | Property: mergeConfig preserves provider settings from override
prop_mergePreservesProviders :: Property
prop_mergePreservesProviders = property $ do
    disabled <- forAll Gen.bool
    let providerCfg =
            ProviderConfig
                { pcApi = Nothing
                , pcModels = Nothing
                , pcOptions = Nothing
                , pcTimeout = Nothing
                , pcDisabled = Just disabled
                , pcName = Nothing
                }
    let cfg1 = defaultConfig
    let cfg2 = defaultConfig{cfgProvider = Just (Map.singleton "openai" providerCfg)}
    let merged = mergeConfig cfg1 cfg2
    case cfgProvider merged of
        Just providers -> do
            case Map.lookup "openai" providers of
                Just pc -> pcDisabled pc === Just disabled
                Nothing -> failure
        Nothing -> failure

-- | Property: keybinds merge preserves defaults
prop_keybindsMergePreservesDefaults :: Property
prop_keybindsMergePreservesDefaults = property $ do
    let cfg1 = defaultConfig
    let cfg2 = defaultConfig{cfgKeybinds = defaultKeybinds{kbLeader = Just "ctrl+space"}}
    let merged = mergeConfig cfg1 cfg2
    -- Leader should be overridden
    kbLeader (cfgKeybinds merged) === Just "ctrl+space"
    -- App exit should still have default
    kbAppExit (cfgKeybinds merged) === Just "ctrl+c,ctrl+d,<leader>q"

-- | Property: default config has all keybinds populated
prop_defaultConfigHasKeybinds :: Property
prop_defaultConfigHasKeybinds = property $ do
    -- Critical: app_exit must be present for Ctrl+C to work
    kbAppExit (cfgKeybinds defaultConfig) === Just "ctrl+c,ctrl+d,<leader>q"
    kbLeader (cfgKeybinds defaultConfig) === Just "ctrl+x"
    kbSessionInterrupt (cfgKeybinds defaultConfig) === Just "escape"

-- Generators
genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

genMaybeText :: Gen (Maybe Text)
genMaybeText = Gen.maybe genText

genKeybindsConfig :: Gen KeybindsConfig
genKeybindsConfig = pure defaultKeybinds

genServerConfig :: Gen ServerConfig
genServerConfig =
    ServerConfig
        <$> genMaybeText
        <*> Gen.maybe (Gen.int (Range.linear 1 65535))
        <*> Gen.maybe Gen.bool
        <*> genMaybeText
        <*> Gen.maybe (Gen.list (Range.linear 0 3) genText)

genScrollAccelerationConfig :: Gen ScrollAccelerationConfig
genScrollAccelerationConfig = ScrollAccelerationConfig <$> Gen.bool

genTUIConfig :: Gen TUIConfig
genTUIConfig =
    TUIConfig
        <$> Gen.maybe (Gen.double (Range.linearFrac 0.1 10.0))
        <*> Gen.maybe genScrollAccelerationConfig
        <*> Gen.maybe (Gen.element [DiffAuto, DiffStacked])

genPermissionConfig :: Gen PermissionConfig
genPermissionConfig =
    pure $
        PermissionConfig
            { permRead = Nothing
            , permEdit = Nothing
            , permGlob = Nothing
            , permGrep = Nothing
            , permList = Nothing
            , permBash = Nothing
            , permTask = Nothing
            , permExternalDirectory = Nothing
            , permTodowrite = Nothing
            , permTodoread = Nothing
            , permQuestion = Nothing
            , permWebfetch = Nothing
            , permWebsearch = Nothing
            , permCodesearch = Nothing
            , permLsp = Nothing
            , permDoomLoop = Nothing
            , permSkill = Nothing
            }

genCompactionConfig :: Gen CompactionConfig
genCompactionConfig =
    CompactionConfig
        <$> Gen.maybe Gen.bool
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe (Gen.int (Range.linear 1024 16384))

genExperimentalConfig :: Gen ExperimentalConfig
genExperimentalConfig =
    ExperimentalConfig
        <$> Gen.maybe Gen.bool -- expDisablePasteSummary
        <*> Gen.maybe Gen.bool -- expBatchTool
        <*> Gen.maybe Gen.bool -- expOpenTelemetry
        <*> Gen.maybe (Gen.list (Range.linear 0 5) genText) -- expPrimaryTools
        <*> Gen.maybe Gen.bool -- expContinueLoopOnDeny

genEnterpriseConfig :: Gen EnterpriseConfig
genEnterpriseConfig =
    EnterpriseConfig
        <$> genMaybeText

genWatcherConfig :: Gen WatcherConfig
genWatcherConfig =
    WatcherConfig
        <$> Gen.maybe (Gen.list (Range.linear 0 5) genText)

genFormatterConfig :: Gen FormatterConfig
genFormatterConfig =
    Gen.choice
        [ pure FormatterDisabled
        , pure (FormatterEnabled Map.empty)
        ]

genConfig :: Gen Config
genConfig =
    Config
        <$> genMaybeText -- model
        <*> genMaybeText -- systemPrompt
        <*> Gen.maybe (Gen.int (Range.linear 100 10000)) -- maxTokens
        <*> Gen.maybe (Gen.element [DEBUG, INFO, WARN, ERROR]) -- logLevel
        <*> genKeybindsConfig -- keybinds
        <*> genServerConfig -- server
        <*> genTUIConfig -- tui
        <*> genPermissionConfig -- permission
        <*> genCompactionConfig -- compaction
        <*> genExperimentalConfig -- experimental
        <*> genEnterpriseConfig -- enterprise
        <*> genWatcherConfig -- watcher
        <*> pure Nothing -- agent
        <*> pure Nothing -- provider
        <*> pure Nothing -- mcp
        <*> Gen.maybe genFormatterConfig -- formatter
        <*> pure Nothing -- lsp
        <*> pure Nothing -- skill
        <*> pure Nothing -- command
        <*> genMaybeText -- theme
        <*> pure Nothing -- themes
        <*> Gen.maybe (Gen.element [ShareManual, ShareAuto, ShareDisabled]) -- share
        <*> Gen.maybe (Gen.element [AutoUpdateEnabled, AutoUpdateDisabled, AutoUpdateNotify]) -- autoUpdate
        <*> pure defaultTelemetry -- telemetry

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Config Property Tests"
        [ testProperty "merge idempotent" prop_mergeIdempotent
        , testProperty "config JSON roundtrip" prop_configJsonRoundtrip
        , testProperty "formatter disabled JSON" prop_formatterDisabledJson
        , testProperty "config merge associative" prop_configMergeAssociative
        , testProperty "config update idempotent" prop_configUpdateIdempotent
        , testProperty "merge right overrides left" prop_mergeRightOverridesLeft
        , testProperty "merge preserves providers" prop_mergePreservesProviders
        , testProperty "keybinds merge preserves defaults" prop_keybindsMergePreservesDefaults
        , testProperty "default config has keybinds" prop_defaultConfigHasKeybinds
        ]
