{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.ConfigMergeProps
Description : Property tests for Config.Merge module

Property-based tests for configuration merging logic.
Tests the merge semantics independently of IO/Dhall loading.
-}
module Property.ConfigMergeProps where

import Config.Merge
import Config.Types
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Generators
-- ════════════════════════════════════════════════════════════════════════════

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

genMaybeText :: Gen (Maybe Text)
genMaybeText = Gen.maybe genText

genMaybeInt :: Gen (Maybe Int)
genMaybeInt = Gen.maybe (Gen.int (Range.linear 0 10000))

genMaybeBool :: Gen (Maybe Bool)
genMaybeBool = Gen.maybe Gen.bool

genLogLevel :: Gen LogLevel
genLogLevel = Gen.element [DEBUG, INFO, WARN, ERROR]

genDiffStyle :: Gen DiffStyle
genDiffStyle = Gen.element [DiffAuto, DiffStacked]

genServerConfig :: Gen ServerConfig
genServerConfig =
    ServerConfig
        <$> genMaybeText
        <*> Gen.maybe (Gen.int (Range.linear 1 65535))
        <*> genMaybeBool
        <*> genMaybeText
        <*> Gen.maybe (Gen.list (Range.linear 0 3) genText)

genScrollAccelerationConfig :: Gen ScrollAccelerationConfig
genScrollAccelerationConfig = ScrollAccelerationConfig <$> Gen.bool

genTUIConfig :: Gen TUIConfig
genTUIConfig =
    TUIConfig
        <$> Gen.maybe (Gen.double (Range.linearFrac 0.1 10.0))
        <*> Gen.maybe genScrollAccelerationConfig
        <*> Gen.maybe genDiffStyle

genCompactionConfig :: Gen CompactionConfig
genCompactionConfig =
    CompactionConfig
        <$> genMaybeBool
        <*> genMaybeBool
        <*> Gen.maybe (Gen.int (Range.linear 1024 16384))

genExperimentalConfig :: Gen ExperimentalConfig
genExperimentalConfig =
    ExperimentalConfig
        <$> genMaybeBool -- expDisablePasteSummary
        <*> genMaybeBool -- expBatchTool
        <*> genMaybeBool -- expOpenTelemetry
        <*> Gen.maybe (Gen.list (Range.linear 0 5) genText) -- expPrimaryTools
        <*> genMaybeBool -- expContinueLoopOnDeny

genEnterpriseConfig :: Gen EnterpriseConfig
genEnterpriseConfig =
    EnterpriseConfig
        <$> genMaybeText

genWatcherConfig :: Gen WatcherConfig
genWatcherConfig =
    WatcherConfig
        <$> Gen.maybe (Gen.list (Range.linear 0 5) genText)

genConfig :: Gen Config
genConfig =
    Config
        <$> genMaybeText -- model
        <*> genMaybeText -- systemPrompt
        <*> genMaybeInt -- maxTokens
        <*> Gen.maybe genLogLevel -- logLevel
        <*> pure defaultKeybinds -- keybinds (use defaults for simplicity)
        <*> genServerConfig
        <*> genTUIConfig
        <*> pure defaultPermission -- permission
        <*> genCompactionConfig
        <*> genExperimentalConfig
        <*> genEnterpriseConfig
        <*> genWatcherConfig
        <*> pure Nothing -- agent
        <*> pure Nothing -- provider
        <*> pure Nothing -- mcp
        <*> pure Nothing -- formatter
        <*> pure Nothing -- lsp
        <*> pure Nothing -- skill
        <*> pure Nothing -- command
        <*> genMaybeText -- theme
        <*> pure Nothing -- themes
        <*> pure Nothing -- share
        <*> pure Nothing -- autoUpdate

-- ════════════════════════════════════════════════════════════════════════════
--                                                    mergeOptional Properties
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: mergeOptional returns override when override is Just
prop_mergeOptionalOverrideWins :: Property
prop_mergeOptionalOverrideWins = property $ do
    base <- forAll genMaybeText
    override <- forAll genText
    mergeOptional base (Just override) === Just override

-- | Property: mergeOptional returns base when override is Nothing
prop_mergeOptionalBaseFallback :: Property
prop_mergeOptionalBaseFallback = property $ do
    base <- forAll genMaybeText
    mergeOptional base Nothing === base

-- | Property: mergeOptional with both Nothing returns Nothing
prop_mergeOptionalBothNothing :: Property
prop_mergeOptionalBothNothing = property $ do
    mergeOptional (Nothing :: Maybe Text) Nothing === Nothing

-- | Property: mergeOptional is idempotent (merging same value twice)
prop_mergeOptionalIdempotent :: Property
prop_mergeOptionalIdempotent = property $ do
    base <- forAll genMaybeText
    override <- forAll genMaybeText
    let merged1 = mergeOptional base override
    let merged2 = mergeOptional merged1 override
    merged1 === merged2

-- ════════════════════════════════════════════════════════════════════════════
--                                                    mergeConfigs Properties
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: mergeConfigs with defaultConfig as base preserves override values
prop_mergeConfigsOverridePreserved :: Property
prop_mergeConfigsOverridePreserved = property $ do
    override <- forAll genConfig
    let merged = mergeConfigs defaultConfig override
    -- Model from override should be preserved
    cfgModel merged === cfgModel override
    -- LogLevel from override should be preserved
    cfgLogLevel merged === mergeOptional (cfgLogLevel defaultConfig) (cfgLogLevel override)

-- | Property: mergeConfigs with same config is identity
prop_mergeConfigsIdentity :: Property
prop_mergeConfigsIdentity = property $ do
    cfg <- forAll genConfig
    let merged = mergeConfigs cfg cfg
    merged === cfg

-- | Property: mergeConfigs is associative
prop_mergeConfigsAssociative :: Property
prop_mergeConfigsAssociative = property $ do
    cfg1 <- forAll genConfig
    cfg2 <- forAll genConfig
    cfg3 <- forAll genConfig
    let left = mergeConfigs (mergeConfigs cfg1 cfg2) cfg3
    let right = mergeConfigs cfg1 (mergeConfigs cfg2 cfg3)
    left === right

-- | Property: mergeConfigs preserves keybinds defaults when override has Nothing
prop_mergeConfigsKeybindsPreserved :: Property
prop_mergeConfigsKeybindsPreserved = property $ do
    let base = defaultConfig
    let override = defaultConfig{cfgKeybinds = defaultKeybinds{kbLeader = Nothing}}
    let merged = mergeConfigs base override
    -- Leader should come from base since override is Nothing
    kbLeader (cfgKeybinds merged) === kbLeader (cfgKeybinds base)

-- | Property: mergeConfigs override wins for keybinds
prop_mergeConfigsKeybindsOverrideWins :: Property
prop_mergeConfigsKeybindsOverrideWins = property $ do
    newLeader <- forAll genText
    let base = defaultConfig
    let override = defaultConfig{cfgKeybinds = defaultKeybinds{kbLeader = Just newLeader}}
    let merged = mergeConfigs base override
    kbLeader (cfgKeybinds merged) === Just newLeader

-- ════════════════════════════════════════════════════════════════════════════
--                                                   Nested Config Properties
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: mergeServer preserves set fields
prop_mergeServerPreservesFields :: Property
prop_mergeServerPreservesFields = property $ do
    base <- forAll genServerConfig
    port <- forAll $ Gen.int (Range.linear 1 65535)
    let override = ServerConfig Nothing (Just port) Nothing Nothing Nothing
    let merged = mergeServer base override
    scPort merged === Just port
    scHostname merged === scHostname base

-- | Property: mergeTUI is associative
prop_mergeTUIAssociative :: Property
prop_mergeTUIAssociative = property $ do
    tui1 <- forAll genTUIConfig
    tui2 <- forAll genTUIConfig
    tui3 <- forAll genTUIConfig
    let left = mergeTUI (mergeTUI tui1 tui2) tui3
    let right = mergeTUI tui1 (mergeTUI tui2 tui3)
    left === right

-- | Property: mergeCompaction is idempotent
prop_mergeCompactionIdempotent :: Property
prop_mergeCompactionIdempotent = property $ do
    base <- forAll genCompactionConfig
    override <- forAll genCompactionConfig
    let merged1 = mergeCompaction base override
    let merged2 = mergeCompaction merged1 override
    merged1 === merged2

-- | Property: mergeExperimental preserves all set fields
prop_mergeExperimentalPreservesFields :: Property
prop_mergeExperimentalPreservesFields = property $ do
    base <- forAll genExperimentalConfig
    batchTool <- forAll Gen.bool
    let override = defaultExperimental{expBatchTool = Just batchTool}
    let merged = mergeExperimental base override
    expBatchTool merged === Just batchTool

-- | Property: mergeEnterprise override wins
prop_mergeEnterpriseOverrideWins :: Property
prop_mergeEnterpriseOverrideWins = property $ do
    base <- forAll genEnterpriseConfig
    url <- forAll genText
    let override = EnterpriseConfig (Just url)
    let merged = mergeEnterprise base override
    entUrl merged === Just url

-- | Property: mergeWatcher override replaces ignore list
prop_mergeWatcherOverrideWins :: Property
prop_mergeWatcherOverrideWins = property $ do
    base <- forAll genWatcherConfig
    ignoreList <- forAll $ Gen.list (Range.linear 0 5) genText
    let override = WatcherConfig (Just ignoreList)
    let merged = mergeWatcher base override
    watchIgnore merged === Just ignoreList

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Test Tree
-- ════════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Config.Merge Property Tests"
        [ testGroup
            "mergeOptional"
            [ testProperty "override wins when Just" prop_mergeOptionalOverrideWins
            , testProperty "base fallback when Nothing" prop_mergeOptionalBaseFallback
            , testProperty "both Nothing returns Nothing" prop_mergeOptionalBothNothing
            , testProperty "idempotent" prop_mergeOptionalIdempotent
            ]
        , testGroup
            "mergeConfigs"
            [ testProperty "override preserved" prop_mergeConfigsOverridePreserved
            , testProperty "identity" prop_mergeConfigsIdentity
            , testProperty "associative" prop_mergeConfigsAssociative
            , testProperty "keybinds preserved" prop_mergeConfigsKeybindsPreserved
            , testProperty "keybinds override wins" prop_mergeConfigsKeybindsOverrideWins
            ]
        , testGroup
            "nested configs"
            [ testProperty "mergeServer preserves fields" prop_mergeServerPreservesFields
            , testProperty "mergeTUI associative" prop_mergeTUIAssociative
            , testProperty "mergeCompaction idempotent" prop_mergeCompactionIdempotent
            , testProperty "mergeExperimental preserves fields" prop_mergeExperimentalPreservesFields
            , testProperty "mergeEnterprise override wins" prop_mergeEnterpriseOverrideWins
            , testProperty "mergeWatcher override wins" prop_mergeWatcherOverrideWins
            ]
        ]
