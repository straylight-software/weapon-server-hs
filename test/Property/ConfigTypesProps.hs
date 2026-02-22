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

genKeybindsConfig :: Gen KeybindsConfig
genKeybindsConfig =
    KeybindsConfig
        <$> Gen.maybe genText
        <*> Gen.maybe genText

genServerConfig :: Gen ServerConfig
genServerConfig =
    ServerConfig
        <$> Gen.maybe genText
        <*> Gen.maybe (Gen.int (Range.linear 1 65535))

genLayoutConfig :: Gen LayoutConfig
genLayoutConfig =
    LayoutConfig
        <$> Gen.maybe (Gen.double (Range.linearFrac 0.1 0.9))
        <*> Gen.maybe Gen.bool

genSkillsConfig :: Gen SkillsConfig
genSkillsConfig =
    SkillsConfig
        <$> Gen.maybe (Gen.list (Range.linear 0 3) genText)
        <*> Gen.maybe (Gen.list (Range.linear 0 3) genText)

genFormatterEntry :: Gen FormatterEntry
genFormatterEntry =
    FormatterEntry
        <$> Gen.maybe Gen.bool
        <*> Gen.maybe (Gen.list (Range.linear 1 3) genNonEmptyText)
        <*> Gen.maybe (Map.fromList <$> Gen.list (Range.linear 0 2) ((,) <$> genNonEmptyText <*> genText))
        <*> Gen.maybe (Gen.list (Range.linear 0 3) genText)

genFormatterConfig :: Gen FormatterConfig
genFormatterConfig =
    Gen.choice
        [ pure FormatterDisabled
        , FormatterConfig . Map.fromList <$> Gen.list (Range.linear 0 3) ((,) <$> genNonEmptyText <*> genFormatterEntry)
        ]

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

prop_layoutConfigRoundtrip :: Property
prop_layoutConfigRoundtrip = property $ do
    cfg <- forAll genLayoutConfig
    let json = encode cfg
    case decode json of
        Nothing -> failure
        Just cfg' -> cfg === cfg'

prop_skillsConfigRoundtrip :: Property
prop_skillsConfigRoundtrip = property $ do
    cfg <- forAll genSkillsConfig
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

-- | Property: LayoutConfig terminalRatio is between 0 and 1 when present
prop_layoutConfigRatioValid :: Property
prop_layoutConfigRatioValid = property $ do
    cfg <- forAll genLayoutConfig
    case lcTerminalRatio cfg of
        Just ratio -> do
            assert $ ratio >= 0.1
            assert $ ratio <= 0.9
        Nothing -> success

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Config.Types Property Tests"
        [ testProperty "KeybindsConfig round-trip" prop_keybindsConfigRoundtrip
        , testProperty "ServerConfig round-trip" prop_serverConfigRoundtrip
        , testProperty "LayoutConfig round-trip" prop_layoutConfigRoundtrip
        , testProperty "SkillsConfig round-trip" prop_skillsConfigRoundtrip
        , testProperty "FormatterEntry round-trip" prop_formatterEntryRoundtrip
        , testProperty "FormatterConfig round-trip" prop_formatterConfigRoundtrip
        , testProperty "FormatterDisabled encodes false" prop_formatterDisabledEncodesFalse
        , testProperty "ServerConfig port positive" prop_serverConfigPortPositive
        , testProperty "LayoutConfig ratio valid" prop_layoutConfigRatioValid
        ]
