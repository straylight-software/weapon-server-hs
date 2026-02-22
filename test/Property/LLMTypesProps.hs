{-# LANGUAGE OverloadedStrings #-}

-- | LLM.Types property tests
module Property.LLMTypesProps where

import Data.Aeson (decode, encode, object, (.=))
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LLM.Types
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 0 100) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 100) Gen.alphaNum

genRole :: Gen Role
genRole = Gen.element [User, Assistant, System]

genToolUse :: Gen ToolUse
genToolUse =
    ToolUse
        <$> genNonEmptyText
        <*> genNonEmptyText
        <*> pure (object ["key" .= ("value" :: Text)])

genToolResult :: Gen ToolResult
genToolResult =
    ToolResult
        <$> genNonEmptyText
        <*> genText
        <*> Gen.bool

genContentBlock :: Gen ContentBlock
genContentBlock =
    Gen.choice
        [ TextBlock <$> genText
        , ImageBlock <$> Gen.element ["image/png", "image/jpeg"] <*> genNonEmptyText
        , ToolUseBlock <$> genToolUse
        , ToolResultBlock <$> genToolResult
        ]

genContent :: Gen Content
genContent =
    Gen.choice
        [ SimpleContent <$> genText
        , BlockContent <$> Gen.list (Range.linear 1 3) genContentBlock
        ]

genMessage :: Gen Message
genMessage =
    Message
        <$> genRole
        <*> genContent

genStopReason :: Gen StopReason
genStopReason = Gen.element [EndTurn, MaxTokens, ToolUseSR, StopSequence]

genUsage :: Gen Usage
genUsage =
    Usage
        <$> Gen.int (Range.linear 0 10000)
        <*> Gen.int (Range.linear 0 10000)
        <*> Gen.maybe (Gen.int (Range.linear 0 5000))
        <*> Gen.maybe (Gen.int (Range.linear 0 5000))

-- ============================================================================
-- Properties
-- ============================================================================

prop_roleRoundtrip :: Property
prop_roleRoundtrip = property $ do
    role <- forAll genRole
    let json = encode role
    case decode json of
        Nothing -> failure
        Just role' -> role === role'

prop_toolUseRoundtrip :: Property
prop_toolUseRoundtrip = property $ do
    tu <- forAll genToolUse
    let json = encode tu
    case decode json of
        Nothing -> failure
        Just tu' -> tu === tu'

prop_toolResultRoundtrip :: Property
prop_toolResultRoundtrip = property $ do
    tr <- forAll genToolResult
    let json = encode tr
    case decode json of
        Nothing -> failure
        Just tr' -> tr === tr'

prop_contentBlockRoundtrip :: Property
prop_contentBlockRoundtrip = property $ do
    block <- forAll genContentBlock
    let json = encode block
    case decode json of
        Nothing -> failure
        Just block' -> block === block'

prop_contentRoundtrip :: Property
prop_contentRoundtrip = property $ do
    content <- forAll genContent
    let json = encode content
    case decode json of
        Nothing -> failure
        Just content' -> content === content'

prop_messageRoundtrip :: Property
prop_messageRoundtrip = property $ do
    msg <- forAll genMessage
    let json = encode msg
    case decode json of
        Nothing -> failure
        Just msg' -> msg === msg'

prop_stopReasonRoundtrip :: Property
prop_stopReasonRoundtrip = property $ do
    sr <- forAll genStopReason
    let json = encode sr
    case decode json of
        Nothing -> failure
        Just sr' -> sr === sr'

prop_usageRoundtrip :: Property
prop_usageRoundtrip = property $ do
    usage <- forAll genUsage
    let json = encode usage
    case decode json of
        Nothing -> failure
        Just usage' -> usage === usage'

-- | Property: Role User encodes as "user"
prop_roleUserEncodesUser :: Property
prop_roleUserEncodesUser = property $ do
    let json = encode User
    json === "\"user\""

-- | Property: Role Assistant encodes as "assistant"
prop_roleAssistantEncodesAssistant :: Property
prop_roleAssistantEncodesAssistant = property $ do
    let json = encode Assistant
    json === "\"assistant\""

-- | Property: Role System encodes as "system"
prop_roleSystemEncodesSystem :: Property
prop_roleSystemEncodesSystem = property $ do
    let json = encode System
    json === "\"system\""

-- | Property: ToolResult isError defaults to false
prop_toolResultIsErrorDefault :: Property
prop_toolResultIsErrorDefault = property $ do
    let json = encode $ object ["tool_use_id" .= ("test" :: Text), "content" .= ("result" :: Text)]
    case decode json of
        Nothing -> failure
        Just (tr :: ToolResult) -> trIsError tr === False

-- | Property: Usage input/output tokens are non-negative
prop_usageTokensNonNegative :: Property
prop_usageTokensNonNegative = property $ do
    usage <- forAll genUsage
    assert $ usageInputTokens usage >= 0
    assert $ usageOutputTokens usage >= 0

-- Test tree
tests :: TestTree
tests =
    testGroup
        "LLM.Types Property Tests"
        [ testProperty "Role round-trip" prop_roleRoundtrip
        , testProperty "ToolUse round-trip" prop_toolUseRoundtrip
        , testProperty "ToolResult round-trip" prop_toolResultRoundtrip
        , testProperty "ContentBlock round-trip" prop_contentBlockRoundtrip
        , testProperty "Content round-trip" prop_contentRoundtrip
        , testProperty "Message round-trip" prop_messageRoundtrip
        , testProperty "StopReason round-trip" prop_stopReasonRoundtrip
        , testProperty "Usage round-trip" prop_usageRoundtrip
        , testProperty "Role User encodes user" prop_roleUserEncodesUser
        , testProperty "Role Assistant encodes assistant" prop_roleAssistantEncodesAssistant
        , testProperty "Role System encodes system" prop_roleSystemEncodesSystem
        , testProperty "ToolResult isError defaults false" prop_toolResultIsErrorDefault
        , testProperty "Usage tokens non-negative" prop_usageTokensNonNegative
        ]
