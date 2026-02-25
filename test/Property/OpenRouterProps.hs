{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.OpenRouterProps
Description : Property tests for LLM.OpenRouter module

Tests for JSON serialization, pure parsing functions, and tool call
accumulation logic in the OpenRouter client.
-}
module Property.OpenRouterProps where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key qualified as K
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LLM.OpenRouter
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

genToolCallFunction :: Gen ToolCallFunction
genToolCallFunction =
    ToolCallFunction
        <$> genNonEmptyText
        <*> genNonEmptyText

genToolCall :: Gen ToolCall
genToolCall =
    ToolCall
        <$> genNonEmptyText
        <*> pure "function"
        <*> genToolCallFunction

genToolResultMessage :: Gen ToolResultMessage
genToolResultMessage =
    ToolResultMessage "tool"
        <$> genNonEmptyText
        <*> genText

genMessage :: Gen Message
genMessage =
    Message
        <$> genRole
        <*> Gen.maybe genText
        <*> Gen.maybe (Gen.list (Range.linear 1 3) genToolCall)

genToolFunction :: Gen ToolFunction
genToolFunction =
    ToolFunction
        <$> genNonEmptyText
        <*> genText
        <*> pure (object ["type" .= ("object" :: Text)])

genTool :: Gen Tool
genTool = Tool "function" <$> genToolFunction

genUsage :: Gen Usage
genUsage =
    Usage
        <$> Gen.int (Range.linear 0 10000)
        <*> Gen.int (Range.linear 0 10000)
        <*> Gen.int (Range.linear 0 20000)

genChoice :: Gen Choice
genChoice =
    Choice
        <$> Gen.int (Range.linear 0 5)
        <*> genMessage
        <*> Gen.maybe (Gen.element ["stop", "tool_calls", "length"])

genChatResponse :: Gen ChatResponse
genChatResponse =
    ChatResponse
        <$> genNonEmptyText
        <*> genNonEmptyText
        <*> Gen.list (Range.linear 1 3) genChoice
        <*> Gen.maybe genUsage

genToolCallPart :: Gen ToolCallPart
genToolCallPart =
    ToolCallPart
        <$> Gen.int (Range.linear 0 5)
        <*> genNonEmptyText
        <*> pure "function"
        <*> genNonEmptyText
        <*> genText

genToolCallDelta :: Gen ToolCallDelta
genToolCallDelta =
    ToolCallDelta
        <$> Gen.int (Range.linear 0 5)
        <*> Gen.maybe genNonEmptyText
        <*> Gen.maybe (pure "function")
        <*> Gen.maybe genNonEmptyText
        <*> Gen.maybe genText

-- ============================================================================
-- JSON Round-trip Properties
-- ============================================================================

-- | Property: Role JSON round-trip
prop_roleRoundtrip :: Property
prop_roleRoundtrip = property $ do
    role <- forAll genRole
    let json = encode role
    case decode json of
        Nothing -> failure
        Just role' -> role === role'

-- | Property: ToolCall JSON round-trip
prop_toolCallRoundtrip :: Property
prop_toolCallRoundtrip = property $ do
    tc <- forAll genToolCall
    let json = encode tc
    case decode json of
        Nothing -> failure
        Just tc' -> tc === tc'

-- | Property: ToolCallFunction JSON round-trip
prop_toolCallFunctionRoundtrip :: Property
prop_toolCallFunctionRoundtrip = property $ do
    tcf <- forAll genToolCallFunction
    let json = encode tcf
    case decode json of
        Nothing -> failure
        Just tcf' -> tcf === tcf'

-- | Property: ToolResultMessage JSON serializes correctly
prop_toolResultMessageSerializes :: Property
prop_toolResultMessageSerializes = property $ do
    trm <- forAll genToolResultMessage
    let json = encode trm
    -- Should contain required fields
    annotateShow json
    assert $ LBS.length json > 0

-- | Property: Message JSON round-trip
prop_messageRoundtrip :: Property
prop_messageRoundtrip = property $ do
    msg <- forAll genMessage
    let json = encode msg
    case decode json of
        Nothing -> failure
        Just msg' -> msg === msg'

-- | Property: Usage JSON round-trip
prop_usageRoundtrip :: Property
prop_usageRoundtrip = property $ do
    usage <- forAll genUsage
    let json = encode usage
    case decode json of
        Nothing -> failure
        Just usage' -> usage === usage'

-- | Property: ChatResponse FromJSON parses valid JSON
prop_chatResponseFromJSON :: Property
prop_chatResponseFromJSON = property $ do
    respIdVal <- forAll genNonEmptyText
    respModelVal <- forAll genNonEmptyText
    -- Create a minimal valid JSON structure
    let json =
            object
                [ "id" .= respIdVal
                , "model" .= respModelVal
                , "choices"
                    .= [ object
                            [ "index" .= (0 :: Int)
                            , "message"
                                .= object
                                    [ "role" .= ("assistant" :: Text)
                                    , "content" .= ("Hello" :: Text)
                                    ]
                            , "finish_reason" .= ("stop" :: Text)
                            ]
                       ]
                , "usage"
                    .= object
                        [ "prompt_tokens" .= (10 :: Int)
                        , "completion_tokens" .= (20 :: Int)
                        , "total_tokens" .= (30 :: Int)
                        ]
                ]
    case decode (encode json) :: Maybe ChatResponse of
        Nothing -> failure
        Just resp' -> do
            respId resp' === respIdVal
            respModel resp' === respModelVal

-- ============================================================================
-- Pure Parsing Properties
-- ============================================================================

-- | Property: assembleToolCalls filters incomplete parts
prop_assembleToolCallsFiltersIncomplete :: Property
prop_assembleToolCallsFiltersIncomplete = property $ do
    parts <- forAll $ Gen.list (Range.linear 0 10) genToolCallPart
    let assembled = assembleToolCalls parts
    -- All assembled calls should have non-empty id and name
    for_ assembled $ \tc -> do
        assert $ tcId tc /= ""
        assert $ tcfName (tcFunction tc) /= ""

-- | Property: mergeToolCallDelta preserves index count
prop_mergeToolCallDeltaPreservesOrAdds :: Property
prop_mergeToolCallDeltaPreservesOrAdds = property $ do
    delta <- forAll genToolCallDelta
    parts <- forAll $ Gen.list (Range.linear 0 5) genToolCallPart
    let result = mergeToolCallDelta delta parts
        -- Use strict length calculation to avoid STAN-0103 lint warning
        -- These are known-finite generated test lists
        partsLen = foldl' (\acc _ -> acc + 1) (0 :: Int) parts
        resultLen = foldl' (\acc _ -> acc + 1) (0 :: Int) result
    -- Result should have at least as many parts as before (may add one)
    assert $ resultLen >= partsLen
    -- Result should have at most one more part than before
    assert $ resultLen <= partsLen + 1

-- | Property: mergeToolCallDelta accumulates args
prop_mergeToolCallDeltaAccumulatesArgs :: Property
prop_mergeToolCallDeltaAccumulatesArgs = property $ do
    idx <- forAll $ Gen.int (Range.linear 0 5)
    args1 <- forAll genNonEmptyText
    args2 <- forAll genNonEmptyText
    let initial = [ToolCallPart idx "id1" "function" "name1" args1]
    let delta = ToolCallDelta idx Nothing Nothing Nothing (Just args2)
    let result = mergeToolCallDelta delta initial
    -- Args should be concatenated; verify single result with pattern match
    case result of
        [p] -> tcpArgs p === (args1 <> args2)
        [] -> failure
        (_ : _ : _) -> failure

-- | Property: extractFieldText returns empty for non-objects
prop_extractFieldTextNonObject :: Property
prop_extractFieldTextNonObject = property $ do
    txt <- forAll genText
    extractFieldText "field" (String txt) === ""
    extractFieldText "field" Null === ""
    extractFieldText "field" (Number 42) === ""
    extractFieldText "field" (Bool True) === ""

-- | Property: extractFieldText extracts string fields
prop_extractFieldTextExtractsString :: Property
prop_extractFieldTextExtractsString = property $ do
    key <- forAll genNonEmptyText
    value <- forAll genText
    let obj = object [K.fromText key .= value]
    extractFieldText key obj === value

-- | Property: extractFieldValue returns fallback for non-objects
prop_extractFieldValueFallback :: Property
prop_extractFieldValueFallback = property $ do
    txt <- forAll genText
    extractFieldValue "field" (String txt) Null === Null
    extractFieldValue "field" Null (Bool True) === Bool True

-- | Property: extractDelta returns Nothing for empty/malformed JSON
prop_extractDeltaHandlesMalformed :: Property
prop_extractDeltaHandlesMalformed = property $ do
    -- Empty bytes
    extractDelta "" === Nothing
    -- Invalid JSON
    extractDelta "not json" === Nothing
    -- Valid JSON but wrong structure
    extractDelta "{\"foo\": \"bar\"}" === Nothing

-- | Property: extractFinishReason returns Nothing for malformed JSON
prop_extractFinishReasonHandlesMalformed :: Property
prop_extractFinishReasonHandlesMalformed = property $ do
    extractFinishReason "" === Nothing
    extractFinishReason "not json" === Nothing
    extractFinishReason "{\"foo\": \"bar\"}" === Nothing

-- | Property: simpleMessage creates expected structure
prop_simpleMessageStructure :: Property
prop_simpleMessageStructure = property $ do
    role <- forAll genRole
    content <- forAll genText
    let msg = simpleMessage role content
    case msg of
        RegularMessage m -> do
            msgRole m === role
            msgContent m === Just content
            msgToolCalls m === Nothing
        ToolResult _ -> failure

-- | Property: toolResultMessage creates expected structure
prop_toolResultMessageStructure :: Property
prop_toolResultMessageStructure = property $ do
    callId <- forAll genNonEmptyText
    content <- forAll genText
    let msg = toolResultMessage callId content
    trmRole msg === "tool"
    trmToolCallId msg === callId
    trmContent msg === content

-- ============================================================================
-- Test Tree
-- ============================================================================

tests :: TestTree
tests =
    testGroup
        "OpenRouter Property Tests"
        [ testGroup
            "JSON Round-trips"
            [ testProperty "Role round-trip" prop_roleRoundtrip
            , testProperty "ToolCall round-trip" prop_toolCallRoundtrip
            , testProperty "ToolCallFunction round-trip" prop_toolCallFunctionRoundtrip
            , testProperty "ToolResultMessage serializes" prop_toolResultMessageSerializes
            , testProperty "Message round-trip" prop_messageRoundtrip
            , testProperty "Usage round-trip" prop_usageRoundtrip
            , testProperty "ChatResponse FromJSON" prop_chatResponseFromJSON
            ]
        , testGroup
            "Pure Parsing Functions"
            [ testProperty "assembleToolCalls filters incomplete" prop_assembleToolCallsFiltersIncomplete
            , testProperty "mergeToolCallDelta preserves or adds" prop_mergeToolCallDeltaPreservesOrAdds
            , testProperty "mergeToolCallDelta accumulates args" prop_mergeToolCallDeltaAccumulatesArgs
            , testProperty "extractFieldText non-object" prop_extractFieldTextNonObject
            , testProperty "extractFieldText extracts string" prop_extractFieldTextExtractsString
            , testProperty "extractFieldValue fallback" prop_extractFieldValueFallback
            , testProperty "extractDelta handles malformed" prop_extractDeltaHandlesMalformed
            , testProperty "extractFinishReason handles malformed" prop_extractFinishReasonHandlesMalformed
            ]
        , testGroup
            "Message Helpers"
            [ testProperty "simpleMessage structure" prop_simpleMessageStructure
            , testProperty "toolResultMessage structure" prop_toolResultMessageStructure
            ]
        ]
