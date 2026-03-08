{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.OpenRouterHistoryProps
Description : Adversarial property tests for OpenRouter history conversion
-}
module Property.OpenRouterHistoryProps where

import Api (
    AssistantMessageInfo (..),
    Message (..),
    MessageInfo (..),
    MessagePath (..),
    MessageTime (..),
    MessageTokens (..),
    ModelSelection (..),
    TokenCache (..),
    UserMessageInfo (..),
 )
import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LLM.OpenRouter qualified as OpenRouter
import LLM.OpenRouter.History qualified as ORHistory
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests =
    testGroup
        "OpenRouter History Property Tests"
        [ testProperty "assistant tool parts yield tool_calls and tool_results" prop_assistant_tool_parts_yield_calls_and_results
        , testProperty "assistant tool results ignored for running status" prop_tool_running_ignored
        , testProperty "assistant tool error yields tool_result from error" prop_tool_error_yields_result
        , testProperty "tool call arguments roundtrip JSON" prop_tool_call_args_roundtrip
        , testProperty "missing tool/callID yields no tool_calls" prop_missing_fields_no_tool_calls
        , testProperty "assistant with no text and no tools yields empty history" prop_assistant_empty_yields_empty
        , testProperty "user message ignores tool parts" prop_user_ignores_tool_parts
        , testProperty "user message includes only text parts" prop_user_includes_text_only
        , testProperty "assistant message preserves text content" prop_assistant_preserves_text
        , testProperty "user message includes file/image parts" prop_user_includes_file_image_parts
        , testProperty "user content parts preserve order" prop_user_content_preserves_order
        , testProperty "tool results are truncated (completed)" prop_tool_result_truncates_output
        , testProperty "tool results are truncated (error)" prop_tool_error_truncates_output
        , testProperty "only completed/error tool parts yield tool_results" prop_only_terminal_tool_results
        , testProperty "deep inputs and outputs survive JSON roundtrip" prop_deep_values_roundtrip
        ]

prop_assistant_tool_parts_yield_calls_and_results :: Property
prop_assistant_tool_parts_yield_calls_and_results = property $ do
    callId <- forAll genNonEmptyText
    toolName <- forAll genNonEmptyText
    inputVal <- forAll genValue
    outputVal <- forAll genValue
    text <- forAll genText
    let parts = [textPart text, toolPartCompleted callId toolName inputVal outputVal]
    let msgs = ORHistory.messageToOpenRouterWith id (mkAssistantMessage parts)
    let (calls, results) = extractCallsAndResults msgs
    case calls of
        [tc] -> do
            OpenRouter.tcId tc === callId
            OpenRouter.tcfName (OpenRouter.tcFunction tc) === toolName
        _other -> failure
    case results of
        [tr] -> do
            OpenRouter.trmToolCallId tr === callId
            OpenRouter.trmContent tr === valueText outputVal
        _other -> failure

prop_tool_running_ignored :: Property
prop_tool_running_ignored = property $ do
    callId <- forAll genNonEmptyText
    toolName <- forAll genNonEmptyText
    inputVal <- forAll genValue
    let parts = [toolPartWithStatus "running" callId toolName inputVal (String "ignored")]
    let msgs = ORHistory.messageToOpenRouterWith id (mkAssistantMessage parts)
    let (_calls, results) = extractCallsAndResults msgs
    results === []

prop_tool_error_yields_result :: Property
prop_tool_error_yields_result = property $ do
    callId <- forAll genNonEmptyText
    toolName <- forAll genNonEmptyText
    inputVal <- forAll genValue
    errVal <- forAll genValue
    let parts = [toolPartError callId toolName inputVal errVal]
    let msgs = ORHistory.messageToOpenRouterWith id (mkAssistantMessage parts)
    let (_calls, results) = extractCallsAndResults msgs
    case results of
        [tr] -> OpenRouter.trmContent tr === valueText errVal
        _other -> failure

prop_tool_call_args_roundtrip :: Property
prop_tool_call_args_roundtrip = property $ do
    callId <- forAll genNonEmptyText
    toolName <- forAll genNonEmptyText
    inputVal <- forAll genValue
    let parts = [toolPartCompleted callId toolName inputVal (String "ok")]
    let msgs = ORHistory.messageToOpenRouterWith id (mkAssistantMessage parts)
    let (calls, _results) = extractCallsAndResults msgs
    case calls of
        [tc] -> do
            let argText = OpenRouter.tcfArguments (OpenRouter.tcFunction tc)
            let decoded = decodeStrict (TE.encodeUtf8 argText) :: Maybe Value
            decoded === Just inputVal
        _other -> failure

prop_missing_fields_no_tool_calls :: Property
prop_missing_fields_no_tool_calls = property $ do
    inputVal <- forAll genValue
    let parts =
            [ object ["type" .= ("tool" :: Text)]
            , object ["type" .= ("tool" :: Text), "tool" .= ("x" :: Text)]
            , object ["type" .= ("tool" :: Text), "callID" .= ("c1" :: Text)]
            , toolPartWithStatus "completed" "" "" inputVal (String "ok")
            ]
    let msgs = ORHistory.messageToOpenRouterWith id (mkAssistantMessage parts)
    let (calls, _results) = extractCallsAndResults msgs
    calls === []

prop_assistant_empty_yields_empty :: Property
prop_assistant_empty_yields_empty = property $ do
    let parts = [object ["type" .= ("file" :: Text), "path" .= ("x" :: Text)]]
    let msgs = ORHistory.messageToOpenRouterWith id (mkAssistantMessage parts)
    msgs === []

prop_user_ignores_tool_parts :: Property
prop_user_ignores_tool_parts = property $ do
    callId <- forAll genNonEmptyText
    toolName <- forAll genNonEmptyText
    inputVal <- forAll genValue
    let parts = [toolPartCompleted callId toolName inputVal (String "ok")]
    let msgs = ORHistory.messageToOpenRouterWith id (mkUserMessage parts)
    case msgs of
        [] -> success
        [OpenRouter.RegularMessage m] -> do
            OpenRouter.msgRole m === OpenRouter.User
            OpenRouter.msgToolCalls m === Nothing
        _other -> failure

prop_user_includes_text_only :: Property
prop_user_includes_text_only = property $ do
    text1 <- forAll genText
    text2 <- forAll genText
    let parts =
            [ textPart text1
            , object ["type" .= ("tool" :: Text), "tool" .= ("x" :: Text), "callID" .= ("c1" :: Text)]
            , textPart text2
            ]
    let msgs = ORHistory.messageToOpenRouterWith id (mkUserMessage parts)
    case msgs of
        [OpenRouter.RegularMessage m] -> OpenRouter.msgContent m === Just (OpenRouter.ContentText (text1 <> "\n" <> text2))
        _other -> failure

prop_assistant_preserves_text :: Property
prop_assistant_preserves_text = property $ do
    text1 <- forAll genText
    text2 <- forAll genText
    let parts = [textPart text1, textPart text2]
    let msgs = ORHistory.messageToOpenRouterWith id (mkAssistantMessage parts)
    case msgs of
        [OpenRouter.RegularMessage m] -> OpenRouter.msgContent m === Just (OpenRouter.ContentText (text1 <> "\n" <> text2))
        _other -> failure

prop_user_includes_file_image_parts :: Property
prop_user_includes_file_image_parts = property $ do
    text1 <- forAll genText
    text2 <- forAll genText
    fileUrl <- forAll genNonEmptyText
    imageUrl <- forAll genNonEmptyText
    fileName <- forAll genNonEmptyText
    let parts =
            [ textPart text1
            , filePart "application/pdf" fileUrl (Just fileName)
            , filePart "image/png" imageUrl Nothing
            , textPart text2
            ]
    let msgs = ORHistory.messageToOpenRouterWith id (mkUserMessage parts)
    case msgs of
        [OpenRouter.RegularMessage m] -> case OpenRouter.msgContent m of
            Just (OpenRouter.ContentParts parts') -> do
                assert (OpenRouter.ContentPartText text1 `elem` parts')
                assert (OpenRouter.ContentPartText text2 `elem` parts')
                assert (OpenRouter.ContentPartFile fileUrl "application/pdf" (Just fileName) `elem` parts')
                assert (OpenRouter.ContentPartImageUrl imageUrl `elem` parts')
            _other -> failure
        _other -> failure

prop_user_content_preserves_order :: Property
prop_user_content_preserves_order = property $ do
    text1 <- forAll genText
    text2 <- forAll genText
    fileUrl <- forAll genNonEmptyText
    imageUrl <- forAll genNonEmptyText
    fileName <- forAll genNonEmptyText
    callId <- forAll genNonEmptyText
    toolName <- forAll genNonEmptyText
    let parts =
            [ textPart text1
            , filePart "application/pdf" fileUrl (Just fileName)
            , filePart "image/png" imageUrl Nothing
            , object ["type" .= ("tool" :: Text), "tool" .= toolName, "callID" .= callId]
            , textPart text2
            ]
    let msgs = ORHistory.messageToOpenRouterWith id (mkUserMessage parts)
    case msgs of
        [OpenRouter.RegularMessage m] -> case OpenRouter.msgContent m of
            Just (OpenRouter.ContentParts parts') ->
                parts'
                    === [ OpenRouter.ContentPartText text1
                        , OpenRouter.ContentPartFile fileUrl "application/pdf" (Just fileName)
                        , OpenRouter.ContentPartImageUrl imageUrl
                        , OpenRouter.ContentPartText text2
                        ]
            _other -> failure
        _other -> failure

prop_tool_result_truncates_output :: Property
prop_tool_result_truncates_output = property $ do
    callId <- forAll genNonEmptyText
    toolName <- forAll genNonEmptyText
    inputVal <- forAll genValue
    outputVal <- forAll genValue
    let trunc = const "TRUNCATED"
    let parts = [toolPartCompleted callId toolName inputVal outputVal]
    let msgs = ORHistory.messageToOpenRouterWith trunc (mkAssistantMessage parts)
    let (_calls, results) = extractCallsAndResults msgs
    case results of
        [tr] -> OpenRouter.trmContent tr === "TRUNCATED"
        _other -> failure

prop_tool_error_truncates_output :: Property
prop_tool_error_truncates_output = property $ do
    callId <- forAll genNonEmptyText
    toolName <- forAll genNonEmptyText
    inputVal <- forAll genValue
    errVal <- forAll genValue
    let trunc = const "TRUNCATED"
    let parts = [toolPartError callId toolName inputVal errVal]
    let msgs = ORHistory.messageToOpenRouterWith trunc (mkAssistantMessage parts)
    let (_calls, results) = extractCallsAndResults msgs
    case results of
        [tr] -> OpenRouter.trmContent tr === "TRUNCATED"
        _other -> failure

prop_only_terminal_tool_results :: Property
prop_only_terminal_tool_results = property $ do
    callId <- forAll genNonEmptyText
    toolName <- forAll genNonEmptyText
    inputVal <- forAll genValueDeep
    outputVal <- forAll genValueDeep
    errVal <- forAll genValueDeep
    status <- forAll $ Gen.element ["running", "completed", "error", "queued", "unknown"]
    let part =
            if status == "error"
                then toolPartError callId toolName inputVal errVal
                else toolPartWithStatus status callId toolName inputVal outputVal
    let msgs = ORHistory.messageToOpenRouterWith id (mkAssistantMessage [part])
    let (_calls, results) = extractCallsAndResults msgs
    case status of
        "completed" -> assert (not (null results))
        "error" -> assert (not (null results))
        _other -> results === []

prop_deep_values_roundtrip :: Property
prop_deep_values_roundtrip = property $ do
    callId <- forAll genNonEmptyText
    toolName <- forAll genNonEmptyText
    inputVal <- forAll genValueDeep
    outputVal <- forAll genValueDeep
    let parts = [toolPartCompleted callId toolName inputVal outputVal]
    let msgs = ORHistory.messageToOpenRouterWith id (mkAssistantMessage parts)
    let (calls, results) = extractCallsAndResults msgs
    case calls of
        [tc] -> do
            let argText = OpenRouter.tcfArguments (OpenRouter.tcFunction tc)
            let decoded = decodeStrict (TE.encodeUtf8 argText) :: Maybe Value
            decoded === Just inputVal
        _other -> failure
    case results of
        [tr] -> do
            let outText = OpenRouter.trmContent tr
            case outputVal of
                String s -> outText === s
                _other -> do
                    let decodedOut = decodeStrict (TE.encodeUtf8 outText) :: Maybe Value
                    decodedOut === Just outputVal
        _other -> failure

-- Helpers

mkUserMessage :: [Value] -> Message
mkUserMessage parts =
    Message
        { msgInfo =
            UserInfo
                UserMessageInfo
                    { umiId = "msg_user"
                    , umiSessionId = "sess"
                    , umiTime = MessageTime 0 Nothing
                    , umiAgent = "agent"
                    , umiModel = ModelSelection "openrouter" "test"
                    }
        , msgParts = parts
        }

mkAssistantMessage :: [Value] -> Message
mkAssistantMessage parts =
    Message
        { msgInfo =
            AssistantInfo
                AssistantMessageInfo
                    { amiId = "msg_assistant"
                    , amiSessionId = "sess"
                    , amiTime = MessageTime 0 Nothing
                    , amiParentId = "msg_user"
                    , amiModelId = "test"
                    , amiProviderId = "openrouter"
                    , amiMode = "normal"
                    , amiAgent = "agent"
                    , amiPath = MessagePath "/tmp" "/tmp"
                    , amiCost = 0
                    , amiTokens = MessageTokens Nothing 0 0 0 (TokenCache 0 0)
                    , amiSummary = Nothing
                    , amiVariant = Nothing
                    , amiFinish = Nothing
                    , amiError = Nothing
                    , amiStructured = Nothing
                    }
        , msgParts = parts
        }

textPart :: Text -> Value
textPart txt = object ["type" .= ("text" :: Text), "text" .= txt]

toolPartCompleted :: Text -> Text -> Value -> Value -> Value
toolPartCompleted = toolPartWithStatus "completed"

toolPartError :: Text -> Text -> Value -> Value -> Value
toolPartError callId toolName inputVal errVal =
    object
        [ "type" .= ("tool" :: Text)
        , "tool" .= toolName
        , "callID" .= callId
        , "state"
            .= object
                [ "status" .= ("error" :: Text)
                , "input" .= inputVal
                , "error" .= errVal
                ]
        ]

toolPartWithStatus :: Text -> Text -> Text -> Value -> Value -> Value
toolPartWithStatus status callId toolName inputVal outputVal =
    object
        [ "type" .= ("tool" :: Text)
        , "tool" .= toolName
        , "callID" .= callId
        , "state"
            .= object
                [ "status" .= status
                , "input" .= inputVal
                , "output" .= outputVal
                ]
        ]

filePart :: Text -> Text -> Maybe Text -> Value
filePart mime url filename =
    object
        [ "type" .= ("file" :: Text)
        , "mime" .= mime
        , "url" .= url
        , "filename" .= filename
        ]

extractCallsAndResults :: [OpenRouter.ChatMessage] -> ([OpenRouter.ToolCall], [OpenRouter.ToolResultMessage])
extractCallsAndResults = foldr go ([], [])
  where
    go msg (calls, results) =
        case msg of
            OpenRouter.RegularMessage m ->
                (maybe calls (++ calls) (OpenRouter.msgToolCalls m), results)
            OpenRouter.ToolResult tr ->
                (calls, tr : results)

valueText :: Value -> Text
valueText (String s) = s
valueText v = TE.decodeUtf8 (BSL.toStrict (encode v))

-- Generators

genText :: Gen Text
genText = Gen.text (Range.linear 0 40) Gen.unicode

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 40) Gen.alphaNum

genValue :: Gen Value
genValue =
    Gen.choice
        [ genSimpleValue
        , Array . V.fromList <$> Gen.list (Range.linear 0 5) genSimpleValue
        , Object <$> genObject
        ]
  where
    genSimpleValue =
        Gen.choice
            [ pure Null
            , Bool <$> Gen.bool
            , Number . fromIntegral <$> Gen.int (Range.linear (-1000) 1000)
            , String <$> Gen.text (Range.linear 0 40) Gen.unicode
            ]
    genObject = do
        pairs <- Gen.list (Range.linear 0 5) $ do
            key <- Gen.text (Range.linear 1 10) Gen.lower
            val <- genSimpleValue
            pure (K.fromText key, val)
        pure $ KM.fromList pairs

genValueDeep :: Gen Value
genValueDeep = genValueSized 3

genValueSized :: Int -> Gen Value
genValueSized depth =
    if depth <= 0
        then genSimpleValue
        else
            Gen.choice
                [ genSimpleValue
                , Array . V.fromList <$> Gen.list (Range.linear 0 6) (genValueSized (depth - 1))
                , Object <$> genObjectSized (depth - 1)
                ]
  where
    genSimpleValue =
        Gen.choice
            [ pure Null
            , Bool <$> Gen.bool
            , Number . fromIntegral <$> Gen.int (Range.linear (-100000) 100000)
            , String <$> Gen.text (Range.linear 0 200) Gen.unicode
            ]
    genObjectSized nextDepth = do
        pairs <- Gen.list (Range.linear 0 6) $ do
            key <- Gen.text (Range.linear 1 12) Gen.lower
            val <- genValueSized nextDepth
            pure (K.fromText key, val)
        pure $ KM.fromList pairs
