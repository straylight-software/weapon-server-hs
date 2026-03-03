{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.PromptAsyncProps
Description : Property tests for Prompt.Async module

Comprehensive property-based tests for async prompt job handling,
including payload construction, storage key generation, and status constants.
-}
module Property.PromptAsyncProps where

import Api (CreateMessageInput (..), PartInput (..))
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prompt.Async qualified as PromptAsync
import Test.Tasty
import Test.Tasty.Hedgehog

-- | Property: promptAsyncKey produces correct key structure
prop_promptAsyncKey :: Property
prop_promptAsyncKey = property $ do
    sid <- forAll genNonEmptyText
    reqId <- forAll genNonEmptyText
    PromptAsync.promptAsyncKey sid reqId === ["prompt_async", sid, reqId]

-- | Property: promptAsyncIndexKey produces correct key structure
prop_promptAsyncIndexKey :: Property
prop_promptAsyncIndexKey = property $ do
    sid <- forAll genNonEmptyText
    PromptAsync.promptAsyncIndexKey sid === ["prompt_async", sid, "index"]

-- | Property: queued payload contains all required fields
prop_queuedPayloadFields :: Property
prop_queuedPayloadFields = property $ do
    sid <- forAll genNonEmptyText
    reqId <- forAll genNonEmptyText
    parts <- forAll $ Gen.list (Range.linear 0 5) genPart
    let payload = PromptAsync.queuedPayload sid reqId (CreateMessageInput Nothing (map PartInput parts) Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
    case payload of
        Object obj -> do
            lookupText "requestID" obj === Just reqId
            lookupText "sessionID" obj === Just sid
            lookupText "status" obj === Just PromptAsync.statusQueued
        _otherValue -> failure

-- | Property: completed payload includes messageID
prop_completedPayloadIncludesMessage :: Property
prop_completedPayloadIncludesMessage = property $ do
    sid <- forAll genNonEmptyText
    reqId <- forAll genNonEmptyText
    msgId <- forAll genNonEmptyText
    let payload = PromptAsync.completedPayload sid reqId msgId
    case payload of
        Object obj -> do
            lookupText "requestID" obj === Just reqId
            lookupText "sessionID" obj === Just sid
            lookupText "status" obj === Just PromptAsync.statusCompleted
            lookupText "messageID" obj === Just msgId
        _otherValue -> failure

-- | Property: started payload contains all required fields
prop_startedPayloadFields :: Property
prop_startedPayloadFields = property $ do
    sid <- forAll genNonEmptyText
    reqId <- forAll genNonEmptyText
    let payload = PromptAsync.startedPayload sid reqId
    case payload of
        Object obj -> do
            lookupText "requestID" obj === Just reqId
            lookupText "sessionID" obj === Just sid
            lookupText "status" obj === Just PromptAsync.statusStarted
        _otherValue -> failure

-- | Property: failed payload includes error message
prop_failedPayloadIncludesError :: Property
prop_failedPayloadIncludesError = property $ do
    sid <- forAll genNonEmptyText
    reqId <- forAll genNonEmptyText
    err <- forAll genNonEmptyText
    let payload = PromptAsync.failedPayload sid reqId err
    case payload of
        Object obj -> do
            lookupText "requestID" obj === Just reqId
            lookupText "sessionID" obj === Just sid
            lookupText "status" obj === Just PromptAsync.statusFailed
            lookupText "error" obj === Just err
        _otherValue -> failure

-- | Property: all status values are valid
prop_statusValuesValid :: Property
prop_statusValuesValid = property $ do
    sid <- forAll genNonEmptyText
    reqId <- forAll genNonEmptyText
    msgId <- forAll genNonEmptyText
    err <- forAll genNonEmptyText
    let payloads =
            [ PromptAsync.queuedPayload sid reqId (CreateMessageInput Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
            , PromptAsync.startedPayload sid reqId
            , PromptAsync.completedPayload sid reqId msgId
            , PromptAsync.failedPayload sid reqId err
            ]
    let statuses = map extractStatus payloads
    let validStatuses =
            [ PromptAsync.statusQueued
            , PromptAsync.statusStarted
            , PromptAsync.statusCompleted
            , PromptAsync.statusFailed
            ]
    assert $ all (`elem` validStatuses) statuses
  where
    extractStatus = promptStatus

-- | Property: lifecycle progresses in correct order
prop_lifecycleOrder :: Property
prop_lifecycleOrder = property $ do
    sid <- forAll genNonEmptyText
    reqId <- forAll genNonEmptyText
    msgId <- forAll genNonEmptyText
    let payloads =
            [ PromptAsync.queuedPayload sid reqId (CreateMessageInput Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
            , PromptAsync.startedPayload sid reqId
            , PromptAsync.completedPayload sid reqId msgId
            ]
    let statuses = map promptStatus payloads
    statuses
        === [ PromptAsync.statusQueued
            , PromptAsync.statusStarted
            , PromptAsync.statusCompleted
            ]

-- | Property: status constants are distinct
prop_statusConstantsDistinct :: Property
prop_statusConstantsDistinct = property $ do
    let statuses =
            [ PromptAsync.statusQueued
            , PromptAsync.statusStarted
            , PromptAsync.statusCompleted
            , PromptAsync.statusFailed
            ]
    -- All statuses should be unique (exactly 4 distinct values)
    let uniqueStatuses = unique statuses
    -- Using explicit count check for finite list
    case (statuses, uniqueStatuses) of
        ([_, _, _, _], [_, _, _, _]) -> success
        _otherCounts -> failure
  where
    unique :: (Eq a) => [a] -> [a]
    unique [] = []
    unique (x : xs) = x : unique (filter (/= x) xs)

-- | Property: keys with different session IDs are different
prop_keysUniqueBySession :: Property
prop_keysUniqueBySession = property $ do
    sid1 <- forAll genNonEmptyText
    sid2 <- forAll $ Gen.filter (/= sid1) genNonEmptyText
    reqId <- forAll genNonEmptyText
    assert $ PromptAsync.promptAsyncKey sid1 reqId /= PromptAsync.promptAsyncKey sid2 reqId

-- | Property: keys with different request IDs are different
prop_keysUniqueByRequest :: Property
prop_keysUniqueByRequest = property $ do
    sid <- forAll genNonEmptyText
    reqId1 <- forAll genNonEmptyText
    reqId2 <- forAll $ Gen.filter (/= reqId1) genNonEmptyText
    assert $ PromptAsync.promptAsyncKey sid reqId1 /= PromptAsync.promptAsyncKey sid reqId2

-- | Property: index key is different from job key
prop_indexKeyDifferentFromJobKey :: Property
prop_indexKeyDifferentFromJobKey = property $ do
    sid <- forAll genNonEmptyText
    reqId <- forAll genNonEmptyText
    assert $ PromptAsync.promptAsyncIndexKey sid /= PromptAsync.promptAsyncKey sid reqId

-- | Property: payloads always produce valid JSON objects
prop_payloadsAreObjects :: Property
prop_payloadsAreObjects = property $ do
    sid <- forAll genNonEmptyText
    reqId <- forAll genNonEmptyText
    msgId <- forAll genNonEmptyText
    err <- forAll genNonEmptyText
    parts <- forAll $ Gen.list (Range.linear 0 3) genPart
    let payloads =
            [ PromptAsync.queuedPayload sid reqId (CreateMessageInput Nothing (map PartInput parts) Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
            , PromptAsync.startedPayload sid reqId
            , PromptAsync.completedPayload sid reqId msgId
            , PromptAsync.failedPayload sid reqId err
            ]
    -- All payloads should be JSON objects
    assert $ all isObject payloads
  where
    isObject (Object _) = True
    isObject _ = False

-- * Helper Functions

-- | Extract status from a payload
promptStatus :: Value -> Text
promptStatus payload = case payload of
    Object obj -> fromMaybe "" (lookupText "status" obj)
    _otherValue -> ""

-- | Look up a text field in a JSON object
lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText key obj = case KM.lookup (Key.fromText key) obj of
    Just (String txt) -> Just txt
    Just _otherValue -> Nothing
    Nothing -> Nothing

-- * Generators

-- | Generate a JSON value representing a message part
genPart :: Gen Value
genPart = do
    content <- Gen.text (Range.linear 0 20) Gen.alphaNum
    Gen.element
        [ object ["type" .= ("text" :: Text), "text" .= content]
        , object ["type" .= ("code" :: Text), "code" .= content]
        ]

-- | Generate non-empty text for IDs
genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 30) Gen.alphaNum

-- * Test Tree

-- | All property tests for Prompt.Async
tests :: TestTree
tests =
    testGroup
        "Prompt Async Property Tests"
        [ testGroup
            "Storage Keys"
            [ testProperty "promptAsyncKey structure" prop_promptAsyncKey
            , testProperty "promptAsyncIndexKey structure" prop_promptAsyncIndexKey
            , testProperty "keys unique by session" prop_keysUniqueBySession
            , testProperty "keys unique by request" prop_keysUniqueByRequest
            , testProperty "index key differs from job key" prop_indexKeyDifferentFromJobKey
            ]
        , testGroup
            "Payload Construction"
            [ testProperty "queued payload fields" prop_queuedPayloadFields
            , testProperty "started payload fields" prop_startedPayloadFields
            , testProperty "completed payload includes message" prop_completedPayloadIncludesMessage
            , testProperty "failed payload includes error" prop_failedPayloadIncludesError
            , testProperty "payloads are JSON objects" prop_payloadsAreObjects
            ]
        , testGroup
            "Status Values"
            [ testProperty "status values valid" prop_statusValuesValid
            , testProperty "status constants distinct" prop_statusConstantsDistinct
            , testProperty "lifecycle order" prop_lifecycleOrder
            ]
        ]
