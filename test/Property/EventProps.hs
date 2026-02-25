{-# LANGUAGE OverloadedStrings #-}

{- | Event property tests for Global.Event module

Tests the SSE event formatting functions to ensure they produce
correctly structured GlobalEvent and Event payloads per the OpenAPI spec.
-}
module Property.EventProps where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as Text
import Global.Event (
    formatSSEMessage,
    heartbeatIntervalMicros,
    mkRawEvent,
    serverConnectedRaw,
    serverHeartbeatRaw,
    sseHeaders,
    wrapEventPayload,
    wrapGlobalEvent,
 )
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- | Helper to fail with annotation on unexpected Value types
failOnNonObject :: (MonadTest m) => String -> Value -> m ()
failOnNonObject context val = do
    annotate $ context ++ ": expected Object but got " ++ valueType val
    failure
  where
    valueType :: Value -> String
    valueType (Object _) = "Object"
    valueType (Array _) = "Array"
    valueType (String _) = "String"
    valueType (Number _) = "Number"
    valueType (Bool _) = "Bool"
    valueType Null = "Null"

-- | Helper to fail with annotation on unexpected Maybe types
failOnNothing :: (MonadTest m) => String -> m ()
failOnNothing context = do
    annotate $ context ++ ": expected Just but got Nothing"
    failure

-- ═══════════════════════════════════════════════════════════════════════════
-- // wrapGlobalEvent properties //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Property: wrapGlobalEvent produces valid GlobalEvent structure
GlobalEvent = { directory: string, payload: { type: string, properties: object } }
-}
prop_wrapGlobalEvent_structure :: Property
prop_wrapGlobalEvent_structure = property $ do
    dir <- forAll genDirectory
    eventType <- forAll genEventType
    props <- forAll genProperties

    let result = wrapGlobalEvent dir eventType props

    -- Must be an object
    case result of
        Object obj -> do
            -- Must have "directory" field as string
            case KM.lookup "directory" obj of
                Just (String d) -> d === dir
                Just other -> do
                    annotate $ "directory field is not a string: " ++ show other
                    failure
                Nothing -> do
                    annotate "missing directory field"
                    failure

            -- Must have "payload" field as object
            case KM.lookup "payload" obj of
                Just (Object payload) -> do
                    -- Payload must have "type" field
                    case KM.lookup "type" payload of
                        Just (String t) -> t === eventType
                        Just other -> do
                            annotate $ "type field is not a string: " ++ show other
                            failure
                        Nothing -> do
                            annotate "missing type field in payload"
                            failure

                    -- Payload must have "properties" field
                    case KM.lookup "properties" payload of
                        Just p -> p === props
                        Nothing -> do
                            annotate "missing properties field in payload"
                            failure
                Just other -> do
                    annotate $ "payload field is not an object: " ++ show other
                    failure
                Nothing -> do
                    annotate "missing payload field"
                    failure
        Array _arr -> do
            annotate "wrapGlobalEvent returned an array, expected object"
            failure
        String _str -> do
            annotate "wrapGlobalEvent returned a string, expected object"
            failure
        Number _num -> do
            annotate "wrapGlobalEvent returned a number, expected object"
            failure
        Bool _b -> do
            annotate "wrapGlobalEvent returned a bool, expected object"
            failure
        Null -> do
            annotate "wrapGlobalEvent returned null, expected object"
            failure

-- | Property: wrapGlobalEvent is deterministic (same inputs -> same output)
prop_wrapGlobalEvent_deterministic :: Property
prop_wrapGlobalEvent_deterministic = property $ do
    dir <- forAll genDirectory
    eventType <- forAll genEventType
    props <- forAll genProperties

    let result1 = wrapGlobalEvent dir eventType props
    let result2 = wrapGlobalEvent dir eventType props
    result1 === result2

-- | Property: wrapGlobalEvent JSON encodes to valid JSON
prop_wrapGlobalEvent_encodable :: Property
prop_wrapGlobalEvent_encodable = property $ do
    dir <- forAll genDirectory
    eventType <- forAll genEventType
    props <- forAll genProperties

    let result = wrapGlobalEvent dir eventType props
    let encoded = encode result
    let decoded = decode encoded :: Maybe Value

    -- Must round-trip through JSON
    decoded === Just result

-- | Property: wrapGlobalEvent preserves directory exactly
prop_wrapGlobalEvent_preserves_directory :: Property
prop_wrapGlobalEvent_preserves_directory = property $ do
    dir <- forAll genDirectoryWithSpecialChars
    eventType <- forAll genEventType
    props <- forAll genProperties

    let result = wrapGlobalEvent dir eventType props

    case result of
        Object obj -> case KM.lookup "directory" obj of
            Just (String d) -> d === dir
            Just other -> failOnNonObject "directory field" other
            Nothing -> failOnNothing "directory field"
        other -> failOnNonObject "wrapGlobalEvent result" other

-- | Property: wrapGlobalEvent preserves event type exactly
prop_wrapGlobalEvent_preserves_type :: Property
prop_wrapGlobalEvent_preserves_type = property $ do
    dir <- forAll genDirectory
    eventType <- forAll genEventTypeWithDots
    props <- forAll genProperties

    let result = wrapGlobalEvent dir eventType props

    case result of
        Object obj -> case KM.lookup "payload" obj of
            Just (Object payload) -> case KM.lookup "type" payload of
                Just (String t) -> t === eventType
                Just other -> failOnNonObject "type field" other
                Nothing -> failOnNothing "type field"
            Just other -> failOnNonObject "payload field" other
            Nothing -> failOnNothing "payload field"
        other -> failOnNonObject "wrapGlobalEvent result" other

-- | Property: wrapGlobalEvent handles empty properties
prop_wrapGlobalEvent_empty_properties :: Property
prop_wrapGlobalEvent_empty_properties = property $ do
    dir <- forAll genDirectory
    eventType <- forAll genEventType

    let result = wrapGlobalEvent dir eventType (object [])

    case result of
        Object obj -> case KM.lookup "payload" obj of
            Just (Object payload) -> case KM.lookup "properties" payload of
                Just (Object props) -> KM.null props === True
                Just other -> failOnNonObject "properties field" other
                Nothing -> failOnNothing "properties field"
            Just other -> failOnNonObject "payload field" other
            Nothing -> failOnNothing "payload field"
        other -> failOnNonObject "wrapGlobalEvent result" other

-- | Property: wrapGlobalEvent handles complex nested properties
prop_wrapGlobalEvent_nested_properties :: Property
prop_wrapGlobalEvent_nested_properties = property $ do
    dir <- forAll genDirectory
    eventType <- forAll genEventType
    props <- forAll genNestedProperties

    let result = wrapGlobalEvent dir eventType props

    case result of
        Object obj -> case KM.lookup "payload" obj of
            Just (Object payload) -> case KM.lookup "properties" payload of
                Just p -> p === props
                Nothing -> failOnNothing "properties field"
            Just other -> failOnNonObject "payload field" other
            Nothing -> failOnNothing "payload field"
        other -> failOnNonObject "wrapGlobalEvent result" other

-- | Property: Different directories produce different outputs
prop_wrapGlobalEvent_different_directories :: Property
prop_wrapGlobalEvent_different_directories = property $ do
    dir1 <- forAll genDirectory
    dir2 <- forAll $ Gen.filter (/= dir1) genDirectory
    eventType <- forAll genEventType
    props <- forAll genProperties

    let result1 = wrapGlobalEvent dir1 eventType props
    let result2 = wrapGlobalEvent dir2 eventType props

    assert $ result1 /= result2

-- | Property: Different event types produce different outputs
prop_wrapGlobalEvent_different_types :: Property
prop_wrapGlobalEvent_different_types = property $ do
    dir <- forAll genDirectory
    eventType1 <- forAll genEventType
    eventType2 <- forAll $ Gen.filter (/= eventType1) genEventType
    props <- forAll genProperties

    let result1 = wrapGlobalEvent dir eventType1 props
    let result2 = wrapGlobalEvent dir eventType2 props

    assert $ result1 /= result2

-- ═══════════════════════════════════════════════════════════════════════════
-- // Generators //
-- ═══════════════════════════════════════════════════════════════════════════

genDirectory :: Gen Text
genDirectory =
    Gen.element
        [ "/home/user/project"
        , "/tmp/test"
        , "/var/data"
        , "/opt/app"
        , "/Users/dev/code"
        , "/home/luke/Projects/weapon-server-hs"
        ]

genDirectoryWithSpecialChars :: Gen Text
genDirectoryWithSpecialChars =
    Gen.choice
        [ genDirectory
        , Gen.element
            [ "/home/user/my project"
            , "/tmp/тест"
            , "/home/user/项目"
            , "/path/with spaces/and-dashes"
            , "/path/with'quotes"
            ]
        ]

genEventType :: Gen Text
genEventType =
    Gen.element
        [ "server.connected"
        , "server.heartbeat"
        , "session.created"
        , "session.updated"
        , "session.deleted"
        , "message.updated"
        , "message.removed"
        , "message.part.updated"
        , "message.part.removed"
        , "permission.updated"
        , "permission.replied"
        , "session.status"
        , "session.idle"
        , "lsp.updated"
        , "project.updated"
        , "global.disposed"
        ]

genEventTypeWithDots :: Gen Text
genEventTypeWithDots = do
    parts <-
        Gen.list (Range.linear 1 4) $
            Gen.text (Range.linear 1 10) Gen.lower
    pure $ Text.intercalate "." parts

genProperties :: Gen Value
genProperties =
    Gen.choice
        [ pure $ object []
        , genSimpleProperties
        , genSessionProperties
        , genMessageProperties
        ]

genSimpleProperties :: Gen Value
genSimpleProperties = do
    sessionId <- genUUID
    pure $
        object
            [ "sessionID" .= sessionId
            ]

genSessionProperties :: Gen Value
genSessionProperties = do
    sessionId <- genUUID
    status <- Gen.element ["idle" :: Text, "running", "pending", "complete"]
    pure $
        object
            [ "sessionID" .= sessionId
            , "status" .= status
            ]

genMessageProperties :: Gen Value
genMessageProperties = do
    messageId <- genUUID
    partId <- genUUID
    role <- Gen.element ["user" :: Text, "assistant", "system"]
    pure $
        object
            [ "message"
                .= object
                    [ "id" .= messageId
                    , "role" .= role
                    ]
            , "part"
                .= object
                    [ "id" .= partId
                    , "messageID" .= messageId
                    ]
            ]

genNestedProperties :: Gen Value
genNestedProperties = do
    sessionId <- genUUID
    messageId <- genUUID
    content <- Gen.text (Range.linear 0 100) Gen.unicode
    pure $
        object
            [ "session"
                .= object
                    [ "id" .= sessionId
                    , "messages"
                        .= [ object
                                [ "id" .= messageId
                                , "content" .= content
                                , "metadata"
                                    .= object
                                        [ "timestamp" .= (1234567890 :: Int)
                                        , "source" .= ("user" :: Text)
                                        ]
                                ]
                           ]
                    ]
            ]

genUUID :: Gen Text
genUUID = do
    -- Generate a simplified UUID-like string
    parts <-
        sequence
            [ Gen.text (Range.singleton 8) Gen.hexit
            , Gen.text (Range.singleton 4) Gen.hexit
            , Gen.text (Range.singleton 4) Gen.hexit
            , Gen.text (Range.singleton 4) Gen.hexit
            , Gen.text (Range.singleton 12) Gen.hexit
            ]
    pure $ Text.intercalate "-" parts

-- ═══════════════════════════════════════════════════════════════════════════
-- // wrapEventPayload properties //
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: wrapEventPayload produces valid structure with directory and payload
prop_wrapEventPayload_structure :: Property
prop_wrapEventPayload_structure = property $ do
    dir <- forAll genDirectory
    payload <- forAll genProperties

    let result = wrapEventPayload dir payload

    case result of
        Object obj -> do
            -- Must have "directory" field
            case KM.lookup "directory" obj of
                Just (String d) -> d === dir
                Just other -> do
                    annotate $ "directory field is not a string: " ++ show other
                    failure
                Nothing -> do
                    annotate "missing directory field"
                    failure

            -- Must have "payload" field equal to input
            case KM.lookup "payload" obj of
                Just p -> p === payload
                Nothing -> do
                    annotate "missing payload field"
                    failure
        other -> failOnNonObject "wrapEventPayload result" other

{- | Property: wrapEventPayload is consistent with wrapGlobalEvent
When we construct the inner payload ourselves, both should produce the same result.
-}
prop_wrapEventPayload_consistent_with_wrapGlobalEvent :: Property
prop_wrapEventPayload_consistent_with_wrapGlobalEvent = property $ do
    dir <- forAll genDirectory
    eventType <- forAll genEventType
    props <- forAll genProperties

    let innerPayload =
            object
                [ "type" .= eventType
                , "properties" .= props
                ]
    let result1 = wrapEventPayload dir innerPayload
    let result2 = wrapGlobalEvent dir eventType props

    result1 === result2

-- ═══════════════════════════════════════════════════════════════════════════
-- // mkRawEvent properties //
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: mkRawEvent produces valid raw event structure
prop_mkRawEvent_structure :: Property
prop_mkRawEvent_structure = property $ do
    eventType <- forAll genEventType
    props <- forAll genProperties

    let result = mkRawEvent eventType props

    case result of
        Object obj -> do
            -- Must have "type" field
            case KM.lookup "type" obj of
                Just (String t) -> t === eventType
                Just other -> do
                    annotate $ "type field is not a string: " ++ show other
                    failure
                Nothing -> do
                    annotate "missing type field"
                    failure

            -- Must have "properties" field
            case KM.lookup "properties" obj of
                Just p -> p === props
                Nothing -> do
                    annotate "missing properties field"
                    failure
        other -> failOnNonObject "mkRawEvent result" other

-- | Property: mkRawEvent is deterministic
prop_mkRawEvent_deterministic :: Property
prop_mkRawEvent_deterministic = property $ do
    eventType <- forAll genEventType
    props <- forAll genProperties

    let result1 = mkRawEvent eventType props
    let result2 = mkRawEvent eventType props
    result1 === result2

-- | Property: mkRawEvent encodes to valid JSON
prop_mkRawEvent_encodable :: Property
prop_mkRawEvent_encodable = property $ do
    eventType <- forAll genEventType
    props <- forAll genProperties

    let result = mkRawEvent eventType props
    let encoded = encode result
    let decoded = decode encoded :: Maybe Value

    decoded === Just result

-- ═══════════════════════════════════════════════════════════════════════════
-- // formatSSEMessage properties //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Property: formatSSEMessage produces valid SSE wire format
Format: "data: <json>\n\n"
-}
prop_formatSSEMessage_format :: Property
prop_formatSSEMessage_format = property $ do
    eventType <- forAll genEventType
    props <- forAll genProperties

    let event = mkRawEvent eventType props
    let jsonBytes = encode event
    let builders = formatSSEMessage jsonBytes
    let result = BSL.toStrict $ mconcat $ map toLazyByteString builders

    -- Must start with "data: "
    assert $ "data: " `BSL.isPrefixOf` BSL.fromStrict result
    -- Must end with "\n\n"
    assert $ "\n\n" `BSL.isSuffixOf` BSL.fromStrict result

-- | Property: formatSSEMessage preserves JSON content
prop_formatSSEMessage_preserves_content :: Property
prop_formatSSEMessage_preserves_content = property $ do
    eventType <- forAll genEventType
    props <- forAll genProperties

    let event = mkRawEvent eventType props
    let jsonBytes = encode event
    let builders = formatSSEMessage jsonBytes
    let result = BSL.toStrict $ mconcat $ map toLazyByteString builders

    -- Extract JSON from SSE format (remove "data: " prefix and "\n\n" suffix)
    let withoutPrefix = BSL.drop 6 (BSL.fromStrict result) -- "data: " is 6 bytes
    let withoutSuffix = BSL.take (BSL.length withoutPrefix - 2) withoutPrefix -- "\n\n" is 2 bytes
    withoutSuffix === jsonBytes

-- ═══════════════════════════════════════════════════════════════════════════
-- // Pre-built event constants //
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: serverConnectedRaw is valid JSON
prop_serverConnectedRaw_valid_json :: Property
prop_serverConnectedRaw_valid_json = property $ do
    let decoded = decode serverConnectedRaw :: Maybe Value
    case decoded of
        Just (Object obj) -> do
            -- Must have "type" field with "server.connected"
            case KM.lookup "type" obj of
                Just (String t) -> t === "server.connected"
                Just other -> do
                    annotate $ "type field is not a string: " ++ show other
                    failure
                Nothing -> do
                    annotate "missing type field"
                    failure
            -- Must have "properties" field
            case KM.lookup "properties" obj of
                Just (Object _) -> success
                Just other -> failOnNonObject "properties field" other
                Nothing -> failOnNothing "properties field"
        Just other -> failOnNonObject "serverConnectedRaw" other
        Nothing -> do
            annotate "serverConnectedRaw is not valid JSON"
            failure

-- | Property: serverHeartbeatRaw has correct structure
prop_serverHeartbeatRaw_structure :: Property
prop_serverHeartbeatRaw_structure = property $ do
    case serverHeartbeatRaw of
        Object obj -> do
            case KM.lookup "type" obj of
                Just (String t) -> t === "server.heartbeat"
                Just other -> do
                    annotate $ "type field is not a string: " ++ show other
                    failure
                Nothing -> do
                    annotate "missing type field"
                    failure
            case KM.lookup "properties" obj of
                Just (Object _) -> success
                Just other -> failOnNonObject "properties field" other
                Nothing -> failOnNothing "properties field"
        other -> failOnNonObject "serverHeartbeatRaw" other

-- ═══════════════════════════════════════════════════════════════════════════
-- // Configuration constants //
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: sseHeaders contains required SSE headers
prop_sseHeaders_contains_required :: Property
prop_sseHeaders_contains_required = property $ do
    let headers = sseHeaders
    let headerNames = map fst headers

    -- Must have Content-Type
    assert $ "Content-Type" `elem` headerNames
    -- Must have Cache-Control
    assert $ "Cache-Control" `elem` headerNames

-- | Property: sseHeaders has correct Content-Type
prop_sseHeaders_content_type :: Property
prop_sseHeaders_content_type = property $ do
    let contentType = lookup "Content-Type" sseHeaders
    contentType === Just "text/event-stream"

-- | Property: heartbeatIntervalMicros is reasonable (between 1 and 60 seconds)
prop_heartbeatInterval_reasonable :: Property
prop_heartbeatInterval_reasonable = property $ do
    assert $ heartbeatIntervalMicros >= 1000000 -- at least 1 second
    assert $ heartbeatIntervalMicros <= 60000000 -- at most 60 seconds

-- ═══════════════════════════════════════════════════════════════════════════
-- // Test tree //
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Event Property Tests"
        [ testGroup
            "wrapGlobalEvent"
            [ testProperty "produces valid GlobalEvent structure" prop_wrapGlobalEvent_structure
            , testProperty "is deterministic" prop_wrapGlobalEvent_deterministic
            , testProperty "encodes to valid JSON" prop_wrapGlobalEvent_encodable
            , testProperty "preserves directory exactly" prop_wrapGlobalEvent_preserves_directory
            , testProperty "preserves event type exactly" prop_wrapGlobalEvent_preserves_type
            , testProperty "handles empty properties" prop_wrapGlobalEvent_empty_properties
            , testProperty "handles nested properties" prop_wrapGlobalEvent_nested_properties
            , testProperty "different directories produce different outputs" prop_wrapGlobalEvent_different_directories
            , testProperty "different event types produce different outputs" prop_wrapGlobalEvent_different_types
            ]
        , testGroup
            "wrapEventPayload"
            [ testProperty "produces valid structure" prop_wrapEventPayload_structure
            , testProperty "is consistent with wrapGlobalEvent" prop_wrapEventPayload_consistent_with_wrapGlobalEvent
            ]
        , testGroup
            "mkRawEvent"
            [ testProperty "produces valid raw event structure" prop_mkRawEvent_structure
            , testProperty "is deterministic" prop_mkRawEvent_deterministic
            , testProperty "encodes to valid JSON" prop_mkRawEvent_encodable
            ]
        , testGroup
            "formatSSEMessage"
            [ testProperty "produces valid SSE wire format" prop_formatSSEMessage_format
            , testProperty "preserves JSON content" prop_formatSSEMessage_preserves_content
            ]
        , testGroup
            "Pre-built events"
            [ testProperty "serverConnectedRaw is valid JSON" prop_serverConnectedRaw_valid_json
            , testProperty "serverHeartbeatRaw has correct structure" prop_serverHeartbeatRaw_structure
            ]
        , testGroup
            "Configuration"
            [ testProperty "sseHeaders contains required headers" prop_sseHeaders_contains_required
            , testProperty "sseHeaders has correct Content-Type" prop_sseHeaders_content_type
            , testProperty "heartbeatIntervalMicros is reasonable" prop_heartbeatInterval_reasonable
            ]
        ]
