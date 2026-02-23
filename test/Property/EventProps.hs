{-# LANGUAGE OverloadedStrings #-}

{- | Event property tests for Global.Event module

Tests the SSE event formatting functions to ensure they produce
correctly structured GlobalEvent and Event payloads per the OpenAPI spec.
-}
module Property.EventProps where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as Text
import Global.Event (wrapGlobalEvent)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

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
        _ -> do
            annotate "wrapGlobalEvent did not return an object"
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
            _ -> failure
        _ -> failure

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
                _ -> failure
            _ -> failure
        _ -> failure

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
                _ -> failure
            _ -> failure
        _ -> failure

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
                Nothing -> failure
            _ -> failure
        _ -> failure

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
        ]
