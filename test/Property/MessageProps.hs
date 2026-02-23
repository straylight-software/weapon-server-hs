{-# LANGUAGE OverloadedStrings #-}

-- | Message property tests
module Property.MessageProps where

import Api (
    AssistantMessageInfo (..),
    CreateMessageInput (..),
    Message (..),
    MessageInfo (..),
    MessagePath (..),
    MessageTime (..),
    MessageTokens (..),
    ModelSelection (..),
    TokenCache (..),
    UserMessageInfo (..),
    messageInfoId,
    messageInfoRole,
    messageInfoSessionId,
 )
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text (Text)
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

-- | Helper to fail on Nothing
failOnNothing :: (MonadTest m) => String -> m ()
failOnNothing context = do
    annotate $ context ++ ": expected Just but got Nothing"
    failure

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

genText :: Gen Text
genText = Gen.text (Range.linear 0 100) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 100) Gen.alphaNum

genDouble :: Gen Double
genDouble = Gen.double (Range.linearFrac 0 1000000)

genNonNegativeDouble :: Gen Double
genNonNegativeDouble = Gen.double (Range.linearFrac 0 1000000)

genMessagePath :: Gen MessagePath
genMessagePath =
    MessagePath
        <$> genNonEmptyText
        <*> genNonEmptyText

genTokenCache :: Gen TokenCache
genTokenCache =
    TokenCache
        <$> genNonNegativeDouble
        <*> genNonNegativeDouble

genMessageTokens :: Gen MessageTokens
genMessageTokens =
    MessageTokens
        <$> Gen.maybe genNonNegativeDouble
        <*> genNonNegativeDouble
        <*> genNonNegativeDouble
        <*> genNonNegativeDouble
        <*> genTokenCache

genMessageTime :: Gen MessageTime
genMessageTime =
    MessageTime
        <$> genNonNegativeDouble
        <*> Gen.maybe genNonNegativeDouble

genUserMessageInfo :: Gen UserMessageInfo
genUserMessageInfo =
    UserMessageInfo
        <$> genNonEmptyText
        <*> genNonEmptyText
        <*> genMessageTime
        <*> Gen.maybe genNonEmptyText

genAssistantMessageInfo :: Gen AssistantMessageInfo
genAssistantMessageInfo =
    AssistantMessageInfo
        <$> genNonEmptyText -- amiId
        <*> genNonEmptyText -- amiSessionId
        <*> genMessageTime -- amiTime
        <*> genNonEmptyText -- amiParentId
        <*> genNonEmptyText -- amiModelId
        <*> genNonEmptyText -- amiProviderId
        <*> genNonEmptyText -- amiMode
        <*> genNonEmptyText -- amiAgent
        <*> genMessagePath -- amiPath
        <*> genNonNegativeDouble -- amiCost
        <*> genMessageTokens -- amiTokens
        <*> Gen.maybe Gen.bool -- amiSummary
        <*> Gen.maybe genNonEmptyText -- amiVariant
        <*> Gen.maybe genNonEmptyText -- amiFinish
        <*> pure Nothing -- amiError (skip for simplicity)
        <*> pure Nothing -- amiStructured (skip for simplicity)

genMessageInfo :: Gen MessageInfo
genMessageInfo =
    Gen.choice
        [ UserInfo <$> genUserMessageInfo
        , AssistantInfo <$> genAssistantMessageInfo
        ]

genMessagePart :: Gen Value
genMessagePart = do
    content <- genText
    Gen.element
        [ object ["type" .= ("text" :: Text), "text" .= content]
        , object ["type" .= ("code" :: Text), "code" .= content]
        , object ["type" .= ("image" :: Text), "url" .= content]
        ]

genMessage :: Gen Message
genMessage =
    Message
        <$> genMessageInfo
        <*> Gen.list (Range.linear 0 5) genMessagePart

genModelSelection :: Gen ModelSelection
genModelSelection =
    ModelSelection
        <$> genNonEmptyText
        <*> genNonEmptyText

genCreateMessageInput :: Gen CreateMessageInput
genCreateMessageInput =
    CreateMessageInput
        <$> Gen.maybe genNonEmptyText
        <*> Gen.list (Range.linear 0 5) genMessagePart
        <*> Gen.maybe genModelSelection
        <*> Gen.maybe genNonEmptyText

-- ═══════════════════════════════════════════════════════════════════════════
-- TokenCache and MessageTokens property tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: TokenCache JSON round-trip
prop_tokenCacheRoundtrip :: Property
prop_tokenCacheRoundtrip = property $ do
    tc <- forAll genTokenCache
    let json = encode tc
    case decode json of
        Nothing -> failure
        Just tc' -> tc === tc'

-- | Property: MessageTokens JSON round-trip
prop_messageTokensRoundtrip :: Property
prop_messageTokensRoundtrip = property $ do
    mt <- forAll genMessageTokens
    let json = encode mt
    case decode json of
        Nothing -> do
            annotate $ "Failed to decode: " <> BSL.unpack json
            failure
        Just mt' -> mt === mt'

-- | Property: MessagePath JSON round-trip
prop_messagePathRoundtrip :: Property
prop_messagePathRoundtrip = property $ do
    mp <- forAll genMessagePath
    let json = encode mp
    case decode json of
        Nothing -> failure
        Just mp' -> mp === mp'

-- ═══════════════════════════════════════════════════════════════════════════
-- UserMessageInfo property tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: UserMessageInfo JSON round-trip
prop_userMessageInfoRoundtrip :: Property
prop_userMessageInfoRoundtrip = property $ do
    umi <- forAll genUserMessageInfo
    let json = encode umi
    case decode json of
        Nothing -> do
            annotate $ "Failed to decode: " <> BSL.unpack json
            failure
        Just umi' -> umi === umi'

-- | Property: UserMessageInfo has role "user" in JSON
prop_userMessageInfoHasRoleUser :: Property
prop_userMessageInfoHasRoleUser = property $ do
    umi <- forAll genUserMessageInfo
    let json = encode umi
    case decode json :: Maybe Value of
        Nothing -> failOnNothing "decode UserMessageInfo"
        Just (Object obj) -> do
            case KM.lookup (Key.fromText "role") obj of
                Just (String "user") -> success
                Just other -> failOnNonObject "role field expected String 'user'" other
                Nothing -> failOnNothing "role field"
        Just other -> failOnNonObject "decoded UserMessageInfo" other

-- ═══════════════════════════════════════════════════════════════════════════
-- AssistantMessageInfo property tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: AssistantMessageInfo JSON round-trip
prop_assistantMessageInfoRoundtrip :: Property
prop_assistantMessageInfoRoundtrip = property $ do
    ami <- forAll genAssistantMessageInfo
    let json = encode ami
    case decode json of
        Nothing -> do
            annotate $ "Failed to decode: " <> BSL.unpack json
            failure
        Just ami' -> ami === ami'

-- | Property: AssistantMessageInfo has role "assistant" in JSON
prop_assistantMessageInfoHasRoleAssistant :: Property
prop_assistantMessageInfoHasRoleAssistant = property $ do
    ami <- forAll genAssistantMessageInfo
    let json = encode ami
    case decode json :: Maybe Value of
        Nothing -> failOnNothing "decode AssistantMessageInfo"
        Just (Object obj) -> do
            case KM.lookup (Key.fromText "role") obj of
                Just (String "assistant") -> success
                Just other -> failOnNonObject "role field expected String 'assistant'" other
                Nothing -> failOnNothing "role field"
        Just other -> failOnNonObject "decoded AssistantMessageInfo" other

-- | Property: AssistantMessageInfo contains all required fields per OpenAPI spec
prop_assistantMessageInfoRequiredFields :: Property
prop_assistantMessageInfoRequiredFields = property $ do
    ami <- forAll genAssistantMessageInfo
    let json = encode ami
    case decode json :: Maybe Value of
        Nothing -> failOnNothing "decode AssistantMessageInfo"
        Just (Object obj) -> do
            -- Check all required fields from OpenAPI spec
            assert $ KM.member (Key.fromText "id") obj
            assert $ KM.member (Key.fromText "sessionID") obj
            assert $ KM.member (Key.fromText "role") obj
            assert $ KM.member (Key.fromText "time") obj
            assert $ KM.member (Key.fromText "parentID") obj
            assert $ KM.member (Key.fromText "modelID") obj
            assert $ KM.member (Key.fromText "providerID") obj
            assert $ KM.member (Key.fromText "mode") obj
            assert $ KM.member (Key.fromText "agent") obj
            assert $ KM.member (Key.fromText "path") obj
            assert $ KM.member (Key.fromText "cost") obj
            assert $ KM.member (Key.fromText "tokens") obj
        Just other -> failOnNonObject "decoded AssistantMessageInfo" other

-- ═══════════════════════════════════════════════════════════════════════════
-- MessageInfo discriminated union property tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: MessageInfo JSON round-trip
prop_messageInfoRoundtrip :: Property
prop_messageInfoRoundtrip = property $ do
    mi <- forAll genMessageInfo
    let json = encode mi
    case decode json of
        Nothing -> do
            annotate $ "Failed to decode: " <> BSL.unpack json
            failure
        Just mi' -> mi === mi'

-- | Property: messageInfoId accessor works correctly
prop_messageInfoIdAccessor :: Property
prop_messageInfoIdAccessor = property $ do
    -- Test with UserInfo
    umi <- forAll genUserMessageInfo
    messageInfoId (UserInfo umi) === umiId umi
    -- Test with AssistantInfo
    ami <- forAll genAssistantMessageInfo
    messageInfoId (AssistantInfo ami) === amiId ami

-- | Property: messageInfoRole accessor works correctly
prop_messageInfoRoleAccessor :: Property
prop_messageInfoRoleAccessor = property $ do
    -- Test with UserInfo
    umi <- forAll genUserMessageInfo
    messageInfoRole (UserInfo umi) === "user"
    -- Test with AssistantInfo
    ami <- forAll genAssistantMessageInfo
    messageInfoRole (AssistantInfo ami) === "assistant"

-- | Property: messageInfoSessionId accessor works correctly
prop_messageInfoSessionIdAccessor :: Property
prop_messageInfoSessionIdAccessor = property $ do
    -- Test with UserInfo
    umi <- forAll genUserMessageInfo
    messageInfoSessionId (UserInfo umi) === umiSessionId umi
    -- Test with AssistantInfo
    ami <- forAll genAssistantMessageInfo
    messageInfoSessionId (AssistantInfo ami) === amiSessionId ami

-- ═══════════════════════════════════════════════════════════════════════════
-- Message property tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: Message JSON round-trip
prop_messageRoundtrip :: Property
prop_messageRoundtrip = property $ do
    msg <- forAll genMessage
    let json = encode msg
    case decode json of
        Nothing -> do
            annotate $ "Failed to decode: " <> BSL.unpack json
            failure
        Just msg' -> msg === msg'

-- | Property: Message with empty parts
prop_messageEmptyParts :: Property
prop_messageEmptyParts = property $ do
    mi <- forAll genMessageInfo
    let msg = Message mi []
    let json = encode msg
    case decode json of
        Nothing -> failure
        Just msg' -> msg === msg'

-- | Property: Message with multiple parts
prop_messageMultipleParts :: Property
prop_messageMultipleParts = property $ do
    mi <- forAll genMessageInfo
    parts <- forAll $ Gen.list (Range.linear 1 10) genMessagePart
    let msg = Message mi parts
    let json = encode msg
    case decode json of
        Nothing -> failure
        Just msg' -> msg === msg'

-- | Property: Message JSON has info and parts keys
prop_messageJsonKeys :: Property
prop_messageJsonKeys = property $ do
    msg <- forAll genMessage
    let json = encode msg
    case decode json :: Maybe Value of
        Nothing -> failure
        Just (Object obj) -> do
            assert $ KM.member (Key.fromText "info") obj
            assert $ KM.member (Key.fromText "parts") obj
        _otherParts -> failure

-- ═══════════════════════════════════════════════════════════════════════════
-- ModelSelection property tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: ModelSelection JSON round-trip
prop_modelSelectionRoundtrip :: Property
prop_modelSelectionRoundtrip = property $ do
    ms <- forAll genModelSelection
    let json = encode ms
    case decode json of
        Nothing -> do
            annotate $ "Failed to decode: " <> BSL.unpack json
            failure
        Just ms' -> ms === ms'

-- | Property: ModelSelection parses from object with providerID and modelID
prop_modelSelectionParseObject :: Property
prop_modelSelectionParseObject = property $ do
    providerId <- forAll genNonEmptyText
    modelId <- forAll genNonEmptyText
    let json = encode $ object ["providerID" .= providerId, "modelID" .= modelId]
    case decode json of
        Nothing -> failure
        Just ms' -> do
            msProviderID ms' === providerId
            msModelID ms' === modelId

-- ═══════════════════════════════════════════════════════════════════════════
-- CreateMessageInput property tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: CreateMessageInput parses with model as object
prop_createMessageInputWithModelObject :: Property
prop_createMessageInputWithModelObject = property $ do
    parts <- forAll $ Gen.list (Range.linear 0 3) genMessagePart
    providerId <- forAll genNonEmptyText
    modelId <- forAll genNonEmptyText
    agentName <- forAll $ Gen.maybe genNonEmptyText
    let json =
            encode $
                object
                    [ "parts" .= parts
                    , "model" .= object ["providerID" .= providerId, "modelID" .= modelId]
                    , "agent" .= agentName
                    ]
    case decode json of
        Nothing -> do
            annotate $ "Failed to decode CreateMessageInput: " <> BSL.unpack json
            failure
        Just cmi -> do
            cmiParts cmi === parts
            case cmiModel cmi of
                Nothing -> do
                    annotate "Model should be present"
                    failure
                Just ms -> do
                    msProviderID ms === providerId
                    msModelID ms === modelId
            cmiAgent cmi === agentName

-- | Property: CreateMessageInput parses without model (optional)
prop_createMessageInputWithoutModel :: Property
prop_createMessageInputWithoutModel = property $ do
    parts <- forAll $ Gen.list (Range.linear 0 3) genMessagePart
    let json = encode $ object ["parts" .= parts]
    case decode json of
        Nothing -> do
            annotate $ "Failed to decode CreateMessageInput: " <> BSL.unpack json
            failure
        Just cmi -> do
            cmiParts cmi === parts
            cmiModel cmi === Nothing
            cmiAgent cmi === Nothing

-- | Property: CreateMessageInput fails when model is string (wrong type)
prop_createMessageInputRejectsStringModel :: Property
prop_createMessageInputRejectsStringModel = property $ do
    parts <- forAll $ Gen.list (Range.linear 0 3) genMessagePart
    modelStr <- forAll genNonEmptyText
    let json = encode $ object ["parts" .= parts, "model" .= modelStr]
    case decode json :: Maybe CreateMessageInput of
        Nothing -> success -- Expected: should fail to parse string as ModelSelection
        Just _ -> do
            annotate "Should have rejected string model, but it parsed successfully"
            failure

-- ═══════════════════════════════════════════════════════════════════════════
-- Test tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Message Property Tests"
        [ testGroup
            "Supporting Types"
            [ testProperty "TokenCache round-trip" prop_tokenCacheRoundtrip
            , testProperty "MessageTokens round-trip" prop_messageTokensRoundtrip
            , testProperty "MessagePath round-trip" prop_messagePathRoundtrip
            ]
        , testGroup
            "UserMessageInfo"
            [ testProperty "UserMessageInfo round-trip" prop_userMessageInfoRoundtrip
            , testProperty "UserMessageInfo has role user" prop_userMessageInfoHasRoleUser
            ]
        , testGroup
            "AssistantMessageInfo"
            [ testProperty "AssistantMessageInfo round-trip" prop_assistantMessageInfoRoundtrip
            , testProperty "AssistantMessageInfo has role assistant" prop_assistantMessageInfoHasRoleAssistant
            , testProperty "AssistantMessageInfo has required fields" prop_assistantMessageInfoRequiredFields
            ]
        , testGroup
            "MessageInfo (discriminated union)"
            [ testProperty "MessageInfo round-trip" prop_messageInfoRoundtrip
            , testProperty "messageInfoId accessor" prop_messageInfoIdAccessor
            , testProperty "messageInfoRole accessor" prop_messageInfoRoleAccessor
            , testProperty "messageInfoSessionId accessor" prop_messageInfoSessionIdAccessor
            ]
        , testGroup
            "Message"
            [ testProperty "Message round-trip" prop_messageRoundtrip
            , testProperty "Message with empty parts" prop_messageEmptyParts
            , testProperty "Message with multiple parts" prop_messageMultipleParts
            , testProperty "Message JSON keys" prop_messageJsonKeys
            ]
        , testGroup
            "ModelSelection"
            [ testProperty "ModelSelection round-trip" prop_modelSelectionRoundtrip
            , testProperty "ModelSelection parses from object" prop_modelSelectionParseObject
            ]
        , testGroup
            "CreateMessageInput"
            [ testProperty "CreateMessageInput with model object" prop_createMessageInputWithModelObject
            , testProperty "CreateMessageInput without model" prop_createMessageInputWithoutModel
            , testProperty "CreateMessageInput rejects string model" prop_createMessageInputRejectsStringModel
            ]
        ]
