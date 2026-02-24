-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                               // weapon-server // api/message
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- Message types and API endpoints. Messages represent the conversation turns
-- within a session, containing parts that can be text, tool calls, or results.
--
-- The Message type is a discriminated union based on "role":
--   - UserMessageInfo for role="user"
--   - AssistantMessageInfo for role="assistant"
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Message (
    -- * Message Types
    MessageInfo (..),
    UserMessageInfo (..),
    AssistantMessageInfo (..),
    MessageTime (..),
    MessagePath (..),
    MessageTokens (..),
    TokenCache (..),
    Message (..),
    CreateMessageInput (..),
    ModelSelection (..),

    -- * Message Accessors
    messageInfoId,
    messageInfoRole,
    messageInfoSessionId,
    messageInfoCreatedTime,

    -- * Message API Endpoints
    SessionMessageListAPI,
    SessionMessageCreateAPI,
    SessionMessageGetAPI,
    SessionMessagePartDeleteAPI,
    SessionMessagePartUpdateAPI,
    SessionPromptAsyncAPI,
)
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics
import Servant

-- ═══════════════════════════════════════════════════════════════════════════
-- // message time //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Message time information (different from SessionTime!)
Messages have {created, completed?} whereas Sessions have {created, updated, ...}
-}
data MessageTime = MessageTime
    { mtimeCreated :: Double
    , mtimeCompleted :: Maybe Double
    }
    deriving (Eq, Show, Generic)

instance ToJSON MessageTime where
    toJSON mt =
        object $
            ("created" .= mtimeCreated mt)
                : maybe [] (\c -> ["completed" .= c]) (mtimeCompleted mt)

instance FromJSON MessageTime where
    parseJSON = withObject "MessageTime" $ \v ->
        MessageTime
            <$> v .: "created"
            <*> v .:? "completed"

-- ═══════════════════════════════════════════════════════════════════════════
-- // message path //
-- ═══════════════════════════════════════════════════════════════════════════

-- | Path information for assistant messages
data MessagePath = MessagePath
    { mpCwd :: Text
    , mpRoot :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON MessagePath where
    toJSON mp =
        object
            [ "cwd" .= mpCwd mp
            , "root" .= mpRoot mp
            ]

instance FromJSON MessagePath where
    parseJSON = withObject "MessagePath" $ \v ->
        MessagePath
            <$> v .: "cwd"
            <*> v .: "root"

-- ═══════════════════════════════════════════════════════════════════════════
-- // token cache //
-- ═══════════════════════════════════════════════════════════════════════════

-- | Token cache information
data TokenCache = TokenCache
    { tcRead :: Double
    , tcWrite :: Double
    }
    deriving (Eq, Show, Generic)

instance ToJSON TokenCache where
    toJSON tc =
        object
            [ "read" .= tcRead tc
            , "write" .= tcWrite tc
            ]

instance FromJSON TokenCache where
    parseJSON = withObject "TokenCache" $ \v ->
        TokenCache
            <$> v .: "read"
            <*> v .: "write"

-- ═══════════════════════════════════════════════════════════════════════════
-- // message tokens //
-- ═══════════════════════════════════════════════════════════════════════════

-- | Token usage information for assistant messages
data MessageTokens = MessageTokens
    { mtTotal :: Maybe Double
    , mtInput :: Double
    , mtOutput :: Double
    , mtReasoning :: Double
    , mtCache :: TokenCache
    }
    deriving (Eq, Show, Generic)

instance ToJSON MessageTokens where
    toJSON mt =
        object $
            [ "input" .= mtInput mt
            , "output" .= mtOutput mt
            , "reasoning" .= mtReasoning mt
            , "cache" .= mtCache mt
            ]
                ++ maybe [] (\t -> ["total" .= t]) (mtTotal mt)

instance FromJSON MessageTokens where
    parseJSON = withObject "MessageTokens" $ \v ->
        MessageTokens
            <$> v .:? "total"
            <*> v .: "input"
            <*> v .: "output"
            <*> v .: "reasoning"
            <*> v .: "cache"

-- ═══════════════════════════════════════════════════════════════════════════
-- // user message info //
-- ═══════════════════════════════════════════════════════════════════════════

-- | User message info (role="user")
data UserMessageInfo = UserMessageInfo
    { umiId :: Text
    , umiSessionId :: Text
    , umiTime :: MessageTime
    , umiAgent :: Maybe Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON UserMessageInfo where
    toJSON info =
        object $
            [ "id" .= umiId info
            , "sessionID" .= umiSessionId info
            , "role" .= ("user" :: Text)
            , "time" .= umiTime info
            ]
                ++ maybe [] (\a -> ["agent" .= a]) (umiAgent info)

instance FromJSON UserMessageInfo where
    parseJSON = withObject "UserMessageInfo" $ \v -> do
        role <- v .: "role" :: Parser Text
        if role /= "user"
            then fail "Expected role 'user'"
            else
                UserMessageInfo
                    <$> v .: "id"
                    <*> v .: "sessionID"
                    <*> v .: "time"
                    <*> v .:? "agent"

-- ═══════════════════════════════════════════════════════════════════════════
-- // assistant message info //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Assistant message info (role="assistant")
Matches the OpenAPI AssistantMessage schema
-}
data AssistantMessageInfo = AssistantMessageInfo
    { amiId :: Text
    , amiSessionId :: Text
    , amiTime :: MessageTime
    , amiParentId :: Text
    , amiModelId :: Text
    , amiProviderId :: Text
    , amiMode :: Text
    , amiAgent :: Text
    , amiPath :: MessagePath
    , amiCost :: Double
    , amiTokens :: MessageTokens
    , amiSummary :: Maybe Bool
    , amiVariant :: Maybe Text
    , amiFinish :: Maybe Text
    , amiError :: Maybe Value
    , amiStructured :: Maybe Value
    }
    deriving (Eq, Show, Generic)

instance ToJSON AssistantMessageInfo where
    toJSON info =
        object $
            [ "id" .= amiId info
            , "sessionID" .= amiSessionId info
            , "role" .= ("assistant" :: Text)
            , "time" .= amiTime info
            , "parentID" .= amiParentId info
            , "modelID" .= amiModelId info
            , "providerID" .= amiProviderId info
            , "mode" .= amiMode info
            , "agent" .= amiAgent info
            , "path" .= amiPath info
            , "cost" .= amiCost info
            , "tokens" .= amiTokens info
            ]
                ++ maybe [] (\s -> ["summary" .= s]) (amiSummary info)
                ++ maybe [] (\v -> ["variant" .= v]) (amiVariant info)
                ++ maybe [] (\f -> ["finish" .= f]) (amiFinish info)
                ++ maybe [] (\e -> ["error" .= e]) (amiError info)
                ++ maybe [] (\st -> ["structured" .= st]) (amiStructured info)

instance FromJSON AssistantMessageInfo where
    parseJSON = withObject "AssistantMessageInfo" $ \v -> do
        role <- v .: "role" :: Parser Text
        if role /= "assistant"
            then fail "Expected role 'assistant'"
            else
                AssistantMessageInfo
                    <$> v .: "id"
                    <*> v .: "sessionID"
                    <*> v .: "time"
                    <*> v .: "parentID"
                    <*> v .: "modelID"
                    <*> v .: "providerID"
                    <*> v .: "mode"
                    <*> v .: "agent"
                    <*> v .: "path"
                    <*> v .: "cost"
                    <*> v .: "tokens"
                    <*> v .:? "summary"
                    <*> v .:? "variant"
                    <*> v .:? "finish"
                    <*> v .:? "error"
                    <*> v .:? "structured"

-- ═══════════════════════════════════════════════════════════════════════════
-- // message info (discriminated union) //
-- ═══════════════════════════════════════════════════════════════════════════

-- | MessageInfo is a discriminated union based on "role" field
data MessageInfo
    = UserInfo UserMessageInfo
    | AssistantInfo AssistantMessageInfo
    deriving (Eq, Show, Generic)

-- | Get the message ID from any MessageInfo
messageInfoId :: MessageInfo -> Text
messageInfoId (UserInfo info) = umiId info
messageInfoId (AssistantInfo info) = amiId info

-- | Get the role from any MessageInfo
messageInfoRole :: MessageInfo -> Text
messageInfoRole (UserInfo _) = "user"
messageInfoRole (AssistantInfo _) = "assistant"

-- | Get the session ID from any MessageInfo
messageInfoSessionId :: MessageInfo -> Text
messageInfoSessionId (UserInfo info) = umiSessionId info
messageInfoSessionId (AssistantInfo info) = amiSessionId info

-- | Get the created time from any MessageInfo
messageInfoCreatedTime :: MessageInfo -> Double
messageInfoCreatedTime (UserInfo info) = mtimeCreated (umiTime info)
messageInfoCreatedTime (AssistantInfo info) = mtimeCreated (amiTime info)

instance ToJSON MessageInfo where
    toJSON (UserInfo info) = toJSON info
    toJSON (AssistantInfo info) = toJSON info

instance FromJSON MessageInfo where
    parseJSON v = withObject "MessageInfo" parseRole v
      where
        parseRole obj = do
            role <- obj .: "role" :: Parser Text
            case role of
                "user" -> UserInfo <$> parseJSON v
                "assistant" -> AssistantInfo <$> parseJSON v
                _ -> fail $ "Unknown role: " ++ show role

-- ═══════════════════════════════════════════════════════════════════════════
-- // message //
-- ═══════════════════════════════════════════════════════════════════════════

data Message = Message
    { msgInfo :: MessageInfo
    , msgParts :: [Value]
    }
    deriving (Eq, Show, Generic)

instance ToJSON Message where
    toJSON message =
        object
            [ "info" .= msgInfo message
            , "parts" .= msgParts message
            ]

instance FromJSON Message where
    parseJSON = withObject "Message" $ \v ->
        Message
            <$> v .: "info"
            <*> v .: "parts"

-- ═══════════════════════════════════════════════════════════════════════════
-- // model selection //
-- ═══════════════════════════════════════════════════════════════════════════

-- | Model selection from the TUI, which sends {providerID, modelID} object
data ModelSelection = ModelSelection
    { msProviderID :: Text
    , msModelID :: Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON ModelSelection where
    parseJSON = withObject "ModelSelection" $ \v ->
        ModelSelection
            <$> v .: "providerID"
            <*> v .: "modelID"

instance ToJSON ModelSelection where
    toJSON ms =
        object
            [ "providerID" .= msProviderID ms
            , "modelID" .= msModelID ms
            ]

-- ═══════════════════════════════════════════════════════════════════════════
-- // message input //
-- ═══════════════════════════════════════════════════════════════════════════

data CreateMessageInput = CreateMessageInput
    { cmiMessageId :: Maybe Text
    , cmiParts :: [Value]
    , cmiModel :: Maybe ModelSelection -- Model selection as {providerID, modelID}
    , cmiAgent :: Maybe Text -- Agent selection
    }
    deriving (Eq, Show, Generic)

instance FromJSON CreateMessageInput where
    parseJSON = withObject "CreateMessageInput" $ \v ->
        CreateMessageInput
            <$> v .:? "messageID"
            <*> v .: "parts"
            <*> v .:? "model"
            <*> v .:? "agent"

-- ═══════════════════════════════════════════════════════════════════════════
-- // api type definitions //
-- ═══════════════════════════════════════════════════════════════════════════

type SessionMessageListAPI =
    "session"
        :> Capture "sessionID" Text
        :> "message"
        :> QueryParam "limit" Int
        :> Get '[JSON] [Message]

type SessionMessageCreateAPI =
    "session"
        :> Capture "sessionID" Text
        :> "message"
        :> ReqBody '[JSON] CreateMessageInput
        :> Post '[JSON] Message

type SessionMessageGetAPI =
    "session"
        :> Capture "sessionID" Text
        :> "message"
        :> Capture "messageID" Text
        :> Get '[JSON] Message

type SessionMessagePartDeleteAPI =
    "session"
        :> Capture "sessionID" Text
        :> "message"
        :> Capture "messageID" Text
        :> "part"
        :> Capture "partID" Text
        :> Delete '[JSON] Bool

type SessionMessagePartUpdateAPI =
    "session"
        :> Capture "sessionID" Text
        :> "message"
        :> Capture "messageID" Text
        :> "part"
        :> Capture "partID" Text
        :> ReqBody '[JSON] Value
        :> Patch '[JSON] Value

type SessionPromptAsyncAPI =
    "session"
        :> Capture "sessionID" Text
        :> "prompt_async"
        :> ReqBody '[JSON] CreateMessageInput
        :> PostNoContent
