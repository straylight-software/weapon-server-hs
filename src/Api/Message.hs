-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                               // weapon-server // api/message
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- Message types and API endpoints. Messages represent the conversation turns
-- within a session, containing parts that can be text, tool calls, or results.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Message
    ( -- * Message Types
      MessageInfo (..)
    , Message (..)
    , CreateMessageInput (..)

      -- * Message API Endpoints
    , SessionMessageListAPI
    , SessionMessageCreateAPI
    , SessionMessageGetAPI
    , SessionMessagePartDeleteAPI
    , SessionMessagePartUpdateAPI
    , SessionPromptAsyncAPI
    ) where

import Api.Session (SessionTime)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant


-- ═══════════════════════════════════════════════════════════════════════════
-- // message info //
-- ═══════════════════════════════════════════════════════════════════════════

data MessageInfo = MessageInfo
    { msgId :: Text
    , msgSessionId :: Text
    , msgRole :: Text
    , msgTime :: SessionTime
    }
    deriving (Eq, Show, Generic)

instance ToJSON MessageInfo where
    toJSON info =
        object
            [ "id" .= msgId info
            , "sessionID" .= msgSessionId info
            , "role" .= msgRole info
            , "time" .= msgTime info
            ]

instance FromJSON MessageInfo where
    parseJSON = withObject "MessageInfo" $ \v ->
        MessageInfo
            <$> v .: "id"
            <*> v .: "sessionID"
            <*> v .: "role"
            <*> v .: "time"


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
-- // message input //
-- ═══════════════════════════════════════════════════════════════════════════

data CreateMessageInput = CreateMessageInput
    { cmiMessageId :: Maybe Text
    , cmiParts :: [Value]
    }
    deriving (Eq, Show, Generic)

instance FromJSON CreateMessageInput where
    parseJSON = withObject "CreateMessageInput" $ \v ->
        CreateMessageInput
            <$> v .:? "messageID"
            <*> v .: "parts"


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
        :> Post '[JSON] Value
