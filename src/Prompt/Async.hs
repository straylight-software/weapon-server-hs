{-# LANGUAGE OverloadedStrings #-}

module Prompt.Async (
    PromptAsyncJob (..),
    promptAsyncKey,
    promptAsyncIndexKey,
    queuedPayload,
    startedPayload,
    completedPayload,
    failedPayload,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)

import Api (CreateMessageInput (..))

data PromptAsyncJob = PromptAsyncJob
    { pajRequestId :: Text
    , pajSessionId :: Text
    , pajInput :: CreateMessageInput
    }

promptAsyncKey :: Text -> Text -> [Text]
promptAsyncKey sid reqId = ["prompt_async", sid, reqId]

promptAsyncIndexKey :: Text -> [Text]
promptAsyncIndexKey sid = ["prompt_async", sid, "index"]

-- | Common base fields for async payload
basePayload :: Text -> Text -> Text -> [Pair]
basePayload sid reqId status =
    [ "requestID" .= reqId
    , "sessionID" .= sid
    , "status" .= status
    ]

queuedPayload :: Text -> Text -> CreateMessageInput -> Value
queuedPayload sid reqId input =
    object $ basePayload sid reqId "queued" ++ ["parts" .= cmiParts input]

startedPayload :: Text -> Text -> Value
startedPayload sid reqId =
    object $ basePayload sid reqId "started"

completedPayload :: Text -> Text -> Text -> Value
completedPayload sid reqId msgId =
    object $ basePayload sid reqId "completed" ++ ["messageID" .= msgId]

failedPayload :: Text -> Text -> Text -> Value
failedPayload sid reqId err =
    object $ basePayload sid reqId "failed" ++ ["error" .= err]
