{-# LANGUAGE OverloadedStrings #-}

-- | Storage key builders for consistent key construction
module Util.StorageKeys
    ( -- * Key builders
      sessionKey
    , sessionPrefix
    , messageKey
    , messagePrefix
    , todoKey
    , projectKey
    ) where

import Data.Text (Text)

-- | Build a session storage key for a specific session
-- Usage: sessionKey projectId sessionId
sessionKey :: Text -> Text -> [Text]
sessionKey projectId sessionId = ["session", projectId, sessionId]

-- | Build a session prefix for listing all sessions in a project
-- Usage: sessionPrefix projectId
sessionPrefix :: Text -> [Text]
sessionPrefix projectId = ["session", projectId]

-- | Build a message storage key
-- Usage: messageKey sessionId messageId
messageKey :: Text -> Text -> [Text]
messageKey sessionId msgId = ["message", sessionId, msgId]

-- | Build a message prefix for listing all messages in a session
-- Usage: messagePrefix sessionId
messagePrefix :: Text -> [Text]
messagePrefix sessionId = ["message", sessionId]

-- | Build a todo storage key
-- Usage: todoKey sessionId
todoKey :: Text -> [Text]
todoKey sessionId = ["todo", sessionId]

-- | Build a project storage key
-- Usage: projectKey projectId
projectKey :: Text -> [Text]
projectKey projectId = ["project", projectId]
