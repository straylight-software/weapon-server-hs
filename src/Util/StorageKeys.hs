{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Util.StorageKeys
Description : Storage key builders for consistent key construction

This module provides type-safe key builders for the storage layer.
All storage operations should use these functions rather than
constructing key paths manually to ensure consistency.

= Key Format

Keys are represented as lists of 'Text' segments. The storage layer
joins these segments to form the actual storage path.

= Key Hierarchy

@
session/<projectId>/<sessionId>  -- Session data
message/<sessionId>/<messageId>  -- Message data
todo/<sessionId>                 -- Todo list for session
project/<projectId>              -- Project metadata
@

= Usage Example

@
import Util.StorageKeys

-- Store a session
let key = 'sessionKey' "proj_123" "sess_456"
-- key = ["session", "proj_123", "sess_456"]

-- List all sessions for a project
let prefix = 'sessionPrefix' "proj_123"
-- prefix = ["session", "proj_123"]
@
-}
module Util.StorageKeys (
    -- * Session keys
    sessionKey,
    sessionPrefix,

    -- * Message keys
    messageKey,
    messagePrefix,

    -- * Other keys
    todoKey,
    projectKey,

    -- * Key utilities
    buildKey,
    buildPrefixKey,
) where

import Data.Text (Text)

{- | Build a session storage key for a specific session.

@
sessionKey "proj_123" "sess_456"
-- Returns: ["session", "proj_123", "sess_456"]
@
-}
sessionKey :: Text -> Text -> [Text]
sessionKey projectId sessionId = buildKey "session" [projectId, sessionId]

{- | Build a session prefix for listing all sessions in a project.

Use this with storage list operations to enumerate all sessions.

@
sessionPrefix "proj_123"
-- Returns: ["session", "proj_123"]
@
-}
sessionPrefix :: Text -> [Text]
sessionPrefix = buildPrefixKey "session"

{- | Build a message storage key.

@
messageKey "sess_456" "msg_789"
-- Returns: ["message", "sess_456", "msg_789"]
@
-}
messageKey :: Text -> Text -> [Text]
messageKey sessionId msgId = buildKey "message" [sessionId, msgId]

{- | Build a message prefix for listing all messages in a session.

@
messagePrefix "sess_456"
-- Returns: ["message", "sess_456"]
@
-}
messagePrefix :: Text -> [Text]
messagePrefix = buildPrefixKey "message"

{- | Build a todo storage key.

Each session has a single todo list stored at this key.

@
todoKey "sess_456"
-- Returns: ["todo", "sess_456"]
@
-}
todoKey :: Text -> [Text]
todoKey = buildPrefixKey "todo"

{- | Build a project storage key.

@
projectKey "proj_123"
-- Returns: ["project", "proj_123"]
@
-}
projectKey :: Text -> [Text]
projectKey = buildPrefixKey "project"

{- | Build a full key with namespace and multiple segments (internal helper).

This is a pure helper function that handles the common pattern of
prepending a namespace to a list of key segments.
-}
buildKey :: Text -> [Text] -> [Text]
buildKey namespace segments = namespace : segments

{- | Build a prefix key with namespace and single ID (internal helper).

Used for keys that have the pattern @namespace/id@.
-}
buildPrefixKey :: Text -> Text -> [Text]
buildPrefixKey namespace keyId = [namespace, keyId]
