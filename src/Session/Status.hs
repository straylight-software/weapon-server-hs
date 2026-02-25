{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Session.Status
Description : Session status types for real-time state tracking

This module defines the status types used to track the real-time state
of a session. The status is used by clients to show whether the AI agent
is actively processing, waiting to retry, or idle.

= Status Types

* 'StatusIdle' - No active processing
* 'StatusBusy' - Actively processing a request
* 'StatusRetry' - Waiting to retry after a rate limit or error

= JSON Serialization

The status serializes to a tagged JSON object matching the OpenAPI spec:

@
{ "type": "idle" }
{ "type": "busy" }
{ "type": "retry", "attempt": 1, "message": "Rate limited", "next": 5000 }
@
-}
module Session.Status (
    -- * Types
    SessionStatus (..),
    SessionStatusType (..),

    -- * Constructors
    idle,
    busy,
    retry,
) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)

{- | The type of session status.

This is the core enumeration of possible states:

[@StatusIdle@]: No active processing. The session is waiting for user input.

[@StatusBusy@]: Actively processing a request. The AI agent is working.

[@StatusRetry@]: Waiting to retry after an error. Contains:

    * @attempt@ - Current retry attempt number (1-based)
    * @message@ - Human-readable reason for the retry
    * @next@ - Milliseconds until the next retry attempt
-}
data SessionStatusType
    = -- | No active processing
      StatusIdle
    | -- | Retry state: attempt number, message, milliseconds until retry
      StatusRetry Int Text Int
    | -- | Actively processing
      StatusBusy
    deriving (Eq, Show)

{- | Wrapper for session status.

This newtype provides a consistent structure for status responses
and allows for future extension without breaking the API.
-}
newtype SessionStatus = SessionStatus
    { ssType :: SessionStatusType
    -- ^ The current status type
    }
    deriving (Eq, Show)

-- | Serialize status to JSON matching the OpenAPI spec.
instance ToJSON SessionStatus where
    toJSON s = case ssType s of
        StatusIdle ->
            object ["type" .= ("idle" :: Text)]
        StatusRetry attempt msg next ->
            object
                [ "type" .= ("retry" :: Text)
                , "attempt" .= attempt
                , "message" .= msg
                , "next" .= next
                ]
        StatusBusy ->
            object ["type" .= ("busy" :: Text)]

-- ═══════════════════════════════════════════════════════════════════════════
-- Convenience Constructors
-- ═══════════════════════════════════════════════════════════════════════════

-- | Create an idle status.
idle :: SessionStatus
idle = SessionStatus StatusIdle

-- | Create a busy status.
busy :: SessionStatus
busy = SessionStatus StatusBusy

{- | Create a retry status.

==== __Example__

@
let status = retry 1 "Rate limited by API" 5000
-- Retry attempt 1, wait 5 seconds
@
-}
retry :: Int -> Text -> Int -> SessionStatus
retry attempt msg next = SessionStatus (StatusRetry attempt msg next)
