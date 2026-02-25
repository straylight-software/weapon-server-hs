{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Prompt.Async
Description : Asynchronous prompt job handling for message processing

This module provides types and utilities for managing asynchronous prompt jobs.
It supports the lifecycle states of async requests: queued, started, completed, and failed.

The async prompt system allows clients to submit message creation requests that are
processed in the background, with status updates available via polling or events.

= Job Lifecycle

A typical async prompt job goes through the following states:

1. __Queued__ - Job is submitted and waiting to be processed
2. __Started__ - Job processing has begun
3. __Completed__ - Job finished successfully with a message ID
4. __Failed__ - Job encountered an error

= Storage Keys

Jobs are stored using a hierarchical key structure:

* Individual job: @["prompt_async", sessionId, requestId]@
* Session index: @["prompt_async", sessionId, "index"]@
-}
module Prompt.Async (
    -- * Types
    PromptAsyncJob (..),

    -- * Storage Key Construction
    promptAsyncKey,
    promptAsyncIndexKey,

    -- * Payload Construction

    {- | Functions for building JSON payloads representing job states.
    Each payload includes standard fields (requestID, sessionID, status)
    plus state-specific fields.
    -}
    queuedPayload,
    startedPayload,
    completedPayload,
    failedPayload,

    -- * Status Constants

    -- | Standard status values used in payloads
    statusQueued,
    statusStarted,
    statusCompleted,
    statusFailed,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)

import Api (CreateMessageInput (..))

{- | Represents an asynchronous prompt job awaiting or undergoing processing.

This structure captures all the information needed to process a message
creation request asynchronously, including:

* A unique request identifier for tracking
* The session context for the message
* The original input parameters

==== Example

@
let job = PromptAsyncJob
      { pajRequestId = "req_abc123"
      , pajSessionId = "sess_xyz789"
      , pajInput = CreateMessageInput Nothing [textPart "Hello"] Nothing Nothing
      }
@
-}
data PromptAsyncJob = PromptAsyncJob
    { pajRequestId :: Text
    -- ^ Unique identifier for this async request
    , pajSessionId :: Text
    -- ^ Session ID where the message will be created
    , pajInput :: CreateMessageInput
    -- ^ Original message creation input parameters
    }

-- | Status value for queued jobs
statusQueued :: Text
statusQueued = "queued"

-- | Status value for started jobs
statusStarted :: Text
statusStarted = "started"

-- | Status value for completed jobs
statusCompleted :: Text
statusCompleted = "completed"

-- | Status value for failed jobs
statusFailed :: Text
statusFailed = "failed"

{- | Construct a storage key for an individual async prompt job.

The key uniquely identifies a job by its session and request IDs.

==== Example

>>> promptAsyncKey "sess_123" "req_456"
["prompt_async", "sess_123", "req_456"]
-}
promptAsyncKey :: Text -> Text -> [Text]
promptAsyncKey sid reqId = ["prompt_async", sid, reqId]

{- | Construct a storage key for the async prompt index of a session.

The index tracks all async jobs for a given session, allowing
enumeration of pending and completed jobs.

==== Example

>>> promptAsyncIndexKey "sess_123"
["prompt_async", "sess_123", "index"]
-}
promptAsyncIndexKey :: Text -> [Text]
promptAsyncIndexKey sid = ["prompt_async", sid, "index"]

{- | Common base fields included in all async payload responses.

Every payload includes:

* @requestID@ - The unique request identifier
* @sessionID@ - The session context
* @status@ - Current job state
-}
basePayload :: Text -> Text -> Text -> [Pair]
basePayload sid reqId status =
    [ "requestID" .= reqId
    , "sessionID" .= sid
    , "status" .= status
    ]

{- | Build a payload for a newly queued job.

Includes the message parts from the original input so clients
can display what was submitted.

==== JSON Structure

@
{
  "requestID": "req_123",
  "sessionID": "sess_456",
  "status": "queued",
  "parts": [...]
}
@
-}
queuedPayload :: Text -> Text -> CreateMessageInput -> Value
queuedPayload sid reqId input =
    object $ basePayload sid reqId statusQueued ++ ["parts" .= cmiParts input]

{- | Build a payload indicating job processing has started.

==== JSON Structure

@
{
  "requestID": "req_123",
  "sessionID": "sess_456",
  "status": "started"
}
@
-}
startedPayload :: Text -> Text -> Value
startedPayload sid reqId =
    object $ basePayload sid reqId statusStarted

{- | Build a payload for a successfully completed job.

Includes the ID of the created message so clients can fetch it.

==== JSON Structure

@
{
  "requestID": "req_123",
  "sessionID": "sess_456",
  "status": "completed",
  "messageID": "msg_789"
}
@
-}
completedPayload :: Text -> Text -> Text -> Value
completedPayload sid reqId msgId =
    object $ basePayload sid reqId statusCompleted ++ ["messageID" .= msgId]

{- | Build a payload for a failed job.

Includes an error message describing what went wrong.

==== JSON Structure

@
{
  "requestID": "req_123",
  "sessionID": "sess_456",
  "status": "failed",
  "error": "Connection timeout"
}
@
-}
failedPayload :: Text -> Text -> Text -> Value
failedPayload sid reqId err =
    object $ basePayload sid reqId statusFailed ++ ["error" .= err]
