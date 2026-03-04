{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Telemetry.Types
Description : Core types for full-take telemetry capture

This module defines the event schema for capturing every interaction
with the AI coding agent. The goal is zero data loss - every prompt,
response, tool invocation, and file change is recorded.

== Design Principles

1. **Append-only**: Events are never modified, only appended
2. **Self-describing**: Each event contains enough context to understand it
3. **Ordered**: ULID + sequence number ensures global and local ordering
4. **Rich metadata**: Token counts, latencies, model info for training

@since 0.1.0
-}
module Telemetry.Types (
    -- * Core Types
    TelemetryEvent (..),
    EventMeta (..),
    EventId,
    SessionId,
    ProjectId,

    -- * Smart Constructors
    mkTelemetryEvent,
    mkEventMeta,
    emptyMeta,

    -- * Utilities
    eventToJSONL,
) where

import Data.Aeson (ToJSON (..), Value, encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

-- | Unique event identifier (ULID format for time-ordered global uniqueness)
type EventId = Text

-- | Session identifier
type SessionId = Text

-- | Project identifier  
type ProjectId = Text

-- | Rich metadata attached to each telemetry event
data EventMeta = EventMeta
    { emModel :: Maybe Text
    -- ^ Model ID (e.g., "anthropic/claude-sonnet-4-20250514")
    , emAgent :: Maybe Text
    -- ^ Agent name (e.g., "general", "explore")
    , emTokensIn :: Maybe Word32
    -- ^ Input token count
    , emTokensOut :: Maybe Word32
    -- ^ Output token count
    , emLatencyMs :: Maybe Word32
    -- ^ Time to first token in milliseconds
    , emTotalLatencyMs :: Maybe Word32
    -- ^ Total request duration in milliseconds
    , emToolName :: Maybe Text
    -- ^ Tool name if this is a tool event
    , emParentEvent :: Maybe EventId
    -- ^ Parent event for correlation (e.g., tool call -> message)
    , emErrorMessage :: Maybe Text
    -- ^ Error message if this event represents a failure
    }
    deriving (Show, Eq, Generic)

instance ToJSON EventMeta where
    toJSON m =
        object
            [ "model" .= emModel m
            , "agent" .= emAgent m
            , "tokens_in" .= emTokensIn m
            , "tokens_out" .= emTokensOut m
            , "latency_ms" .= emLatencyMs m
            , "total_latency_ms" .= emTotalLatencyMs m
            , "tool_name" .= emToolName m
            , "parent_event" .= emParentEvent m
            , "error_message" .= emErrorMessage m
            ]

-- | A single telemetry event capturing one interaction
data TelemetryEvent = TelemetryEvent
    { teId :: EventId
    -- ^ Globally unique event ID (ULID)
    , teSeq :: Word64
    -- ^ Session-local sequence number for ordering
    , teTimestamp :: UTCTime
    -- ^ Wall clock time
    , teMonotonicNs :: Word64
    -- ^ Monotonic nanoseconds for accurate duration calculation
    , teSessionId :: SessionId
    -- ^ Session this event belongs to
    , teProjectId :: ProjectId
    -- ^ Project context
    , teDirectory :: Text
    -- ^ Working directory
    , teType :: Text
    -- ^ Event type (e.g., "message.created", "tool.completed")
    , tePayload :: Value
    -- ^ Full event payload (raw JSON from bus)
    , teMeta :: EventMeta
    -- ^ Rich metadata for training
    }
    deriving (Show, Eq, Generic)

instance ToJSON TelemetryEvent where
    toJSON e =
        object
            [ "id" .= teId e
            , "seq" .= teSeq e
            , "timestamp" .= teTimestamp e
            , "monotonic_ns" .= teMonotonicNs e
            , "session_id" .= teSessionId e
            , "project_id" .= teProjectId e
            , "directory" .= teDirectory e
            , "type" .= teType e
            , "payload" .= tePayload e
            , "meta" .= teMeta e
            ]

-- | Create empty metadata (for events without rich context)
emptyMeta :: EventMeta
emptyMeta =
    EventMeta
        { emModel = Nothing
        , emAgent = Nothing
        , emTokensIn = Nothing
        , emTokensOut = Nothing
        , emLatencyMs = Nothing
        , emTotalLatencyMs = Nothing
        , emToolName = Nothing
        , emParentEvent = Nothing
        , emErrorMessage = Nothing
        }

-- | Create metadata with common fields
mkEventMeta :: Maybe Text -> Maybe Text -> EventMeta
mkEventMeta model agent = emptyMeta{emModel = model, emAgent = agent}

-- | Create a telemetry event (sequence number must be provided by caller)
mkTelemetryEvent ::
    EventId ->
    Word64 ->
    UTCTime ->
    Word64 ->
    SessionId ->
    ProjectId ->
    Text ->
    Text ->
    Value ->
    EventMeta ->
    TelemetryEvent
mkTelemetryEvent = TelemetryEvent

-- | Serialize event to JSON Lines format (newline-delimited JSON)
eventToJSONL :: TelemetryEvent -> ByteString
eventToJSONL e = encode e <> "\n"
