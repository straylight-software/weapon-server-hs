{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Global.Event
Description : Server-Sent Events (SSE) handlers for real-time event streaming
Stability   : experimental

This module provides SSE (Server-Sent Events) handlers for streaming events
to connected clients. Two endpoints are supported:

* @\/global\/event@ - Events wrapped in GlobalEvent format with directory context
* @\/event@ - Raw events without directory wrapper (backwards compatibility)

Both handlers:

* Send a @server.connected@ event immediately on connection
* Send @server.heartbeat@ events every 10 seconds to keep the connection alive
* Forward all bus events to connected clients

= Event Formats

== GlobalEvent (\/global\/event)

@
{
  "directory": "\/path\/to\/project",
  "payload": {
    "type": "event.type",
    "properties": { ... }
  }
}
@

== Raw Event (\/event)

@
{
  "type": "event.type",
  "properties": { ... }
}
@
-}
module Global.Event (
    -- * SSE Handlers
    globalEventHandler,
    eventHandler,

    -- * Pure Event Formatting
    -- $formatting

    -- ** GlobalEvent format
    wrapGlobalEvent,
    wrapEventPayload,

    -- ** Raw event format
    mkRawEvent,
    serverConnectedRaw,
    serverHeartbeatRaw,

    -- ** SSE wire format
    formatSSEMessage,

    -- * Configuration
    sseHeaders,
    heartbeatIntervalMicros,
) where

import Control.Concurrent (myThreadId, threadDelay)
import Util.Thread (forkLogged)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, throwIO)
import Control.Monad (forever)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Builder (Builder, lazyByteString, string8)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Katip qualified
import Log (Logger, logMsg)
import Network.HTTP.Types (ResponseHeaders, status200)
import Network.Wai (Application, responseStream)
import Servant (Handler, Tagged (..))
import State

-- ═══════════════════════════════════════════════════════════════════════════
-- Configuration
-- ═══════════════════════════════════════════════════════════════════════════

{- | Standard SSE response headers.

These headers configure the response for proper SSE streaming:

* @Content-Type: text\/event-stream@ - Required for SSE
* @Cache-Control: no-cache@ - Prevents caching of the stream
* @Connection: keep-alive@ - Maintains persistent connection
* @X-Accel-Buffering: no@ - Disables nginx buffering
* @X-Content-Type-Options: nosniff@ - Security header
-}
sseHeaders :: ResponseHeaders
sseHeaders =
    [ ("Content-Type", "text/event-stream")
    , ("Cache-Control", "no-cache")
    , ("Connection", "keep-alive")
    , ("X-Accel-Buffering", "no")
    , ("X-Content-Type-Options", "nosniff")
    ]

{- | Heartbeat interval in microseconds (10 seconds).

Heartbeats keep the SSE connection alive and help detect
disconnected clients.
-}
heartbeatIntervalMicros :: Int
heartbeatIntervalMicros = 10 * 1000000

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Event Formatting
-- ═══════════════════════════════════════════════════════════════════════════

{- $formatting
These functions construct event payloads in various formats.
They are pure and can be easily tested without IO.
-}

{- | Wrap a payload in GlobalEvent format.

Creates a JSON object with the structure:

@
{
  "directory": \<dir\>,
  "payload": {
    "type": \<eventType\>,
    "properties": \<props\>
  }
}
@

This is the format used by the @\/global\/event@ endpoint.

==== __Examples__

>>> wrapGlobalEvent "/home/user" "server.connected" (object [])
Object (fromList [("directory",String "/home/user"),("payload",Object ...)])
-}
wrapGlobalEvent :: Text -> Text -> Value -> Value
wrapGlobalEvent dir eventType props =
    object
        [ "directory" .= dir
        , "payload"
            .= object
                [ "type" .= eventType
                , "properties" .= props
                ]
        ]

{- | Wrap a raw event payload with directory context.

Similar to 'wrapGlobalEvent' but takes a pre-formed payload object
(containing @type@ and @properties@) rather than constructing it.

@
{
  "directory": \<dir\>,
  "payload": \<payload\>
}
@
-}
wrapEventPayload :: Text -> Value -> Value
wrapEventPayload dir payload =
    object
        [ "directory" .= dir
        , "payload" .= payload
        ]

{- | Create a raw event object.

Creates a JSON object with the structure:

@
{
  "type": \<eventType\>,
  "properties": \<props\>
}
@

This is the format used by the @\/event@ endpoint.

==== __Examples__

>>> mkRawEvent "server.heartbeat" (object [])
Object (fromList [("type",String "server.heartbeat"),("properties",Object ...)])
-}
mkRawEvent :: Text -> Value -> Value
mkRawEvent eventType props =
    object
        [ "type" .= eventType
        , "properties" .= props
        ]

{- | Pre-formatted server.connected event for raw SSE.

This is a pre-encoded JSON string for efficiency since this event
is sent on every new connection.
-}
serverConnectedRaw :: BSL.ByteString
serverConnectedRaw = "{\"type\":\"server.connected\",\"properties\":{}}"

-- | Create a server.heartbeat event in raw format.
serverHeartbeatRaw :: Value
serverHeartbeatRaw = mkRawEvent "server.heartbeat" (object [])

{- | Format a JSON value as an SSE data message.

Produces SSE wire format: @data: \<json\>\\n\\n@

Returns a list of 'Builder' chunks for efficient streaming.
-}
formatSSEMessage :: BSL.ByteString -> [Builder]
formatSSEMessage jsonBytes =
    [ string8 "data: "
    , lazyByteString jsonBytes
    , string8 "\n\n"
    ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Internal Helpers
-- ═══════════════════════════════════════════════════════════════════════════

{- | Log an SSE-related message at INFO level.

Internal helper that provides consistent logging format for SSE events.
-}
logSSE :: Logger -> Text -> IO ()
logSSE lg = logMsg lg Katip.InfoS

{- | Send an SSE message using the provided send function.

Takes the raw JSON bytes and sends them in SSE wire format.
-}
sendSSE :: (Builder -> IO ()) -> BSL.ByteString -> IO ()
sendSSE send jsonBytes = mapM_ send (formatSSEMessage jsonBytes)

{- | Start a heartbeat thread that sends periodic events.

The heartbeat thread runs forever until the connection closes.
Any errors during flush are logged but don't stop the heartbeat.

Returns the forked thread (for potential cleanup, though typically
the thread terminates when the connection closes).
-}
startHeartbeat ::
    -- | Logger for error reporting
    Logger ->
    -- | Log prefix (e.g., "global/event" or "event")
    Text ->
    -- | Function to create heartbeat event JSON
    IO BSL.ByteString ->
    -- | SSE send function
    (Builder -> IO ()) ->
    -- | SSE flush function
    IO () ->
    IO ()
startHeartbeat logger prefix mkHeartbeat send flush = do
    _ <- forkLogged logger (prefix <> "-heartbeat") $ forever $ do
        threadDelay heartbeatIntervalMicros
        logSSE logger $ prefix <> ": sending heartbeat"
        heartbeatJson <- mkHeartbeat
        sendSSE send heartbeatJson
        flush
            `catch` \(e :: SomeException) ->
                logSSE logger $ prefix <> ": heartbeat flush error: " <> T.pack (show e)
    pure ()

{- | Run the main event loop that forwards bus events to SSE.

Reads events from the channel and sends them to the client.
Continues until an exception occurs (typically connection close).
-}
runEventLoop ::
    -- | Logger for tracing
    Logger ->
    -- | Log prefix
    Text ->
    -- | Event channel to read from
    TChan Value ->
    -- | Transform event before sending
    (Value -> BSL.ByteString) ->
    -- | Optional event logging function
    Maybe (BSL.ByteString -> IO ()) ->
    -- | SSE send function
    (Builder -> IO ()) ->
    -- | SSE flush function
    IO () ->
    IO ()
runEventLoop logger prefix chan transformEvent logEvent send flush = do
    logSSE logger $ prefix <> ": entering event loop"
    let loop = do
            logSSE logger $ prefix <> ": waiting for event from bus..."
            val <- atomically $ readTChan chan
            let eventJson = transformEvent val
            -- Optional event-specific logging
            case logEvent of
                Just logFn -> logFn eventJson
                Nothing -> logSSE logger $ prefix <> ": got event from bus, sending"
            sendSSE send eventJson
            flush
            logSSE logger $ prefix <> ": event sent and flushed"
            loop
    loop
        `catch` \(e :: SomeException) -> do
            -- Log at ERROR level - SSE loop termination is a critical failure
            -- that leaves connected clients in a broken state
            logMsg logger Katip.ErrorS $ prefix <> ": event loop terminated: " <> T.pack (show e)
            -- Re-throw so the caller (WAI/Servant) can properly close the connection
            throwIO e

-- ═══════════════════════════════════════════════════════════════════════════
-- SSE Handlers
-- ═══════════════════════════════════════════════════════════════════════════

{- | SSE handler for @\/global\/event@ endpoint.

Returns all events wrapped in GlobalEvent format with directory context.
This is the preferred endpoint for clients that need to handle events
from multiple project directories.

== Connection Lifecycle

1. Client connects
2. Server sends @server.connected@ event immediately
3. Server starts heartbeat thread (every 10 seconds)
4. Server forwards all bus events to client
5. Connection closes on client disconnect or error

== Event Format

All events are wrapped in GlobalEvent format:

@
{
  "directory": "\/path\/to\/project",
  "payload": { "type": "...", "properties": {...} }
}
@
-}
globalEventHandler :: AppState -> Tagged Handler Application
globalEventHandler state = Tagged $ \_ respond' -> do
    let logger = stLogger state
    let directory = stDirectory state
    let prefix = "global/event"

    -- Log connection
    tid <- myThreadId
    logSSE logger $ prefix <> ": client connected (thread " <> T.pack (show tid) <> ")"

    -- Duplicate channel for this client
    chan <- atomically $ dupTChan (stEventChan state)

    respond' $ responseStream status200 sseHeaders $ \send flush -> do
        logSSE logger $ prefix <> ": starting stream body"

        -- Send initial connection event
        let connectedEvent = wrapGlobalEvent directory "server.connected" (object [])
        logSSE logger $ prefix <> ": sending server.connected"
        sendSSE send (encode connectedEvent)
        flush
        logSSE logger $ prefix <> ": flushed server.connected"

        -- Start heartbeat
        let mkHeartbeat = pure $ encode $ wrapGlobalEvent directory "server.heartbeat" (object [])
        startHeartbeat logger prefix mkHeartbeat send flush

        -- Run main event loop
        let transformEvent = encode . wrapEventPayload directory
        runEventLoop logger prefix chan transformEvent Nothing send flush

{- | SSE handler for @\/event@ endpoint.

Returns raw events without directory wrapper. This endpoint exists
for backwards compatibility with clients that don't need multi-directory
support.

== Connection Lifecycle

Same as 'globalEventHandler'.

== Event Format

Events are sent in raw format:

@
{
  "type": "...",
  "properties": {...}
}
@
-}
eventHandler :: AppState -> Tagged Handler Application
eventHandler state = Tagged $ \_req respond' -> do
    let logger = stLogger state
    let prefix = "event"

    -- Log connection
    tid <- myThreadId
    logSSE logger $ prefix <> ": client connected (thread " <> T.pack (show tid) <> ")"

    -- Duplicate channel for this client
    chan <- atomically $ dupTChan (stEventChan state)

    respond' $ responseStream status200 sseHeaders $ \send flush -> do
        logSSE logger $ prefix <> ": starting stream body"

        -- Send initial connection event (pre-formatted for efficiency)
        sendSSE send serverConnectedRaw
        flush
        logSSE logger $ prefix <> ": flushed server.connected"

        -- Start heartbeat
        let mkHeartbeat = pure $ encode serverHeartbeatRaw
        startHeartbeat logger prefix mkHeartbeat send flush

        -- Run main event loop with verbose logging
        let transformEvent = encode
        let logEventFn jsonBytes =
                logSSE logger $ prefix <> ": got event, sending: " <> T.take 200 (TE.decodeUtf8 (BSL.toStrict jsonBytes))
        runEventLoop logger prefix chan transformEvent (Just logEventFn) send flush
