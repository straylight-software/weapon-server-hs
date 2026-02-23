{-# LANGUAGE OverloadedStrings #-}

module Global.Event (
    globalEventHandler,
    eventHandler,

    -- * Exported for testing
    wrapGlobalEvent,
) where

import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Builder (lazyByteString, string8)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Katip qualified
import Log (Logger, logMsg)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseStream)
import Servant (Handler, Tagged (..))
import State

-- | Wrap a payload in GlobalEvent format: { directory: "...", payload: { type: "...", properties: {...} } }
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

-- | Helper for logging
logSSE :: Logger -> Text -> IO ()
logSSE lg = logMsg lg Katip.InfoS

-- | SSE Handler for /global/event - returns all events wrapped with directory
globalEventHandler :: AppState -> Tagged Handler Application
globalEventHandler state = Tagged $ \_ respond' -> do
    let logger = stLogger state
    tid <- myThreadId
    logSSE logger $ "global/event: client connected (thread " <> T.pack (show tid) <> ")"

    chan <- atomically $ dupTChan (stEventChan state)
    let directory = stDirectory state

    respond'
        $ responseStream
            status200
            [ ("Content-Type", "text/event-stream")
            , ("Cache-Control", "no-cache")
            , ("Connection", "keep-alive")
            , ("X-Accel-Buffering", "no")
            , ("X-Content-Type-Options", "nosniff")
            ]
        $ \send flush -> do
            logSSE logger "global/event: starting stream body"

            -- Send server.connected wrapped in GlobalEvent format
            let connectedEvent = wrapGlobalEvent directory "server.connected" (object [])
            logSSE logger "global/event: sending server.connected"
            send $ string8 "data: "
            send $ lazyByteString (encode connectedEvent)
            send $ string8 "\n\n"
            flush
            logSSE logger "global/event: flushed server.connected"

            -- Start heartbeat thread (every 10 seconds)
            _ <- forkIO $ forever $ do
                threadDelay (10 * 1000000) -- 10 seconds
                logSSE logger "global/event: sending heartbeat"
                let heartbeatEvent = wrapGlobalEvent directory "server.heartbeat" (object [])
                send $ string8 "data: "
                send $ lazyByteString (encode heartbeatEvent)
                send $ string8 "\n\n"
                flush
                    `catch` \(e :: SomeException) ->
                        logSSE logger $ "global/event: heartbeat flush error: " <> T.pack (show e)

            logSSE logger "global/event: entering event loop"
            let loop = do
                    logSSE logger "global/event: waiting for event from bus..."
                    val <- atomically $ readTChan chan
                    logSSE logger "global/event: got event from bus, sending"
                    -- Wrap the event in GlobalEvent format
                    -- val is already { type, properties } from BusEvent
                    let wrappedVal =
                            object
                                [ "directory" .= directory
                                , "payload" .= val
                                ]
                    send $ string8 "data: "
                    send $ lazyByteString (encode wrappedVal)
                    send $ string8 "\n\n"
                    flush
                    logSSE logger "global/event: event sent and flushed"
                    loop
            loop
                `catch` \(e :: SomeException) ->
                    logSSE logger $ "global/event: loop ended with: " <> T.pack (show e)

-- | SSE Handler for /event - sends raw events (no directory/payload wrapper)
eventHandler :: AppState -> Tagged Handler Application
eventHandler state = Tagged $ \_req respond' -> do
    let logger = stLogger state
    tid <- myThreadId
    logSSE logger $ "event: client connected (thread " <> T.pack (show tid) <> ")"

    chan <- atomically $ dupTChan (stEventChan state)

    respond'
        $ responseStream
            status200
            [ ("Content-Type", "text/event-stream")
            , ("Cache-Control", "no-cache")
            , ("Connection", "keep-alive")
            , ("X-Accel-Buffering", "no")
            , ("X-Content-Type-Options", "nosniff")
            ]
        $ \send flush -> do
            logSSE logger "event: starting stream body"

            -- Send server.connected as raw event
            send $ string8 "data: {\"type\":\"server.connected\",\"properties\":{}}\n\n"
            flush
            logSSE logger "event: flushed server.connected"

            -- Start heartbeat thread (every 10 seconds)
            _ <- forkIO $ forever $ do
                threadDelay (10 * 1000000) -- 10 seconds
                logSSE logger "event: sending heartbeat"
                let heartbeatEvent =
                        object
                            [ "type" .= ("server.heartbeat" :: Text)
                            , "properties" .= object []
                            ]
                send $ string8 "data: "
                send $ lazyByteString (encode heartbeatEvent)
                send $ string8 "\n\n"
                flush
                    `catch` \(e :: SomeException) ->
                        logSSE logger $ "event: heartbeat flush error: " <> T.pack (show e)

            logSSE logger "event: entering event loop"
            let loop = do
                    logSSE logger "event: waiting for event from bus..."
                    val <- atomically $ readTChan chan
                    -- Log the event type for debugging
                    let eventJson = encode val
                    logSSE logger $ "event: got event, sending: " <> T.take 200 (TE.decodeUtf8 (BSL.toStrict eventJson))
                    -- Send raw event (val is already { type, properties })
                    send $ string8 "data: "
                    send $ lazyByteString eventJson
                    send $ string8 "\n\n"
                    flush
                    logSSE logger "event: event sent and flushed"
                    loop
            loop
                `catch` \(e :: SomeException) ->
                    logSSE logger $ "event: loop ended with: " <> T.pack (show e)
