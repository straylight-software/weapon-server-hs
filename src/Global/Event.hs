{-# LANGUAGE OverloadedStrings #-}

module Global.Event (
    globalEventHandler,
    eventHandler,

    -- * Exported for testing
    wrapGlobalEvent,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Builder (lazyByteString, string8)
import Data.Text (Text)
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

-- | SSE Handler for /global/event - returns all events wrapped with directory
globalEventHandler :: AppState -> Tagged Handler Application
globalEventHandler state = Tagged $ \_ respond' -> do
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
            -- Send server.connected wrapped in GlobalEvent format
            let connectedEvent = wrapGlobalEvent directory "server.connected" (object [])
            send $ string8 "data: "
            send $ lazyByteString (encode connectedEvent)
            send $ string8 "\n\n"
            flush

            -- Start heartbeat thread (every 10 seconds)
            _ <- forkIO $ forever $ do
                threadDelay (10 * 1000000) -- 10 seconds
                let heartbeatEvent = wrapGlobalEvent directory "server.heartbeat" (object [])
                send $ string8 "data: "
                send $ lazyByteString (encode heartbeatEvent)
                send $ string8 "\n\n"
                flush

            let loop = do
                    val <- atomically $ readTChan chan
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
                    loop
            loop

-- | SSE Handler for /event - sends raw events (no directory/payload wrapper)
eventHandler :: AppState -> Tagged Handler Application
eventHandler state = Tagged $ \_req respond' -> do
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
            -- Send server.connected as raw event
            send $ string8 "data: {\"type\":\"server.connected\",\"properties\":{}}\n\n"
            flush

            -- Start heartbeat thread (every 10 seconds)
            _ <- forkIO $ forever $ do
                threadDelay (10 * 1000000) -- 10 seconds
                let heartbeatEvent =
                        object
                            [ "type" .= ("server.heartbeat" :: Text)
                            , "properties" .= object []
                            ]
                send $ string8 "data: "
                send $ lazyByteString (encode heartbeatEvent)
                send $ string8 "\n\n"
                flush

            let loop = do
                    val <- atomically $ readTChan chan
                    -- Send raw event (val is already { type, properties })
                    send $ string8 "data: "
                    send $ lazyByteString (encode val)
                    send $ string8 "\n\n"
                    flush
                    loop
            loop
