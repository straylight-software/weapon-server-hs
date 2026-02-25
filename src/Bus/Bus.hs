{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Bus.Bus
Description : STM-based pub/sub event system for the AI coding agent

This module provides a thread-safe, STM-based publish/subscribe event bus
for broadcasting events throughout the application. It mirrors the TypeScript
Bus namespace and is the primary mechanism for:

* SSE (Server-Sent Events) delivery to connected clients
* Inter-component communication (sessions, messages, PTY, etc.)
* Decoupled event-driven architecture

== Architecture

The bus uses 'TChan' broadcast semantics:

* Publishers write to a single broadcast channel
* Each subscriber gets their own duplicate channel
* Events published before subscription are not seen
* Unsubscribe kills the listener thread

== Usage Example

@
import Bus.Bus
import Bus.Event (EventType(..), eventTypeToText)
import Data.Aeson (object, (.=))

main :: IO ()
main = do
    bus <- newBus

    -- Subscribe to session events
    unsubscribe <- subscribe bus "session.created" $ \\event ->
        putStrLn $ "Session created: " ++ show (beProperties event)

    -- Publish an event
    publish bus "session.created" (object ["sessionID" .= "abc123"])

    -- Later: stop receiving events
    unsubscribe
@

@since 0.1.0
-}
module Bus.Bus (
    -- * Types
    Bus,
    BusEvent (..),

    -- * Bus Operations
    newBus,
    publish,
    publishTyped,

    -- * Subscriptions
    subscribe,
    subscribeAll,
    subscribeTyped,

    -- * Pure Helpers
    mkBusEvent,
    matchesEventType,

    -- * STM Operations (for advanced use)
    publishSTM,
    dupBusChan,
) where

import Bus.Event (EventType, eventTypeToText)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
import Control.Monad (forever, when)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)

-- ═══════════════════════════════════════════════════════════════════════════
-- Types
-- ═══════════════════════════════════════════════════════════════════════════

{- | A bus event with a type identifier and JSON properties payload.

This is the wire format used for SSE events and internal pub/sub.
The 'beType' uses the dotted notation (e.g., @"session.created"@).
-}
data BusEvent = BusEvent
    { beType :: Text
    -- ^ Event type in dotted notation (e.g., @"session.created"@)
    , beProperties :: Value
    -- ^ JSON payload with event-specific data
    }
    deriving (Show, Eq)

instance ToJSON BusEvent where
    toJSON = busEventToJSON

instance FromJSON BusEvent where
    parseJSON = parseBusEventJSON

{- | The event bus - a broadcast channel for pub/sub messaging.

The bus is thread-safe and supports multiple concurrent publishers
and subscribers. It uses STM 'TChan' broadcast semantics.
-}
newtype Bus = Bus {unBus :: TChan BusEvent}

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Functions (testable without IO)
-- ═══════════════════════════════════════════════════════════════════════════

{- | Create a 'BusEvent' from type and properties (pure).

==== __Examples__

>>> import Data.Aeson (object, (.=))
>>> mkBusEvent "session.created" (object ["sessionID" .= "abc"])
BusEvent {beType = "session.created", beProperties = ...}
-}
mkBusEvent :: Text -> Value -> BusEvent
mkBusEvent = BusEvent

{- | Check if a 'BusEvent' matches a given event type (pure).

==== __Examples__

>>> let event = mkBusEvent "session.created" Null
>>> matchesEventType "session.created" event
True
>>> matchesEventType "session.deleted" event
False
-}
matchesEventType :: Text -> BusEvent -> Bool
matchesEventType expectedType event = beType event == expectedType

-- | Pure JSON serialization for 'BusEvent'.
busEventToJSON :: BusEvent -> Value
busEventToJSON e =
    object
        [ "type" .= beType e
        , "properties" .= beProperties e
        ]

-- | Pure JSON parsing for 'BusEvent'.
parseBusEventJSON :: Value -> Parser BusEvent
parseBusEventJSON = withObject "BusEvent" $ \v ->
    BusEvent
        <$> v .: "type"
        <*> v .: "properties"

-- ═══════════════════════════════════════════════════════════════════════════
-- Bus Creation
-- ═══════════════════════════════════════════════════════════════════════════

{- | Create a new event bus.

The bus is initially empty and ready for publishers and subscribers.
Uses a broadcast 'TChan' under the hood for fan-out delivery.

==== __Examples__

>>> bus <- newBus
>>> -- Now ready for publish/subscribe
-}
newBus :: IO Bus
newBus = Bus <$> newBroadcastTChanIO

-- ═══════════════════════════════════════════════════════════════════════════
-- Publishing
-- ═══════════════════════════════════════════════════════════════════════════

{- | Publish an event to the bus with a raw 'Text' event type.

All current subscribers (created before this call) will receive the event.
This is a non-blocking operation.

==== __Examples__

>>> bus <- newBus
>>> publish bus "session.created" (object ["sessionID" .= "abc"])
-}
publish :: Bus -> Text -> Value -> IO ()
publish bus typ props = atomically $ publishSTM bus typ props

{- | Publish an event using a typed 'EventType'.

This is the preferred way to publish events as it provides type safety.

==== __Examples__

>>> import Bus.Event (EventType(..))
>>> bus <- newBus
>>> publishTyped bus SessionCreated (object ["sessionID" .= "abc"])
-}
publishTyped :: Bus -> EventType -> Value -> IO ()
publishTyped bus et = publish bus (eventTypeToText et)

{- | STM action to publish an event (composable with other STM operations).

Use this when you need to publish as part of a larger STM transaction.
-}
publishSTM :: Bus -> Text -> Value -> STM ()
publishSTM bus typ props = writeTChan (unBus bus) (mkBusEvent typ props)

-- ═══════════════════════════════════════════════════════════════════════════
-- Subscriptions
-- ═══════════════════════════════════════════════════════════════════════════

{- | Subscribe to all events on the bus.

Returns an unsubscribe action that stops delivery and cleans up resources.
The callback is invoked on a dedicated thread for each event.

__Important__: Events published before this subscription are NOT received.

==== __Examples__

>>> bus <- newBus
>>> unsubscribe <- subscribeAll bus $ \event ->
>>>     putStrLn $ "Received: " ++ show (beType event)
>>> -- Later: stop receiving
>>> unsubscribe
-}
subscribeAll :: Bus -> (BusEvent -> IO ()) -> IO (IO ())
subscribeAll bus callback = do
    chan <- dupBusChan bus
    tid <- startSubscriberThread chan callback
    pure $ stopSubscriber tid

{- | Subscribe to events of a specific type (using raw 'Text').

Only events matching the given type string are delivered to the callback.
Returns an unsubscribe action.

==== __Examples__

>>> bus <- newBus
>>> unsubscribe <- subscribe bus "session.created" $ \event ->
>>>     putStrLn "New session!"
>>> unsubscribe
-}
subscribe :: Bus -> Text -> (BusEvent -> IO ()) -> IO (IO ())
subscribe bus eventType callback = subscribeAll bus $ \event ->
    when (matchesEventType eventType event) $ callback event

{- | Subscribe to events using a typed 'EventType'.

This is the preferred way to subscribe as it provides type safety.

==== __Examples__

>>> import Bus.Event (EventType(..))
>>> bus <- newBus
>>> unsubscribe <- subscribeTyped bus SessionCreated $ \event ->
>>>     putStrLn "New session!"
>>> unsubscribe
-}
subscribeTyped :: Bus -> EventType -> (BusEvent -> IO ()) -> IO (IO ())
subscribeTyped bus et = subscribe bus (eventTypeToText et)

-- ═══════════════════════════════════════════════════════════════════════════
-- STM Operations (advanced)
-- ═══════════════════════════════════════════════════════════════════════════

{- | Duplicate the bus channel for manual reading.

This is useful for SSE handlers that need direct 'TChan' access.
The returned channel receives all events published after duplication.
-}
dupBusChan :: Bus -> IO (TChan BusEvent)
dupBusChan bus = atomically $ dupTChan (unBus bus)

-- ═══════════════════════════════════════════════════════════════════════════
-- Internal Helpers (separated for testability)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Start a subscriber thread that reads from a channel and invokes the callback.
startSubscriberThread :: TChan BusEvent -> (BusEvent -> IO ()) -> IO ThreadId
startSubscriberThread chan callback =
    forkIO $ forever $ do
        event <- atomically $ readTChan chan
        callback event

-- | Stop a subscriber by killing its thread.
stopSubscriber :: ThreadId -> IO ()
stopSubscriber = killThread
