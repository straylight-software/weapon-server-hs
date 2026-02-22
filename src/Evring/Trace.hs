{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Event trace recording and replay for deterministic testing.
--
-- A 'Trace' captures completion events from a machine run, allowing
-- exact replay without actual I/O. This is the key to testability:
-- record once, replay deterministically forever.
--
-- Usage:
--
-- @
-- -- Record a trace during actual I/O
-- (result, trace) <- runTraced ring machine
--
-- -- Later, replay without I/O
-- let replayResult = replay machine (traceEvents trace)
-- @
module Evring.Trace
  ( -- * Trace type
    Trace(..)
  , emptyTrace
    -- * Recording
  , record
  , recordAll
    -- * Accessors
  , traceEvents
  , traceSize
    -- * Serialization (for golden tests)
  , serializeTrace
  , deserializeTrace
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)

import Evring.Event (Event(eventData))

-- | A trace: recorded events for replay testing.
--
-- The trace owns copies of all event data, so it can outlive
-- the original buffers used during the actual I/O.
data Trace = Trace
  { _traceEvents :: ![Event]
    -- ^ Events in order of occurrence
  } deriving stock (Eq, Show, Generic)

-- | Empty trace.
emptyTrace :: Trace
emptyTrace = Trace []

-- | Record a single event into a trace.
--
-- Note: We copy the event data so the trace owns its data.
record :: Event -> Trace -> Trace
record event (Trace events) = Trace (events ++ [copyEvent event])
  where
    -- Ensure we own the ByteString data
    copyEvent e = e { eventData = BS.copy (eventData e) }

-- | Record multiple events.
recordAll :: [Event] -> Trace -> Trace
recordAll newEvents trace = foldr record trace (reverse newEvents)

-- | Get all events from a trace.
traceEvents :: Trace -> [Event]
traceEvents (Trace events) = events

-- | Get the number of events in a trace.
traceSize :: Trace -> Int
traceSize (Trace events) = length events

-- | Serialize a trace to bytes (for golden tests / persistence).
--
-- Format: Simple length-prefixed encoding.
-- This is a placeholder - real implementation would use a proper format.
serializeTrace :: Trace -> ByteString
serializeTrace (Trace _events) = BS.empty  -- TODO: implement

-- | Deserialize a trace from bytes.
deserializeTrace :: ByteString -> Either String Trace
deserializeTrace _bs = Right emptyTrace  -- TODO: implement
