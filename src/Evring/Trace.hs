{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

{- |
Module      : Evring.Trace
Description : Event trace recording and replay for deterministic testing
Stability   : stable

A 'Trace' captures completion events from a machine run, allowing
exact replay without actual I/O. This is the key to testability:
record once, replay deterministically forever.

= Recording

@
-- Record a trace during actual I/O
(result, trace) <- runTraced ring machine

-- Save for later
BS.writeFile \"trace.bin\" (serializeTrace trace)
@

= Replay

@
-- Load saved trace
Right trace <- deserializeTrace \<$\> BS.readFile \"trace.bin\"

-- Replay without I/O (deterministic!)
let replayResult = replay machine (traceEvents trace)
@

= Golden Testing

Traces enable golden testing: record the \"correct\" behavior once,
then verify future runs match exactly:

@
test_golden :: TestTree
test_golden = testCase \"behavior matches golden trace\" $ do
    golden <- loadGoldenTrace \"test\/golden\/my_machine.trace\"
    let result = replay myMachine (traceEvents golden)
    result \@?= expectedFinalState
@
-}
module Evring.Trace (
    -- * Trace type
    Trace (..),
    emptyTrace,

    -- * Recording
    record,
    recordAll,

    -- * Accessors
    traceEvents,
    traceSize,

    -- * Serialization (for golden tests)
    serializeTrace,
    deserializeTrace,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)

import Evring.Event (Event (eventData))

{- | A trace: recorded events for replay testing.

The trace owns copies of all event data, so it can outlive
the original buffers used during the actual I/O.
-}
data Trace = Trace
    { _traceEvents :: ![Event]
    -- ^ Events in order of occurrence
    , _traceSize :: !Int
    -- ^ Cached size for O(1) access
    }
    deriving stock (Eq, Show, Generic)

-- | Empty trace.
emptyTrace :: Trace
emptyTrace = Trace [] 0

{- | Record a single event into a trace.

Note: We copy the event data so the trace owns its data.
-}
record :: Event -> Trace -> Trace
record event (Trace events size) = Trace (events ++ [copyEvent event]) (size + 1)
  where
    -- Ensure we own the ByteString data
    copyEvent e = e{eventData = BS.copy (eventData e)}

-- | Record multiple events.
recordAll :: [Event] -> Trace -> Trace
recordAll newEvents trace = foldl' (flip record) trace newEvents

-- | Get all events from a trace.
traceEvents :: Trace -> [Event]
traceEvents (Trace events _size) = events

-- | Get the number of events in a trace.
traceSize :: Trace -> Int
traceSize (Trace _events size) = size

{- | Serialize a trace to bytes (for golden tests / persistence).

TODO[b7r6]: NOT YET IMPLEMENTED. Will crash if called.

When implementing: use Binary or Aeson for Event serialization.
The Event type contains Handle (use packHandle\/unpackHandle),
OperationType, Int64, ByteString, and Word64.
-}
serializeTrace :: Trace -> ByteString
serializeTrace _ =
    error "serializeTrace: not implemented - see Evring.Trace module for implementation notes"

{- | Deserialize a trace from bytes.

TODO[b7r6]: NOT YET IMPLEMENTED. Will crash if called.

When implementing: must roundtrip with serializeTrace.
-}
deserializeTrace :: ByteString -> Either String Trace
deserializeTrace _ =
    error "deserializeTrace: not implemented - see Evring.Trace module for implementation notes"
