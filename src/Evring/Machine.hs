{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Pure state machine abstractions for async I/O.
--
-- The core pattern is the Mealy machine:
--
-- @
--   State × Event → State × [Operation]
-- @
--
-- Machines are pure: no IO in step/generate, only in the runner.
-- This enables deterministic replay testing.
module Evring.Machine
  ( -- * Core Machine class
    Machine(..)
  , StepResult(..)
    -- * Generator Machine extension
  , GeneratorMachine(..)
    -- * Replay (pure, for testing)
  , replay
  , replayWithOperations
  , replayGenerate
  ) where

import Evring.Event (Event, Operation)

-- | Result of a state machine step: new state + operations to enqueue.
data StepResult s = StepResult
  { stepState      :: !s
  , stepOperations :: ![Operation]
  } deriving (Eq, Show)

-- | Machine concept: the pure functional core.
--
-- Implementations define: @step state event → (state, [operation])@
--
-- Laws:
--
-- 1. @done (stepState (step m (initial m) e))@ should eventually become True
--    for well-behaved machines
-- 2. @step@ must be pure and deterministic
-- 3. Operations returned by @step@ are requests to the kernel, not effects
class Machine m where
  -- | The state type for this machine
  type State m

  -- | Initial state
  initial :: m -> State m

  -- | Process an event, returning new state and operations to submit
  step :: m -> State m -> Event -> StepResult (State m)

  -- | Check if machine has completed its work
  done :: m -> State m -> Bool

-- | Generator machine: can produce operations without waiting for completions.
--
-- Used for bulk operations where we know the work upfront (e.g., stat 10000 files).
-- The runner calls @generate@ when the ring has capacity and @wantsToSubmit@ is True.
class Machine m => GeneratorMachine m where
  -- | Returns True if machine has operations ready to submit
  wantsToSubmit :: m -> State m -> Bool

  -- | Generate up to @maxOps@ operations (no event required)
  generate :: m -> State m -> Int -> StepResult (State m)

-- | Replay executor: runs a machine against a recorded event stream.
--
-- This is the key to testability - no I/O, completely deterministic.
--
-- Mirrors @run@: @initial → step(empty) → step(events...)@
replay :: Machine m => m -> [Event] -> State m
replay machine events = finalState
  where
    s0 = initial machine
    -- Mirror run(): first step with empty event to trigger initial operations
    StepResult s1 _ = step machine s0 emptyEvent
    -- Then process recorded completion events
    finalState = foldl stepOne s1 events
    stepOne s e = stepState (step machine s e)
    emptyEvent = mempty

-- | Replay with operation capture: for testing that correct operations are emitted.
--
-- Returns final state and all operations emitted at each step.
replayWithOperations :: Machine m => m -> [Event] -> (State m, [[Operation]])
replayWithOperations machine events = (finalState, reverse allOps)
  where
    s0 = initial machine
    -- First step with empty event
    StepResult s1 ops1 = step machine s0 emptyEvent
    -- Process events, collecting operations
    (finalState, allOps) = foldl stepCollect (s1, [ops1]) events
    stepCollect (s, opsAcc) e =
      let StepResult s' ops = step machine s e
      in (s', ops : opsAcc)
    emptyEvent = mempty

-- | Replay for generator machines.
--
-- Generators don't use an initial empty event - they use @generate@ to produce work.
-- Replay just feeds completion events directly to @step@.
replayGenerate :: GeneratorMachine m => m -> [Event] -> State m
replayGenerate machine events = foldl stepOne (initial machine) events
  where
    stepOne s e = stepState (step machine s e)
