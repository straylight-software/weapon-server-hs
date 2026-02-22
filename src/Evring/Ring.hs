{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Ring runner: connects pure state machines to actual io_uring I/O.
--
-- This is the only module that performs actual I/O. The machine abstraction
-- remains pure, enabling deterministic replay testing.
--
-- The runner implements the event loop:
--
-- 1. Get initial state from machine
-- 2. Step with empty event to get initial operations
-- 3. Submit operations to ring
-- 4. Wait for completions
-- 5. Step machine with completion event
-- 6. Repeat until done
module Evring.Ring
  ( -- * Running machines
    run
  , runTraced
  , runGenerate
  , runGenerateTraced
    -- * Ring configuration
  , RingConfig(..)
  , defaultRingConfig
  ) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')

import Evring.Event (Event, Operation, emptyEvent)
import Evring.Machine
  ( Machine(State, initial, step, done)
  , GeneratorMachine(wantsToSubmit, generate)
  , StepResult(StepResult)
  )
import Evring.Trace (Trace, emptyTrace, record)

-- | Ring configuration.
data RingConfig = RingConfig
  { ringEntries    :: !Int
    -- ^ Number of SQ entries (default 256)
  , ringCqEntries  :: !Int
    -- ^ Number of CQ entries (default 512)
  , ringBatchSize  :: !Int
    -- ^ Max operations to submit at once (default 64)
  } deriving (Eq, Show)

-- | Default ring configuration.
defaultRingConfig :: RingConfig
defaultRingConfig = RingConfig
  { ringEntries   = 256
  , ringCqEntries = 512
  , ringBatchSize = 64
  }

-- | Run a machine to completion.
--
-- This is the main entry point for executing a machine with actual I/O.
-- The machine's step function is called for each completion event until
-- the machine reports done.
run :: forall m. Machine m => RingConfig -> m -> IO (State m)
run _config machine = do
    -- Initialize state
    let s0 = initial machine
    
    -- First step with empty event to trigger initial operations
    let StepResult s1 ops1 = step machine s0 emptyEvent
    
    -- Submit and process until done
    runLoop s1 ops1
  where
    runLoop :: State m -> [Operation] -> IO (State m)
    runLoop s ops = do
      -- Submit operations
      submitOperations ops
      
      -- Check if done
      if done machine s
        then return s
        else do
          -- Wait for completion and step
          event <- awaitCompletion
          let StepResult s' ops' = step machine s event
          runLoop s' ops'

-- | Run a machine to completion, recording a trace.
--
-- The trace can be used for replay testing.
runTraced :: forall m. Machine m => RingConfig -> m -> IO (State m, Trace)
runTraced _config machine = do
    traceRef <- newIORef emptyTrace
    
    let s0 = initial machine
    let StepResult s1 ops1 = step machine s0 emptyEvent
    
    finalState <- runLoop traceRef s1 ops1
    trace <- readIORef traceRef
    return (finalState, trace)
  where
    runLoop :: IORef Trace -> State m -> [Operation] -> IO (State m)
    runLoop traceRef s ops = do
      submitOperations ops
      
      if done machine s
        then return s
        else do
          event <- awaitCompletion
          -- Record the event
          modifyIORef' traceRef (record event)
          let StepResult s' ops' = step machine s event
          runLoop traceRef s' ops'

-- | Run a generator machine to completion.
--
-- Generator machines can produce operations proactively via generate(),
-- not just in response to events. This is used for bulk operations.
runGenerate :: forall m. GeneratorMachine m => RingConfig -> m -> IO (State m)
runGenerate config machine = do
    let s0 = initial machine
    runLoop s0
  where
    maxOps = ringBatchSize config
    
    runLoop :: State m -> IO (State m)
    runLoop s
      | done machine s = return s
      | wantsToSubmit machine s = do
          -- Generate operations
          let StepResult s' ops = generate machine s maxOps
          submitOperations ops
          
          -- Wait for completions and process
          if null ops
            then runLoop s'
            else do
              event <- awaitCompletion
              let StepResult s'' _ = step machine s' event
              runLoop s''
      | otherwise = do
          -- No more to generate, wait for completions
          event <- awaitCompletion
          let StepResult s' _ = step machine s event
          runLoop s'

-- | Run a generator machine to completion, recording a trace.
runGenerateTraced :: forall m. GeneratorMachine m => RingConfig -> m -> IO (State m, Trace)
runGenerateTraced config machine = do
    traceRef <- newIORef emptyTrace
    let s0 = initial machine
    finalState <- runLoop traceRef s0
    trace <- readIORef traceRef
    return (finalState, trace)
  where
    maxOps = ringBatchSize config
    
    runLoop :: IORef Trace -> State m -> IO (State m)
    runLoop traceRef s
      | done machine s = return s
      | wantsToSubmit machine s = do
          let StepResult s' ops = generate machine s maxOps
          submitOperations ops
          
          if null ops
            then runLoop traceRef s'
            else do
              event <- awaitCompletion
              modifyIORef' traceRef (record event)
              let StepResult s'' _ = step machine s' event
              runLoop traceRef s''
      | otherwise = do
          event <- awaitCompletion
          modifyIORef' traceRef (record event)
          let StepResult s' _ = step machine s event
          runLoop traceRef s'

-- ============================================================================
-- Internal: I/O operations (to be implemented with actual io_uring)
-- ============================================================================

-- | Submit operations to the ring.
--
-- TODO: Implement with actual io_uring via System.IoUring
submitOperations :: [Operation] -> IO ()
submitOperations _ops = return ()  -- Placeholder

-- | Wait for a completion event.
--
-- TODO: Implement with actual io_uring via System.IoUring
awaitCompletion :: IO Event
awaitCompletion = return emptyEvent  -- Placeholder
