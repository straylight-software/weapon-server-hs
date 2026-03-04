{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Module      : Evring.Wai.Loop
Description : Single-threaded event loop with continuations
Stability   : experimental

This module provides the core event loop for the io_uring WAI server.
It uses continuation-passing style (CPS) for zero-overhead async I/O.

= Architecture

No threads, no MVars, no blocking per-connection. Each io_uring
completion resumes the corresponding continuation.

= Optimizations

* Flat array for O(1) continuation lookup (no IntMap)
* Freelist for slot reuse (no allocation in steady state)
* Batch completion draining (up to 64 per iteration)

= Continuation Model

Each operation is associated with a 'Cont' that handles its completion:

@
ioRecv loop fd buf len $ Cont $ \\case
    Success bytesRead -> processData bytesRead
    Failure errno -> handleError errno
@
-}
module Evring.Wai.Loop (
    -- * Core types
    Loop,
    Cont (..),
    CompletionResult (..),
    SlotId,

    -- * Running
    withLoop,
    runLoop,
    shutdown,

    -- * Operations (called from continuations)
    ioAccept,
    ioRecv,
    ioSend,
    ioClose,

    -- * Blocking operations (for ResponseRaw / WebSocket)
    ioRecvBlocking,
    ioSendBlocking,
) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Data.Aeson (ToJSON)
import Data.IORef
import Data.Int (Int32, Int64)
import GHC.Generics (Generic)
import Katip (LogItem (..), PayloadSelection (..), ToObject (..))
import Data.Primitive (
    MutablePrimArray,
    mutablePrimArrayContents,
    newPinnedPrimArray,
    readPrimArray,
    writePrimArray,
 )
import Data.Primitive.Array (MutableArray, newArray, readArray, writeArray)
import Data.Word (Word8)
import Foreign (Ptr, castPtr, nullPtr)
import Foreign.C.Types (CULLong (..))
import GHC.Exts (RealWorld)
import System.Posix.Types (Fd (..))

import Log qualified
import System.IoUring.Internal.FFI
import System.IoUring.URing qualified as URing

-- | Result passed to continuation
data CompletionResult
    = Success !Int64 -- positive result (bytes, fd, etc)
    | Failure !Int -- errno (positive)
    deriving (Show)

-- | Continuation: receives completion result, returns next continuation (or Nothing to finish)
newtype Cont = Cont {runCont :: CompletionResult -> IO (Maybe Cont)}

-- | Slot ID - index into continuation array
type SlotId = Int

-- | Sentinel for empty slots in freelist
emptySlot :: Int32
emptySlot = -1

-- | Structured payload for orphan completion errors (indicates a bug)
data OrphanCompletionPayload = OrphanCompletionPayload
    { ocpSlot :: !Int         -- ^ The slot index that was already freed
    , ocpResult :: !Int64     -- ^ The io_uring completion result
    , ocpCapacity :: !Int     -- ^ Total slot capacity (for context)
    }
    deriving (Show, Generic, ToJSON)

instance ToObject OrphanCompletionPayload
instance LogItem OrphanCompletionPayload where
    payloadKeys _ _ = AllKeys

-- | The event loop state
data Loop = Loop
    { loopRing :: !URing.URing
    , loopConts :: !(MutableArray RealWorld (Maybe Cont)) -- flat array of continuations
    , loopFreeHead :: !(IORef SlotId) -- head of freelist
    , loopFreeList :: !(MutablePrimArray RealWorld Int32) -- next-free indices
    , loopCapacity :: !Int -- max slots
    , loopRunning :: !(IORef Bool) -- shutdown flag
    , loopLogger :: !Log.Logger -- for error reporting
    }

-- | Create and run with a loop
withLoop :: Log.Logger -> Int -> (Loop -> IO a) -> IO a
withLoop logger ringSize action = do
    let capacity = ringSize * 4 -- plenty of room for in-flight ops
    bracket (URing.initURing 0 ringSize (ringSize * 2)) URing.closeURing $ \ring -> do
        -- Initialize continuation array with Nothing
        conts <- newArray capacity Nothing

        -- Initialize freelist: each slot points to next, last points to -1
        freeList <- newPinnedPrimArray capacity
        initFreeList freeList 0 capacity

        freeHead <- newIORef 0
        running <- newIORef True

        let loop =
                Loop
                    { loopRing = ring
                    , loopConts = conts
                    , loopFreeHead = freeHead
                    , loopFreeList = freeList
                    , loopCapacity = capacity
                    , loopRunning = running
                    , loopLogger = Log.withNS logger "loop"
                    }
        action loop
  where
    initFreeList arr i cap
        | i >= cap = pure ()
        | i == cap - 1 = writePrimArray arr i emptySlot
        | otherwise = do
            writePrimArray arr i (fromIntegral (i + 1))
            initFreeList arr (i + 1) cap

-- | Signal shutdown
shutdown :: Loop -> IO ()
shutdown Loop{..} = writeIORef loopRunning False

-- | Allocate a slot and register continuation
-- Returns Nothing if no slots available (capacity exhaustion)
{-# INLINE allocSlot #-}
allocSlot :: Loop -> Cont -> IO (Maybe SlotId)
allocSlot Loop{..} cont = do
    slot <- readIORef loopFreeHead
    if slot < 0 || slot >= loopCapacity
        then pure Nothing -- Capacity exhaustion - caller should handle gracefully
        else do
            -- Pop from freelist
            nextFree <- readPrimArray loopFreeList slot
            writeIORef loopFreeHead (fromIntegral nextFree)
            -- Store continuation
            writeArray loopConts slot (Just cont)
            pure (Just slot)

-- | Free a slot back to freelist
{-# INLINE freeSlot #-}
freeSlot :: Loop -> SlotId -> IO ()
freeSlot Loop{..} slot = do
    writeArray loopConts slot Nothing
    oldHead <- readIORef loopFreeHead
    writePrimArray loopFreeList slot (fromIntegral oldHead)
    writeIORef loopFreeHead slot

-- | Submit accept and register continuation
-- Returns False if operation could not be submitted (capacity exhaustion)
{-# INLINE ioAccept #-}
ioAccept :: Loop -> Fd -> Ptr () -> Ptr () -> Cont -> IO Bool
ioAccept loop@Loop{..} (Fd fd) addrBuf addrLenBuf cont = do
    mSlot <- allocSlot loop cont
    case mSlot of
        Nothing -> pure False -- No slots available
        Just slot -> do
            let ringPtr = URing.uRingPtr loopRing
            sqe <- c_io_uring_get_sqe ringPtr
            if sqe == nullPtr
                then do
                    -- SQ full - submit what we have and retry
                    _ <- URing.submitIO loopRing
                    sqe' <- c_io_uring_get_sqe ringPtr
                    if sqe' == nullPtr
                        then do
                            -- Still full after submit - return slot and fail gracefully
                            freeSlot loop slot
                            pure False
                        else do
                            c_hs_uring_prep_accept sqe' fd (castPtr addrBuf) (castPtr addrLenBuf) 0
                            c_hs_uring_sqe_set_data sqe' (CULLong (fromIntegral slot))
                            pure True
                else do
                    c_hs_uring_prep_accept sqe fd (castPtr addrBuf) (castPtr addrLenBuf) 0
                    c_hs_uring_sqe_set_data sqe (CULLong (fromIntegral slot))
                    pure True

-- | Submit recv and register continuation
-- Returns False if operation could not be submitted (capacity exhaustion)
{-# INLINE ioRecv #-}
ioRecv :: Loop -> Fd -> MutablePrimArray RealWorld Word8 -> Int -> Cont -> IO Bool
ioRecv loop@Loop{..} (Fd fd) buf len cont = do
    mSlot <- allocSlot loop cont
    case mSlot of
        Nothing -> pure False -- No slots available
        Just slot -> do
            let ringPtr = URing.uRingPtr loopRing
                ptr = mutablePrimArrayContents buf
            sqe <- c_io_uring_get_sqe ringPtr
            if sqe == nullPtr
                then do
                    _ <- URing.submitIO loopRing
                    sqe' <- c_io_uring_get_sqe ringPtr
                    if sqe' == nullPtr
                        then do
                            freeSlot loop slot
                            pure False
                        else do
                            c_hs_uring_prep_recv sqe' fd (castPtr ptr) (fromIntegral len) 0
                            c_hs_uring_sqe_set_data sqe' (CULLong (fromIntegral slot))
                            pure True
                else do
                    c_hs_uring_prep_recv sqe fd (castPtr ptr) (fromIntegral len) 0
                    c_hs_uring_sqe_set_data sqe (CULLong (fromIntegral slot))
                    pure True

-- | Submit send (from pointer) and register continuation
-- Returns False if operation could not be submitted (capacity exhaustion)
{-# INLINE ioSend #-}
ioSend :: Loop -> Fd -> Ptr Word8 -> Int -> Cont -> IO Bool
ioSend loop@Loop{..} (Fd fd) ptr len cont = do
    mSlot <- allocSlot loop cont
    case mSlot of
        Nothing -> pure False -- No slots available
        Just slot -> do
            let ringPtr = URing.uRingPtr loopRing
            sqe <- c_io_uring_get_sqe ringPtr
            if sqe == nullPtr
                then do
                    _ <- URing.submitIO loopRing
                    sqe' <- c_io_uring_get_sqe ringPtr
                    if sqe' == nullPtr
                        then do
                            freeSlot loop slot
                            pure False
                        else do
                            c_hs_uring_prep_send sqe' fd (castPtr ptr) (fromIntegral len) 0
                            c_hs_uring_sqe_set_data sqe' (CULLong (fromIntegral slot))
                            pure True
                else do
                    c_hs_uring_prep_send sqe fd (castPtr ptr) (fromIntegral len) 0
                    c_hs_uring_sqe_set_data sqe (CULLong (fromIntegral slot))
                    pure True

-- | Submit close and register continuation
-- Returns False if operation could not be submitted (capacity exhaustion)
-- Note: If close fails to submit, the fd may leak. Callers should log this.
{-# INLINE ioClose #-}
ioClose :: Loop -> Fd -> Cont -> IO Bool
ioClose loop@Loop{..} (Fd fd) cont = do
    mSlot <- allocSlot loop cont
    case mSlot of
        Nothing -> pure False -- No slots available - fd may leak
        Just slot -> do
            let ringPtr = URing.uRingPtr loopRing
            sqe <- c_io_uring_get_sqe ringPtr
            if sqe == nullPtr
                then do
                    _ <- URing.submitIO loopRing
                    sqe' <- c_io_uring_get_sqe ringPtr
                    if sqe' == nullPtr
                        then do
                            freeSlot loop slot
                            pure False -- fd may leak
                        else do
                            c_hs_uring_prep_close sqe' fd
                            c_hs_uring_sqe_set_data sqe' (CULLong (fromIntegral slot))
                            pure True
                else do
                    c_hs_uring_prep_close sqe fd
                    c_hs_uring_sqe_set_data sqe (CULLong (fromIntegral slot))
                    pure True

-- | Run the event loop until shutdown
runLoop :: Loop -> IO ()
runLoop loop@Loop{..} = go
  where
    go = do
        running <- readIORef loopRunning
        if not running
            then pure ()
            else do
                -- Submit pending SQEs
                URing.submitIO loopRing

                -- Wait for at least one completion
                comp <- URing.awaitIO loopRing
                dispatch loop comp

                -- Drain any additional ready completions (unrolled for perf)
                drainReady (64 :: Int) -- Process up to 64 completions per iteration
                go

    -- Unrolled completion drain with limit to avoid starvation
    {-# INLINE drainReady #-}
    drainReady :: Int -> IO ()
    drainReady 0 = pure ()
    drainReady !n = do
        mComp <- URing.peekIO loopRing
        case mComp of
            Nothing -> pure ()
            Just comp -> do
                dispatch loop comp
                drainReady (n - 1)

-- | Dispatch a completion to its continuation
{-# INLINE dispatch #-}
dispatch :: Loop -> URing.IOCompletion -> IO ()
dispatch loop@Loop{..} (URing.IOCompletion (URing.IOOpId cid) (URing.IOResult res)) = do
    let slot = fromIntegral cid
    mCont <- readArray loopConts slot
    case mCont of
        Nothing -> do
            -- CRITICAL BUG: Completion arrived for slot that was already freed.
            -- This indicates double-free, use-after-free, or slot ID corruption.
            -- If you see this in logs, investigate immediately!
            Log.logError loopLogger
                "Orphan io_uring completion - slot already freed (BUG!)"
                OrphanCompletionPayload
                    { ocpSlot = slot
                    , ocpResult = res
                    , ocpCapacity = loopCapacity
                    }
        Just (Cont k) -> do
            let !result =
                    if res < 0
                        then Failure (fromIntegral (-res))
                        else Success res
            mNext <- k result
            case mNext of
                Nothing -> freeSlot loop slot
                Just next -> writeArray loopConts slot (Just next)

-- ════════════════════════════════════════════════════════════════════════════
-- BLOCKING OPERATIONS (for ResponseRaw / WebSocket handlers)
--
-- These use MVars to bridge between the CPS loop and blocking code.
-- The raw handler runs in a separate thread, submits ops via the loop,
-- and blocks on MVars while the main loop continues processing other connections.
-- ════════════════════════════════════════════════════════════════════════════

{- | Blocking recv - submits to io_uring and waits for completion.
MUST be called from a separate thread (not the main loop thread).
Returns bytes read, or 0 on EOF/error/capacity exhaustion.
-}
ioRecvBlocking :: Loop -> Fd -> MutablePrimArray RealWorld Word8 -> Int -> IO Int
ioRecvBlocking loop@Loop{..} fd buf len = do
    resultVar <- newEmptyMVar
    submitted <- ioRecv loop fd buf len $ Cont $ \case
        Success n -> putMVar resultVar (fromIntegral n) >> pure Nothing
        Failure _ -> putMVar resultVar 0 >> pure Nothing
    if submitted
        then do
            -- Force submit so the main loop can see this operation
            URing.submitIO loopRing
            takeMVar resultVar
        else pure 0 -- Capacity exhaustion

{- | Blocking send - submits to io_uring and waits for completion.
MUST be called from a separate thread (not the main loop thread).
Returns bytes sent, or 0 on error/capacity exhaustion.
-}
ioSendBlocking :: Loop -> Fd -> Ptr Word8 -> Int -> IO Int
ioSendBlocking loop@Loop{..} fd ptr len = do
    resultVar <- newEmptyMVar
    submitted <- ioSend loop fd ptr len $ Cont $ \case
        Success n -> putMVar resultVar (fromIntegral n) >> pure Nothing
        Failure _ -> putMVar resultVar 0 >> pure Nothing
    if submitted
        then do
            -- Force submit so the main loop can see this operation
            URing.submitIO loopRing
            takeMVar resultVar
        else pure 0 -- Capacity exhaustion
