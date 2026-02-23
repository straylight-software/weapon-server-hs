{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Single-threaded event loop with continuations.

No threads, no MVars, no blocking per-connection. Each io_uring
completion resumes the corresponding continuation.

Optimizations:
- Flat array for O(1) continuation lookup (no IntMap)
- Freelist for slot reuse (no allocation on steady state)
- Batch completion draining
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
import Control.Monad (when)
import Data.IORef
import Data.Int (Int32, Int64)
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

-- | The event loop state
data Loop = Loop
    { loopRing :: !URing.URing
    , loopConts :: !(MutableArray RealWorld (Maybe Cont)) -- flat array of continuations
    , loopFreeHead :: !(IORef SlotId) -- head of freelist
    , loopFreeList :: !(MutablePrimArray RealWorld Int32) -- next-free indices
    , loopCapacity :: !Int -- max slots
    , loopRunning :: !(IORef Bool) -- shutdown flag
    }

-- | Create and run with a loop
withLoop :: Int -> (Loop -> IO a) -> IO a
withLoop ringSize action = do
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
{-# INLINE allocSlot #-}
allocSlot :: Loop -> Cont -> IO SlotId
allocSlot Loop{..} cont = do
    slot <- readIORef loopFreeHead
    if slot < 0 || slot >= loopCapacity
        then error "Out of continuation slots"
        else do
            -- Pop from freelist
            nextFree <- readPrimArray loopFreeList slot
            writeIORef loopFreeHead (fromIntegral nextFree)
            -- Store continuation
            writeArray loopConts slot (Just cont)
            pure slot

-- | Free a slot back to freelist
{-# INLINE freeSlot #-}
freeSlot :: Loop -> SlotId -> IO ()
freeSlot Loop{..} slot = do
    writeArray loopConts slot Nothing
    oldHead <- readIORef loopFreeHead
    writePrimArray loopFreeList slot (fromIntegral oldHead)
    writeIORef loopFreeHead slot

-- | Submit accept and register continuation
{-# INLINE ioAccept #-}
ioAccept :: Loop -> Fd -> Ptr () -> Ptr () -> Cont -> IO ()
ioAccept loop@Loop{..} (Fd fd) addrBuf addrLenBuf cont = do
    slot <- allocSlot loop cont
    let ringPtr = URing.uRingPtr loopRing
    sqe <- c_io_uring_get_sqe ringPtr
    if sqe == nullPtr
        then do
            -- SQ full - submit what we have and retry
            _ <- URing.submitIO loopRing
            sqe' <- c_io_uring_get_sqe ringPtr
            when (sqe' == nullPtr) $ error "SQ still full after submit"
            c_hs_uring_prep_accept sqe' fd (castPtr addrBuf) (castPtr addrLenBuf) 0
            c_hs_uring_sqe_set_data sqe' (CULLong (fromIntegral slot))
        else do
            c_hs_uring_prep_accept sqe fd (castPtr addrBuf) (castPtr addrLenBuf) 0
            c_hs_uring_sqe_set_data sqe (CULLong (fromIntegral slot))

-- | Submit recv and register continuation
{-# INLINE ioRecv #-}
ioRecv :: Loop -> Fd -> MutablePrimArray RealWorld Word8 -> Int -> Cont -> IO ()
ioRecv loop@Loop{..} (Fd fd) buf len cont = do
    slot <- allocSlot loop cont
    let ringPtr = URing.uRingPtr loopRing
        ptr = mutablePrimArrayContents buf
    sqe <- c_io_uring_get_sqe ringPtr
    if sqe == nullPtr
        then do
            _ <- URing.submitIO loopRing
            sqe' <- c_io_uring_get_sqe ringPtr
            when (sqe' == nullPtr) $ error "SQ still full after submit"
            c_hs_uring_prep_recv sqe' fd (castPtr ptr) (fromIntegral len) 0
            c_hs_uring_sqe_set_data sqe' (CULLong (fromIntegral slot))
        else do
            c_hs_uring_prep_recv sqe fd (castPtr ptr) (fromIntegral len) 0
            c_hs_uring_sqe_set_data sqe (CULLong (fromIntegral slot))

-- | Submit send (from pointer) and register continuation
{-# INLINE ioSend #-}
ioSend :: Loop -> Fd -> Ptr Word8 -> Int -> Cont -> IO ()
ioSend loop@Loop{..} (Fd fd) ptr len cont = do
    slot <- allocSlot loop cont
    let ringPtr = URing.uRingPtr loopRing
    sqe <- c_io_uring_get_sqe ringPtr
    if sqe == nullPtr
        then do
            _ <- URing.submitIO loopRing
            sqe' <- c_io_uring_get_sqe ringPtr
            when (sqe' == nullPtr) $ error "SQ still full after submit"
            c_hs_uring_prep_send sqe' fd (castPtr ptr) (fromIntegral len) 0
            c_hs_uring_sqe_set_data sqe' (CULLong (fromIntegral slot))
        else do
            c_hs_uring_prep_send sqe fd (castPtr ptr) (fromIntegral len) 0
            c_hs_uring_sqe_set_data sqe (CULLong (fromIntegral slot))

-- | Submit close and register continuation
{-# INLINE ioClose #-}
ioClose :: Loop -> Fd -> Cont -> IO ()
ioClose loop@Loop{..} (Fd fd) cont = do
    slot <- allocSlot loop cont
    let ringPtr = URing.uRingPtr loopRing
    sqe <- c_io_uring_get_sqe ringPtr
    if sqe == nullPtr
        then do
            _ <- URing.submitIO loopRing
            sqe' <- c_io_uring_get_sqe ringPtr
            when (sqe' == nullPtr) $ error "SQ still full after submit"
            c_hs_uring_prep_close sqe' fd
            c_hs_uring_sqe_set_data sqe' (CULLong (fromIntegral slot))
        else do
            c_hs_uring_prep_close sqe fd
            c_hs_uring_sqe_set_data sqe (CULLong (fromIntegral slot))

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
        Nothing -> pure () -- orphan completion (shouldn't happen)
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
Returns bytes read, or 0 on EOF/error.
-}
ioRecvBlocking :: Loop -> Fd -> MutablePrimArray RealWorld Word8 -> Int -> IO Int
ioRecvBlocking loop@Loop{..} fd buf len = do
    resultVar <- newEmptyMVar
    ioRecv loop fd buf len $ Cont $ \case
        Success n -> putMVar resultVar (fromIntegral n) >> pure Nothing
        Failure _ -> putMVar resultVar 0 >> pure Nothing
    -- Force submit so the main loop can see this operation
    URing.submitIO loopRing
    takeMVar resultVar

{- | Blocking send - submits to io_uring and waits for completion.
MUST be called from a separate thread (not the main loop thread).
Returns bytes sent, or 0 on error.
-}
ioSendBlocking :: Loop -> Fd -> Ptr Word8 -> Int -> IO Int
ioSendBlocking loop@Loop{..} fd ptr len = do
    resultVar <- newEmptyMVar
    ioSend loop fd ptr len $ Cont $ \case
        Success n -> putMVar resultVar (fromIntegral n) >> pure Nothing
        Failure _ -> putMVar resultVar 0 >> pure Nothing
    -- Force submit so the main loop can see this operation
    URing.submitIO loopRing
    takeMVar resultVar
