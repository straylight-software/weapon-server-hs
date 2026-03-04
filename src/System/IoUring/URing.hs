{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : System.IoUring.URing
Description : Core io_uring ring management
Stability   : internal

This module provides the core ring management for io_uring, including
initialization, submission, and completion handling.

This is an internal module. For the high-level API, use "System.IoUring".
-}
module System.IoUring.URing (
    -- * Ring Handle
    URing (URing, uRingPtr),
    URingParams (..),

    -- * Lifecycle
    initURing,
    closeURing,
    cleanupURing,
    validURing,

    -- * Submission
    submitIO,

    -- * Completions
    awaitIO,
    peekIO,
    IOCompletion (..),
    IOResult (..),
    IOOpId (..),

    -- * Pure Helpers (for testing)
    parseCqe,
)
where

import Control.Monad (when)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Foreign (Ptr, alloca, callocBytes, free, peek, peekByteOff)
import System.IoUring.Internal.FFI (
    c_hs_uring_cqe_seen,
    c_hs_uring_peek_cqe,
    c_hs_uring_wait_cqe,
    c_io_uring_queue_exit,
    c_io_uring_queue_init,
    c_io_uring_submit,
 )

-- ============================================================================
-- TYPES
-- ============================================================================

{- | Result code from an I/O operation.

Contains the raw result from the kernel: positive values typically
indicate bytes transferred, negative values are negated errno codes.
-}
newtype IOResult = IOResult Int64
    deriving stock (Show, Eq)

{- | Identifier for a submitted operation.

This is the user_data value set when the operation was submitted,
used to correlate completions with their original requests.
-}
newtype IOOpId = IOOpId Word64
    deriving stock (Show, Eq)

-- | A completed I/O operation.
data IOCompletion = IOCompletion
    { completionId :: !IOOpId
    -- ^ Identifier matching the submitted operation
    , completionRes :: !IOResult
    -- ^ Result code from the kernel
    }
    deriving stock (Show, Eq)

-- | Parameters for ring initialization.
data URingParams = URingParams
    { uringSqEntries :: !Word32
    -- ^ Number of submission queue entries
    , uringCqEntries :: !Word32
    -- ^ Number of completion queue entries
    , uringFlags :: !Word32
    -- ^ Setup flags
    }
    deriving stock (Show, Eq)

-- ============================================================================
-- RING HANDLE
-- ============================================================================

{- | Handle to an io_uring instance.

Each 'URing' wraps a pointer to the C io_uring structure.
Create with 'initURing', destroy with 'closeURing'.
-}
newtype URing = URing
    { uRingPtr :: Ptr ()
    -- ^ Pointer to the underlying io_uring structure
    }

-- ============================================================================
-- LIFECYCLE
-- ============================================================================

{- | Initialize an io_uring instance.

@initURing capNo sqEntries cqEntries@ creates a new ring.

* @capNo@ - Capability number (currently unused, reserved for future)
* @sqEntries@ - Number of submission queue entries
* @cqEntries@ - Number of completion queue entries (currently unused)

Throws an IOError if initialization fails.
-}
initURing :: Int -> Int -> Int -> IO URing
initURing _capNo sqEntries _cqEntries = do
    -- Allocate memory for the io_uring structure
    -- 4096 bytes is a conservative estimate for safety
    ptr <- callocBytes 4096
    ret <- c_io_uring_queue_init (fromIntegral sqEntries) ptr 0
    if ret < 0
        then do
            free ptr
            ioError $ userError $ "io_uring_queue_init failed with errno " ++ show (-ret)
        else return $ URing ptr

-- | Close an io_uring instance and free resources.
closeURing :: URing -> IO ()
closeURing (URing ptr) = do
    c_io_uring_queue_exit ptr
    free ptr

-- | Alias for 'closeURing'.
cleanupURing :: URing -> IO ()
cleanupURing = closeURing

{- | Check if a ring is valid.

Currently always returns 'True'. Reserved for future validation.
-}
validURing :: URing -> IO Bool
validURing _ = return True

-- ============================================================================
-- SUBMISSION
-- ============================================================================

{- | Submit all prepared SQEs to the kernel.

Throws an IOError if submission fails.
-}
submitIO :: URing -> IO ()
submitIO (URing ptr) = do
    ret <- c_io_uring_submit ptr
    when (ret < 0) $
        ioError $
            userError $
                "io_uring_submit failed: " ++ show ret

-- ============================================================================
-- COMPLETIONS
-- ============================================================================

{- | Wait for and consume a completion.

Blocks until a completion is available, then returns it.
Throws an IOError if the wait fails.
-}
awaitIO :: URing -> IO IOCompletion
awaitIO (URing ringPtr) = alloca $ \cqePtrPtr -> do
    res <- c_hs_uring_wait_cqe ringPtr cqePtrPtr
    if res < 0
        then ioError $ userError $ "io_uring_wait_cqe failed: " ++ show res
        else do
            cqePtr <- peek cqePtrPtr
            completion <- readCqe cqePtr
            c_hs_uring_cqe_seen ringPtr cqePtr
            return completion

{- | Check for a completion without blocking.

Returns 'Just completion' if one is available, 'Nothing' otherwise.
-}
peekIO :: URing -> IO (Maybe IOCompletion)
peekIO (URing ringPtr) = alloca $ \cqePtrPtr -> do
    res <- c_hs_uring_peek_cqe ringPtr cqePtrPtr
    if res == 0 -- 0 means success (found cqe)
        then do
            cqePtr <- peek cqePtrPtr
            completion <- readCqe cqePtr
            c_hs_uring_cqe_seen ringPtr cqePtr
            return $ Just completion
        else return Nothing

{- | Read completion data from a CQE pointer.

This is the shared implementation for 'awaitIO' and 'peekIO'.
-}
readCqe :: Ptr () -> IO IOCompletion
readCqe cqePtr = do
    userData <- peekByteOff cqePtr 0 :: IO Word64
    res32 <- peekByteOff cqePtr 8 :: IO Int32
    return $ parseCqe userData res32

{- | Parse raw CQE fields into an 'IOCompletion'.

This pure function allows testing the parsing logic independently
of the actual I/O operations.
-}
parseCqe :: Word64 -> Int32 -> IOCompletion
parseCqe userData res32 =
    IOCompletion (IOOpId userData) (IOResult (fromIntegral res32))
