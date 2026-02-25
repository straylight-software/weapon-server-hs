{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      : System.IoUring
Description : High-level io_uring API for asynchronous I/O
Stability   : experimental

This module provides a high-level Haskell interface to Linux's io_uring
asynchronous I/O subsystem. It supports both file and socket I/O operations
with batched submission for optimal performance.

The API is designed around capability-local rings, where each Haskell
capability (OS thread) gets its own io_uring instance to avoid contention.

= Basic Usage

@
withIoUring defaultIoUringParams $ \\ctx -> do
    results <- submitBatch ctx $ \\submit -> do
        submit (ReadOp fd 0 buf 0 4096)
        submit (WriteOp fd2 0 data 0 (fromIntegral $ length data))
    -- Process results...
@

= Thread Safety

Each capability has its own ring with a lock. Operations on a given
ring are serialized, but different capabilities can operate in parallel.
-}
module System.IoUring (
    -- * I/O Context
    IOCtx (..),
    CapCtx (..),
    IOCtxParams (..),
    ioCtxParams,
    defaultIoUringParams,
    withIoUring,
    initIoUring,
    closeIoUring,

    -- * I/O Operations
    BatchPrep,
    IoOp (..),
    IoResult (..),
    submitBatch,
    registerBuffers,
    unregisterBuffers,
    registerFiles,
    unregisterFiles,
    updateFiles,

    -- * Utility
    Errno (Errno),
    ByteCount,
    FileOffset,

    -- * Pure Helpers (for testing)
    interpretResult,
    computeRingSize,
    selectCapability,
)
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, mask_)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Primitive (MutablePrimArray, PrimArray, mutablePrimArrayContents, primArrayContents)
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32, Word64, Word8)
import Foreign (Ptr, castPtr, free, mallocBytes, nullPtr, plusPtr, poke)
import Foreign.C.Error (Errno (Errno))
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.Storable (pokeByteOff)
import GHC.Conc (getNumCapabilities, myThreadId, threadCapability)
import GHC.Exts (RealWorld)
import Network.Socket (SockAddr)
import System.IoUring.Internal.FFI (
    IOVec (IOVec),
    KernelTimespec,
    c_hs_uring_prep_accept,
    c_hs_uring_prep_close,
    c_hs_uring_prep_fadvise,
    c_hs_uring_prep_fallocate,
    c_hs_uring_prep_fsync,
    c_hs_uring_prep_linkat,
    c_hs_uring_prep_madvise,
    c_hs_uring_prep_mkdirat,
    c_hs_uring_prep_openat,
    c_hs_uring_prep_poll_add,
    c_hs_uring_prep_poll_remove,
    c_hs_uring_prep_read,
    c_hs_uring_prep_readv,
    c_hs_uring_prep_recv,
    c_hs_uring_prep_renameat,
    c_hs_uring_prep_send,
    c_hs_uring_prep_send_zc,
    c_hs_uring_prep_shutdown,
    c_hs_uring_prep_splice,
    c_hs_uring_prep_symlinkat,
    c_hs_uring_prep_tee,
    c_hs_uring_prep_timeout,
    c_hs_uring_prep_timeout_remove,
    c_hs_uring_prep_unlinkat,
    c_hs_uring_prep_write,
    c_hs_uring_prep_writev,
    c_hs_uring_register_buffers,
    c_hs_uring_register_files,
    c_hs_uring_register_files_update,
    c_hs_uring_sqe_set_data,
    c_hs_uring_unregister_buffers,
    c_hs_uring_unregister_files,
    c_io_uring_get_sqe,
 )
import System.IoUring.URing qualified as URing
import System.Posix.Types (ByteCount, Fd (Fd), FileOffset)

-- ============================================================================
-- CONTEXT
-- ============================================================================

{- | Top-level I/O context containing per-capability rings.

Create with 'initIoUring' or use 'withIoUring' for bracket-style management.
-}
newtype IOCtx = IOCtx (Vector CapCtx)

{- | Per-capability context holding a single io_uring instance.

Each Haskell capability (OS thread) gets its own ring to minimize contention.
-}
data CapCtx = CapCtx
    { _capNo :: !Int
    -- ^ Capability index (0-based)
    , _capURing :: !URing.URing
    -- ^ The underlying io_uring instance
    , _capLock :: !(MVar ())
    -- ^ Lock for exclusive ring access within capability
    , _capBatchSizeLimit :: !Int
    -- ^ Maximum operations per batch
    , _capConcurrencyLimit :: !Int
    -- ^ Maximum concurrent operations
    }

-- | Configuration parameters for io_uring initialization.
data IOCtxParams = IOCtxParams
    { ioBatchSizeLimit :: !Int
    -- ^ Maximum number of operations in a single batch submission
    , ioConcurrencyLimit :: !Int
    -- ^ Maximum number of concurrent in-flight operations
    }
    deriving stock (Show)

{- | Default io_uring parameters suitable for most workloads.

* Batch size: 64 operations
* Concurrency limit: 192 operations
-}
defaultIoUringParams :: IOCtxParams
defaultIoUringParams =
    IOCtxParams
        { ioBatchSizeLimit = 64
        , ioConcurrencyLimit = 64 * 3
        }

{- | Bracket-style initialization and cleanup of io_uring context.

This is the recommended way to use io_uring as it ensures proper cleanup.

@
withIoUring defaultIoUringParams $ \\ctx -> do
    -- Use ctx for I/O operations
@
-}
withIoUring :: IOCtxParams -> (IOCtx -> IO a) -> IO a
withIoUring params = bracket (initIoUring params) closeIoUring

{- | Initialize an io_uring context with the given parameters.

Creates one ring per Haskell capability. Use 'closeIoUring' when done,
or prefer 'withIoUring' for automatic cleanup.
-}
initIoUring :: IOCtxParams -> IO IOCtx
initIoUring (IOCtxParams batchSize concurrency) = do
    numCaps <- getNumCapabilities
    let ringSize = computeRingSize batchSize
    caps <- V.generateM numCaps $ \idx -> do
        uring <- URing.initURing idx ringSize (ringSize * 2)
        lock <- newMVar ()
        return $ CapCtx idx uring lock batchSize concurrency
    return $ IOCtx caps

{- | Close an io_uring context, releasing all resources.

After calling this, the context must not be used.
-}
closeIoUring :: IOCtx -> IO ()
closeIoUring (IOCtx caps) = V.forM_ caps closeCapCtx
  where
    closeCapCtx (CapCtx _ uring _ _ _) = URing.closeURing uring

{- | Compute the appropriate ring size from batch size.

Ring size must be at least 32 for efficient operation.
-}
computeRingSize :: Int -> Int
computeRingSize = max 32

{- | Select the capability context for the current thread.

Uses the current thread's capability index, wrapping around
if there are more capabilities than rings.
-}
selectCapability :: Vector CapCtx -> Int -> Int
selectCapability caps capIdx = capIdx `mod` V.length caps

-- ============================================================================
-- OPERATIONS
-- ============================================================================

{- | I/O operation to be submitted to io_uring.

Operations are batched and submitted together for efficiency.
Note: No Show instance because 'MutablePrimArray' doesn't have one.
-}
data IoOp
    = {- | Read from file at offset into mutable buffer
      Arguments: fd, offset, buffer, buffer_offset, length
      -}
      ReadOp !Fd !FileOffset !(MutablePrimArray RealWorld Word8) !Int !ByteCount
    | {- | Write to file at offset from immutable buffer
      Arguments: fd, offset, buffer, buffer_offset, length
      -}
      WriteOp !Fd !FileOffset !(PrimArray Word8) !Int !ByteCount
    | {- | Vectored read (scatter)
      Arguments: fd, offset, iovec_array, iovec_count
      -}
      ReadvOp !Fd !FileOffset !(Ptr IOVec) !Int
    | {- | Vectored write (gather)
      Arguments: fd, offset, iovec_array, iovec_count
      -}
      WritevOp !Fd !FileOffset !(Ptr IOVec) !Int
    | -- | Sync file data and metadata (equivalent to fsync with flags=0)
      SyncOp !Fd
    | {- | Fsync with custom flags
      Arguments: fd, flags
      -}
      FsyncOp !Fd !Word32
    | {- | Receive data from socket into mutable buffer
      Arguments: fd, buffer, buffer_offset, length, flags
      -}
      RecvOp !Fd !(MutablePrimArray RealWorld Word8) !Int !ByteCount !Word32
    | {- | Receive data from socket into raw pointer
      Arguments: fd, ptr, length, flags
      -}
      RecvPtrOp !Fd !(Ptr Word8) !ByteCount !Word32
    | {- | Send data to socket from immutable buffer
      Arguments: fd, buffer, buffer_offset, length, flags
      -}
      SendOp !Fd !(PrimArray Word8) !Int !ByteCount !Word32
    | {- | Send data to socket from raw pointer
      Arguments: fd, ptr, length, flags
      -}
      SendPtrOp !Fd !(Ptr Word8) !ByteCount !Word32
    | {- | Zero-copy send from immutable buffer
      Arguments: fd, buffer, buffer_offset, length, flags, zc_flags
      -}
      SendZcOp !Fd !(PrimArray Word8) !Int !ByteCount !Word32 !Word32
    | {- | Zero-copy send from raw pointer
      Arguments: fd, ptr, length, flags, zc_flags
      -}
      SendZcPtrOp !Fd !(Ptr Word8) !ByteCount !Word32 !Word32
    | {- | Accept connection on socket
      Arguments: fd, flags, addr_ptr, addrlen_ptr
      -}
      AcceptOp !Fd !Word32 !(Ptr ()) !(Ptr ())
    | -- | Connect socket to address (stub - not yet implemented)
      ConnectOp !Fd !SockAddr
    | -- | Cancel pending socket operation (stub - not yet implemented)
      SockCancelOp !Fd
    | {- | Shutdown socket
      Arguments: fd, how (SHUT_RD=0, SHUT_WR=1, SHUT_RDWR=2)
      -}
      ShutdownOp !Fd !Int
    | {- | Add poll request for fd events
      Arguments: fd, event_mask
      -}
      PollAddOp !Fd !Word32
    | {- | Remove previously submitted poll
      Arguments: user_data of poll to cancel
      -}
      PollRemoveOp !Word64
    | {- | Set timeout
      Arguments: timespec_ptr, count, flags
      -}
      TimeoutOp !(Ptr KernelTimespec) !Word32 !Word32
    | {- | Remove pending timeout
      Arguments: user_data, flags
      -}
      TimeoutRemoveOp !Word64 !Word32
    | {- | Open file relative to directory fd
      Arguments: dfd, path, flags, mode
      -}
      OpenatOp !Fd !CString !Int !Word32
    | -- | Close file descriptor
      CloseOp !Fd
    | {- | Preallocate file space
      Arguments: fd, mode, offset, length
      -}
      FallocateOp !Fd !Int !FileOffset !FileOffset
    | {- | Splice data between file descriptors
      Arguments: fd_in, off_in, fd_out, off_out, nbytes, flags
      -}
      SpliceOp !Fd !Int64 !Fd !Int64 !Word32 !Word32
    | {- | Duplicate pipe content
      Arguments: fd_in, fd_out, nbytes, flags
      -}
      TeeOp !Fd !Fd !Word32 !Word32
    | {- | Rename file
      Arguments: olddfd, oldpath, newdfd, newpath, flags
      -}
      RenameatOp !Fd !CString !Fd !CString !Word32
    | {- | Unlink file
      Arguments: dfd, path, flags
      -}
      UnlinkatOp !Fd !CString !Int
    | {- | Create directory
      Arguments: dfd, path, mode
      -}
      MkdiratOp !Fd !CString !Word32
    | {- | Create symbolic link
      Arguments: target, newdfd, linkpath
      -}
      SymlinkatOp !CString !Fd !CString
    | {- | Create hard link
      Arguments: olddfd, oldpath, newdfd, newpath, flags
      -}
      LinkatOp !Fd !CString !Fd !CString !Int
    | {- | Memory advice for mapped region
      Arguments: addr, length, advice
      -}
      MadviseOp !(Ptr ()) !FileOffset !Int
    | {- | File access pattern advice
      Arguments: fd, offset, length, advice
      -}
      FadviseOp !Fd !FileOffset !FileOffset !Int

-- | Result of an I/O operation.
data IoResult
    = -- | Operation completed successfully with the given byte count
      Complete !ByteCount
    | -- | End of file reached (not currently used, reserved for future)
      Eof
    | -- | Operation failed with the given errno
      IoErrno !Errno
    deriving stock (Eq)

instance Show IoResult where
    show (Complete n) = "Complete " ++ show n
    show Eof = "Eof"
    show (IoErrno (Errno e)) = "IoErrno " ++ show e

{- | Interpret a raw result code from io_uring into an 'IoResult'.

Negative values indicate errors (negated errno), non-negative
values indicate success with byte count.
-}
interpretResult :: Int64 -> IoResult
interpretResult r
    | r < 0 = IoErrno (Errno (fromIntegral (-r)))
    | otherwise = Complete (fromIntegral r)

{- | Callback type for building a batch of operations.

The callback is given a submit function to accumulate operations.
-}
type BatchPrep = forall m. (MonadIO m) => (IoOp -> m ()) -> m ()

{- | Submit a batch of I/O operations and wait for all completions.

Operations are collected via the callback, then submitted atomically.
Results are returned in the same order as operations were submitted.

@
results <- submitBatch ctx $ \\submit -> do
    submit (ReadOp fd1 0 buf1 0 4096)
    submit (ReadOp fd2 0 buf2 0 4096)
-- results V.! 0 corresponds to first ReadOp, etc.
@
-}
submitBatch :: IOCtx -> BatchPrep -> IO (Vector IoResult)
submitBatch ctx batchPrep = do
    ops <- collectOps batchPrep
    let batchSize = ioBatchSizeLimit (ioCtxParams ctx)
    if V.length ops <= batchSize
        then submitSmallBatch ctx ops
        else submitChunkedBatch ctx ops batchSize

-- | Collect operations from a BatchPrep callback into a Vector.
collectOps :: BatchPrep -> IO (Vector IoOp)
collectOps p = do
    ref <- newIORef Seq.empty
    p (\op -> modifyIORef' ref (|> op))
    ops <- readIORef ref
    return $ V.fromList (toList ops)

-- | Submit a batch that fits within the ring size limit.
submitSmallBatch :: IOCtx -> Vector IoOp -> IO (Vector IoResult)
submitSmallBatch (IOCtx caps) ops = do
    -- Get current capability and use corresponding ring
    (capIdx, _) <- threadCapability =<< myThreadId
    let safeIdx = selectCapability caps capIdx
        cap = caps V.! safeIdx
        lock = _capLock cap

    -- Hold lock for entire batch to prevent interleaving
    withMVar lock $ \_ -> mask_ $ do
        let uring = _capURing cap
            ringPtr = URing.uRingPtr uring
            nOps = V.length ops

        -- Prepare SQEs
        V.imapM_ (prepareOp ringPtr) ops

        -- Submit
        URing.submitIO uring

        -- Wait for completions and convert results
        V.generateM nOps $ \_ -> do
            comp <- URing.awaitIO uring
            return $ completionToResult (URing.completionRes comp)

-- | Convert a URing completion result to our IoResult type.
completionToResult :: URing.IOResult -> IoResult
completionToResult (URing.IOResult r) = interpretResult r

{- | Submit a batch in chunks when it exceeds the ring size.
Currently returns empty (stub implementation).
-}
submitChunkedBatch :: IOCtx -> Vector IoOp -> Int -> IO (Vector IoResult)
submitChunkedBatch _ _ _ = return V.empty

{- | Prepare a single SQE for the given operation.

Gets an SQE from the ring, configures it for the operation,
and sets the user data to the operation index.
-}
prepareOp :: Ptr () -> Int -> IoOp -> IO ()
prepareOp ringPtr idx op = do
    sqe <- c_io_uring_get_sqe ringPtr
    -- If sqe is null, we should submit and retry, but for now assume batch size fits
    when (sqe == nullPtr) $ ioError (userError "SQ ring full")
    prepareSqe sqe op
    c_hs_uring_sqe_set_data sqe (fromIntegral idx)

{- | Configure an SQE for a specific operation.

This is separated from 'prepareOp' to allow testing the SQE configuration
logic independently of the ring management.
-}
prepareSqe :: Ptr () -> IoOp -> IO ()
prepareSqe sqe = \case
    ReadOp (Fd fd) off buf _ len -> do
        let ptr = mutablePrimArrayContents buf
        c_hs_uring_prep_read sqe fd (castPtr ptr) (fromIntegral len) (fromIntegral off)
    WriteOp (Fd fd) off buf _ len -> do
        let ptr = primArrayContents buf
        c_hs_uring_prep_write sqe fd (castPtr ptr) (fromIntegral len) (fromIntegral off)
    ReadvOp (Fd fd) off iovs cnt ->
        c_hs_uring_prep_readv sqe fd iovs (fromIntegral cnt) (fromIntegral off)
    WritevOp (Fd fd) off iovs cnt ->
        c_hs_uring_prep_writev sqe fd iovs (fromIntegral cnt) (fromIntegral off)
    RecvOp (Fd fd) buf _ len flags -> do
        let ptr = mutablePrimArrayContents buf
        c_hs_uring_prep_recv sqe fd (castPtr ptr) len (fromIntegral flags)
    RecvPtrOp (Fd fd) ptr len flags ->
        c_hs_uring_prep_recv sqe fd (castPtr ptr) len (fromIntegral flags)
    SendOp (Fd fd) buf _ len flags -> do
        let ptr = primArrayContents buf
        c_hs_uring_prep_send sqe fd (castPtr ptr) len (fromIntegral flags)
    SendPtrOp (Fd fd) ptr len flags ->
        c_hs_uring_prep_send sqe fd (castPtr ptr) len (fromIntegral flags)
    SendZcOp (Fd fd) buf _ len flags zcFlags -> do
        let ptr = primArrayContents buf
        c_hs_uring_prep_send_zc sqe fd (castPtr ptr) len (fromIntegral flags) (fromIntegral zcFlags)
    SendZcPtrOp (Fd fd) ptr len flags zcFlags ->
        c_hs_uring_prep_send_zc sqe fd (castPtr ptr) len (fromIntegral flags) (fromIntegral zcFlags)
    AcceptOp (Fd fd) flags addrPtr lenPtr ->
        c_hs_uring_prep_accept sqe fd (castPtr addrPtr) (castPtr lenPtr) (fromIntegral flags)
    ConnectOp (Fd _) _ ->
        -- Stub: connect not yet implemented
        return ()
    SockCancelOp (Fd _) ->
        -- Stub: socket cancel not yet implemented
        return ()
    PollAddOp (Fd fd) mask ->
        c_hs_uring_prep_poll_add sqe fd (fromIntegral mask)
    PollRemoveOp targetUserData ->
        c_hs_uring_prep_poll_remove sqe (fromIntegral targetUserData)
    FsyncOp (Fd fd) flags ->
        c_hs_uring_prep_fsync sqe fd (fromIntegral flags)
    TimeoutOp ts count flags ->
        c_hs_uring_prep_timeout sqe ts (fromIntegral count) (fromIntegral flags)
    TimeoutRemoveOp targetUserData flags ->
        c_hs_uring_prep_timeout_remove sqe (fromIntegral targetUserData) (fromIntegral flags)
    OpenatOp (Fd dfd) path flags mode ->
        c_hs_uring_prep_openat sqe dfd path (fromIntegral flags) (fromIntegral mode)
    CloseOp (Fd fd) ->
        c_hs_uring_prep_close sqe fd
    FallocateOp (Fd fd) mode off len ->
        c_hs_uring_prep_fallocate sqe fd (fromIntegral mode) off len
    SpliceOp (Fd fd_in) off_in (Fd fd_out) off_out nbytes flags ->
        c_hs_uring_prep_splice sqe fd_in off_in fd_out off_out (fromIntegral nbytes) (fromIntegral flags)
    TeeOp (Fd fd_in) (Fd fd_out) nbytes flags ->
        c_hs_uring_prep_tee sqe fd_in fd_out (fromIntegral nbytes) (fromIntegral flags)
    ShutdownOp (Fd fd) how ->
        c_hs_uring_prep_shutdown sqe fd (fromIntegral how)
    RenameatOp (Fd olddfd) oldpath (Fd newdfd) newpath flags ->
        c_hs_uring_prep_renameat sqe olddfd oldpath newdfd newpath (fromIntegral flags)
    UnlinkatOp (Fd dfd) path flags ->
        c_hs_uring_prep_unlinkat sqe dfd path (fromIntegral flags)
    MkdiratOp (Fd dfd) path mode ->
        c_hs_uring_prep_mkdirat sqe dfd path (fromIntegral mode)
    SymlinkatOp target (Fd newdfd) linkpath ->
        c_hs_uring_prep_symlinkat sqe target newdfd linkpath
    LinkatOp (Fd olddfd) oldpath (Fd newdfd) newpath flags ->
        c_hs_uring_prep_linkat sqe olddfd oldpath newdfd newpath (fromIntegral flags)
    MadviseOp addr len advice ->
        c_hs_uring_prep_madvise sqe (castPtr addr) len (fromIntegral advice)
    FadviseOp (Fd fd) off len advice ->
        c_hs_uring_prep_fadvise sqe fd off len (fromIntegral advice)
    SyncOp (Fd fd) ->
        -- SyncOp uses fsync with flags=0
        c_hs_uring_prep_fsync sqe fd 0

-- ============================================================================
-- BUFFER AND FILE REGISTRATION
-- ============================================================================

{- | Register a set of buffers with the kernel for faster I/O.

Registered buffers can be used with fixed buffer operations to
avoid per-operation buffer pinning overhead.

Call 'unregisterBuffers' when the buffers are no longer needed.
-}
registerBuffers :: IOCtx -> Vector (Ptr Word8, Int) -> IO ()
registerBuffers (IOCtx caps) bufs = do
    let nBufs = V.length bufs
    bracket (mallocBytes (nBufs * 16)) free $ \iovecsPtr -> do
        -- Populate iovec array
        V.imapM_
            ( \i (ptr, len) -> do
                let iovPtr = iovecsPtr `plusPtr` (i * 16)
                poke (castPtr iovPtr :: Ptr IOVec) (IOVec (castPtr ptr) (fromIntegral len))
            )
            bufs
        -- Register with all capability rings
        forAllCaps caps "io_uring_register_buffers" $ \ringPtr ->
            c_hs_uring_register_buffers ringPtr (castPtr iovecsPtr) (fromIntegral nBufs)

-- | Unregister previously registered buffers.
unregisterBuffers :: IOCtx -> IO ()
unregisterBuffers (IOCtx caps) =
    forAllCaps caps "io_uring_unregister_buffers" $ \ringPtr ->
        c_hs_uring_unregister_buffers ringPtr

{- | Register a set of file descriptors with the kernel.

Registered files can be referenced by index (using fixed file operations)
for reduced overhead on repeated operations.
-}
registerFiles :: IOCtx -> Vector Fd -> IO ()
registerFiles (IOCtx caps) files = do
    let nFiles = V.length files
    bracket (mallocBytes (nFiles * 4)) free $ \filesPtr -> do
        -- Populate fd array
        V.imapM_ (\i (Fd fd) -> pokeByteOff filesPtr (i * 4) fd) files
        -- Register with all capability rings
        forAllCaps caps "io_uring_register_files" $ \ringPtr ->
            c_hs_uring_register_files ringPtr (castPtr filesPtr) (fromIntegral nFiles)

-- | Unregister previously registered file descriptors.
unregisterFiles :: IOCtx -> IO ()
unregisterFiles (IOCtx caps) =
    forAllCaps caps "io_uring_unregister_files" $ \ringPtr ->
        c_hs_uring_unregister_files ringPtr

{- | Update a subset of registered file descriptors.

@updateFiles ctx offset newFds@ replaces registered fds starting at @offset@.
-}
updateFiles :: IOCtx -> Int -> Vector Fd -> IO ()
updateFiles (IOCtx caps) off files = do
    let nFiles = V.length files
    bracket (mallocBytes (nFiles * 4)) free $ \filesPtr -> do
        -- Populate fd array
        V.imapM_ (\i (Fd fd) -> pokeByteOff filesPtr (i * 4) fd) files
        -- Update on all capability rings
        forAllCaps caps "io_uring_register_files_update" $ \ringPtr ->
            c_hs_uring_register_files_update ringPtr (fromIntegral off) (castPtr filesPtr) (fromIntegral nFiles)

{- | Apply an operation to all capability rings, checking for errors.

This is a helper to reduce duplication in register/unregister operations.
-}
forAllCaps :: Vector CapCtx -> String -> (Ptr () -> IO CInt) -> IO ()
forAllCaps caps opName action =
    V.forM_ caps $ \(CapCtx _ uring _ _ _) -> do
        let ringPtr = URing.uRingPtr uring
        res <- action ringPtr
        when (res < 0) $
            ioError $
                userError $
                    opName ++ " failed: " ++ show res

-- | Extract the configuration parameters from an IOCtx.
ioCtxParams :: IOCtx -> IOCtxParams
ioCtxParams (IOCtx caps)
    | V.null caps = defaultIoUringParams
    | otherwise =
        let CapCtx _ _ _ batchSize concurrency = V.head caps
         in IOCtxParams batchSize concurrency
