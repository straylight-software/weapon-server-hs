{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

-- | High-level io_uring API supporting both file and socket I/O
module System.IoUring
  ( -- * I/O Context
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
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, mask_)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Int (Int64)
import Data.Primitive (MutablePrimArray, PrimArray, mutablePrimArrayContents, primArrayContents)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32, Word64, Word8)
import Foreign (Ptr, castPtr, free, mallocBytes, nullPtr, plusPtr, poke)
import Foreign.C.Error (Errno (Errno))
import Foreign.C.String (CString)
import Foreign.Storable (pokeByteOff)
import GHC.Conc (getNumCapabilities, myThreadId, threadCapability)
import GHC.Exts (RealWorld)
import Network.Socket (SockAddr)
import System.IoUring.Internal.FFI
  ( IOVec (IOVec),
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
import qualified System.IoUring.URing as URing
import System.Posix.Types (ByteCount, Fd (Fd), FileOffset)

-- ============================================================================
-- CONTEXT
-- ============================================================================

newtype IOCtx = IOCtx (Vector CapCtx)

data CapCtx = CapCtx
  { _capNo :: !Int,
    _capURing :: !URing.URing,
    _capLock :: !(MVar ()), -- Lock for exclusive ring access
    _capBatchSizeLimit :: !Int,
    _capConcurrencyLimit :: !Int
  }

data IOCtxParams = IOCtxParams
  { ioBatchSizeLimit :: !Int,
    ioConcurrencyLimit :: !Int
  }
  deriving stock (Show)

defaultIoUringParams :: IOCtxParams
defaultIoUringParams =
  IOCtxParams
    { ioBatchSizeLimit = 64,
      ioConcurrencyLimit = 64 * 3
    }

withIoUring :: IOCtxParams -> (IOCtx -> IO a) -> IO a
withIoUring params = bracket (initIoUring params) closeIoUring

initIoUring :: IOCtxParams -> IO IOCtx
initIoUring (IOCtxParams batchSize concurrency) = do
  numCaps <- getNumCapabilities
  let ringSize = max 32 batchSize
  caps <- V.generateM numCaps $ \idx -> do
    uring <- URing.initURing idx ringSize (ringSize * 2)
    lock <- newMVar ()
    return $ CapCtx idx uring lock batchSize concurrency
  return $ IOCtx caps

closeIoUring :: IOCtx -> IO ()
closeIoUring (IOCtx caps) = V.forM_ caps closeCapCtx
  where
    closeCapCtx (CapCtx _ uring _ _ _) = URing.closeURing uring

-- ============================================================================
-- OPERATIONS
-- ============================================================================

data IoOp
  = -- File operations (disk I/O)
    ReadOp !Fd !FileOffset !(MutablePrimArray RealWorld Word8) !Int !ByteCount
  | WriteOp !Fd !FileOffset !(PrimArray Word8) !Int !ByteCount
  | ReadvOp !Fd !FileOffset !(Ptr IOVec) !Int
  | WritevOp !Fd !FileOffset !(Ptr IOVec) !Int
  | SyncOp !Fd
  | FsyncOp !Fd !Word32 -- flags
  | -- Socket operations (network I/O)
    RecvOp !Fd !(MutablePrimArray RealWorld Word8) !Int !ByteCount !Word32
  | RecvPtrOp !Fd !(Ptr Word8) !ByteCount !Word32
  | SendOp !Fd !(PrimArray Word8) !Int !ByteCount !Word32
  | SendPtrOp !Fd !(Ptr Word8) !ByteCount !Word32
  | SendZcOp !Fd !(PrimArray Word8) !Int !ByteCount !Word32 !Word32
  | SendZcPtrOp !Fd !(Ptr Word8) !ByteCount !Word32 !Word32
  | AcceptOp !Fd !Word32 !(Ptr ()) !(Ptr ()) -- addr and addrlen buffers
  | ConnectOp !Fd !SockAddr
  | SockCancelOp !Fd
  | ShutdownOp !Fd !Int
  | -- Polling & Timeouts
    PollAddOp !Fd !Word32
  | PollRemoveOp !Word64 -- user_data
  | TimeoutOp !(Ptr KernelTimespec) !Word32 !Word32 -- ts, count, flags
  | TimeoutRemoveOp !Word64 !Word32 -- user_data, flags
  | -- Advanced File Ops
    OpenatOp !Fd !CString !Int !Word32 -- dfd, path, flags, mode
  | CloseOp !Fd
  | FallocateOp !Fd !Int !FileOffset !FileOffset -- mode, offset, len
  | SpliceOp !Fd !Int64 !Fd !Int64 !Word32 !Word32 -- fd_in, off_in, fd_out, off_out, nbytes, flags
  | TeeOp !Fd !Fd !Word32 !Word32 -- fd_in, fd_out, nbytes, flags
  | -- Path Ops
    RenameatOp !Fd !CString !Fd !CString !Word32 -- olddfd, oldpath, newdfd, newpath, flags
  | UnlinkatOp !Fd !CString !Int -- dfd, path, flags
  | MkdiratOp !Fd !CString !Word32 -- dfd, path, mode
  | SymlinkatOp !CString !Fd !CString -- target, newdfd, linkpath
  | LinkatOp !Fd !CString !Fd !CString !Int -- olddfd, oldpath, newdfd, newpath, flags
  | -- Memory/Advice
    MadviseOp !(Ptr ()) !FileOffset !Int -- addr, len, advice
  | FadviseOp !Fd !FileOffset !FileOffset !Int -- offset, len, advice

-- Note: No Show instance because MutablePrimArray doesn't have one

data IoResult = Complete !ByteCount | Eof | IoErrno !Errno

instance Show IoResult where
  show (Complete n) = "Complete " ++ show n
  show Eof = "Eof"
  show (IoErrno (Errno e)) = "IoErrno " ++ show e

type BatchPrep = forall m. (MonadIO m) => (IoOp -> m ()) -> m ()

submitBatch :: IOCtx -> BatchPrep -> IO (Vector IoResult)
submitBatch ctx batchPrep = do
  ops <- collectOps batchPrep
  let batchSize = ioBatchSizeLimit (ioCtxParams ctx)
  if V.length ops <= batchSize
    then submitSmallBatch ctx ops
    else submitChunkedBatch ctx ops batchSize
  where
    collectOps :: BatchPrep -> IO (Vector IoOp)
    collectOps p = do
      ref <- newIORef []
      p (\op -> modifyIORef ref (op :))
      ops <- readIORef ref
      return $ V.fromList (reverse ops)

    submitSmallBatch :: IOCtx -> Vector IoOp -> IO (Vector IoResult)
    submitSmallBatch (IOCtx caps) ops = do
      -- Get current capability and use corresponding ring
      (capIdx, _) <- threadCapability =<< myThreadId
      let safeIdx = capIdx `mod` V.length caps
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

        -- Wait for completions
        -- We expect nOps completions. This is a synchronous batch submit.
        results <- V.generateM nOps $ \_ -> do
          comp <- URing.awaitIO uring
          return $ case URing.completionRes comp of
            URing.IOResult r ->
              if r < 0
                then IoErrno (Errno (fromIntegral (-r)))
                else Complete (fromIntegral r)

        return results

    prepareOp :: Ptr () -> Int -> IoOp -> IO ()
    prepareOp ringPtr idx op = do
      sqe <- c_io_uring_get_sqe ringPtr
      -- If sqe is null, we should submit and retry, but for now assume batch size fits
      when (sqe == nullPtr) $ ioError (userError "SQ ring full")

      let userData = fromIntegral idx

      case op of
        ReadOp (Fd fd) off buf _ len -> do
          let ptr = mutablePrimArrayContents buf
          c_hs_uring_prep_read sqe fd (castPtr ptr) (fromIntegral len) (fromIntegral off)
        WriteOp (Fd fd) off buf _ len -> do
          let ptr = primArrayContents buf
          c_hs_uring_prep_write sqe fd (castPtr ptr) (fromIntegral len) (fromIntegral off)
        ReadvOp (Fd fd) off iovs cnt -> do
          c_hs_uring_prep_readv sqe fd iovs (fromIntegral cnt) (fromIntegral off)
        WritevOp (Fd fd) off iovs cnt -> do
          c_hs_uring_prep_writev sqe fd iovs (fromIntegral cnt) (fromIntegral off)
        RecvOp (Fd fd) buf _ len flags -> do
          let ptr = mutablePrimArrayContents buf
          c_hs_uring_prep_recv sqe fd (castPtr ptr) len (fromIntegral flags)
        RecvPtrOp (Fd fd) ptr len flags -> do
          c_hs_uring_prep_recv sqe fd (castPtr ptr) len (fromIntegral flags)
        SendOp (Fd fd) buf _ len flags -> do
          let ptr = primArrayContents buf
          c_hs_uring_prep_send sqe fd (castPtr ptr) len (fromIntegral flags)
        SendPtrOp (Fd fd) ptr len flags -> do
          c_hs_uring_prep_send sqe fd (castPtr ptr) len (fromIntegral flags)
        SendZcOp (Fd fd) buf _ len flags zcFlags -> do
          let ptr = primArrayContents buf
          c_hs_uring_prep_send_zc sqe fd (castPtr ptr) len (fromIntegral flags) (fromIntegral zcFlags)
        SendZcPtrOp (Fd fd) ptr len flags zcFlags -> do
          c_hs_uring_prep_send_zc sqe fd (castPtr ptr) len (fromIntegral flags) (fromIntegral zcFlags)
        AcceptOp (Fd fd) flags addrPtr lenPtr -> do
          c_hs_uring_prep_accept sqe fd (castPtr addrPtr) (castPtr lenPtr) (fromIntegral flags)
        ConnectOp (Fd _) _ -> do
          -- Stub
          return ()
        SockCancelOp (Fd _) -> do
          -- Stub
          return ()

        -- New Parity Ops
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
        _ -> return ()

      c_hs_uring_sqe_set_data sqe userData

    submitChunkedBatch :: IOCtx -> Vector IoOp -> Int -> IO (Vector IoResult)
    submitChunkedBatch _ _ _ = return V.empty

registerBuffers :: IOCtx -> Vector (Ptr Word8, Int) -> IO ()
registerBuffers (IOCtx caps) bufs = do
  let nBufs = V.length bufs
  let arraySize = nBufs * 16
  bracket (mallocBytes arraySize) free $ \iovecsPtr -> do
    V.imapM_
      ( \i (ptr, len) -> do
          let iovPtr = iovecsPtr `plusPtr` (i * 16)
          poke (castPtr iovPtr :: Ptr IOVec) (IOVec (castPtr ptr) (fromIntegral len))
      )
      bufs

    V.forM_ caps $ \(CapCtx _ uring _ _ _) -> do
      let ringPtr = URing.uRingPtr uring
      res <- c_hs_uring_register_buffers ringPtr (castPtr iovecsPtr) (fromIntegral nBufs)
      when (res < 0) $ ioError $ userError $ "io_uring_register_buffers failed: " ++ show res

unregisterBuffers :: IOCtx -> IO ()
unregisterBuffers (IOCtx caps) = do
  V.forM_ caps $ \(CapCtx _ uring _ _ _) -> do
    let ringPtr = URing.uRingPtr uring
    res <- c_hs_uring_unregister_buffers ringPtr
    when (res < 0) $ ioError $ userError $ "io_uring_unregister_buffers failed: " ++ show res

-- | Register a set of file descriptors to reduce overhead.
registerFiles :: IOCtx -> Vector Fd -> IO ()
registerFiles (IOCtx caps) files = do
  let nFiles = V.length files
  let arraySize = nFiles * 4 -- sizeof(int)
  bracket (mallocBytes arraySize) free $ \filesPtr -> do
    V.imapM_
      ( \i (Fd fd) -> do
          pokeByteOff filesPtr (i * 4) fd
      )
      files

    V.forM_ caps $ \(CapCtx _ uring _ _ _) -> do
      let ringPtr = URing.uRingPtr uring
      res <- c_hs_uring_register_files ringPtr (castPtr filesPtr) (fromIntegral nFiles)
      when (res < 0) $ ioError $ userError $ "io_uring_register_files failed: " ++ show res

unregisterFiles :: IOCtx -> IO ()
unregisterFiles (IOCtx caps) = do
  V.forM_ caps $ \(CapCtx _ uring _ _ _) -> do
    let ringPtr = URing.uRingPtr uring
    res <- c_hs_uring_unregister_files ringPtr
    when (res < 0) $ ioError $ userError $ "io_uring_unregister_files failed: " ++ show res

updateFiles :: IOCtx -> Int -> Vector Fd -> IO ()
updateFiles (IOCtx caps) off files = do
  let nFiles = V.length files
  let arraySize = nFiles * 4
  bracket (mallocBytes arraySize) free $ \filesPtr -> do
    V.imapM_
      ( \i (Fd fd) -> do
          pokeByteOff filesPtr (i * 4) fd
      )
      files

    V.forM_ caps $ \(CapCtx _ uring _ _ _) -> do
      let ringPtr = URing.uRingPtr uring
      res <- c_hs_uring_register_files_update ringPtr (fromIntegral off) (castPtr filesPtr) (fromIntegral nFiles)
      when (res < 0) $ ioError $ userError $ "io_uring_register_files_update failed: " ++ show res

ioCtxParams :: IOCtx -> IOCtxParams
ioCtxParams (IOCtx caps) =
  if V.null caps
    then defaultIoUringParams
    else
      let CapCtx _ _ _ batchSize concurrency = V.head caps
       in IOCtxParams batchSize concurrency
