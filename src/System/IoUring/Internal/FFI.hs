{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall #-}

{- |
Module      : System.IoUring.Internal.FFI
Description : Low-level FFI bindings for Linux io_uring
Stability   : internal

This module provides raw FFI bindings to the liburing C library.
These are low-level bindings intended for internal use only.

For a high-level API, use "System.IoUring" instead.

= Safety

Most functions are imported as @unsafe@ for performance, which means
they must not block and must complete quickly. The exceptions are
the completion waiting functions which are imported as @safe@.
-}
module System.IoUring.Internal.FFI (
    -- * Types

    -- | Opaque wrapper types for C structures
    IoUring (..),
    IoUringParams (..),
    URingPtr (..),

    -- * Operation Codes

    -- | io_uring operation type identifiers
    OpCode (..),
    opcode,

    -- * Core Ring Functions

    -- | Queue initialization, submission, and cleanup
    c_io_uring_queue_init,
    c_io_uring_queue_exit,
    c_io_uring_submit,
    c_io_uring_get_sqe,
    c_io_uring_register,

    -- * SQE Preparation Helpers

    -- | Functions to configure submission queue entries

    -- ** Basic Operations
    c_hs_uring_prep_nop,
    c_hs_uring_prep_readv,
    c_hs_uring_prep_writev,
    c_hs_uring_prep_read,
    c_hs_uring_prep_write,
    c_hs_uring_sqe_set_data,

    -- ** Socket Operations
    c_hs_uring_prep_recv,
    c_hs_uring_prep_send,
    c_hs_uring_prep_send_zc,
    c_hs_uring_prep_accept,
    c_hs_uring_prep_connect,
    c_hs_uring_prep_cancel,

    -- ** Completion Handling
    c_hs_uring_peek_cqe,
    c_hs_uring_wait_cqe,
    c_hs_uring_cqe_seen,

    -- ** Buffer Registration
    c_hs_uring_register_buffers,
    c_hs_uring_unregister_buffers,

    -- ** Polling and Timeouts
    c_hs_uring_prep_poll_add,
    c_hs_uring_prep_poll_remove,
    c_hs_uring_prep_fsync,
    c_hs_uring_prep_timeout,
    c_hs_uring_prep_timeout_remove,

    -- ** File Operations
    c_hs_uring_prep_openat,
    c_hs_uring_prep_close,
    c_hs_uring_prep_fallocate,
    c_hs_uring_prep_splice,
    c_hs_uring_prep_tee,
    c_hs_uring_prep_shutdown,

    -- ** Path Operations
    c_hs_uring_prep_renameat,
    c_hs_uring_prep_unlinkat,
    c_hs_uring_prep_mkdirat,
    c_hs_uring_prep_symlinkat,
    c_hs_uring_prep_linkat,

    -- ** Memory Advice
    c_hs_uring_prep_madvise,
    c_hs_uring_prep_fadvise,

    -- ** File Registration
    c_hs_uring_register_files,
    c_hs_uring_unregister_files,
    c_hs_uring_register_files_update,

    -- * C Structures

    -- | Haskell representations of C structs
    IOVec (..),
    KernelTimespec (..),

    -- * Constants

    -- | io_uring flags and constants
    enterFlags,
    iosqeIoLink,
    msgDontwait,
    ioringRegisterBuffers,
    ioringUnregisterBuffers,
) where

import Data.Int (Int64)
import Data.Word (Word32, Word8)
import Foreign (Ptr, Storable (alignment, peek, poke, sizeOf), peekByteOff, pokeByteOff)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (CInt), CSize (CSize), CUInt (CUInt), CULLong (CULLong))
import System.Posix.Types (COff (COff))

{- | Opaque handle to the io_uring ring structure.

We treat this as @Ptr ()@ for simplicity since we don't need to
inspect its internals from Haskell.
-}
newtype IoUring = IoUring {unIoUring :: Ptr ()}

-- | Opaque handle to io_uring initialization parameters.
newtype IoUringParams = IoUringParams {unIoUringParams :: Ptr ()}

-- | Typed pointer to an io_uring ring.
newtype URingPtr = URingPtr (Ptr IoUring)

{- | I/O vector for scatter/gather operations.

Corresponds to the C @struct iovec@.
Size: 16 bytes (8 for pointer + 8 for size_t on 64-bit)
-}
data IOVec = IOVec
    { iovBase :: !(Ptr ())
    -- ^ Pointer to data buffer
    , iovLen :: !CSize
    -- ^ Length of data in bytes
    }
    deriving (Show, Eq)

instance Storable IOVec where
    sizeOf _ = 16
    alignment _ = 8
    peek ptr = do
        base <- peekByteOff ptr 0
        len <- peekByteOff ptr 8
        return $ IOVec base len
    poke ptr (IOVec base len) = do
        pokeByteOff ptr 0 base
        pokeByteOff ptr 8 len

{- | Kernel timespec structure for timeout operations.

Corresponds to @struct __kernel_timespec@.
Size: 16 bytes (8 + 8 for two int64 fields)
-}
data KernelTimespec = KernelTimespec
    { ktTvSec :: !Int64
    -- ^ Seconds
    , ktTvNsec :: !Int64
    -- ^ Nanoseconds
    }
    deriving (Show, Eq)

instance Storable KernelTimespec where
    sizeOf _ = 16
    alignment _ = 8
    peek ptr = do
        sec <- peekByteOff ptr 0
        nsec <- peekByteOff ptr 8
        return $ KernelTimespec sec nsec
    poke ptr (KernelTimespec sec nsec) = do
        pokeByteOff ptr 0 sec
        pokeByteOff ptr 8 nsec

{- | io_uring operation codes.

These correspond to the @IORING_OP_*@ constants in the Linux kernel.
Use 'opcode' to convert to the numeric value.
-}
data OpCode
    = -- | No operation (for testing)
      OpNop
    | -- | Vectored read
      OpReadv
    | -- | Vectored write
      OpWritev
    | -- | File sync
      OpFsync
    | -- | Read with fixed buffer
      OpReadFixed
    | -- | Write with fixed buffer
      OpWriteFixed
    | -- | Add poll request
      OpPollAdd
    | -- | Remove poll request
      OpPollRemove
    | -- | Sync file range
      OpSyncFileRange
    | -- | Send message on socket
      OpSendMsg
    | -- | Receive message from socket
      OpRecvMsg
    | -- | Set timeout
      OpTimeout
    | -- | Remove timeout
      OpTimeoutRemove
    | -- | Accept connection
      OpAccept
    | -- | Cancel async operation
      OpAsyncCancel
    | -- | Linked timeout
      OpLinkTimeout
    | -- | Connect socket
      OpConnect
    | -- | File allocation
      OpFallocate
    | -- | Open file (extended)
      OpOpenat2
    | -- | Get file status
      OpStatx
    | -- | File access advice
      OpFadvise
    | -- | Memory advice
      OpMadvise
    | -- | Send to socket
      OpSend
    | -- | Receive from socket
      OpRecv
    | -- | Open file
      OpOpenat
    | -- | Close file descriptor
      OpClose
    | -- | Update registered files
      OpFilesUpdate
    | -- | Read from file
      OpRead
    | -- | Write to file
      OpWrite
    | -- | Truncate file
      OpFtruncate
    | -- | Remove file
      OpRemove
    | -- | Provide buffers for buffer selection
      OpProvideBuffers
    | -- | Remove provided buffers
      OpRemoveBuffers
    | -- | Duplicate pipe content
      OpTee
    | -- | Shutdown socket
      OpShutdown
    | -- | Rename file
      OpRenameAt
    | -- | Unlink file
      OpUnlinkAt
    | -- | Create directory
      OpMkdirAt
    | -- | Create symbolic link
      OpSymlinkAt
    | -- | Create hard link
      OpLinkAt
    | -- | Send message to another ring
      OpMsgRing
    | -- | Set extended attribute
      OpFsetxattr
    | -- | Get extended attribute
      OpFgetxattr
    | -- | Create socket
      OpSocket
    | -- | io_uring command
      OpUringCmd
    | -- | Zero-copy send
      OpSendZC
    | -- | Zero-copy send message
      OpSendMsgZC
    deriving (Show, Eq)

{- | Convert an 'OpCode' to its numeric value.

These values match the @IORING_OP_*@ constants in the Linux kernel.
-}
opcode :: OpCode -> Word8
opcode OpNop = 0
opcode OpReadv = 1
opcode OpWritev = 2
opcode OpFsync = 3
opcode OpReadFixed = 4
opcode OpWriteFixed = 5
opcode OpPollAdd = 6
opcode OpPollRemove = 7
opcode OpSyncFileRange = 8
opcode OpSendMsg = 9
opcode OpRecvMsg = 10
opcode OpTimeout = 11
opcode OpTimeoutRemove = 12
opcode OpAccept = 13
opcode OpAsyncCancel = 14
opcode OpLinkTimeout = 15
opcode OpConnect = 16
opcode OpFallocate = 17
opcode OpOpenat2 = 18
opcode OpStatx = 19
opcode OpFadvise = 20
opcode OpMadvise = 21
opcode OpSend = 22
opcode OpRecv = 23
opcode OpOpenat = 24
opcode OpClose = 25
opcode OpFilesUpdate = 26
opcode OpRead = 27
opcode OpWrite = 28
opcode OpFtruncate = 29
opcode OpRemove = 30
opcode OpProvideBuffers = 31
opcode OpRemoveBuffers = 32
opcode OpTee = 33
opcode OpShutdown = 34
opcode OpRenameAt = 35
opcode OpUnlinkAt = 36
opcode OpMkdirAt = 37
opcode OpSymlinkAt = 38
opcode OpLinkAt = 39
opcode OpMsgRing = 40
opcode OpFsetxattr = 41
opcode OpFgetxattr = 42
opcode OpSocket = 43
opcode OpUringCmd = 44
opcode OpSendZC = 45
opcode OpSendMsgZC = 46

-- ============================================================================
-- Core Ring FFI
-- ============================================================================

{- | Initialize an io_uring queue.

@c_io_uring_queue_init entries ring flags@

* @entries@ - Number of SQ entries (must be power of 2)
* @ring@ - Pointer to io_uring structure to initialize
* @flags@ - Setup flags (0 for default)

Returns 0 on success, negative errno on failure.
-}
foreign import ccall unsafe "io_uring_queue_init"
    c_io_uring_queue_init :: CInt -> Ptr () -> Word32 -> IO CInt

-- | Clean up and release an io_uring queue.
foreign import ccall unsafe "io_uring_queue_exit"
    c_io_uring_queue_exit :: Ptr () -> IO ()

{- | Submit all prepared SQEs to the kernel.

Returns number of SQEs submitted, or negative errno on failure.
-}
foreign import ccall unsafe "io_uring_submit"
    c_io_uring_submit :: Ptr () -> IO CInt

{- | Get the next available SQE from the submission queue.

Returns NULL if the SQ is full.
-}
foreign import ccall unsafe "io_uring_get_sqe"
    c_io_uring_get_sqe :: Ptr () -> IO (Ptr ())

-- | Register resources with the kernel.
foreign import ccall unsafe "io_uring_register"
    c_io_uring_register :: CInt -> CUInt -> Ptr () -> CUInt -> IO CInt

-- ============================================================================
-- Constants
-- ============================================================================

-- | io_uring_enter flags: get completion events.
enterFlags :: Word32
enterFlags = 8 -- IORING_ENTER_GETEVENTS

-- | SQE flag: link this operation to the next one.
iosqeIoLink :: Word8
iosqeIoLink = 4 -- IOSQE_IO_LINK

-- | Socket flag: don't block on send/recv.
msgDontwait :: Word32
msgDontwait = 64 -- MSG_DONTWAIT

-- | Registration opcode: register buffers.
ioringRegisterBuffers :: CUInt
ioringRegisterBuffers = 0

-- | Registration opcode: unregister buffers.
ioringUnregisterBuffers :: CUInt
ioringUnregisterBuffers = 1

-- ============================================================================
-- SQE Preparation Helpers
-- ============================================================================
--
-- These are thin wrappers around the liburing io_uring_prep_* functions.
-- Each configures an SQE for a specific operation type.

-- | Prepare a no-op SQE (useful for testing).
foreign import ccall unsafe "hs_uring_prep_nop"
    c_hs_uring_prep_nop :: Ptr () -> IO ()

-- | Prepare vectored read: @sqe fd iovecs count offset@
foreign import ccall unsafe "hs_uring_prep_readv"
    c_hs_uring_prep_readv :: Ptr () -> CInt -> Ptr IOVec -> CUInt -> CULLong -> IO ()

-- | Prepare vectored write: @sqe fd iovecs count offset@
foreign import ccall unsafe "hs_uring_prep_writev"
    c_hs_uring_prep_writev :: Ptr () -> CInt -> Ptr IOVec -> CUInt -> CULLong -> IO ()

-- | Prepare read: @sqe fd buf nbytes offset@
foreign import ccall unsafe "hs_uring_prep_read"
    c_hs_uring_prep_read :: Ptr () -> CInt -> Ptr () -> CUInt -> CULLong -> IO ()

-- | Prepare write: @sqe fd buf nbytes offset@
foreign import ccall unsafe "hs_uring_prep_write"
    c_hs_uring_prep_write :: Ptr () -> CInt -> Ptr () -> CUInt -> CULLong -> IO ()

-- | Set user data on an SQE for identification in completions.
foreign import ccall unsafe "hs_uring_sqe_set_data"
    c_hs_uring_sqe_set_data :: Ptr () -> CULLong -> IO ()

-- | Prepare socket receive: @sqe fd buf len flags@
foreign import ccall unsafe "hs_uring_prep_recv"
    c_hs_uring_prep_recv :: Ptr () -> CInt -> Ptr () -> CSize -> CInt -> IO ()

-- | Prepare socket send: @sqe fd buf len flags@
foreign import ccall unsafe "hs_uring_prep_send"
    c_hs_uring_prep_send :: Ptr () -> CInt -> Ptr () -> CSize -> CInt -> IO ()

-- | Prepare accept: @sqe fd addr addrlen flags@
foreign import ccall unsafe "hs_uring_prep_accept"
    c_hs_uring_prep_accept :: Ptr () -> CInt -> Ptr () -> Ptr () -> CInt -> IO ()

-- | Prepare connect: @sqe fd addr addrlen@
foreign import ccall unsafe "hs_uring_prep_connect"
    c_hs_uring_prep_connect :: Ptr () -> CInt -> Ptr () -> CSize -> IO ()

-- | Prepare cancel: @sqe user_data flags@
foreign import ccall unsafe "hs_uring_prep_cancel"
    c_hs_uring_prep_cancel :: Ptr () -> Ptr () -> CInt -> IO ()

{- | Peek for a completion without blocking.
Returns 0 if found, negative errno otherwise.
-}
foreign import ccall unsafe "hs_uring_peek_cqe"
    c_hs_uring_peek_cqe :: Ptr () -> Ptr (Ptr ()) -> IO CInt

{- | Wait for a completion (blocking).
Imported as @safe@ since it may block.
-}
foreign import ccall safe "hs_uring_wait_cqe"
    c_hs_uring_wait_cqe :: Ptr () -> Ptr (Ptr ()) -> IO CInt

-- | Mark a CQE as consumed so its slot can be reused.
foreign import ccall unsafe "hs_uring_cqe_seen"
    c_hs_uring_cqe_seen :: Ptr () -> Ptr () -> IO ()

-- | Prepare zero-copy send: @sqe fd buf len flags zc_flags@
foreign import ccall unsafe "hs_uring_prep_send_zc"
    c_hs_uring_prep_send_zc :: Ptr () -> CInt -> Ptr () -> CSize -> CInt -> CUInt -> IO ()

-- | Register fixed buffers with the ring.
foreign import ccall unsafe "hs_uring_register_buffers"
    c_hs_uring_register_buffers :: Ptr () -> Ptr IOVec -> CUInt -> IO CInt

-- | Unregister fixed buffers from the ring.
foreign import ccall unsafe "hs_uring_unregister_buffers"
    c_hs_uring_unregister_buffers :: Ptr () -> IO CInt

-- ============================================================================
-- Polling and Timeouts
-- ============================================================================

-- | Prepare poll add: @sqe fd poll_mask@
foreign import ccall unsafe "hs_uring_prep_poll_add"
    c_hs_uring_prep_poll_add :: Ptr () -> CInt -> CUInt -> IO ()

-- | Prepare poll remove: @sqe user_data@
foreign import ccall unsafe "hs_uring_prep_poll_remove"
    c_hs_uring_prep_poll_remove :: Ptr () -> CULLong -> IO ()

-- | Prepare fsync: @sqe fd flags@
foreign import ccall unsafe "hs_uring_prep_fsync"
    c_hs_uring_prep_fsync :: Ptr () -> CInt -> CUInt -> IO ()

-- | Prepare timeout: @sqe ts count flags@
foreign import ccall unsafe "hs_uring_prep_timeout"
    c_hs_uring_prep_timeout :: Ptr () -> Ptr KernelTimespec -> CUInt -> CUInt -> IO ()

-- | Prepare timeout remove: @sqe user_data flags@
foreign import ccall unsafe "hs_uring_prep_timeout_remove"
    c_hs_uring_prep_timeout_remove :: Ptr () -> CULLong -> CUInt -> IO ()

-- ============================================================================
-- File Operations
-- ============================================================================

-- | Prepare openat: @sqe dfd path flags mode@
foreign import ccall unsafe "hs_uring_prep_openat"
    c_hs_uring_prep_openat :: Ptr () -> CInt -> CString -> CInt -> CUInt -> IO ()

-- | Prepare close: @sqe fd@
foreign import ccall unsafe "hs_uring_prep_close"
    c_hs_uring_prep_close :: Ptr () -> CInt -> IO ()

-- | Prepare fallocate: @sqe fd mode offset len@
foreign import ccall unsafe "hs_uring_prep_fallocate"
    c_hs_uring_prep_fallocate :: Ptr () -> CInt -> CInt -> COff -> COff -> IO ()

-- | Prepare splice: @sqe fd_in off_in fd_out off_out nbytes flags@
foreign import ccall unsafe "hs_uring_prep_splice"
    c_hs_uring_prep_splice :: Ptr () -> CInt -> Int64 -> CInt -> Int64 -> CUInt -> CUInt -> IO ()

-- | Prepare tee: @sqe fd_in fd_out nbytes flags@
foreign import ccall unsafe "hs_uring_prep_tee"
    c_hs_uring_prep_tee :: Ptr () -> CInt -> CInt -> CUInt -> CUInt -> IO ()

-- | Prepare shutdown: @sqe fd how@
foreign import ccall unsafe "hs_uring_prep_shutdown"
    c_hs_uring_prep_shutdown :: Ptr () -> CInt -> CInt -> IO ()

-- ============================================================================
-- Path Operations
-- ============================================================================

-- | Prepare renameat: @sqe olddfd oldpath newdfd newpath flags@
foreign import ccall unsafe "hs_uring_prep_renameat"
    c_hs_uring_prep_renameat :: Ptr () -> CInt -> CString -> CInt -> CString -> CUInt -> IO ()

-- | Prepare unlinkat: @sqe dfd path flags@
foreign import ccall unsafe "hs_uring_prep_unlinkat"
    c_hs_uring_prep_unlinkat :: Ptr () -> CInt -> CString -> CInt -> IO ()

-- | Prepare mkdirat: @sqe dfd path mode@
foreign import ccall unsafe "hs_uring_prep_mkdirat"
    c_hs_uring_prep_mkdirat :: Ptr () -> CInt -> CString -> CUInt -> IO ()

-- | Prepare symlinkat: @sqe target newdfd linkpath@
foreign import ccall unsafe "hs_uring_prep_symlinkat"
    c_hs_uring_prep_symlinkat :: Ptr () -> CString -> CInt -> CString -> IO ()

-- | Prepare linkat: @sqe olddfd oldpath newdfd newpath flags@
foreign import ccall unsafe "hs_uring_prep_linkat"
    c_hs_uring_prep_linkat :: Ptr () -> CInt -> CString -> CInt -> CString -> CInt -> IO ()

-- ============================================================================
-- Memory Advice
-- ============================================================================

-- | Prepare madvise: @sqe addr len advice@
foreign import ccall unsafe "hs_uring_prep_madvise"
    c_hs_uring_prep_madvise :: Ptr () -> Ptr () -> COff -> CInt -> IO ()

-- | Prepare fadvise: @sqe fd offset len advice@
foreign import ccall unsafe "hs_uring_prep_fadvise"
    c_hs_uring_prep_fadvise :: Ptr () -> CInt -> COff -> COff -> CInt -> IO ()

-- ============================================================================
-- File Registration
-- ============================================================================

-- | Register file descriptors with the ring.
foreign import ccall unsafe "hs_uring_register_files"
    c_hs_uring_register_files :: Ptr () -> Ptr CInt -> CUInt -> IO CInt

-- | Unregister file descriptors from the ring.
foreign import ccall unsafe "hs_uring_unregister_files"
    c_hs_uring_unregister_files :: Ptr () -> IO CInt

-- | Update registered file descriptors: @ring offset fds count@
foreign import ccall unsafe "hs_uring_register_files_update"
    c_hs_uring_register_files_update :: Ptr () -> CUInt -> Ptr CInt -> CUInt -> IO CInt
