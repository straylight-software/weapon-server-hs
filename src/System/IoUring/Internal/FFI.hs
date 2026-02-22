-- Manual FFI bindings for io_uring (no hsc)
{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall #-}

module System.IoUring.Internal.FFI
  ( -- * Types
    IoUring(..)
  , IoUringParams(..)
  , URingPtr(..)
    -- * Operation codes
  , OpCode(..)
  , opcode
    -- * FFI functions
  , c_io_uring_queue_init
  , c_io_uring_queue_exit
  , c_io_uring_submit
  , c_io_uring_get_sqe
  , c_io_uring_register
    -- * Helper functions
  , c_hs_uring_prep_nop
  , c_hs_uring_prep_readv
  , c_hs_uring_prep_writev
  , c_hs_uring_prep_read
  , c_hs_uring_prep_write
  , c_hs_uring_sqe_set_data
  , c_hs_uring_prep_recv
  , c_hs_uring_prep_send
  , c_hs_uring_prep_send_zc
  , c_hs_uring_prep_accept
  , c_hs_uring_prep_connect
  , c_hs_uring_prep_cancel
  , c_hs_uring_peek_cqe
  , c_hs_uring_wait_cqe
  , c_hs_uring_cqe_seen
  , c_hs_uring_register_buffers
  , c_hs_uring_unregister_buffers
  -- * New Parity Helpers
  , c_hs_uring_prep_poll_add
  , c_hs_uring_prep_poll_remove
  , c_hs_uring_prep_fsync
  , c_hs_uring_prep_timeout
  , c_hs_uring_prep_timeout_remove
  , c_hs_uring_prep_openat
  , c_hs_uring_prep_close
  , c_hs_uring_prep_fallocate
  , c_hs_uring_prep_splice
  , c_hs_uring_prep_tee
  , c_hs_uring_prep_shutdown
  , c_hs_uring_prep_renameat
  , c_hs_uring_prep_unlinkat
  , c_hs_uring_prep_mkdirat
  , c_hs_uring_prep_symlinkat
  , c_hs_uring_prep_linkat
  , c_hs_uring_prep_madvise
  , c_hs_uring_prep_fadvise
  , c_hs_uring_register_files
  , c_hs_uring_unregister_files
  , c_hs_uring_register_files_update
    -- * Structures
  , IOVec(..)
  , KernelTimespec(..)

    -- * Constants
  , enterFlags
  , iosqeIoLink
  , msgDontwait
  , ioringRegisterBuffers
  , ioringUnregisterBuffers
  ) where

import Foreign (Ptr, Storable(sizeOf, alignment, peek, poke), peekByteOff, pokeByteOff)
import Foreign.C.Types (CInt(CInt), CULLong(CULLong), CUInt(CUInt), CSize(CSize))
import Foreign.C.String (CString)
import System.Posix.Types (COff(COff))
import Data.Word (Word8, Word32)
import Data.Int (Int64)

-- Opaque C types (we treat them as void* for simplicity)
newtype IoUring = IoUring { unIoUring :: Ptr () }
newtype IoUringParams = IoUringParams { unIoUringParams :: Ptr () }

-- FFI pointer types  
newtype URingPtr = URingPtr (Ptr IoUring)

-- IOVec structure
data IOVec = IOVec
  { iovBase :: !(Ptr ())
  , iovLen  :: !CSize
  } deriving (Show, Eq)

instance Storable IOVec where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = do
    base <- peekByteOff ptr 0
    len  <- peekByteOff ptr 8
    return $ IOVec base len
  poke ptr (IOVec base len) = do
    pokeByteOff ptr 0 base
    pokeByteOff ptr 8 len

-- KernelTimespec structure
data KernelTimespec = KernelTimespec
  { ktTvSec  :: !Int64
  , ktTvNsec :: !Int64
  } deriving (Show, Eq)

instance Storable KernelTimespec where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = do
    sec  <- peekByteOff ptr 0
    nsec <- peekByteOff ptr 8
    return $ KernelTimespec sec nsec
  poke ptr (KernelTimespec sec nsec) = do
    pokeByteOff ptr 0 sec
    pokeByteOff ptr 8 nsec

-- Operation codes
data OpCode
  = OpNop | OpReadv | OpWritev | OpFsync | OpReadFixed | OpWriteFixed
  | OpPollAdd | OpPollRemove | OpSyncFileRange | OpSendMsg | OpRecvMsg
  | OpTimeout | OpTimeoutRemove | OpAccept | OpAsyncCancel | OpLinkTimeout
  | OpConnect | OpFallocate | OpOpenat2 | OpStatx | OpFadvise | OpMadvise
  | OpSend | OpRecv | OpOpenat | OpClose | OpFilesUpdate
  | OpRead | OpWrite | OpFtruncate | OpRemove | OpProvideBuffers | OpRemoveBuffers
  | OpTee | OpShutdown | OpRenameAt | OpUnlinkAt | OpMkdirAt | OpSymlinkAt | OpLinkAt
  | OpMsgRing | OpFsetxattr | OpFgetxattr | OpSocket | OpUringCmd | OpSendZC | OpSendMsgZC
  deriving (Show, Eq, Enum, Bounded)

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

-- FFI bindings using Ptr () for opaque types
foreign import ccall unsafe "io_uring_queue_init"
  c_io_uring_queue_init :: CInt -> Ptr () -> Word32 -> IO CInt

foreign import ccall unsafe "io_uring_queue_exit"
  c_io_uring_queue_exit :: Ptr () -> IO ()

foreign import ccall unsafe "io_uring_submit"
  c_io_uring_submit :: Ptr () -> IO CInt

foreign import ccall unsafe "io_uring_get_sqe"
  c_io_uring_get_sqe :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "io_uring_register"
  c_io_uring_register :: CInt -> CUInt -> Ptr () -> CUInt -> IO CInt

-- Constants
enterFlags :: Word32
enterFlags = 8  -- IORING_ENTER_GETEVENTS

iosqeIoLink :: Word8
iosqeIoLink = 4  -- IOSQE_IO_LINK

msgDontwait :: Word32
msgDontwait = 64  -- MSG_DONTWAIT

ioringRegisterBuffers :: CUInt
ioringRegisterBuffers = 0

ioringUnregisterBuffers :: CUInt
ioringUnregisterBuffers = 1

-- Helper functions
foreign import ccall unsafe "hs_uring_prep_nop"
  c_hs_uring_prep_nop :: Ptr () -> IO ()

foreign import ccall unsafe "hs_uring_prep_readv"
  c_hs_uring_prep_readv :: Ptr () -> CInt -> Ptr IOVec -> CUInt -> CULLong -> IO ()

foreign import ccall unsafe "hs_uring_prep_writev"
  c_hs_uring_prep_writev :: Ptr () -> CInt -> Ptr IOVec -> CUInt -> CULLong -> IO ()

foreign import ccall unsafe "hs_uring_prep_read"
  c_hs_uring_prep_read :: Ptr () -> CInt -> Ptr () -> CUInt -> CULLong -> IO ()

foreign import ccall unsafe "hs_uring_prep_write"
  c_hs_uring_prep_write :: Ptr () -> CInt -> Ptr () -> CUInt -> CULLong -> IO ()

foreign import ccall unsafe "hs_uring_sqe_set_data"
  c_hs_uring_sqe_set_data :: Ptr () -> CULLong -> IO ()

foreign import ccall unsafe "hs_uring_prep_recv"
  c_hs_uring_prep_recv :: Ptr () -> CInt -> Ptr () -> CSize -> CInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_send"
  c_hs_uring_prep_send :: Ptr () -> CInt -> Ptr () -> CSize -> CInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_accept"
  c_hs_uring_prep_accept :: Ptr () -> CInt -> Ptr () -> Ptr () -> CInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_connect"
  c_hs_uring_prep_connect :: Ptr () -> CInt -> Ptr () -> CSize -> IO ()

foreign import ccall unsafe "hs_uring_prep_cancel"
  c_hs_uring_prep_cancel :: Ptr () -> Ptr () -> CInt -> IO ()

foreign import ccall unsafe "hs_uring_peek_cqe"
  c_hs_uring_peek_cqe :: Ptr () -> Ptr (Ptr ()) -> IO CInt

foreign import ccall safe "hs_uring_wait_cqe"
  c_hs_uring_wait_cqe :: Ptr () -> Ptr (Ptr ()) -> IO CInt

foreign import ccall unsafe "hs_uring_cqe_seen"
  c_hs_uring_cqe_seen :: Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "hs_uring_prep_send_zc"
  c_hs_uring_prep_send_zc :: Ptr () -> CInt -> Ptr () -> CSize -> CInt -> CUInt -> IO ()

foreign import ccall unsafe "hs_uring_register_buffers"
  c_hs_uring_register_buffers :: Ptr () -> Ptr IOVec -> CUInt -> IO CInt

foreign import ccall unsafe "hs_uring_unregister_buffers"
  c_hs_uring_unregister_buffers :: Ptr () -> IO CInt

-- New Parity Imports

foreign import ccall unsafe "hs_uring_prep_poll_add"
  c_hs_uring_prep_poll_add :: Ptr () -> CInt -> CUInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_poll_remove"
  c_hs_uring_prep_poll_remove :: Ptr () -> CULLong -> IO ()

foreign import ccall unsafe "hs_uring_prep_fsync"
  c_hs_uring_prep_fsync :: Ptr () -> CInt -> CUInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_timeout"
  c_hs_uring_prep_timeout :: Ptr () -> Ptr KernelTimespec -> CUInt -> CUInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_timeout_remove"
  c_hs_uring_prep_timeout_remove :: Ptr () -> CULLong -> CUInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_openat"
  c_hs_uring_prep_openat :: Ptr () -> CInt -> CString -> CInt -> CUInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_close"
  c_hs_uring_prep_close :: Ptr () -> CInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_fallocate"
  c_hs_uring_prep_fallocate :: Ptr () -> CInt -> CInt -> COff -> COff -> IO ()

foreign import ccall unsafe "hs_uring_prep_splice"
  c_hs_uring_prep_splice :: Ptr () -> CInt -> Int64 -> CInt -> Int64 -> CUInt -> CUInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_tee"
  c_hs_uring_prep_tee :: Ptr () -> CInt -> CInt -> CUInt -> CUInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_shutdown"
  c_hs_uring_prep_shutdown :: Ptr () -> CInt -> CInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_renameat"
  c_hs_uring_prep_renameat :: Ptr () -> CInt -> CString -> CInt -> CString -> CUInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_unlinkat"
  c_hs_uring_prep_unlinkat :: Ptr () -> CInt -> CString -> CInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_mkdirat"
  c_hs_uring_prep_mkdirat :: Ptr () -> CInt -> CString -> CUInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_symlinkat"
  c_hs_uring_prep_symlinkat :: Ptr () -> CString -> CInt -> CString -> IO ()

foreign import ccall unsafe "hs_uring_prep_linkat"
  c_hs_uring_prep_linkat :: Ptr () -> CInt -> CString -> CInt -> CString -> CInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_madvise"
  c_hs_uring_prep_madvise :: Ptr () -> Ptr () -> COff -> CInt -> IO ()

foreign import ccall unsafe "hs_uring_prep_fadvise"
  c_hs_uring_prep_fadvise :: Ptr () -> CInt -> COff -> COff -> CInt -> IO ()

foreign import ccall unsafe "hs_uring_register_files"
  c_hs_uring_register_files :: Ptr () -> Ptr CInt -> CUInt -> IO CInt

foreign import ccall unsafe "hs_uring_unregister_files"
  c_hs_uring_unregister_files :: Ptr () -> IO CInt

foreign import ccall unsafe "hs_uring_register_files_update"
  c_hs_uring_register_files_update :: Ptr () -> CUInt -> Ptr CInt -> CUInt -> IO CInt
