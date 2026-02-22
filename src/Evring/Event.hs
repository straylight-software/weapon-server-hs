{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Events and Operations for the evring state machine model.
--
-- An 'Event' is a completion from the kernel (what happened).
-- An 'Operation' is a request to the kernel (what to do).
--
-- The state machine processes Events and emits Operations:
--
-- @
--   step :: State -> Event -> (State, [Operation])
-- @
module Evring.Event
  ( -- * Events (completions from kernel)
    Event(..)
  , emptyEvent
    -- * Operations (requests to kernel)
  , Operation(..)
  , OperationType(..)
    -- * Handles
  , Handle
    -- * Parameters
  , ReadParams(..)
  , WriteParams(..)
  , OpenParams(..)
  , OpenatParams(..)
  , StatxParams(..)
  , AcceptParams(..)
  , ConnectParams(..)
  , SendParams(..)
  , RecvParams(..)
  , SocketParams(..)
  , ShutdownParams(..)
  , PollAddParams(..)
  , TimeoutParams(..)
  , CancelParams(..)
  , UnlinkParams(..)
  , UnlinkatParams(..)
  , MkdirParams(..)
  , MkdiratParams(..)
  , RenameParams(..)
  , RenameatParams(..)
  , SymlinkParams(..)
  , SymlinkatParams(..)
  , LinkParams(..)
  , LinkatParams(..)
  , OperationParams(..)
    -- * Builders
  , makeNop
  , makeOpen
  , makeOpenat
  , makeClose
  , makeRead
  , makeWrite
  , makeFsync
  , makeFdatasync
  , makeStatx
  , makeMkdir
  , makeMkdirat
  , makeUnlink
  , makeUnlinkat
  , makeRmdir
  , makeRename
  , makeRenameat
  , makeSymlink
  , makeSymlinkat
  , makeLink
  , makeLinkat
  , makeSocket
  , makeConnect
  , makeAccept
  , makeSend
  , makeRecv
  , makeShutdown
  , makePollAdd
  , makeTimeout
  , makeCancel
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import System.Posix.Types (Fd, FileMode)

import Evring.Handle (Handle, invalidHandle)

-- ============================================================================
-- Events (completions from kernel)
-- ============================================================================

-- | An event: what happened (completion from the kernel).
data Event = Event
  { eventHandle   :: !Handle
    -- ^ Resource this event pertains to
  , eventType     :: !OperationType
    -- ^ What operation completed
  , eventResult   :: !Int64
    -- ^ Bytes transferred, fd for open/accept, or -errno
  , eventData     :: !ByteString
    -- ^ For reads: the data that was read
  , eventUserData :: !Word64
    -- ^ User-provided context for correlation
  } deriving stock (Eq, Show, Generic)

-- | Empty event, used to trigger initial operations in replay.
emptyEvent :: Event
emptyEvent = Event
  { eventHandle   = invalidHandle
  , eventType     = Nop
  , eventResult   = 0
  , eventData     = mempty
  , eventUserData = 0
  }

instance Semigroup Event where
  _ <> e = e

instance Monoid Event where
  mempty = emptyEvent

-- ============================================================================
-- Operation Types
-- ============================================================================

-- | Operation type enum (matches C++ evring::operation_type).
data OperationType
  = Nop
  -- File operations
  | Open
  | Openat
  | Close
  | Read
  | Write
  | Fsync
  | Fdatasync
  -- Metadata operations
  | Statx
  | Fstat
  -- Directory operations
  | Mkdir
  | Mkdirat
  | Rmdir
  | Unlink
  | Unlinkat
  | Rename
  | Renameat
  | Symlink
  | Symlinkat
  | Link
  | Linkat
  | Readlink
  -- Socket operations
  | Socket
  | Connect
  | Accept
  | Send
  | Recv
  | Shutdown
  | PollAdd
  -- Timing
  | Timeout
  | Cancel
  deriving stock (Eq, Show, Enum, Bounded, Generic)

-- ============================================================================
-- Operation Parameters
-- ============================================================================

data ReadParams = ReadParams
  { readBuffer :: !(Ptr Word8)
  , readLength :: !Word64
  , readOffset :: !Int64  -- -1 for current position
  } deriving stock (Eq, Show, Generic)

data WriteParams = WriteParams
  { writeBuffer :: !(Ptr Word8)
  , writeLength :: !Word64
  , writeOffset :: !Int64  -- -1 for current position
  } deriving stock (Eq, Show, Generic)

data OpenParams = OpenParams
  { openPath  :: !ByteString
  , openFlags :: !CInt
  , openMode  :: !FileMode
  } deriving stock (Eq, Show, Generic)

data OpenatParams = OpenatParams
  { openatDirFd :: !Fd
  , openatPath  :: !ByteString
  , openatFlags :: !CInt
  , openatMode  :: !FileMode
  } deriving stock (Eq, Show, Generic)

data StatxParams = StatxParams
  { statxDirFd  :: !Fd
  , statxPath   :: !ByteString
  , statxFlags  :: !CInt
  , statxMask   :: !Word32
  , statxBuffer :: !(Ptr ())
  } deriving stock (Eq, Show, Generic)

data AcceptParams = AcceptParams
  { acceptAddress    :: !(Ptr ())
  , acceptAddressLen :: !(Ptr Word32)
  , acceptFlags      :: !CInt
  } deriving stock (Eq, Show, Generic)

data ConnectParams = ConnectParams
  { connectAddress    :: !(Ptr ())
  , connectAddressLen :: !Word32
  } deriving stock (Eq, Show, Generic)

data SendParams = SendParams
  { sendBuffer :: !(Ptr Word8)
  , sendLength :: !Word64
  , sendFlags  :: !CInt
  } deriving stock (Eq, Show, Generic)

data RecvParams = RecvParams
  { recvBuffer :: !(Ptr Word8)
  , recvLength :: !Word64
  , recvFlags  :: !CInt
  } deriving stock (Eq, Show, Generic)

data SocketParams = SocketParams
  { socketDomain   :: !CInt
  , socketType     :: !CInt
  , socketProtocol :: !CInt
  , socketFlags    :: !CInt
  } deriving stock (Eq, Show, Generic)

data ShutdownParams = ShutdownParams
  { shutdownHow :: !CInt
  } deriving stock (Eq, Show, Generic)

data PollAddParams = PollAddParams
  { pollMask :: !Word32
  } deriving stock (Eq, Show, Generic)

data TimeoutParams = TimeoutParams
  { timeoutNanoseconds :: !Word64
  } deriving stock (Eq, Show, Generic)

data CancelParams = CancelParams
  { cancelTarget :: !Handle
  } deriving stock (Eq, Show, Generic)

data UnlinkParams = UnlinkParams
  { unlinkPath :: !ByteString
  } deriving stock (Eq, Show, Generic)

data UnlinkatParams = UnlinkatParams
  { unlinkatDirFd :: !Fd
  , unlinkatPath  :: !ByteString
  , unlinkatFlags :: !CInt
  } deriving stock (Eq, Show, Generic)

data MkdirParams = MkdirParams
  { mkdirPath :: !ByteString
  , mkdirMode :: !FileMode
  } deriving stock (Eq, Show, Generic)

data MkdiratParams = MkdiratParams
  { mkdiratDirFd :: !Fd
  , mkdiratPath  :: !ByteString
  , mkdiratMode  :: !FileMode
  } deriving stock (Eq, Show, Generic)

data RenameParams = RenameParams
  { renameOldPath :: !ByteString
  , renameNewPath :: !ByteString
  } deriving stock (Eq, Show, Generic)

data RenameatParams = RenameatParams
  { renameatOldDirFd :: !Fd
  , renameatOldPath  :: !ByteString
  , renameatNewDirFd :: !Fd
  , renameatNewPath  :: !ByteString
  , renameatFlags    :: !Word32
  } deriving stock (Eq, Show, Generic)

data SymlinkParams = SymlinkParams
  { symlinkTarget   :: !ByteString
  , symlinkLinkpath :: !ByteString
  } deriving stock (Eq, Show, Generic)

data SymlinkatParams = SymlinkatParams
  { symlinkatTarget   :: !ByteString
  , symlinkatDirFd    :: !Fd
  , symlinkatLinkpath :: !ByteString
  } deriving stock (Eq, Show, Generic)

data LinkParams = LinkParams
  { linkOldPath :: !ByteString
  , linkNewPath :: !ByteString
  } deriving stock (Eq, Show, Generic)

data LinkatParams = LinkatParams
  { linkatOldDirFd :: !Fd
  , linkatOldPath  :: !ByteString
  , linkatNewDirFd :: !Fd
  , linkatNewPath  :: !ByteString
  , linkatFlags    :: !CInt
  } deriving stock (Eq, Show, Generic)

-- ============================================================================
-- Operations (requests to kernel)
-- ============================================================================

-- | An operation: request to the kernel (what to do).
data Operation = Operation
  { opHandle   :: !Handle
    -- ^ Resource this operation pertains to
  , opType     :: !OperationType
    -- ^ What operation to perform
  , opUserData :: !Word64
    -- ^ User-provided context for correlation
  , opParams   :: !OperationParams
    -- ^ Operation-specific parameters
  } deriving stock (Eq, Show, Generic)

-- | Operation-specific parameters (sum type for type safety).
data OperationParams
  = NoParams
  | ParamsOpen    !OpenParams
  | ParamsOpenat  !OpenatParams
  | ParamsRead    !ReadParams
  | ParamsWrite   !WriteParams
  | ParamsStatx   !StatxParams
  | ParamsAccept  !AcceptParams
  | ParamsConnect !ConnectParams
  | ParamsSend    !SendParams
  | ParamsRecv    !RecvParams
  | ParamsSocket  !SocketParams
  | ParamsShutdown !ShutdownParams
  | ParamsPollAdd !PollAddParams
  | ParamsTimeout !TimeoutParams
  | ParamsCancel  !CancelParams
  | ParamsUnlink  !UnlinkParams
  | ParamsUnlinkat !UnlinkatParams
  | ParamsMkdir   !MkdirParams
  | ParamsMkdirat !MkdiratParams
  | ParamsRename  !RenameParams
  | ParamsRenameat !RenameatParams
  | ParamsSymlink !SymlinkParams
  | ParamsSymlinkat !SymlinkatParams
  | ParamsLink    !LinkParams
  | ParamsLinkat  !LinkatParams
  deriving stock (Eq, Show, Generic)

-- ============================================================================
-- Operation Builders
-- ============================================================================

makeNop :: Word64 -> Operation
makeNop userData = Operation
  { opHandle   = invalidHandle
  , opType     = Nop
  , opUserData = userData
  , opParams   = NoParams
  }

makeOpen :: ByteString -> CInt -> FileMode -> Word64 -> Operation
makeOpen path flags mode userData = Operation
  { opHandle   = invalidHandle
  , opType     = Open
  , opUserData = userData
  , opParams   = ParamsOpen (OpenParams path flags mode)
  }

makeOpenat :: Fd -> ByteString -> CInt -> FileMode -> Word64 -> Operation
makeOpenat dirFd path flags mode userData = Operation
  { opHandle   = invalidHandle
  , opType     = Openat
  , opUserData = userData
  , opParams   = ParamsOpenat (OpenatParams dirFd path flags mode)
  }

makeClose :: Handle -> Word64 -> Operation
makeClose h userData = Operation
  { opHandle   = h
  , opType     = Close
  , opUserData = userData
  , opParams   = NoParams
  }

makeRead :: Handle -> Ptr Word8 -> Word64 -> Int64 -> Word64 -> Operation
makeRead h buf len offset userData = Operation
  { opHandle   = h
  , opType     = Read
  , opUserData = userData
  , opParams   = ParamsRead (ReadParams buf len offset)
  }

makeWrite :: Handle -> Ptr Word8 -> Word64 -> Int64 -> Word64 -> Operation
makeWrite h buf len offset userData = Operation
  { opHandle   = h
  , opType     = Write
  , opUserData = userData
  , opParams   = ParamsWrite (WriteParams buf len offset)
  }

makeFsync :: Handle -> Word64 -> Operation
makeFsync h userData = Operation
  { opHandle   = h
  , opType     = Fsync
  , opUserData = userData
  , opParams   = NoParams
  }

makeFdatasync :: Handle -> Word64 -> Operation
makeFdatasync h userData = Operation
  { opHandle   = h
  , opType     = Fdatasync
  , opUserData = userData
  , opParams   = NoParams
  }

makeStatx :: Fd -> ByteString -> CInt -> Word32 -> Ptr () -> Word64 -> Operation
makeStatx dirFd path flags mask buf userData = Operation
  { opHandle   = invalidHandle
  , opType     = Statx
  , opUserData = userData
  , opParams   = ParamsStatx (StatxParams dirFd path flags mask buf)
  }

makeMkdir :: ByteString -> FileMode -> Word64 -> Operation
makeMkdir path mode userData = Operation
  { opHandle   = invalidHandle
  , opType     = Mkdir
  , opUserData = userData
  , opParams   = ParamsMkdir (MkdirParams path mode)
  }

makeMkdirat :: Fd -> ByteString -> FileMode -> Word64 -> Operation
makeMkdirat dirFd path mode userData = Operation
  { opHandle   = invalidHandle
  , opType     = Mkdirat
  , opUserData = userData
  , opParams   = ParamsMkdirat (MkdiratParams dirFd path mode)
  }

makeUnlink :: ByteString -> Word64 -> Operation
makeUnlink path userData = Operation
  { opHandle   = invalidHandle
  , opType     = Unlink
  , opUserData = userData
  , opParams   = ParamsUnlink (UnlinkParams path)
  }

makeUnlinkat :: Fd -> ByteString -> CInt -> Word64 -> Operation
makeUnlinkat dirFd path flags userData = Operation
  { opHandle   = invalidHandle
  , opType     = Unlinkat
  , opUserData = userData
  , opParams   = ParamsUnlinkat (UnlinkatParams dirFd path flags)
  }

makeRmdir :: ByteString -> Word64 -> Operation
makeRmdir path userData = Operation
  { opHandle   = invalidHandle
  , opType     = Rmdir
  , opUserData = userData
  , opParams   = ParamsUnlink (UnlinkParams path)
  }

makeRename :: ByteString -> ByteString -> Word64 -> Operation
makeRename oldPath newPath userData = Operation
  { opHandle   = invalidHandle
  , opType     = Rename
  , opUserData = userData
  , opParams   = ParamsRename (RenameParams oldPath newPath)
  }

makeRenameat :: Fd -> ByteString -> Fd -> ByteString -> Word32 -> Word64 -> Operation
makeRenameat oldDirFd oldPath newDirFd newPath flags userData = Operation
  { opHandle   = invalidHandle
  , opType     = Renameat
  , opUserData = userData
  , opParams   = ParamsRenameat (RenameatParams oldDirFd oldPath newDirFd newPath flags)
  }

makeSymlink :: ByteString -> ByteString -> Word64 -> Operation
makeSymlink target linkpath userData = Operation
  { opHandle   = invalidHandle
  , opType     = Symlink
  , opUserData = userData
  , opParams   = ParamsSymlink (SymlinkParams target linkpath)
  }

makeSymlinkat :: ByteString -> Fd -> ByteString -> Word64 -> Operation
makeSymlinkat target dirFd linkpath userData = Operation
  { opHandle   = invalidHandle
  , opType     = Symlinkat
  , opUserData = userData
  , opParams   = ParamsSymlinkat (SymlinkatParams target dirFd linkpath)
  }

makeLink :: ByteString -> ByteString -> Word64 -> Operation
makeLink oldPath newPath userData = Operation
  { opHandle   = invalidHandle
  , opType     = Link
  , opUserData = userData
  , opParams   = ParamsLink (LinkParams oldPath newPath)
  }

makeLinkat :: Fd -> ByteString -> Fd -> ByteString -> CInt -> Word64 -> Operation
makeLinkat oldDirFd oldPath newDirFd newPath flags userData = Operation
  { opHandle   = invalidHandle
  , opType     = Linkat
  , opUserData = userData
  , opParams   = ParamsLinkat (LinkatParams oldDirFd oldPath newDirFd newPath flags)
  }

makeSocket :: CInt -> CInt -> CInt -> CInt -> Word64 -> Operation
makeSocket domain typ protocol flags userData = Operation
  { opHandle   = invalidHandle
  , opType     = Socket
  , opUserData = userData
  , opParams   = ParamsSocket (SocketParams domain typ protocol flags)
  }

makeConnect :: Handle -> Ptr () -> Word32 -> Word64 -> Operation
makeConnect h addr addrLen userData = Operation
  { opHandle   = h
  , opType     = Connect
  , opUserData = userData
  , opParams   = ParamsConnect (ConnectParams addr addrLen)
  }

makeAccept :: Handle -> Ptr () -> Ptr Word32 -> CInt -> Word64 -> Operation
makeAccept h addr addrLen flags userData = Operation
  { opHandle   = h
  , opType     = Accept
  , opUserData = userData
  , opParams   = ParamsAccept (AcceptParams addr addrLen flags)
  }

makeSend :: Handle -> Ptr Word8 -> Word64 -> CInt -> Word64 -> Operation
makeSend h buf len flags userData = Operation
  { opHandle   = h
  , opType     = Send
  , opUserData = userData
  , opParams   = ParamsSend (SendParams buf len flags)
  }

makeRecv :: Handle -> Ptr Word8 -> Word64 -> CInt -> Word64 -> Operation
makeRecv h buf len flags userData = Operation
  { opHandle   = h
  , opType     = Recv
  , opUserData = userData
  , opParams   = ParamsRecv (RecvParams buf len flags)
  }

makeShutdown :: Handle -> CInt -> Word64 -> Operation
makeShutdown h how userData = Operation
  { opHandle   = h
  , opType     = Shutdown
  , opUserData = userData
  , opParams   = ParamsShutdown (ShutdownParams how)
  }

makePollAdd :: Handle -> Word32 -> Word64 -> Operation
makePollAdd h mask userData = Operation
  { opHandle   = h
  , opType     = PollAdd
  , opUserData = userData
  , opParams   = ParamsPollAdd (PollAddParams mask)
  }

makeTimeout :: Word64 -> Word64 -> Operation
makeTimeout nanos userData = Operation
  { opHandle   = invalidHandle
  , opType     = Timeout
  , opUserData = userData
  , opParams   = ParamsTimeout (TimeoutParams nanos)
  }

makeCancel :: Handle -> Word64 -> Operation
makeCancel target userData = Operation
  { opHandle   = invalidHandle
  , opType     = Cancel
  , opUserData = userData
  , opParams   = ParamsCancel (CancelParams target)
  }
