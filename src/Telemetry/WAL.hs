{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Telemetry.WAL
Description : Write-ahead log for durable event capture

Append-only write-ahead log that ensures zero data loss. Every event
is written to disk with fsync before being acknowledged.

== Architecture

@
Events → WAL Writer → Local Files → Replication Worker → R2
                ↓
         fsync (durable)
@

== File Layout

@
\$XDG_DATA_HOME\/weapon\/wal\/
  {session_id}\/
    meta.json           -- Session metadata
    0000000000.jsonl    -- Events 0-9999
    0000000001.jsonl    -- Events 10000-19999
    ...
    hwm                 -- High water mark (last replicated seq)
@

@since 0.1.0
-}
module Telemetry.WAL (
    -- * Types
    WALConfig (..),
    WALHandle,
    WALError (..),

    -- * Operations
    openWAL,
    closeWAL,
    appendEvent,
    appendEventSync,

    -- * Querying
    getHighWaterMark,
    listUnreplicatedSegments,

    -- * Configuration
    defaultWALConfig,
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception)
import Control.Monad (unless, when)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Log qualified
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO (BufferMode (..), Handle, IOMode (..), hClose, hFlush, hSetBuffering, openFile)
import System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (Fd)
import System.Posix.Unistd (fileSynchronise)
import Telemetry.Types (TelemetryEvent (..), eventToJSONL)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | WAL configuration
data WALConfig = WALConfig
    { walBaseDir :: FilePath
    -- ^ Base directory for WAL files
    , walSegmentSize :: Int
    -- ^ Number of events per segment file
    , walSyncOnWrite :: Bool
    -- ^ Whether to fsync after every write (safest but slower)
    , walLogger :: Log.Logger
    -- ^ Logger for error reporting
    }

-- | Default configuration (sync on every write for durability)
defaultWALConfig :: FilePath -> Log.Logger -> WALConfig
defaultWALConfig baseDir logger =
    WALConfig
        { walBaseDir = baseDir
        , walSegmentSize = 10000
        , walSyncOnWrite = True
        , walLogger = Log.withNS logger "wal"
        }

-- | Errors that can occur during WAL operations
data WALError
    = WALDirectoryError FilePath String
    | WALWriteError FilePath String
    | WALReadError FilePath String
    deriving (Show, Eq)

instance Exception WALError

-- | Internal state for an open WAL segment
data SegmentState = SegmentState
    { ssHandle :: Handle
    , ssFd :: Fd
    -- ^ File descriptor for fsync (same file as ssHandle)
    , ssPath :: FilePath
    , ssSegmentNum :: Word64
    , ssEventCount :: Int
    }

-- | Handle to an open WAL for a session
data WALHandle = WALHandle
    { whConfig :: WALConfig
    , whSessionId :: Text
    , whSessionDir :: FilePath
    , whSequence :: TVar Word64
    -- ^ Next sequence number
    , whCurrentSegment :: MVar (Maybe SegmentState)
    -- ^ Currently open segment
    , whHighWaterMark :: IORef Word64
    -- ^ Last replicated sequence
    , whLogger :: Log.Logger
    -- ^ Logger for error reporting
    }

-- | Open or create a WAL for a session
openWAL :: WALConfig -> Text -> IO WALHandle
openWAL config sessionId = do
    let sessionDir = walBaseDir config </> T.unpack sessionId

    -- Create session directory
    createDirectoryIfMissing True sessionDir

    let logger = walLogger config

    -- Read or initialize sequence number
    let seqFile = sessionDir </> "seq"
    seqExists <- doesFileExist seqFile
    initialSeq <-
        if seqExists
            then do
                content <- TIO.readFile seqFile
                case readMaybe (T.unpack content) of
                    Just n -> pure n
                    Nothing -> do
                        unless (T.null (T.strip content)) $
                            Log.logWarn logger ("Corrupted sequence file " <> T.pack seqFile <> ", content: " <> T.take 50 content <> " - resetting to 0") ()
                        pure 0
            else pure 0

    -- Read high water mark
    let hwmFile = sessionDir </> "hwm"
    hwmExists <- doesFileExist hwmFile
    hwm <-
        if hwmExists
            then do
                content <- TIO.readFile hwmFile
                case readMaybe (T.unpack content) of
                    Just n -> pure n
                    Nothing -> do
                        unless (T.null (T.strip content)) $
                            Log.logWarn logger ("Corrupted high water mark file " <> T.pack hwmFile <> ", content: " <> T.take 50 content <> " - resetting to 0") ()
                        pure 0
            else pure 0

    seqVar <- newTVarIO initialSeq
    segmentVar <- newMVar Nothing
    hwmRef <- newIORef hwm

    pure $
        WALHandle
            { whConfig = config
            , whSessionId = sessionId
            , whSessionDir = sessionDir
            , whSequence = seqVar
            , whCurrentSegment = segmentVar
            , whHighWaterMark = hwmRef
            , whLogger = logger
            }

-- | Close a WAL handle, flushing any buffered data
closeWAL :: WALHandle -> IO ()
closeWAL wh = do
    mSeg <- takeMVar (whCurrentSegment wh)
    case mSeg of
        Nothing -> putMVar (whCurrentSegment wh) Nothing
        Just seg -> do
            hFlush (ssHandle seg)
            fileSynchronise (ssFd seg)
            hClose (ssHandle seg)
            closeFd (ssFd seg)
            putMVar (whCurrentSegment wh) Nothing

    -- Persist sequence number
    seq' <- readTVarIO (whSequence wh)
    let seqFile = whSessionDir wh </> "seq"
    TIO.writeFile seqFile (T.pack (show seq'))

-- | Append an event to the WAL (async, batched fsync)
appendEvent :: WALHandle -> TelemetryEvent -> IO Word64
appendEvent wh event = do
    seq' <- atomically $ do
        s <- readTVar (whSequence wh)
        writeTVar (whSequence wh) (s + 1)
        pure s

    let eventWithSeq = event{teSeq = seq'}

    modifyMVar_ (whCurrentSegment wh) $ \mSeg -> do
        seg <- ensureSegment wh mSeg seq'
        LBS.hPut (ssHandle seg) (eventToJSONL eventWithSeq)

        let newCount = ssEventCount seg + 1
        if newCount >= walSegmentSize (whConfig wh)
            then do
                -- Rotate segment
                hFlush (ssHandle seg)
                fileSynchronise (ssFd seg)
                hClose (ssHandle seg)
                closeFd (ssFd seg)
                pure Nothing
            else pure $ Just seg{ssEventCount = newCount}

    pure seq'

-- | Append an event with immediate fsync (slower but guaranteed durable)
appendEventSync :: WALHandle -> TelemetryEvent -> IO Word64
appendEventSync wh event = do
    seq' <- appendEvent wh event
    when (walSyncOnWrite (whConfig wh)) $
        withMVar (whCurrentSegment wh) $ \case
            Just seg -> do
                hFlush (ssHandle seg)
                fileSynchronise (ssFd seg)
            Nothing -> pure ()
    pure seq'

-- | Get the high water mark (last replicated sequence number)
getHighWaterMark :: WALHandle -> IO Word64
getHighWaterMark = readIORef . whHighWaterMark

-- | List segment files that haven't been fully replicated
listUnreplicatedSegments :: WALHandle -> IO [(FilePath, Word64, Word64)]
listUnreplicatedSegments wh = do
    hwm <- getHighWaterMark wh
    files <- listDirectory (whSessionDir wh)
    let segments = filter isSegmentFile files
        segmentInfos = map parseSegmentFile segments
        unreplicated = filter (\(_, start, _) -> start >= hwm) segmentInfos
    pure $ map (\(name, start, end) -> (whSessionDir wh </> name, start, end)) unreplicated
  where
    isSegmentFile f = take 10 f == replicate 10 '0' || all (`elem` ['0' .. '9']) (take 10 f)

    parseSegmentFile :: FilePath -> (FilePath, Word64, Word64)
    parseSegmentFile name =
        let segNum = fromMaybe 0 (readMaybe (takeWhile (/= '.') name))
            segSize = fromIntegral $ walSegmentSize (whConfig wh)
            start = segNum * segSize
            end = start + segSize - 1
         in (name, start, end)

-- | Ensure we have an open segment for the given sequence number
ensureSegment :: WALHandle -> Maybe SegmentState -> Word64 -> IO SegmentState
ensureSegment wh Nothing seq' = openSegmentForSeq wh seq'
ensureSegment wh (Just seg) seq' = do
    let segSize = fromIntegral $ walSegmentSize (whConfig wh)
        expectedSegNum = seq' `div` segSize
    if ssSegmentNum seg == expectedSegNum
        then pure seg
        else do
            -- Need to rotate
            hFlush (ssHandle seg)
            fileSynchronise (ssFd seg)
            hClose (ssHandle seg)
            closeFd (ssFd seg)
            openSegmentForSeq wh seq'

-- | Open a new segment file for the given sequence number
openSegmentForSeq :: WALHandle -> Word64 -> IO SegmentState
openSegmentForSeq wh seq' = do
    let segSize = fromIntegral $ walSegmentSize (whConfig wh)
        segNum = seq' `div` segSize
        segName = printf "%010d.jsonl" segNum
        segPath = whSessionDir wh </> segName

    -- Open Handle for buffered writes
    h <- openFile segPath AppendMode
    hSetBuffering h (BlockBuffering (Just 65536))

    -- Open separate Fd for fsync (same file, won't be closed by Handle)
    fd <- openFd segPath WriteOnly defaultFileFlags

    pure $
        SegmentState
            { ssHandle = h
            , ssFd = fd
            , ssPath = segPath
            , ssSegmentNum = segNum
            , ssEventCount = 0
            }
