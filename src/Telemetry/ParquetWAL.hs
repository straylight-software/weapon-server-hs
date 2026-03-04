{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Telemetry.ParquetWAL
Description : Parquet-based write-ahead log for telemetry

Write-ahead log that writes telemetry events directly to Parquet files
using the Rust FFI. Provides the same interface as the JSONL WAL but
with columnar compression.

== File Layout

@
\$XDG_DATA_HOME\/weapon\/wal\/
  {session_id}\/
    0000000000.parquet   -- Events 0-9999
    0000000001.parquet   -- Events 10000-19999
    ...
    seq                  -- Current sequence number
    hwm                  -- High water mark (last replicated seq)
@

@since 0.1.0
-}
module Telemetry.ParquetWAL (
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
import Control.Exception (Exception, IOException, catch)
import Control.Monad (when)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Log qualified
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Telemetry.Parquet qualified as Parquet
import Telemetry.Types (TelemetryEvent (..))
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | WAL configuration
data WALConfig = WALConfig
    { walBaseDir :: FilePath
    -- ^ Base directory for WAL files
    , walSegmentSize :: Int
    -- ^ Number of events per segment file (row group size)
    , walSyncOnWrite :: Bool
    -- ^ Whether to flush after every write (safest but slower)
    , walLogger :: Log.Logger
    -- ^ Logger for error reporting
    }

-- | Default configuration
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
    | WALParquetError Text
    deriving (Show, Eq)

instance Exception WALError

-- | Internal state for an open segment
data SegmentState = SegmentState
    { ssWriter :: Parquet.ParquetWriter
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

    -- Read or initialize sequence number
    let seqFile = sessionDir </> "seq"
    seqExists <- doesFileExist seqFile
    initialSeq <-
        if seqExists
            then do
                content <- TIO.readFile seqFile
                pure $ maybe 0 id (readMaybe (T.unpack content))
            else pure 0

    -- Read high water mark
    let hwmFile = sessionDir </> "hwm"
    hwmExists <- doesFileExist hwmFile
    hwm <-
        if hwmExists
            then do
                content <- TIO.readFile hwmFile
                pure $ maybe 0 id (readMaybe (T.unpack content))
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
            , whLogger = walLogger config
            }

-- | Close a WAL handle, flushing any buffered data
closeWAL :: WALHandle -> IO ()
closeWAL wh = do
    mSeg <- takeMVar (whCurrentSegment wh)
    case mSeg of
        Nothing -> putMVar (whCurrentSegment wh) Nothing
        Just seg -> do
            Parquet.close (ssWriter seg)
            putMVar (whCurrentSegment wh) Nothing

    -- Persist sequence number
    seq' <- readTVarIO (whSequence wh)
    let seqFile = whSessionDir wh </> "seq"
    TIO.writeFile seqFile (T.pack (show seq'))

-- | Append an event to the WAL
appendEvent :: WALHandle -> TelemetryEvent -> IO Word64
appendEvent wh event = do
    seq' <- atomically $ do
        s <- readTVar (whSequence wh)
        writeTVar (whSequence wh) (s + 1)
        pure s

    let eventWithSeq = event{teSeq = seq'}

    modifyMVar_ (whCurrentSegment wh) $ \mSeg -> do
        seg <- ensureSegment wh mSeg seq'
        
        -- Append to parquet
        Parquet.appendEvent (ssWriter seg) eventWithSeq

        let newCount = ssEventCount seg + 1
        if newCount >= walSegmentSize (whConfig wh)
            then do
                -- Rotate segment - close current, will open new on next write
                Parquet.close (ssWriter seg)
                pure Nothing
            else pure $ Just seg{ssEventCount = newCount}

    pure seq'

-- | Append an event with immediate flush
appendEventSync :: WALHandle -> TelemetryEvent -> IO Word64
appendEventSync wh event = do
    seq' <- appendEvent wh event
    when (walSyncOnWrite (whConfig wh)) $ do
        withMVar (whCurrentSegment wh) $ \mSeg ->
            case mSeg of
                Just seg -> Parquet.flush (ssWriter seg)
                Nothing -> pure ()
    pure seq'

-- | Get the high water mark (last replicated sequence number)
getHighWaterMark :: WALHandle -> IO Word64
getHighWaterMark = readIORef . whHighWaterMark

-- | List segment files that haven't been fully replicated
listUnreplicatedSegments :: WALHandle -> IO [(FilePath, Word64, Word64)]
listUnreplicatedSegments wh = do
    hwm <- getHighWaterMark wh
    files <- listDirectory (whSessionDir wh) `catch` handleListError
    let segments = filter isSegmentFile files
        segmentInfos = map parseSegmentFile segments
        unreplicated = filter (\(_, start, _) -> start >= hwm) segmentInfos
    pure $ map (\(name, start, end) -> (whSessionDir wh </> name, start, end)) unreplicated
  where
    handleListError :: IOException -> IO [FilePath]
    handleListError e
        | isDoesNotExistError e = pure []
        | otherwise = do
            Log.logError (whLogger wh) ("Failed to list WAL directory: " <> T.pack (show e)) ()
            pure []

    isSegmentFile f = 
        length f > 8 && 
        take 10 f == replicate 10 '0' || all (`elem` ['0' .. '9']) (take 10 f)

    parseSegmentFile :: FilePath -> (FilePath, Word64, Word64)
    parseSegmentFile name =
        let segNum = maybe 0 id (readMaybe (takeWhile (/= '.') name))
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
            Parquet.close (ssWriter seg)
            openSegmentForSeq wh seq'

-- | Open a new segment file for the given sequence number
openSegmentForSeq :: WALHandle -> Word64 -> IO SegmentState
openSegmentForSeq wh seq' = do
    let segSize = fromIntegral $ walSegmentSize (whConfig wh)
        segNum = seq' `div` segSize
        segName = printf "%010d.parquet" segNum
        segPath = whSessionDir wh </> segName

    writer <- Parquet.newWriter segPath (walSegmentSize (whConfig wh))

    pure $
        SegmentState
            { ssWriter = writer
            , ssPath = segPath
            , ssSegmentNum = segNum
            , ssEventCount = 0
            }
