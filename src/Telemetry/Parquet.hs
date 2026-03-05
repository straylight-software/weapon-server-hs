{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Telemetry.Parquet
Description : Parquet file writing via Rust FFI

High-performance Parquet writing for telemetry events using arrow-rs
via C FFI. Events are buffered in row groups and compressed with ZSTD.

== Usage

@
import Telemetry.Parquet qualified as Parquet

main = do
    writer <- Parquet.newWriter "events.parquet" 10000
    Parquet.appendEvent writer event
    Parquet.close writer
@

@since 0.1.0
-}
module Telemetry.Parquet (
    -- * Types
    ParquetWriter,
    ParquetError (..),

    -- * Lifecycle
    newWriter,
    close,

    -- * Writing
    appendEvent,
    flush,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word32)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Telemetry.Types (EventMeta (..), TelemetryEvent (..))

-- | Opaque handle to a Parquet writer
newtype ParquetWriter = ParquetWriter (Ptr ())

-- | Parquet errors
newtype ParquetError = ParquetError Text
    deriving (Show, Eq)

instance Exception ParquetError

-- ═══════════════════════════════════════════════════════════════════════════
-- Foreign imports
-- ═══════════════════════════════════════════════════════════════════════════

foreign import ccall unsafe "parquet_writer_new"
    c_parquet_writer_new :: CString -> CSize -> Ptr CString -> IO (Ptr ())

foreign import ccall unsafe "parquet_writer_append"
    c_parquet_writer_append ::
        Ptr () ->
        -- | event_id
        CString ->
        -- | seq
        Int64 ->
        -- | timestamp_us
        Int64 ->
        -- | monotonic_ns
        Int64 ->
        -- | session_id
        CString ->
        -- | project_id
        CString ->
        -- | directory
        CString ->
        -- | event_type
        CString ->
        -- | payload
        CString ->
        -- | payload_len
        CSize ->
        -- | meta_model
        CString ->
        -- | meta_agent
        CString ->
        -- | meta_tokens_in
        CInt ->
        -- | meta_tokens_in_null
        CInt ->
        -- | meta_tokens_out
        CInt ->
        -- | meta_tokens_out_null
        CInt ->
        -- | meta_tool_name
        CString ->
        -- | meta_error
        CString ->
        IO CInt

foreign import ccall unsafe "parquet_writer_flush"
    c_parquet_writer_flush :: Ptr () -> Ptr CString -> IO CInt

foreign import ccall unsafe "parquet_writer_close"
    c_parquet_writer_close :: Ptr () -> Ptr CString -> IO CInt

foreign import ccall unsafe "parquet_free_error"
    c_parquet_free_error :: CString -> IO ()

-- ═══════════════════════════════════════════════════════════════════════════
-- Public API
-- ═══════════════════════════════════════════════════════════════════════════

-- | Create a new Parquet writer
newWriter ::
    -- | Output file path
    FilePath ->
    -- | Row group size
    Int ->
    IO ParquetWriter
newWriter path rowGroupSize = do
    alloca $ \errPtr -> do
        poke errPtr nullPtr
        withCString path $ \pathPtr -> do
            ptr <- c_parquet_writer_new pathPtr (fromIntegral rowGroupSize) errPtr
            if ptr == nullPtr
                then do
                    err <- peek errPtr
                    msg <- peekCString err
                    c_parquet_free_error err
                    throwIO $ ParquetError (T.pack msg)
                else pure $ ParquetWriter ptr

-- | Append a telemetry event
appendEvent :: ParquetWriter -> TelemetryEvent -> IO ()
appendEvent (ParquetWriter ptr) event = do
    let timestampUs = floor (utcTimeToPOSIXSeconds (teTimestamp event) * 1000000)
        payload = LBS.toStrict $ encode (tePayload event)
        meta = teMeta event

    withText (teId event) $ \eventIdPtr ->
        withText (teSessionId event) $ \sessionIdPtr ->
            withText (teProjectId event) $ \projectIdPtr ->
                withText (teDirectory event) $ \directoryPtr ->
                    withText (teType event) $ \eventTypePtr ->
                        unsafeUseAsCStringLen payload $ \(payloadPtr, payloadLen) ->
                            withMaybeText (emModel meta) $ \modelPtr ->
                                withMaybeText (emAgent meta) $ \agentPtr ->
                                    withMaybeText (emToolName meta) $ \toolPtr ->
                                        withMaybeText (emErrorMessage meta) $ \errorPtr -> do
                                            let (tokensIn, tokensInNull) = maybeWord32 (emTokensIn meta)
                                                (tokensOut, tokensOutNull) = maybeWord32 (emTokensOut meta)
                                            _ <-
                                                c_parquet_writer_append
                                                    ptr
                                                    eventIdPtr
                                                    (fromIntegral (teSeq event))
                                                    timestampUs
                                                    (fromIntegral (teMonotonicNs event))
                                                    sessionIdPtr
                                                    projectIdPtr
                                                    directoryPtr
                                                    eventTypePtr
                                                    payloadPtr
                                                    (fromIntegral payloadLen)
                                                    modelPtr
                                                    agentPtr
                                                    tokensIn
                                                    tokensInNull
                                                    tokensOut
                                                    tokensOutNull
                                                    toolPtr
                                                    errorPtr
                                            pure ()

-- | Flush buffered rows to disk
flush :: ParquetWriter -> IO ()
flush (ParquetWriter ptr) = do
    alloca $ \errPtr -> do
        poke errPtr nullPtr
        result <- c_parquet_writer_flush ptr errPtr
        when (result /= 0) $ do
            err <- peek errPtr
            msg <- peekCString err
            c_parquet_free_error err
            throwIO $ ParquetError (T.pack msg)

-- | Close the writer and finalize the file
close :: ParquetWriter -> IO ()
close (ParquetWriter ptr) = do
    alloca $ \errPtr -> do
        poke errPtr nullPtr
        result <- c_parquet_writer_close ptr errPtr
        when (result /= 0) $ do
            err <- peek errPtr
            msg <- peekCString err
            c_parquet_free_error err
            throwIO $ ParquetError (T.pack msg)

-- ═══════════════════════════════════════════════════════════════════════════
-- Helpers
-- ═══════════════════════════════════════════════════════════════════════════

withText :: Text -> (CString -> IO a) -> IO a
withText t = BS.useAsCString (TE.encodeUtf8 t)

withMaybeText :: Maybe Text -> (CString -> IO a) -> IO a
withMaybeText Nothing f = f nullPtr
withMaybeText (Just t) f = withText t f

maybeWord32 :: Maybe Word32 -> (CInt, CInt)
maybeWord32 Nothing = (0, 1)
maybeWord32 (Just v) = (fromIntegral v, 0)
