{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Evring.Wai.Conn
Description : HTTP connection handler as continuation chain
Stability   : experimental

This module implements HTTP connection handling using continuation-passing
style. Each connection is a chain of continuations that handle recv, send,
and close operations.

= Zero-Allocation Optimizations

* Recv buffers from pool (see 'Evring.Wai.Pool')
* Pre-computed response bytes where possible
* Minimal ByteString allocations in steady state

= Architecture

@
Accept → Recv (recvCont) → Parse → App → Serialize → Send (sendCont) → Close
               ↑                                            │
               └────────────── Keep-Alive ──────────────────┘
@
-}
module Evring.Wai.Conn (
    -- * Connection Context
    ConnContext (..),
    newConnContext,

    -- * Connection Handling
    startConnection,
) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BC

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Unsafe qualified as BSU
import Data.CaseInsensitive qualified as CI
import Data.IORef
import Data.Primitive (mutablePrimArrayContents, newPinnedPrimArray)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import Data.Vault.Lazy qualified as Vault
import Data.Word (Word8)
import Foreign (Ptr, castPtr, copyBytes, peekArray)
import Log qualified
import Network.HTTP.Types
import Network.Socket (SockAddr (..))
import Network.Wai
import Network.Wai.Internal (Response (..), ResponseReceived (..))
import System.Posix.Types (Fd (..))

import Evring.Wai.Internal (formatHeader, stripCR)

import Data.Maybe (fromMaybe)
import Evring.Wai.Loop
import Evring.Wai.Pool
import Text.Read (readMaybe)

-- | Buffer size for recv (16KB for better HTTP request capture)
bufferSize :: Int
bufferSize = 16384

-- | Connection context - shared resources
data ConnContext = ConnContext
    { ctxRecvPool :: !BufferPool
    , ctxSendPool :: !BufferPool
    , ctxLogger :: !Log.Logger
    }

-- | Create connection context with pools
newConnContext :: Log.Logger -> Int -> IO ConnContext
newConnContext logger maxConns = do
    recvPool <- newBufferPool maxConns bufferSize
    sendPool <- newBufferPool maxConns (256 * 1024) -- 256KB for JSON responses
    pure $ ConnContext recvPool sendPool (Log.withNS logger "conn")

-- | Start handling a new connection
startConnection :: ConnContext -> Loop -> Fd -> SockAddr -> Application -> IO ()
startConnection ctx@ConnContext{..} loop clientFd clientAddr app = do
    -- Get recv buffer from pool
    recvBuf <- acquireBuffer ctxRecvPool

    -- State for accumulating partial requests
    leftoverRef <- newIORef BS.empty

    -- Submit first recv
    ioRecv
        loop
        clientFd
        (bufArray recvBuf)
        bufferSize
        (recvCont ctx loop clientFd clientAddr app recvBuf leftoverRef)

{- | Continuation after recv completes.

Handles three cases:

1. __Failure\/EOF__: Release buffers and close connection
2. __Incomplete request__: Accumulate data and recv more
3. __Complete request__: Parse, run WAI app, send response

For streaming responses (SSE), hands off to 'handleStreamingResponse'.
For WebSocket (ResponseRaw), forks to 'handleRawResponse'.
-}
recvCont ::
    ConnContext ->
    Loop ->
    Fd ->
    SockAddr ->
    Application ->
    Buffer ->
    IORef ByteString ->
    Cont
recvCont ctx@ConnContext{..} loop clientFd clientAddr app recvBuf leftoverRef = Cont $ \case
    Failure _errno -> do
        -- Recv failed, close connection
        releaseBuffer ctxRecvPool recvBuf
        ioClose loop clientFd closeCont
        pure Nothing
    Success 0 -> do
        -- EOF, client closed
        releaseBuffer ctxRecvPool recvBuf
        ioClose loop clientFd closeCont
        pure Nothing
    Success bytesRead -> do
        -- Read data from buffer
        let ptr = bufPtr recvBuf
            len = fromIntegral bytesRead
        -- Create ByteString - uses memcpy but very fast for small HTTP requests
        newData <- BSU.unsafePackCStringFinalizer ptr len (pure ())

        -- Combine with leftover
        leftover <- readIORef leftoverRef
        let !allData = if BS.null leftover then newData else leftover <> newData

        -- Try to parse HTTP request
        case tryParseRequest allData of
            Nothing -> do
                -- Incomplete, save and recv more
                writeIORef leftoverRef allData
                ioRecv
                    loop
                    clientFd
                    (bufArray recvBuf)
                    bufferSize
                    (recvCont ctx loop clientFd clientAddr app recvBuf leftoverRef)
                pure Nothing
            Just (req, newLeftover) -> do
                writeIORef leftoverRef newLeftover

                -- Run WAI application (create body ref from parsed body)
                bodyRef <- newIORef (prBody req)
                response <- runApp ctxLogger app (buildRequest req clientAddr bodyRef)

                let handleNormalResponse = do
                        -- Check if this is a streaming response (SSE, chunked, etc.)
                        let (status, headers, withBody) = responseToStream response
                            isStreaming = isStreamingResponse headers

                        if isStreaming
                            then do
                                -- Streaming response: send headers, then stream chunks via CPS
                                releaseBuffer ctxRecvPool recvBuf
                                handleStreamingResponse ctx loop clientFd status headers withBody
                                pure Nothing
                            else do
                                -- Normal response handling
                                -- Get send buffer from pool
                                sendBuf <- acquireBuffer ctxSendPool

                                -- Serialize response directly into send buffer
                                respLen <- serializeResponseTo response (bufPtr sendBuf) (bufSize sendBuf)

                                -- Determine if keep-alive
                                let keepAlive = checkKeepAlive req

                                -- Submit send
                                ioSend
                                    loop
                                    clientFd
                                    (bufPtr sendBuf)
                                    respLen
                                    (sendCont ctx loop clientFd clientAddr app recvBuf leftoverRef sendBuf keepAlive)
                                pure Nothing

                -- Check for ResponseRaw (WebSocket, etc.)
                case response of
                    ResponseRaw rawApp _fallback -> do
                        -- Hand off to raw handler in separate thread
                        -- This allows the main loop to continue while WebSocket runs
                        releaseBuffer ctxRecvPool recvBuf -- We'll allocate fresh buffers in thread
                        _ <- forkIO $ handleRawResponse ctx loop clientFd rawApp newLeftover
                        pure Nothing
                    -- Normal HTTP responses (File, Builder, Stream)
                    ResponseFile{} -> handleNormalResponse
                    ResponseBuilder{} -> handleNormalResponse
                    ResponseStream{} -> handleNormalResponse

{- | Continuation after send completes.

On success:

* If keep-alive: submit another recv for the next request
* Otherwise: close the connection

On failure: release buffers and close.
-}
sendCont ::
    ConnContext ->
    Loop ->
    Fd ->
    SockAddr ->
    Application ->
    Buffer ->
    IORef ByteString ->
    Buffer ->
    Bool ->
    Cont
sendCont ctx@ConnContext{..} loop clientFd clientAddr app recvBuf leftoverRef sendBuf keepAlive = Cont $ \case
    Failure _errno -> do
        releaseBuffer ctxSendPool sendBuf
        releaseBuffer ctxRecvPool recvBuf
        ioClose loop clientFd closeCont
        pure Nothing
    Success _bytesSent -> do
        releaseBuffer ctxSendPool sendBuf

        if keepAlive
            then do
                -- Keep-alive: recv next request
                ioRecv
                    loop
                    clientFd
                    (bufArray recvBuf)
                    bufferSize
                    (recvCont ctx loop clientFd clientAddr app recvBuf leftoverRef)
                pure Nothing
            else do
                -- Close connection
                releaseBuffer ctxRecvPool recvBuf
                ioClose loop clientFd closeCont
                pure Nothing

-- | Continuation after close completes
closeCont :: Cont
closeCont = Cont $ \_ -> pure Nothing

-- | Parsed request (minimal)
data ParsedReq = ParsedReq
    { prMethod :: !ByteString
    , prPath :: !ByteString
    , prHeaders :: ![(ByteString, ByteString)]
    , prBody :: !ByteString
    }

-- | Try to parse an HTTP request from buffer
{-# INLINE tryParseRequest #-}
tryParseRequest :: ByteString -> Maybe (ParsedReq, ByteString)
tryParseRequest bs = do
    -- Find end of headers
    let (before, after) = BS.breakSubstring "\r\n\r\n" bs
    if BS.null after
        then Nothing
        else do
            let headerSection = before
                bodyStart = BS.drop 4 after

            case BC.lines headerSection of
                [] -> Nothing
                (reqLine : headerLines) -> do
                    let parts = BC.words (stripCR reqLine)
                    case parts of
                        (method : path : _httpVersion) -> do
                            let headers = parseHeaders headerLines
                                contentLength = getContentLength headers
                                (body, leftover) = BS.splitAt contentLength bodyStart

                            if BS.length body < contentLength
                                then Nothing
                                else Just (ParsedReq method path headers body, leftover)
                        [] -> Nothing
                        [_methodOnly] -> Nothing

{- | Parse headers into raw (ByteString, ByteString) pairs.

This is a simplified version that doesn't use CaseInsensitive keys,
for use with the ParsedReq type. Uses 'stripCR' from Internal.
-}
{-# INLINE parseHeaders #-}
parseHeaders :: [ByteString] -> [(ByteString, ByteString)]
parseHeaders = foldr go []
  where
    go line acc = case BC.break (== ':') (stripCR line) of
        (name, rest)
            | not (BS.null rest) ->
                let value = BS.dropWhile (== 32) (BS.drop 1 rest)
                 in (name, value) : acc
        (_nameWithoutColon, _emptyRest) -> acc -- Malformed header, skip

{- | Get Content-Length from raw header pairs.

This version works with @[(ByteString, ByteString)]@ headers
(as opposed to 'Evring.Wai.Internal.getContentLength' which uses RequestHeaders).
-}
{-# INLINE getContentLength #-}
getContentLength :: [(ByteString, ByteString)] -> Int
getContentLength headers =
    case lookup "Content-Length" headers of
        Just val -> fromMaybe 0 (readMaybe (BC.unpack val))
        Nothing -> 0

-- Note: stripCR is imported from Evring.Wai.Internal

{-# INLINE checkKeepAlive #-}
checkKeepAlive :: ParsedReq -> Bool
checkKeepAlive ParsedReq{..} =
    case lookup "Connection" prHeaders of
        Just val | CI.mk val == "close" -> False
        Just _otherValue -> True -- keep-alive or unknown, default to keep-alive
        Nothing -> True -- HTTP/1.1 default is keep-alive

-- | Build WAI Request from parsed request
{-# INLINE buildRequest #-}
buildRequest :: ParsedReq -> SockAddr -> IORef ByteString -> Request
buildRequest ParsedReq{..} clientAddr bodyRef =
    setRequestBodyChunks bodyChunks $
        defaultRequest
            { requestMethod = prMethod
            , rawPathInfo = path
            , rawQueryString = query
            , pathInfo = decodePathSegments path -- Servant uses pathInfo for routing
            , queryString = parseQuery query -- Servant uses queryString for QueryParam
            , requestHeaders = map (first CI.mk) prHeaders
            , isSecure = False
            , remoteHost = clientAddr
            , vault = Vault.empty
            , requestHeaderHost = lookup "Host" prHeaders
            , requestBodyLength = KnownLength (fromIntegral (BS.length prBody))
            }
  where
    (path, query) = BC.break (== '?') prPath
    bodyChunks = do
        body <- readIORef bodyRef
        writeIORef bodyRef BS.empty -- WAI expects chunked reads, return empty after first
        pure body

-- | Run WAI app safely, logging exceptions
{-# INLINE runApp #-}
runApp :: Log.Logger -> Application -> Request -> IO Response
runApp lg app req = do
    responseRef <- newIORef Nothing
    let respond resp = do
            writeIORef responseRef (Just resp)
            pure ResponseReceived
    catch
        (void $ app req respond)
        (\(e :: SomeException) -> do
            -- Log the exception - don't silently swallow
            let path = T.decodeUtf8 (rawPathInfo req)
            Log.logError lg ("Request handler exception on " <> path <> ": " <> T.pack (show e)) ()
        )
    fromMaybe (responseLBS status500 [] "Internal Server Error") <$> readIORef responseRef

-- | Serialize a response directly to a buffer, returns bytes written
{-# INLINE serializeResponseTo #-}
serializeResponseTo :: Response -> Ptr Word8 -> Int -> IO Int
serializeResponseTo resp bufPtr maxLen = do
    let (status, headers, withBody) = responseToStream resp

    -- Collect body
    bodyRef <- newIORef mempty
    withBody $ \streamingBody ->
        streamingBody
            (\chunk -> modifyIORef' bodyRef (<> chunk))
            (pure ())

    bodyBuilder <- readIORef bodyRef
    let body = Builder.toLazyByteString bodyBuilder
        bodyLen = LBS.length body

    -- Build response directly
    let !respBuilder =
            mconcat
                [ Builder.byteString "HTTP/1.1 "
                , Builder.intDec (statusCode status)
                , Builder.char8 ' '
                , Builder.byteString (statusMessage status)
                , Builder.byteString "\r\n"
                , mconcat [formatHeader h | h <- headers]
                , Builder.byteString "Content-Length: "
                , Builder.int64Dec bodyLen
                , Builder.byteString "\r\nConnection: keep-alive\r\n\r\n"
                , Builder.lazyByteString body
                ]
        !respBytes = LBS.toStrict $ Builder.toLazyByteString respBuilder
        !len = BS.length respBytes

    if len > maxLen
        then error "Response too large for buffer"
        else do
            BSU.unsafeUseAsCStringLen respBytes $ \(src, srcLen) ->
                copyBytes bufPtr (castPtr src) srcLen
            pure len

-- Note: formatHeader is imported from Evring.Wai.Internal

-- ════════════════════════════════════════════════════════════════════════════
-- RESPONSERAW / WEBSOCKET SUPPORT
-- ════════════════════════════════════════════════════════════════════════════

{- | Handle a ResponseRaw (WebSocket, etc.)
Runs in a separate thread to avoid blocking the main CPS loop.
The raw handler receives blocking recv/send functions that use io_uring under the hood.
-}
handleRawResponse ::
    ConnContext ->
    Loop ->
    Fd ->
    (IO ByteString -> (ByteString -> IO ()) -> IO ()) ->
    ByteString -> -- leftover data from request parsing
    IO ()
handleRawResponse ctx loop clientFd rawApp leftoverData = do
    let lg = ctxLogger ctx
    -- Leftover buffer for data read but not consumed
    leftoverRef <- newIORef leftoverData

    -- Allocate a recv buffer for raw handler
    rawBuf <- newPinnedPrimArray rawBufferSize

    -- Create blocking recv action for the raw handler
    let recvAction :: IO ByteString
        recvAction = do
            -- First check leftover
            leftover <- readIORef leftoverRef
            if not (BS.null leftover)
                then do
                    writeIORef leftoverRef BS.empty
                    pure leftover
                else do
                    -- Blocking recv via io_uring
                    bytesRead <- ioRecvBlocking loop clientFd rawBuf rawBufferSize
                    if bytesRead <= 0
                        then pure BS.empty
                        else do
                            let ptr = mutablePrimArrayContents rawBuf
                            bytes <- peekArray bytesRead (castPtr ptr :: Ptr Word8)
                            pure $ BS.pack bytes

    -- Create blocking send action for the raw handler
    let sendAction :: ByteString -> IO ()
        sendAction bytes
            | BS.null bytes = pure ()
            | otherwise = do
                BSU.unsafeUseAsCStringLen bytes $ \(ptr, len) -> do
                    -- Simple send - if partial, we'd need a loop (WebSockets handles framing)
                    -- Partial sends indicate connection likely closing
                    void $ ioSendBlocking loop clientFd (castPtr ptr) len

    -- Run the raw handler
    catch
        (rawApp recvAction sendAction)
        ( \(e :: SomeException) ->
            Log.logError lg ("Raw handler error: " <> T.pack (show e)) ()
        )

    -- After raw handler completes, close the connection
    ioClose loop clientFd closeCont
  where
    rawBufferSize = 16384 -- 16KB for WebSocket frames

-- ════════════════════════════════════════════════════════════════════════════
-- STREAMING RESPONSE SUPPORT (SSE, chunked transfer)
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if response needs streaming (SSE, chunked)
{-# INLINE isStreamingResponse #-}
isStreamingResponse :: ResponseHeaders -> Bool
isStreamingResponse headers =
    case lookup "Content-Type" headers of
        Just ct | "text/event-stream" `BS.isInfixOf` ct -> True
        Just _otherContentType -> checkTransferEncoding
        Nothing -> checkTransferEncoding
  where
    checkTransferEncoding = case lookup "Transfer-Encoding" headers of
        Just te | "chunked" `BS.isInfixOf` te -> True
        Just _otherEncoding -> False
        Nothing -> False

{- | Handle a streaming response (SSE, etc.)
Runs streaming body in a separate thread that uses ioSendBlocking
to push chunks through the io_uring ring.
-}
handleStreamingResponse ::
    ConnContext ->
    Loop ->
    Fd ->
    Status ->
    ResponseHeaders ->
    ((StreamingBody -> IO ()) -> IO ()) ->
    IO ()
handleStreamingResponse ctx loop clientFd status headers withBody = do
    let lg = ctxLogger ctx
    -- First send HTTP headers with Transfer-Encoding: chunked
    let headerBytes = buildStreamingHeaders status headers

    -- Send headers using blocking send (from separate thread context)
    _ <- forkIO $ do
        -- Send headers
        BSU.unsafeUseAsCStringLen headerBytes $ \(ptr, len) -> do
            _ <- ioSendBlocking loop clientFd (castPtr ptr) len
            pure ()

        -- Create chunk sender that encodes as HTTP chunks
        let sendChunk :: Builder.Builder -> IO ()
            sendChunk builder = do
                let !chunkData = LBS.toStrict $ Builder.toLazyByteString builder
                if BS.null chunkData
                    then pure ()
                    else do
                        -- HTTP chunked format: size in hex, CRLF, data, CRLF
                        let !chunk =
                                LBS.toStrict $
                                    Builder.toLazyByteString $
                                        Builder.wordHex (fromIntegral (BS.length chunkData))
                                            <> Builder.byteString "\r\n"
                                            <> Builder.byteString chunkData
                                            <> Builder.byteString "\r\n"
                        BSU.unsafeUseAsCStringLen chunk $ \(ptr, len) -> do
                            _ <- ioSendBlocking loop clientFd (castPtr ptr) len
                            pure ()

            flushAction :: IO ()
            flushAction = pure () -- No-op for now, chunks are sent immediately

        -- Run the streaming body
        -- withBody :: (StreamingBody -> IO a) -> IO a
        -- StreamingBody :: (Builder -> IO ()) -> IO () -> IO ()
        catch
            (withBody $ \streamingBody -> streamingBody sendChunk flushAction)
            ( \(e :: SomeException) ->
                Log.logError lg ("Streaming response error: " <> T.pack (show e)) ()
            )

        -- Send final chunk (0\r\n\r\n)
        let finalChunk = "0\r\n\r\n" :: ByteString
        BSU.unsafeUseAsCStringLen finalChunk $ \(ptr, len) -> do
            _ <- ioSendBlocking loop clientFd (castPtr ptr) len
            pure ()

        -- Close connection (streaming responses don't keep-alive)
        ioClose loop clientFd closeCont

    pure ()

{- | Build HTTP headers for streaming response.

Uses 'formatHeader' from 'Evring.Wai.Internal'.
-}
{-# INLINE buildStreamingHeaders #-}
buildStreamingHeaders :: Status -> ResponseHeaders -> ByteString
buildStreamingHeaders status headers =
    let respBuilder =
            mconcat
                [ Builder.byteString "HTTP/1.1 "
                , Builder.intDec (statusCode status)
                , Builder.char8 ' '
                , Builder.byteString (statusMessage status)
                , Builder.byteString "\r\n"
                , mconcat [formatHeader h | h <- headers]
                , Builder.byteString "Transfer-Encoding: chunked\r\n"
                , Builder.byteString "Connection: close\r\n"
                , Builder.byteString "\r\n"
                ]
     in LBS.toStrict $ Builder.toLazyByteString respBuilder
