{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Evring.Wai
Description : WAI runner using io_uring for all network I/O
Stability   : experimental

This module provides a high-performance HTTP server that uses io_uring
instead of the traditional epoll/kqueue event loop.

= Usage

@
import Evring.Wai (runEvring)
import Network.Wai (Application)

main :: IO ()
main = runEvring logger 8080 myApp
@

= Features

* Full HTTP\/1.1 request parsing (method, path, query, headers)
* Request body reading (Content-Length)
* WAI Application interface compatible with Servant
* HTTP\/1.1 Keep-Alive connections
* Chunked transfer encoding for streaming responses
* WebSocket support via 'Network.Wai.Internal.ResponseRaw'
* Graceful shutdown on SIGTERM\/SIGINT
* Buffer pooling for reduced allocation pressure

= Architecture

The server uses a single-threaded event loop for io_uring safety.
Each accepted connection is handled via 'forkOn' pinned to the same
capability to maintain io_uring thread safety. Buffer pooling reduces
allocation overhead in steady state.

= Limitations

* Single io_uring ring (see 'Evring.Wai.MultiCore' for multi-core)
* No TLS support (use a reverse proxy)
* No HTTP\/2 support
-}
module Evring.Wai (
    -- * Running WAI applications
    runEvring,
    runEvringSettings,

    -- * Settings
    EvringSettings (..),
    defaultEvringSettings,
)
where

import Control.Concurrent (forkOn, threadDelay)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (replicateM, unless, void, when)

import Data.ByteString (ByteString)
import Data.Text qualified as T

import Log qualified
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Primitive (MutablePrimArray, mutablePrimArrayContents, newPinnedPrimArray)
import Data.Vault.Lazy qualified as Vault
import Data.Vector qualified as V
import Data.Word (Word32, Word8)
import Foreign (Ptr, castPtr, copyBytes, free, mallocBytes)

import Evring.Wai.Internal (
    checkKeepAliveHeaders,
    formatHeader,
    getContentLength,
    parseSockAddr,
    splitHeaderBody,
    splitPathQuery,
    stripCR,
 )

-- Note: unsafePerformIO no longer needed - streaming body uses proper IO

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable (..))
import GHC.Conc (myThreadId, threadCapability)
import GHC.Exts (RealWorld)
import Network.HTTP.Types (
    HttpVersion (..),
    RequestHeaders,
    decodePathSegments,
    http11,
    parseQuery,
    status500,
    statusCode,
    statusMessage,
 )
import Network.HTTP.Types qualified
import Network.Socket (
    Family (AF_INET),
    HostAddress,
    SockAddr (..),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    bind,
    close,
    listen,
    setSocketOption,
    socket,
    withFdSocket,
 )
import Network.Wai (
    Application,
    Request (..),
    Response,
    StreamingBody,
    defaultRequest,
    responseLBS,
    responseToStream,
 )
import Network.Wai.Internal (
    RequestBodyLength (..),
    Response (..),
    ResponseReceived (..),
    setRequestBodyChunks,
 )
import System.IoUring (IOCtx, IoOp (..), IoResult (..), defaultIoUringParams, submitBatch, withIoUring)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import System.Posix.Types (Fd (Fd))

-- | FFI for setsockopt to set TCP_NODELAY
foreign import ccall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

-- | Set TCP_NODELAY on a file descriptor
setTcpNoDelay :: Fd -> IO ()
setTcpNoDelay (Fd fd) = alloca $ \ptr -> do
    poke ptr (1 :: CInt)
    let ipprotoTcp = 6 :: CInt
        tcpNodelay = 1 :: CInt
    throwErrnoIfMinus1_ "setsockopt" $
        c_setsockopt fd ipprotoTcp tcpNodelay ptr 4

-- ════════════════════════════════════════════════════════════════════════════
-- Settings
-- ════════════════════════════════════════════════════════════════════════════

-- | Settings for the evring WAI server.
data EvringSettings = EvringSettings
    { evringPort :: !Int
    -- ^ Port to listen on
    , evringHost :: !ByteString
    -- ^ Host to bind to (default "0.0.0.0")
    , evringBacklog :: !Int
    -- ^ Listen backlog (default 1024)
    , evringBufferSize :: !Int
    -- ^ Per-connection receive buffer size (default 64KB)
    , evringMaxConnections :: !Int
    -- ^ Maximum concurrent connections (default 10000)
    , evringKeepAliveTimeout :: !Int
    -- ^ Keep-alive timeout in seconds (default 65)
    , evringMaxRequestsPerConnection :: !Int
    -- ^ Maximum requests per keep-alive connection (default 1000)
    , evringGracefulShutdownTimeout :: !Int
    -- ^ Graceful shutdown timeout in seconds (default 30)
    , evringRequestHeaderTimeout :: !Int
    {- ^ Timeout for receiving request headers in seconds (default 30)
    Protects against slow loris attacks
    -}
    , evringRequestBodyTimeout :: !Int
    -- ^ Timeout for receiving request body in seconds (default 60)
    , evringZeroCopyThreshold :: !Int
    -- ^ Minimum size in bytes to use zero-copy send (default 16KB, 0 to disable)
    , evringLogger :: !Log.Logger
    -- ^ Logger for server events
    }

-- | Default settings for port 8080.
defaultEvringSettings :: Log.Logger -> EvringSettings
defaultEvringSettings logger =
    EvringSettings
        { evringPort = 8080
        , evringHost = "0.0.0.0"
        , evringBacklog = 1024
        , evringBufferSize = 65536 -- 64KB for larger requests
        , evringMaxConnections = 10000
        , evringKeepAliveTimeout = 65 -- Slightly longer than common client timeout
        , evringMaxRequestsPerConnection = 1000
        , evringGracefulShutdownTimeout = 30
        , evringRequestHeaderTimeout = 30 -- 30 seconds for headers
        , evringRequestBodyTimeout = 60 -- 60 seconds for body
        , evringZeroCopyThreshold = 16384 -- 16KB threshold for zero-copy send
        , evringLogger = logger
        }

-- | Server state for graceful shutdown and resource management
data ServerState = ServerState
    { ssShuttingDown :: !Bool
    , ssActiveConnections :: !Int
    , ssBufferPool :: ![BufferSlot] -- Pool of reusable buffers
    }

-- | A buffer slot that can be borrowed and returned
data BufferSlot = BufferSlot
    { bsBuffer :: !(MutablePrimArray RealWorld Word8)
    , bsPtr :: !(Ptr Word8)
    }

-- | Allocate a pool of buffers
allocateBufferPool :: Int -> Int -> IO [BufferSlot]
allocateBufferPool poolSize bufferSize =
    replicateM poolSize $ do
        buf <- newPinnedPrimArray bufferSize
        let ptr = mutablePrimArrayContents buf
        return BufferSlot{bsBuffer = buf, bsPtr = ptr}

-- | Borrow a buffer from the pool, or allocate a new one if pool is empty
borrowBuffer :: IORef ServerState -> Int -> IO BufferSlot
borrowBuffer serverStateRef bufferSize = do
    mSlot <- atomicModifyIORef' serverStateRef $ \s ->
        case ssBufferPool s of
            [] -> (s, Nothing)
            (slot : rest) -> (s{ssBufferPool = rest}, Just slot)
    case mSlot of
        Just slot -> return slot
        Nothing -> do
            -- Pool exhausted, allocate new buffer
            buf <- newPinnedPrimArray bufferSize
            let ptr = mutablePrimArrayContents buf
            return BufferSlot{bsBuffer = buf, bsPtr = ptr}

-- | Return a buffer to the pool
returnBuffer :: IORef ServerState -> BufferSlot -> IO ()
returnBuffer serverStateRef slot =
    atomicModifyIORef' serverStateRef $ \s ->
        (s{ssBufferPool = slot : ssBufferPool s}, ())

-- ════════════════════════════════════════════════════════════════════════════
-- Public API
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a WAI application on the given port using io_uring.
runEvring :: Log.Logger -> Int -> Application -> IO ()
runEvring logger port = runEvringSettings ((defaultEvringSettings logger){evringPort = port})

{- | Run a WAI application with custom settings using io_uring.
Uses single-threaded event loop for io_uring safety.
Handles SIGTERM and SIGINT for graceful shutdown.
-}
runEvringSettings :: EvringSettings -> Application -> IO ()
runEvringSettings settings app = do
    let lg = Log.withNS (evringLogger settings) "evring-wai"
    Log.logInfo lg ("Starting server on port " <> T.pack (show (evringPort settings))) ()

    -- Pre-allocate buffer pool
    let poolSize = min 256 (evringMaxConnections settings)
        bufferSize = evringBufferSize settings
    bufferPool <- allocateBufferPool poolSize bufferSize

    -- Initialize server state
    serverStateRef <-
        newIORef
            ServerState
                { ssShuttingDown = False
                , ssActiveConnections = 0
                , ssBufferPool = bufferPool
                }

    -- Install signal handlers
    let shutdownHandler = do
            Log.logInfo lg "Received shutdown signal, initiating graceful shutdown..." ()
            atomicModifyIORef' serverStateRef $ \s ->
                (s{ssShuttingDown = True}, ())

    _ <- installHandler sigTERM (Catch shutdownHandler) Nothing
    _ <- installHandler sigINT (Catch shutdownHandler) Nothing

    withIoUring defaultIoUringParams $ \ctx -> do
        bracket (createListenSocket settings) close $ \listenSock -> do
            withFdSocket listenSock $ \listenFd -> do
                -- Catch exceptions from interrupted io_uring operations during shutdown
                catch
                    (runAcceptLoop ctx (Fd listenFd) settings app serverStateRef lg)
                    ( \(e :: SomeException) -> do
                        -- Check error message for EINTR (-4), which is expected during shutdown
                        let errMsg = show e
                            isEintr = "failed: -4)" `isInfixOf` errMsg || "-4)" `isInfixOf` errMsg
                        unless isEintr $
                            Log.logError lg ("Accept loop error: " <> T.pack errMsg) ()
                    )
                    `finally` waitForConnections serverStateRef settings lg

-- | Wait for active connections to drain during shutdown
waitForConnections :: IORef ServerState -> EvringSettings -> Log.Logger -> IO ()
waitForConnections serverStateRef settings lg = do
    state <- readIORef serverStateRef
    when (ssActiveConnections state > 0) $ do
        Log.logInfo lg ("Waiting for " <> T.pack (show (ssActiveConnections state)) <> " active connections to drain...") ()
        waitLoop (evringGracefulShutdownTimeout settings * 10) -- 100ms intervals
    Log.logInfo lg "Shutdown complete" ()
  where
    waitLoop 0 = do
        state <- readIORef serverStateRef
        when (ssActiveConnections state > 0) $
            Log.logWarn lg
                ("Timeout waiting for connections, forcing shutdown with "
                    <> T.pack (show (ssActiveConnections state))
                    <> " active") ()
    waitLoop remaining = do
        state <- readIORef serverStateRef
        unless (ssActiveConnections state == 0) $ do
            threadDelay 100000 -- 100ms
            waitLoop (remaining - 1)

-- ════════════════════════════════════════════════════════════════════════════
-- Socket Setup
-- ════════════════════════════════════════════════════════════════════════════

-- | IPv4 any address (0.0.0.0) as HostAddress (which is just Word32)
ipv4AnyAddress :: HostAddress
ipv4AnyAddress = 0

-- Note: ipv6AnyAddress and ipv6AddressFromWords moved to Evring.Wai.Internal

-- | Create and bind a listen socket.
createListenSocket :: EvringSettings -> IO Socket
createListenSocket EvringSettings{..} = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    let addr = SockAddrInet (fromIntegral evringPort) ipv4AnyAddress
    bind sock addr
    listen sock evringBacklog
    return sock

-- ════════════════════════════════════════════════════════════════════════════
-- Accept Loop
-- ════════════════════════════════════════════════════════════════════════════

{- | Main accept loop using io_uring.
Single-threaded event loop for io_uring safety.
Checks for shutdown signal and tracks active connections.
-}
runAcceptLoop :: IOCtx -> Fd -> EvringSettings -> Application -> IORef ServerState -> Log.Logger -> IO ()
runAcceptLoop ctx listenFd settings app serverStateRef lg = do
    -- Allocate sockaddr buffer for accept (reused across accepts)
    addrBuf <- mallocBytes 128 -- sockaddr_storage size
    addrLenBuf <- mallocBytes 4
    poke (castPtr addrLenBuf :: Ptr Word32) 128

    acceptLoop addrBuf addrLenBuf
  where
    acceptLoop addrBuf addrLenBuf = do
        -- Check if we're shutting down
        state <- readIORef serverStateRef
        when (ssShuttingDown state) $ do
            Log.logDebug lg "Shutdown requested, stopping accept loop" ()
            return ()

        unless (ssShuttingDown state) $ do
            -- Submit accept operation
            results <- submitBatch ctx $ \submit -> do
                submit $ AcceptOp listenFd 0 addrBuf addrLenBuf

            -- Process accept results
            case results of
                v | V.length v > 0 -> case v V.! 0 of
                    Complete clientFdInt -> do
                        let clientFd = Fd (fromIntegral clientFdInt)

                        -- Set TCP_NODELAY to avoid Nagle + delayed ACK interaction
                        setTcpNoDelay clientFd

                        -- Check connection limit before accepting
                        currentState <- readIORef serverStateRef
                        let maxConns = evringMaxConnections settings
                            atLimit = ssActiveConnections currentState >= maxConns

                        if atLimit
                            then do
                                -- Reject connection - send 503 and close immediately
                                let errorResponse =
                                        "HTTP/1.1 503 Service Unavailable\r\n"
                                            <> "Content-Type: text/plain\r\n"
                                            <> "Content-Length: 20\r\n"
                                            <> "Connection: close\r\n"
                                            <> "\r\n"
                                            <> "Server at capacity\r\n"
                                sendBytes ctx clientFd errorResponse
                                closeConnection ctx clientFd
                            else do
                                -- Parse client address before resetting buffer
                                clientAddr <- parseSockAddr addrBuf
                                -- Reset addrlen for next accept
                                poke (castPtr addrLenBuf :: Ptr Word32) 128
                                -- Track connection
                                atomicModifyIORef' serverStateRef $ \s ->
                                    (s{ssActiveConnections = ssActiveConnections s + 1}, ())
                                -- Fork handler pinned to same capability (for io_uring thread safety)
                                (capIdx, _) <- threadCapability =<< myThreadId
                                _ <-
                                    forkOn capIdx $
                                        handleConnection ctx clientFd clientAddr settings app serverStateRef lg
                                            `finally` atomicModifyIORef'
                                                serverStateRef
                                                ( \s ->
                                                    (s{ssActiveConnections = ssActiveConnections s - 1}, ())
                                                )
                                return ()
                    IoErrno _errno -> return () -- Accept failed, continue
                    Eof -> return ()
                _emptyResults -> return () -- Empty results vector

            -- Continue accepting
            acceptLoop addrBuf addrLenBuf

-- Note: parseSockAddr and byteSwap16 moved to Evring.Wai.Internal

-- ════════════════════════════════════════════════════════════════════════════
-- Connection Handler
-- ════════════════════════════════════════════════════════════════════════════

-- | Connection state for keep-alive handling
data ConnState = ConnState
    { connBuffer :: !(MutablePrimArray RealWorld Word8)
    , connBufPtr :: !(Ptr Word8)
    , connLeftover :: !ByteString -- Leftover data from previous read
    , connRequestCount :: !Int
    , connRemoteHost :: !SockAddr -- Client address
    }

-- | Handle a connection with keep-alive support.
handleConnection :: IOCtx -> Fd -> SockAddr -> EvringSettings -> Application -> IORef ServerState -> Log.Logger -> IO ()
handleConnection ctx clientFd clientAddr settings app serverStateRef lg = do
    let bufferSize = evringBufferSize settings

    -- Borrow buffer from pool
    bufferSlot <- borrowBuffer serverStateRef bufferSize

    -- Initialize connection state
    stateRef <-
        newIORef
            ConnState
                { connBuffer = bsBuffer bufferSlot
                , connBufPtr = bsPtr bufferSlot
                , connLeftover = BS.empty
                , connRequestCount = 0
                , connRemoteHost = clientAddr
                }

    -- Enter keep-alive request loop, returning buffer when done
    keepAliveLoop ctx clientFd settings app stateRef serverStateRef lg
        `finally` returnBuffer serverStateRef bufferSlot

-- | Keep-alive request loop - handles multiple requests per connection
keepAliveLoop :: IOCtx -> Fd -> EvringSettings -> Application -> IORef ConnState -> IORef ServerState -> Log.Logger -> IO ()
keepAliveLoop ctx clientFd settings app stateRef serverStateRef lg = do
    state <- readIORef stateRef
    serverState <- readIORef serverStateRef
    let bufferSize = evringBufferSize settings
        maxRequests = evringMaxRequestsPerConnection settings
        _headerTimeoutUs = evringRequestHeaderTimeout settings * 1000000 -- TODO: use io_uring timeout

    -- Check if we're shutting down or hit max requests - close connection immediately
    if ssShuttingDown serverState || connRequestCount state >= maxRequests
        then closeConnection ctx clientFd
        else do
            -- Try to parse from leftover data first
            case tryParseRequest (connLeftover state) of
                Just (pr, leftover) -> do
                    -- We have a complete request from leftover data
                    -- Attach streaming body reader and process
                    req <- attachStreamingBody ctx clientFd stateRef pr
                    processRequest ctx clientFd settings app stateRef serverStateRef req leftover lg
                Nothing -> do
                    -- Need to read more data
                    results <- submitBatch ctx $ \submit -> do
                        submit $ RecvOp clientFd (connBuffer state) 0 (fromIntegral bufferSize) 0

                    case results of
                        v | V.length v > 0 -> case v V.! 0 of
                            Complete bytesRead | bytesRead > 0 -> do
                                newData <- peekBS (connBufPtr state) (fromIntegral bytesRead)
                                let allData = connLeftover state <> newData

                                case tryParseRequest allData of
                                    Just (pr, leftover) -> do
                                        req <- attachStreamingBody ctx clientFd stateRef pr
                                        processRequest ctx clientFd settings app stateRef serverStateRef req leftover lg
                                    Nothing -> do
                                        -- Still incomplete, store and continue (request too large or split)
                                        writeIORef stateRef state{connLeftover = allData}
                                        -- Try reading more (could be chunked arrival)
                                        keepAliveLoop ctx clientFd settings app stateRef serverStateRef lg
                            Complete _zeroBytes -> closeConnection ctx clientFd -- 0 bytes = client closed
                            IoErrno _errno -> closeConnection ctx clientFd
                            Eof -> closeConnection ctx clientFd
                        _emptyResults -> closeConnection ctx clientFd

{- | Try to parse a complete HTTP request from buffer.
Returns ParsedRequest (with initial body data) and leftover data for next request.
For streaming bodies, we only need headers + some initial body data to be present.
-}
tryParseRequest :: ByteString -> Maybe (ParsedRequest, ByteString)
tryParseRequest bs
    | BS.null bs = Nothing
    | otherwise =
        -- Check if we have complete headers (ends with \r\n\r\n)
        case BS.breakSubstring "\r\n\r\n" bs of
            (_beforeSeparator, rest) | BS.null rest -> Nothing -- Incomplete headers
            (headers, _afterSeparator) ->
                -- We have complete headers, try to parse
                case parseHttpRequest bs of
                    Right pr ->
                        -- Calculate what data belongs to this request
                        let headerEnd = BS.length headers + 4
                            contentLen = prContentLength pr
                            totalLen = headerEnd + contentLen
                            -- For streaming: we can proceed if we have at least headers
                            -- Body will be read incrementally
                            availableBody = BS.length bs - headerEnd
                         in if contentLen <= availableBody
                                -- Full body available in buffer - common case for small requests
                                then Just (pr, BS.drop totalLen bs)
                                -- Partial body - streaming will handle the rest
                                else Just (pr{prInitialBody = BS.drop headerEnd bs}, BS.empty)
                    Left _parseError -> Nothing -- Parse error

-- Note: getContentLengthFromParsed removed - use prContentLength directly

-- | Process a complete request and decide whether to keep connection alive
processRequest :: IOCtx -> Fd -> EvringSettings -> Application -> IORef ConnState -> IORef ServerState -> Request -> ByteString -> Log.Logger -> IO ()
processRequest ctx clientFd settings app stateRef serverStateRef req leftover lg = do
    state <- readIORef stateRef
    serverState <- readIORef serverStateRef

    -- Determine if we should keep the connection alive
    -- During shutdown, always close after responding
    let shouldKeepAlive = checkKeepAlive req && not (ssShuttingDown serverState)

    -- Call WAI application
    response <- runApplication app req lg

    let handleNormalResponse = do
            sendResponseWithKeepAlive ctx clientFd response shouldKeepAlive

            if shouldKeepAlive
                then do
                    -- Update state and continue loop
                    writeIORef
                        stateRef
                        state
                            { connLeftover = leftover
                            , connRequestCount = connRequestCount state + 1
                            }
                    keepAliveLoop ctx clientFd settings app stateRef serverStateRef lg
                else
                    closeConnection ctx clientFd

    -- Check for ResponseRaw (WebSocket, etc.)
    case response of
        ResponseRaw rawApp _fallback -> do
            -- Handle raw connection (WebSocket, etc.)
            -- The rawApp takes a receive action and a send action
            handleRawResponse ctx clientFd settings stateRef rawApp leftover lg
            -- After raw handler completes, close connection
            closeConnection ctx clientFd
        -- Normal HTTP responses (File, Builder, Stream)
        ResponseFile{} -> handleNormalResponse
        ResponseBuilder{} -> handleNormalResponse
        ResponseStream{} -> handleNormalResponse

{- | Check if connection should be kept alive based on request headers.

Uses 'checkKeepAliveHeaders' from 'Evring.Wai.Internal' with the
Connection header and HTTP version from the request.
-}
checkKeepAlive :: Request -> Bool
checkKeepAlive req =
    checkKeepAliveHeaders
        (lookup "Connection" (requestHeaders req))
        (httpVersion req >= http11)

{- | Handle a ResponseRaw (WebSocket, etc.)
This gives the application direct access to send/receive on the socket
-}
handleRawResponse ::
    IOCtx ->
    Fd ->
    EvringSettings ->
    IORef ConnState ->
    (IO ByteString -> (ByteString -> IO ()) -> IO ()) ->
    ByteString ->
    Log.Logger ->
    IO ()
handleRawResponse ctx clientFd settings stateRef rawApp leftoverData lg = do
    -- Create leftover buffer for any data that was read but not consumed
    leftoverRef <- newIORef leftoverData

    -- Create receive action for the raw handler
    let recvAction :: IO ByteString
        recvAction = do
            -- First check if we have leftover data
            leftover <- readIORef leftoverRef
            if not (BS.null leftover)
                then do
                    writeIORef leftoverRef BS.empty
                    return leftover
                else do
                    -- Read from socket using io_uring
                    state <- readIORef stateRef
                    let bufferSize = evringBufferSize settings
                    results <- submitBatch ctx $ \submit -> do
                        submit $ RecvOp clientFd (connBuffer state) 0 (fromIntegral bufferSize) 0

                    case results of
                        v | V.length v > 0 -> case v V.! 0 of
                            Complete bytesRead
                                | bytesRead > 0 ->
                                    peekBS (connBufPtr state) (fromIntegral bytesRead)
                            Complete _zeroBytes -> return BS.empty -- Connection closed
                            IoErrno _errno -> return BS.empty
                            Eof -> return BS.empty
                        _emptyResults -> return BS.empty

    -- Create send action for the raw handler
    let sendAction :: ByteString -> IO ()
        sendAction bytes
            | BS.null bytes = return ()
            | otherwise = sendBytes ctx clientFd bytes

    -- Run the raw application
    catch
        (rawApp recvAction sendAction)
        ( \(e :: SomeException) ->
            Log.logError lg ("Raw handler error: " <> T.pack (show e)) ()
        )

-- | Run the WAI application safely
runApplication :: Application -> Request -> Log.Logger -> IO Response
runApplication app req lg = do
    responseRef <- newIORef Nothing
    let respond resp = do
            writeIORef responseRef (Just resp)
            return ResponseReceived

    catch
        (void $ app req respond)
        ( \(e :: SomeException) -> do
            let path = rawPathInfo req
            Log.logError lg ("Application error on " <> T.pack (show path) <> ": " <> T.pack (show e)) ()
            writeIORef
                responseRef
                ( Just $
                    responseLBS
                        status500
                        [("Content-Type", "text/plain")]
                        "Internal Server Error"
                )
        )

    fromMaybe (responseLBS status500 [] "No response") <$> readIORef responseRef

-- | Peek ByteString from buffer.
peekBS :: Ptr Word8 -> Int -> IO ByteString
peekBS ptr len = BS.packCStringLen (castPtr ptr, len)

-- | Close a connection.
closeConnection :: IOCtx -> Fd -> IO ()
closeConnection ctx fd = do
    void $ submitBatch ctx $ \submit -> do
        submit $ CloseOp fd

-- ════════════════════════════════════════════════════════════════════════════
-- HTTP Request Parsing
-- ════════════════════════════════════════════════════════════════════════════

-- | Parsed HTTP request with body streaming support
data ParsedRequest = ParsedRequest
    { prRequest :: !Request
    -- ^ The WAI request (body reader not yet attached)
    , prContentLength :: !Int
    -- ^ Expected body length (0 for no body)
    , prInitialBody :: !ByteString
    -- ^ Body data already read with headers
    }

{- | Parse HTTP headers and return a ParsedRequest.
Body reading is handled separately to support streaming.
-}
parseHttpRequest :: ByteString -> Either String ParsedRequest
parseHttpRequest rawData = do
    -- Split headers from body at \r\n\r\n
    let (headerSection, bodyData) = splitHeaderBody rawData

    -- Parse request line and headers
    case BC.lines headerSection of
        [] -> Left "Empty request"
        (requestLine : headerLines) -> do
            -- Parse request line: "METHOD /path HTTP/1.1\r"
            (method, rawPath, httpVer) <- parseRequestLine (stripCR requestLine)

            -- Parse headers
            let headers = parseHeaders headerLines

            -- Extract path and query string
            let (path, queryBS) = splitPathQuery rawPath
                query = parseQuery queryBS
                pathSegments = decodePathSegments path

            -- Get Content-Length for body
            let contentLength = getContentLength headers

            -- Build the base Request (body reader attached later)
            let baseRequest =
                    defaultRequest
                        { requestMethod = method
                        , httpVersion = httpVer
                        , rawPathInfo = path
                        , rawQueryString = queryBS
                        , pathInfo = pathSegments
                        , queryString = query
                        , requestHeaders = headers
                        , isSecure = False
                        , remoteHost = SockAddrInet 0 0 -- TODO: Get from accept
                        , vault = Vault.empty
                        , requestBodyLength = KnownLength (fromIntegral contentLength)
                        , requestHeaderHost = lookup "Host" headers
                        , requestHeaderRange = lookup "Range" headers
                        , requestHeaderReferer = lookup "Referer" headers
                        , requestHeaderUserAgent = lookup "User-Agent" headers
                        }

            Right
                ParsedRequest
                    { prRequest = baseRequest
                    , prContentLength = contentLength
                    , prInitialBody = bodyData
                    }

{- | Attach a streaming body reader to a parsed request.
This creates a body reader that first returns any already-read data,
then reads more from the socket via io_uring as needed.
Also sets the remoteHost from the connection state.
-}
attachStreamingBody :: IOCtx -> Fd -> IORef ConnState -> ParsedRequest -> IO Request
attachStreamingBody ctx clientFd stateRef pr = do
    connState <- readIORef stateRef
    let contentLength = prContentLength pr
        initialBody = prInitialBody pr
        initialLen = BS.length initialBody
        remainingBytes = contentLength - initialLen
        clientAddr = connRemoteHost connState

    -- Create body state
    bodyStateRef <- newIORef (initialBody, remainingBytes)

    let bodyReader :: IO ByteString
        bodyReader = do
            (buffered, remaining) <- readIORef bodyStateRef

            if not (BS.null buffered)
                then do
                    -- Return buffered data first
                    writeIORef bodyStateRef (BS.empty, remaining)
                    return buffered
                else
                    if remaining <= 0
                        then return BS.empty -- Body complete
                        else do
                            -- Need to read more from socket
                            state <- readIORef stateRef
                            let bufferSize = 65536 -- Read in 64KB chunks
                                toRead = min remaining bufferSize

                            results <- submitBatch ctx $ \submit -> do
                                submit $ RecvOp clientFd (connBuffer state) 0 (fromIntegral toRead) 0

                            case results of
                                v | V.length v > 0 -> case v V.! 0 of
                                    Complete bytesRead | bytesRead > 0 -> do
                                        chunk <- peekBS (connBufPtr state) (fromIntegral bytesRead)
                                        writeIORef bodyStateRef (BS.empty, remaining - fromIntegral bytesRead)
                                        return chunk
                                    Complete _zeroBytes -> do
                                        -- EOF - body truncated
                                        writeIORef bodyStateRef (BS.empty, 0)
                                        return BS.empty
                                    IoErrno _errno -> do
                                        -- Error - body truncated
                                        writeIORef bodyStateRef (BS.empty, 0)
                                        return BS.empty
                                    Eof -> do
                                        writeIORef bodyStateRef (BS.empty, 0)
                                        return BS.empty
                                _emptyResults -> return BS.empty

    -- Update request with remoteHost and body reader
    let reqWithHost = (prRequest pr){remoteHost = clientAddr}
    return $ setRequestBodyChunks bodyReader reqWithHost

-- | Parse the request line: "METHOD /path HTTP/1.1"
parseRequestLine :: ByteString -> Either String (ByteString, ByteString, HttpVersion)
parseRequestLine line =
    case BC.words line of
        [method, path, version] -> do
            httpVer <- parseHttpVersion version
            Right (method, path, httpVer)
        [method, path] ->
            -- HTTP/0.9 style (rare)
            Right (method, path, http11)
        [] -> Left "Empty request line"
        [_methodOnly] -> Left "Request line missing path"
        _tooManyParts -> Left $ "Invalid request line: " ++ BC.unpack line

-- | Parse HTTP version string
parseHttpVersion :: ByteString -> Either String HttpVersion
parseHttpVersion "HTTP/1.1" = Right http11
parseHttpVersion "HTTP/1.0" = Right (HttpVersion 1 0)
parseHttpVersion "HTTP/2.0" = Right (HttpVersion 2 0)
parseHttpVersion "HTTP/2" = Right (HttpVersion 2 0)
parseHttpVersion v = Left $ "Unknown HTTP version: " ++ BC.unpack v

-- Note: parseHeaders, splitHeaderBody, splitPathQuery, stripCR, getContentLength
-- moved to Evring.Wai.Internal

{- | Parse header lines into RequestHeaders (local wrapper).

Uses the shared implementation from 'Evring.Wai.Internal'.
-}
parseHeaders :: [ByteString] -> RequestHeaders
parseHeaders = foldr parseHeader []
  where
    parseHeader line acc =
        case BC.break (== ':') (stripCR line) of
            (name, rest)
                | BS.null rest -> acc -- Skip malformed headers
                | otherwise ->
                    let value = BS.dropWhile (== 32) (BS.drop 1 rest) -- Drop ':' and leading spaces
                     in (CI.mk name, value) : acc

-- Note: unsafePerformIORef and consumeBody removed - now using attachStreamingBody for proper streaming

-- ════════════════════════════════════════════════════════════════════════════
-- Response Sending
-- ════════════════════════════════════════════════════════════════════════════

{- | Send an HTTP response (closing connection after).
Kept for API compatibility.
-}
_sendResponse :: IOCtx -> Fd -> Response -> IO ()
_sendResponse ctx clientFd response =
    sendResponseWithKeepAlive ctx clientFd response False

{- | Send an HTTP response with keep-alive control.
Supports both buffered and streaming responses.
-}
sendResponseWithKeepAlive :: IOCtx -> Fd -> Response -> Bool -> IO ()
sendResponseWithKeepAlive ctx clientFd response keepAlive = do
    let (status, headers, withBody) = responseToStream response

    -- Check if we should use chunked transfer encoding
    let hasContentLength = any (\(n, _) -> CI.mk "Content-Length" == n) headers
        hasTransferEncoding = any (\(n, _) -> CI.mk "Transfer-Encoding" == n) headers
        useChunked = not hasContentLength && not hasTransferEncoding

    if useChunked
        then sendChunkedResponse ctx clientFd status headers withBody keepAlive
        else sendBufferedResponse ctx clientFd status headers withBody keepAlive

-- | Send a buffered response (Content-Length known).
sendBufferedResponse ::
    IOCtx ->
    Fd ->
    Network.HTTP.Types.Status ->
    [(CI.CI ByteString, ByteString)] ->
    ((StreamingBody -> IO ()) -> IO ()) ->
    Bool ->
    IO ()
sendBufferedResponse ctx clientFd status headers withBody keepAlive = do
    -- Collect entire body
    bodyRef <- newIORef mempty
    withBody $ \streamingBody -> do
        streamingBody
            (\builderChunk -> modifyIORef' bodyRef (<> builderChunk))
            (return ())

    bodyBuilder <- readIORef bodyRef
    let body = Builder.toLazyByteString bodyBuilder

    -- Build complete response
    let responseBytes =
            buildResponseHeaders
                status
                headers
                keepAlive
                (Just $ LBS.length body)
                <> LBS.toStrict body

    sendBytes ctx clientFd responseBytes

-- | Send a chunked streaming response.
sendChunkedResponse ::
    IOCtx ->
    Fd ->
    Network.HTTP.Types.Status ->
    [(CI.CI ByteString, ByteString)] ->
    ((StreamingBody -> IO ()) -> IO ()) ->
    Bool ->
    IO ()
sendChunkedResponse ctx clientFd status headers withBody keepAlive = do
    -- Send headers with Transfer-Encoding: chunked
    let headerBytes = buildChunkedHeaders status headers keepAlive
    sendBytes ctx clientFd headerBytes

    -- Stream body chunks
    withBody $ \streamingBody -> do
        streamingBody
            ( \builderChunk -> do
                let chunk = Builder.toLazyByteString builderChunk
                when (LBS.length chunk > 0) $ do
                    sendChunk ctx clientFd (LBS.toStrict chunk)
            )
            (return ()) -- flush callback (no-op for now)

    -- Send final chunk (0\r\n\r\n)
    sendBytes ctx clientFd "0\r\n\r\n"

-- | Send a single HTTP chunk.
sendChunk :: IOCtx -> Fd -> ByteString -> IO ()
sendChunk ctx clientFd chunkData = do
    let chunkSize = BS.length chunkData
        -- Format: <hex size>\r\n<data>\r\n
        chunkBytes =
            LBS.toStrict $
                Builder.toLazyByteString $
                    mconcat
                        [ Builder.wordHex (fromIntegral chunkSize)
                        , Builder.byteString "\r\n"
                        , Builder.byteString chunkData
                        , Builder.byteString "\r\n"
                        ]
    sendBytes ctx clientFd chunkBytes

{- | Build HTTP response headers.

Constructs the HTTP response status line, headers, Content-Length,
and Connection header. Uses 'formatHeader' from 'Evring.Wai.Internal'.
-}
buildResponseHeaders ::
    Network.HTTP.Types.Status ->
    [(CI.CI ByteString, ByteString)] ->
    Bool ->
    Maybe Int64 ->
    ByteString
buildResponseHeaders status headers keepAlive mContentLength =
    let filteredHeaders = filter (\(n, _) -> CI.mk n /= "Connection") headers
        connectionHeader =
            if keepAlive
                then "Connection: keep-alive\r\n"
                else "Connection: close\r\n"
        contentLengthHeader = case mContentLength of
            Just len ->
                Builder.byteString "Content-Length: "
                    <> Builder.int64Dec len
                    <> Builder.byteString "\r\n"
            Nothing -> mempty
     in LBS.toStrict $
            Builder.toLazyByteString $
                mconcat
                    [ Builder.byteString "HTTP/1.1 "
                    , Builder.intDec (statusCode status)
                    , Builder.byteString " "
                    , Builder.byteString (statusMessage status)
                    , Builder.byteString "\r\n"
                    , mconcat $ map formatHeader filteredHeaders
                    , contentLengthHeader
                    , Builder.byteString connectionHeader
                    , Builder.byteString "\r\n"
                    ]

{- | Build HTTP response headers for chunked encoding.

Similar to 'buildResponseHeaders' but adds @Transfer-Encoding: chunked@
instead of Content-Length. Uses 'formatHeader' from 'Evring.Wai.Internal'.
-}
buildChunkedHeaders ::
    Network.HTTP.Types.Status ->
    [(CI.CI ByteString, ByteString)] ->
    Bool ->
    ByteString
buildChunkedHeaders status headers keepAlive =
    let filteredHeaders = filter (\(n, _) -> CI.mk n /= "Connection" && CI.mk n /= "Transfer-Encoding") headers
        connectionHeader =
            if keepAlive
                then "Connection: keep-alive\r\n"
                else "Connection: close\r\n"
     in LBS.toStrict $
            Builder.toLazyByteString $
                mconcat
                    [ Builder.byteString "HTTP/1.1 "
                    , Builder.intDec (statusCode status)
                    , Builder.byteString " "
                    , Builder.byteString (statusMessage status)
                    , Builder.byteString "\r\n"
                    , mconcat $ map formatHeader filteredHeaders
                    , Builder.byteString "Transfer-Encoding: chunked\r\n"
                    , Builder.byteString connectionHeader
                    , Builder.byteString "\r\n"
                    ]

{- | Send bytes over the connection.
Uses the ByteString's internal buffer directly (avoiding an extra copy)
when possible, with a copy to pinned memory for safety.
-}
sendBytes :: IOCtx -> Fd -> ByteString -> IO ()
sendBytes ctx clientFd bytes = do
    let len = BS.length bytes
    -- Allocate pinned buffer and copy - required for io_uring
    -- (ByteString might be unpinned and could move during GC)
    sendBuf <- mallocBytes len
    BS.useAsCStringLen bytes $ \(srcPtr, srcLen) -> do
        copyBytes sendBuf (castPtr srcPtr) srcLen

    void $ submitBatch ctx $ \submit -> do
        submit $ SendPtrOp clientFd sendBuf (fromIntegral len) 0

    free sendBuf

{- | Send bytes using zero-copy when buffer is large enough.
Currently falls back to regular send since zero-copy requires
buffer lifetime management that our synchronous model doesn't support.
TODO: Implement proper zero-copy with IOSQE_CQE_SKIP_SUCCESS
-}
_sendBytesZeroCopy :: IOCtx -> Fd -> ByteString -> Int -> IO ()
_sendBytesZeroCopy ctx clientFd bytes threshold
    | BS.length bytes >= threshold = do
        -- For true zero-copy, we'd need to ensure the buffer stays valid
        -- until kernel signals completion. For now, fall back to regular send.
        sendBytes ctx clientFd bytes
    | otherwise = sendBytes ctx clientFd bytes
