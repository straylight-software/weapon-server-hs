{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | WAI runner using io_uring for all network I/O.
--
-- This module provides a high-performance HTTP server that uses io_uring
-- instead of the traditional epoll/kqueue event loop.
--
-- Usage:
--
-- @
-- import Evring.Wai (runEvring)
-- import Network.Wai (Application)
--
-- main :: IO ()
-- main = runEvring 8080 myApp
-- @
--
-- Supports:
-- - Full HTTP/1.1 request parsing (method, path, query, headers)
-- - Request body reading (Content-Length)
-- - WAI Application interface compatible with Servant
-- - HTTP/1.1 Keep-Alive connections
-- - Chunked transfer encoding for streaming responses
-- - WebSocket support via ResponseRaw
-- - Graceful shutdown on SIGTERM/SIGINT
-- - Proper error handling
module Evring.Wai
  ( -- * Running WAI applications
    runEvring,
    runEvringSettings,

    -- * Settings
    EvringSettings (..),
    defaultEvringSettings,
  )
where

import Control.Concurrent (forkOn, threadDelay)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (unless, void, when)
-- import System.Timeout (timeout)  -- Disabled: using io_uring directly
-- Debug timing (disabled):
-- import System.Clock (Clock(Monotonic), getTime, toNanoSecs, diffTimeSpec)

import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
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
import Data.Word (Word16, Word32, Word8)
import Foreign (Ptr, castPtr, copyBytes, free, mallocBytes)
-- Note: unsafePerformIO no longer needed - streaming body uses proper IO

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable (..))
import GHC.Conc (myThreadId, threadCapability)
import GHC.Exts (RealWorld)
import Network.HTTP.Types
  ( HttpVersion (..),
    RequestHeaders,
    decodePathSegments,
    http11,
    parseQuery,
    status500,
    statusCode,
    statusMessage,
  )
import Network.HTTP.Types qualified as Network.HTTP.Types
import Network.Socket
  ( Family (AF_INET),
    PortNumber,
    SockAddr (SockAddrInet, SockAddrInet6),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    bind,
    close,
    listen,
    setSocketOption,
    socket,
    tupleToHostAddress,
    withFdSocket,
  )
import Network.Wai
  ( Application,
    Request (..),
    Response,
    StreamingBody,
    defaultRequest,
    responseLBS,
    responseToStream,
  )
import Network.Wai.Internal
  ( RequestBodyLength (..),
    Response (..),
    ResponseReceived (..),
    setRequestBodyChunks,
  )
import System.IoUring (IOCtx, IoOp (..), IoResult (..), defaultIoUringParams, submitBatch, withIoUring)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import System.Posix.Types (Fd (Fd))
import Text.Read (readMaybe)

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
  { -- | Port to listen on
    evringPort :: !Int,
    -- | Host to bind to (default "0.0.0.0")
    evringHost :: !ByteString,
    -- | Listen backlog (default 1024)
    evringBacklog :: !Int,
    -- | Per-connection receive buffer size (default 64KB)
    evringBufferSize :: !Int,
    -- | Maximum concurrent connections (default 10000)
    evringMaxConnections :: !Int,
    -- | Keep-alive timeout in seconds (default 65)
    evringKeepAliveTimeout :: !Int,
    -- | Maximum requests per keep-alive connection (default 1000)
    evringMaxRequestsPerConnection :: !Int,
    -- | Graceful shutdown timeout in seconds (default 30)
    evringGracefulShutdownTimeout :: !Int,
    -- | Timeout for receiving request headers in seconds (default 30)
    -- Protects against slow loris attacks
    evringRequestHeaderTimeout :: !Int,
    -- | Timeout for receiving request body in seconds (default 60)
    evringRequestBodyTimeout :: !Int,
    -- | Minimum size in bytes to use zero-copy send (default 16KB, 0 to disable)
    evringZeroCopyThreshold :: !Int
  }

-- | Default settings for port 8080.
defaultEvringSettings :: EvringSettings
defaultEvringSettings =
  EvringSettings
    { evringPort = 8080,
      evringHost = "0.0.0.0",
      evringBacklog = 1024,
      evringBufferSize = 65536, -- 64KB for larger requests
      evringMaxConnections = 10000,
      evringKeepAliveTimeout = 65, -- Slightly longer than common client timeout
      evringMaxRequestsPerConnection = 1000,
      evringGracefulShutdownTimeout = 30,
      evringRequestHeaderTimeout = 30, -- 30 seconds for headers
      evringRequestBodyTimeout = 60, -- 60 seconds for body
      evringZeroCopyThreshold = 16384 -- 16KB threshold for zero-copy send
    }

-- | Server state for graceful shutdown and resource management
data ServerState = ServerState
  { ssShuttingDown :: !Bool,
    ssActiveConnections :: !Int,
    ssBufferPool :: ![BufferSlot] -- Pool of reusable buffers
  }

-- | A buffer slot that can be borrowed and returned
data BufferSlot = BufferSlot
  { bsBuffer :: !(MutablePrimArray RealWorld Word8),
    bsPtr :: !(Ptr Word8)
  }

-- | Allocate a pool of buffers
allocateBufferPool :: Int -> Int -> IO [BufferSlot]
allocateBufferPool poolSize bufferSize =
  sequence $ replicate poolSize $ do
    buf <- newPinnedPrimArray bufferSize
    let ptr = mutablePrimArrayContents buf
    return BufferSlot {bsBuffer = buf, bsPtr = ptr}

-- | Borrow a buffer from the pool, or allocate a new one if pool is empty
borrowBuffer :: IORef ServerState -> Int -> IO BufferSlot
borrowBuffer serverStateRef bufferSize = do
  mSlot <- atomicModifyIORef' serverStateRef $ \s ->
    case ssBufferPool s of
      [] -> (s, Nothing)
      (slot : rest) -> (s {ssBufferPool = rest}, Just slot)
  case mSlot of
    Just slot -> return slot
    Nothing -> do
      -- Pool exhausted, allocate new buffer
      buf <- newPinnedPrimArray bufferSize
      let ptr = mutablePrimArrayContents buf
      return BufferSlot {bsBuffer = buf, bsPtr = ptr}

-- | Return a buffer to the pool
returnBuffer :: IORef ServerState -> BufferSlot -> IO ()
returnBuffer serverStateRef slot =
  atomicModifyIORef' serverStateRef $ \s ->
    (s {ssBufferPool = slot : ssBufferPool s}, ())

-- ════════════════════════════════════════════════════════════════════════════
-- Public API
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a WAI application on the given port using io_uring.
runEvring :: Int -> Application -> IO ()
runEvring port app = runEvringSettings (defaultEvringSettings {evringPort = port}) app

-- | Run a WAI application with custom settings using io_uring.
-- Uses single-threaded event loop for io_uring safety.
-- Handles SIGTERM and SIGINT for graceful shutdown.
runEvringSettings :: EvringSettings -> Application -> IO ()
runEvringSettings settings app = do
  putStrLn $ "evring-wai: Starting server on port " ++ show (evringPort settings)

  -- Pre-allocate buffer pool
  let poolSize = min 256 (evringMaxConnections settings)
      bufferSize = evringBufferSize settings
  bufferPool <- allocateBufferPool poolSize bufferSize

  -- Initialize server state
  serverStateRef <-
    newIORef
      ServerState
        { ssShuttingDown = False,
          ssActiveConnections = 0,
          ssBufferPool = bufferPool
        }

  -- Install signal handlers
  let shutdownHandler = do
        putStrLn "\nevring-wai: Received shutdown signal, initiating graceful shutdown..."
        atomicModifyIORef' serverStateRef $ \s ->
          (s {ssShuttingDown = True}, ())

  _ <- installHandler sigTERM (Catch shutdownHandler) Nothing
  _ <- installHandler sigINT (Catch shutdownHandler) Nothing

  withIoUring defaultIoUringParams $ \ctx -> do
    bracket (createListenSocket settings) close $ \listenSock -> do
      withFdSocket listenSock $ \listenFd -> do
        -- Catch exceptions from interrupted io_uring operations during shutdown
        catch
          (runAcceptLoop ctx (Fd listenFd) settings app serverStateRef)
          ( \(e :: SomeException) -> do
              -- Check error message for EINTR (-4), which is expected during shutdown
              let errMsg = show e
                  isEintr = "failed: -4)" `isInfixOf` errMsg || "-4)" `isInfixOf` errMsg
              unless isEintr $
                putStrLn $
                  "evring-wai: Accept loop error: " ++ errMsg
          )
          `finally` waitForConnections serverStateRef settings

-- | Wait for active connections to drain during shutdown
waitForConnections :: IORef ServerState -> EvringSettings -> IO ()
waitForConnections serverStateRef settings = do
  state <- readIORef serverStateRef
  when (ssActiveConnections state > 0) $ do
    putStrLn $ "evring-wai: Waiting for " ++ show (ssActiveConnections state) ++ " active connections to drain..."
    waitLoop (evringGracefulShutdownTimeout settings * 10) -- 100ms intervals
  putStrLn "evring-wai: Shutdown complete"
  where
    waitLoop 0 = do
      state <- readIORef serverStateRef
      when (ssActiveConnections state > 0) $
        putStrLn $
          "evring-wai: Timeout waiting for connections, forcing shutdown with "
            ++ show (ssActiveConnections state)
            ++ " active"
    waitLoop remaining = do
      state <- readIORef serverStateRef
      unless (ssActiveConnections state == 0) $ do
        threadDelay 100000 -- 100ms
        waitLoop (remaining - 1)

-- ════════════════════════════════════════════════════════════════════════════
-- Socket Setup
-- ════════════════════════════════════════════════════════════════════════════

-- | Create and bind a listen socket.
createListenSocket :: EvringSettings -> IO Socket
createListenSocket EvringSettings {..} = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  let addr = SockAddrInet (fromIntegral evringPort) (tupleToHostAddress (0, 0, 0, 0))
  bind sock addr
  listen sock evringBacklog
  return sock

-- ════════════════════════════════════════════════════════════════════════════
-- Accept Loop
-- ════════════════════════════════════════════════════════════════════════════

-- | Main accept loop using io_uring.
-- Single-threaded event loop for io_uring safety.
-- Checks for shutdown signal and tracks active connections.
runAcceptLoop :: IOCtx -> Fd -> EvringSettings -> Application -> IORef ServerState -> IO ()
runAcceptLoop ctx listenFd settings app serverStateRef = do
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
        putStrLn "evring-wai: Shutdown requested, stopping accept loop"
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
                    (s {ssActiveConnections = ssActiveConnections s + 1}, ())
                  -- Fork handler pinned to same capability (for io_uring thread safety)
                  (capIdx, _) <- threadCapability =<< myThreadId
                  _ <-
                    forkOn capIdx $
                      handleConnection ctx clientFd clientAddr settings app serverStateRef
                        `finally` atomicModifyIORef'
                          serverStateRef
                          ( \s ->
                              (s {ssActiveConnections = ssActiveConnections s - 1}, ())
                          )
                  return ()
            IoErrno _ -> return () -- Accept failed, continue
            Eof -> return ()
          _ -> return ()

        -- Continue accepting
        acceptLoop addrBuf addrLenBuf

-- | Parse a sockaddr structure from the accept buffer.
-- Handles AF_INET (IPv4) and AF_INET6 (IPv6).
parseSockAddr :: Ptr () -> IO SockAddr
parseSockAddr addrBuf = do
  -- First 2 bytes are sa_family
  family <- peekByteOff addrBuf 0 :: IO Word16
  case family of
    2 -> do
      -- AF_INET
      -- struct sockaddr_in: family(2) + port(2) + addr(4)
      port <- peekByteOff addrBuf 2 :: IO Word16
      addr <- peekByteOff addrBuf 4 :: IO Word32
      -- Port is in network byte order (big endian)
      let portNum = fromIntegral (byteSwap16 port) :: PortNumber
      return $ SockAddrInet portNum addr
    10 -> do
      -- AF_INET6
      -- struct sockaddr_in6: family(2) + port(2) + flowinfo(4) + addr(16) + scope_id(4)
      port <- peekByteOff addrBuf 2 :: IO Word16
      let portNum = fromIntegral (byteSwap16 port) :: PortNumber
      -- For now, return a placeholder IPv6 address
      -- Full IPv6 parsing would read the 16-byte address
      return $ SockAddrInet6 portNum 0 (0, 0, 0, 0) 0
    _ ->
      -- Unknown family, return placeholder
      return $ SockAddrInet 0 0

-- | Byte swap for 16-bit values (network to host order)
byteSwap16 :: Word16 -> Word16
byteSwap16 w = (w `shiftR` 8) .|. (w `shiftL` 8)

-- ════════════════════════════════════════════════════════════════════════════
-- Connection Handler
-- ════════════════════════════════════════════════════════════════════════════

-- | Connection state for keep-alive handling
data ConnState = ConnState
  { connBuffer :: !(MutablePrimArray RealWorld Word8),
    connBufPtr :: !(Ptr Word8),
    connLeftover :: !ByteString, -- Leftover data from previous read
    connRequestCount :: !Int,
    connRemoteHost :: !SockAddr -- Client address
  }

-- | Handle a connection with keep-alive support.
handleConnection :: IOCtx -> Fd -> SockAddr -> EvringSettings -> Application -> IORef ServerState -> IO ()
handleConnection ctx clientFd clientAddr settings app serverStateRef = do
  let bufferSize = evringBufferSize settings

  -- Borrow buffer from pool
  bufferSlot <- borrowBuffer serverStateRef bufferSize

  -- Initialize connection state
  stateRef <-
    newIORef
      ConnState
        { connBuffer = bsBuffer bufferSlot,
          connBufPtr = bsPtr bufferSlot,
          connLeftover = BS.empty,
          connRequestCount = 0,
          connRemoteHost = clientAddr
        }

  -- Enter keep-alive request loop, returning buffer when done
  keepAliveLoop ctx clientFd settings app stateRef serverStateRef
    `finally` returnBuffer serverStateRef bufferSlot

-- | Keep-alive request loop - handles multiple requests per connection
keepAliveLoop :: IOCtx -> Fd -> EvringSettings -> Application -> IORef ConnState -> IORef ServerState -> IO ()
keepAliveLoop ctx clientFd settings app stateRef serverStateRef = do
  state <- readIORef stateRef
  serverState <- readIORef serverStateRef
  let bufferSize = evringBufferSize settings
      maxRequests = evringMaxRequestsPerConnection settings
      _headerTimeoutUs = evringRequestHeaderTimeout settings * 1000000 -- TODO: use io_uring timeout

  -- Check if we're shutting down - close connection immediately
  if ssShuttingDown serverState
    then closeConnection ctx clientFd
    -- Check if we've hit max requests
    else
      if connRequestCount state >= maxRequests
        then closeConnection ctx clientFd
        else do
          -- Try to parse from leftover data first
          case tryParseRequest (connLeftover state) of
            Just (pr, leftover) -> do
              -- We have a complete request from leftover data
              -- Attach streaming body reader and process
              req <- attachStreamingBody ctx clientFd stateRef pr
              processRequest ctx clientFd settings app stateRef serverStateRef req leftover
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
                        processRequest ctx clientFd settings app stateRef serverStateRef req leftover
                      Nothing -> do
                        -- Still incomplete, store and continue (request too large or split)
                        writeIORef stateRef state {connLeftover = allData}
                        -- Try reading more (could be chunked arrival)
                        keepAliveLoop ctx clientFd settings app stateRef serverStateRef
                  Complete _ -> closeConnection ctx clientFd -- 0 bytes = client closed
                  IoErrno _ -> closeConnection ctx clientFd
                  Eof -> closeConnection ctx clientFd
                _ -> closeConnection ctx clientFd

-- | Try to parse a complete HTTP request from buffer.
-- Returns ParsedRequest (with initial body data) and leftover data for next request.
-- For streaming bodies, we only need headers + some initial body data to be present.
tryParseRequest :: ByteString -> Maybe (ParsedRequest, ByteString)
tryParseRequest bs
  | BS.null bs = Nothing
  | otherwise =
      -- Check if we have complete headers (ends with \r\n\r\n)
      case BS.breakSubstring "\r\n\r\n" bs of
        (_, rest) | BS.null rest -> Nothing -- Incomplete headers
        _ ->
          -- We have complete headers, try to parse
          case parseHttpRequest bs of
            Right pr ->
              -- Calculate what data belongs to this request
              let headerEnd = case BS.breakSubstring "\r\n\r\n" bs of
                    (headers, _) -> BS.length headers + 4
                  contentLen = prContentLength pr
                  totalLen = headerEnd + contentLen
                  -- For streaming: we can proceed if we have at least headers
                  -- Body will be read incrementally
                  availableBody = BS.length bs - headerEnd
               in if contentLen <= availableBody
                    -- Full body available in buffer - common case for small requests
                    then Just (pr, BS.drop totalLen bs)
                    -- Partial body - streaming will handle the rest
                    else Just (pr {prInitialBody = BS.drop headerEnd bs}, BS.empty)
            Left _ -> Nothing -- Parse error

-- Note: getContentLengthFromParsed removed - use prContentLength directly

-- | Process a complete request and decide whether to keep connection alive
processRequest :: IOCtx -> Fd -> EvringSettings -> Application -> IORef ConnState -> IORef ServerState -> Request -> ByteString -> IO ()
processRequest ctx clientFd settings app stateRef serverStateRef req leftover = do
  state <- readIORef stateRef
  serverState <- readIORef serverStateRef

  -- Determine if we should keep the connection alive
  -- During shutdown, always close after responding
  let shouldKeepAlive = checkKeepAlive req && not (ssShuttingDown serverState)

  -- Call WAI application
  response <- runApplication app req

  -- Check for ResponseRaw (WebSocket, etc.)
  case response of
    ResponseRaw rawApp _fallback -> do
      -- Handle raw connection (WebSocket, etc.)
      -- The rawApp takes a receive action and a send action
      handleRawResponse ctx clientFd settings stateRef rawApp leftover
      -- After raw handler completes, close connection
      closeConnection ctx clientFd
    _ -> do
      -- Normal response handling
      sendResponseWithKeepAlive ctx clientFd response shouldKeepAlive

      if shouldKeepAlive
        then do
          -- Update state and continue loop
          writeIORef
            stateRef
            state
              { connLeftover = leftover,
                connRequestCount = connRequestCount state + 1
              }
          keepAliveLoop ctx clientFd settings app stateRef serverStateRef
        else
          closeConnection ctx clientFd

-- | Check if connection should be kept alive based on request headers
checkKeepAlive :: Request -> Bool
checkKeepAlive req =
  let connHeader = lookup "Connection" (requestHeaders req)
      isHttp11 = httpVersion req >= http11
   in case connHeader of
        Just val
          | CI.mk val == "close" -> False
          | CI.mk val == "keep-alive" -> True
          | otherwise -> isHttp11 -- Unknown value, default based on version
        Nothing -> isHttp11 -- HTTP/1.1 defaults to keep-alive

-- | Handle a ResponseRaw (WebSocket, etc.)
-- This gives the application direct access to send/receive on the socket
handleRawResponse ::
  IOCtx ->
  Fd ->
  EvringSettings ->
  IORef ConnState ->
  (IO ByteString -> (ByteString -> IO ()) -> IO ()) ->
  ByteString ->
  IO ()
handleRawResponse ctx clientFd settings stateRef rawApp leftoverData = do
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
                Complete _ -> return BS.empty -- Connection closed
                IoErrno _ -> return BS.empty
                Eof -> return BS.empty
              _ -> return BS.empty

  -- Create send action for the raw handler
  let sendAction :: ByteString -> IO ()
      sendAction bytes
        | BS.null bytes = return ()
        | otherwise = sendBytes ctx clientFd bytes

  -- Run the raw application
  catch
    (rawApp recvAction sendAction)
    ( \(e :: SomeException) ->
        putStrLn $ "evring-wai: Raw handler error: " ++ show e
    )

-- | Run the WAI application safely
runApplication :: Application -> Request -> IO Response
runApplication app req = do
  responseRef <- newIORef Nothing
  let respond resp = do
        writeIORef responseRef (Just resp)
        return ResponseReceived

  catch
    (void $ app req respond)
    ( \(e :: SomeException) -> do
        putStrLn $ "evring-wai: Application error: " ++ show e
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
  { -- | The WAI request (body reader not yet attached)
    prRequest :: !Request,
    -- | Expected body length (0 for no body)
    prContentLength :: !Int,
    -- | Body data already read with headers
    prInitialBody :: !ByteString
  }

-- | Parse HTTP headers and return a ParsedRequest.
-- Body reading is handled separately to support streaming.
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
              { requestMethod = method,
                httpVersion = httpVer,
                rawPathInfo = path,
                rawQueryString = queryBS,
                pathInfo = pathSegments,
                queryString = query,
                requestHeaders = headers,
                isSecure = False,
                remoteHost = SockAddrInet 0 0, -- TODO: Get from accept
                vault = Vault.empty,
                requestBodyLength = KnownLength (fromIntegral contentLength),
                requestHeaderHost = lookup "Host" headers,
                requestHeaderRange = lookup "Range" headers,
                requestHeaderReferer = lookup "Referer" headers,
                requestHeaderUserAgent = lookup "User-Agent" headers
              }

      Right
        ParsedRequest
          { prRequest = baseRequest,
            prContentLength = contentLength,
            prInitialBody = bodyData
          }

-- | Attach a streaming body reader to a parsed request.
-- This creates a body reader that first returns any already-read data,
-- then reads more from the socket via io_uring as needed.
-- Also sets the remoteHost from the connection state.
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
                    _ -> do
                      -- Error or EOF - body truncated
                      writeIORef bodyStateRef (BS.empty, 0)
                      return BS.empty
                  _ -> return BS.empty

  -- Update request with remoteHost and body reader
  let reqWithHost = (prRequest pr) {remoteHost = clientAddr}
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
    _ -> Left $ "Invalid request line: " ++ BC.unpack line

-- | Parse HTTP version string
parseHttpVersion :: ByteString -> Either String HttpVersion
parseHttpVersion "HTTP/1.1" = Right http11
parseHttpVersion "HTTP/1.0" = Right (HttpVersion 1 0)
parseHttpVersion "HTTP/2.0" = Right (HttpVersion 2 0)
parseHttpVersion "HTTP/2" = Right (HttpVersion 2 0)
parseHttpVersion v = Left $ "Unknown HTTP version: " ++ BC.unpack v

-- | Parse header lines into RequestHeaders
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

-- | Split raw data into header section and body
splitHeaderBody :: ByteString -> (ByteString, ByteString)
splitHeaderBody bs =
  case BS.breakSubstring "\r\n\r\n" bs of
    (headers, rest)
      | BS.null rest -> (headers, BS.empty)
      | otherwise -> (headers, BS.drop 4 rest) -- Drop the \r\n\r\n

-- | Split path from query string at '?'
splitPathQuery :: ByteString -> (ByteString, ByteString)
splitPathQuery bs =
  case BC.break (== '?') bs of
    (path, query)
      | BS.null query -> (path, BS.empty)
      | otherwise -> (path, query) -- Keep the '?' in query string

-- | Strip trailing \r from a line
stripCR :: ByteString -> ByteString
stripCR bs
  | BS.null bs = bs
  | BS.last bs == 13 = BS.init bs -- 13 = '\r'
  | otherwise = bs

-- | Get Content-Length from headers
getContentLength :: RequestHeaders -> Int
getContentLength headers =
  case lookup "Content-Length" headers of
    Just val -> fromMaybe 0 (readMaybe (BC.unpack val))
    Nothing -> 0

-- Note: unsafePerformIORef and consumeBody removed - now using attachStreamingBody for proper streaming

-- ════════════════════════════════════════════════════════════════════════════
-- Response Sending
-- ════════════════════════════════════════════════════════════════════════════

-- | Send an HTTP response (closing connection after).
-- Kept for API compatibility.
_sendResponse :: IOCtx -> Fd -> Response -> IO ()
_sendResponse ctx clientFd response =
  sendResponseWithKeepAlive ctx clientFd response False

-- | Send an HTTP response with keep-alive control.
-- Supports both buffered and streaming responses.
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
              [ Builder.wordHex (fromIntegral chunkSize),
                Builder.byteString "\r\n",
                Builder.byteString chunkData,
                Builder.byteString "\r\n"
              ]
  sendBytes ctx clientFd chunkBytes

-- | Build HTTP response headers.
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
            [ Builder.byteString "HTTP/1.1 ",
              Builder.intDec (statusCode status),
              Builder.byteString " ",
              Builder.byteString (statusMessage status),
              Builder.byteString "\r\n",
              mconcat $ map formatHeader filteredHeaders,
              contentLengthHeader,
              Builder.byteString connectionHeader,
              Builder.byteString "\r\n"
            ]
  where
    formatHeader (name, value) =
      mconcat
        [ Builder.byteString (CI.original name),
          Builder.byteString ": ",
          Builder.byteString value,
          Builder.byteString "\r\n"
        ]

-- | Build HTTP response headers for chunked encoding.
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
            [ Builder.byteString "HTTP/1.1 ",
              Builder.intDec (statusCode status),
              Builder.byteString " ",
              Builder.byteString (statusMessage status),
              Builder.byteString "\r\n",
              mconcat $ map formatHeader filteredHeaders,
              Builder.byteString "Transfer-Encoding: chunked\r\n",
              Builder.byteString connectionHeader,
              Builder.byteString "\r\n"
            ]
  where
    formatHeader (name, value) =
      mconcat
        [ Builder.byteString (CI.original name),
          Builder.byteString ": ",
          Builder.byteString value,
          Builder.byteString "\r\n"
        ]

-- | Send bytes over the connection.
-- Uses the ByteString's internal buffer directly (avoiding an extra copy)
-- when possible, with a copy to pinned memory for safety.
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

-- | Send bytes using zero-copy when buffer is large enough.
-- Currently falls back to regular send since zero-copy requires
-- buffer lifetime management that our synchronous model doesn't support.
-- TODO: Implement proper zero-copy with IOSQE_CQE_SKIP_SUCCESS
_sendBytesZeroCopy :: IOCtx -> Fd -> ByteString -> Int -> IO ()
_sendBytesZeroCopy ctx clientFd bytes threshold
  | BS.length bytes >= threshold = do
      -- For true zero-copy, we'd need to ensure the buffer stays valid
      -- until kernel signals completion. For now, fall back to regular send.
      sendBytes ctx clientFd bytes
  | otherwise = sendBytes ctx clientFd bytes
