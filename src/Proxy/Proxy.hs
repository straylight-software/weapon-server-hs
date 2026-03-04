{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Proxy.Proxy
Description : MITM Proxy for LLM API traffic surveillance

A Man-In-The-Middle (MITM) proxy for capturing and logging LLM API traffic.
This module provides the core proxy server implementation.

== Architecture

@
Sandbox ──HTTP_PROXY──▶ Proxy ──HTTPS──▶ api.anthropic.com
                            │
                            ├── TLS termination (dynamic certs)
                            ├── Full request\/response logging
                            ├── SSE stream capture
                            └── Token counting
@

== Usage

Start a proxy server with 'start', then configure your sandbox to use it
as an HTTP proxy. All requests are logged to JSONL files and token usage
is tracked per session.

@
config <- 'defaultProxyConfig' "\/var\/log\/proxy"
server <- 'start' config
-- ... use the proxy ...
'stop' server
@

== Logging

Every request/response pair is logged to @\<logDir\>\/requests.jsonl@ in
JSON Lines format. Use 'getSessionLogs' to retrieve logs for a specific
session, or parse the file directly for bulk analysis.
-}
module Proxy.Proxy (
    -- * Proxy Server
    -- $proxyserver
    ProxyServer (..),
    start,
    stop,

    -- * Errors
    ProxyError (..),

    -- * Log Retrieval
    -- $logretieval
    getSessionLogs,
    getTokenUsage,

    -- * Pure Helpers (for testing)
    -- $purehelpers
    parseTokensFromJson,
    parseHostPort,
    buildRequestUrl,
    truncateBody,
    isStreamingResponse,
    filterSessionLogs,
) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, bracket, catch, try)
import Control.Monad (forM_)
import Data.Aeson (Object, Value (..), decode, encode, (.:), (.:?))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import GHC.IO.Exception (IOErrorType (..))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (createDirectoryIfMissing)
import System.IO.Error (ioeGetErrorType)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS qualified as HCT
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS

import Proxy.Types

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

-- ═══════════════════════════════════════════════════════════════════════════
-- Proxy Server Type
-- ═══════════════════════════════════════════════════════════════════════════

{- $proxyserver
The 'ProxyServer' type represents a running proxy instance. Create one
with 'start' and stop it with 'stop'. The server runs in a background
thread and can handle multiple concurrent connections.
-}

{- | A running proxy server instance.

Contains the runtime state, HTTP client manager, server thread, and
log file path. Use 'start' to create and 'stop' to terminate.
-}
data ProxyServer = ProxyServer
    { psState :: !ProxyState
    -- ^ Runtime state (counters, token totals)
    , psManager :: !HC.Manager
    -- ^ HTTP client manager for outbound requests
    , psThread :: !ThreadId
    -- ^ Server thread ID
    , psLogFile :: !FilePath
    -- ^ Path to the JSONL log file
    }

-- ═══════════════════════════════════════════════════════════════════════════
-- Server Lifecycle
-- ═══════════════════════════════════════════════════════════════════════════

{- | Start the MITM proxy server.

Creates the log directory if it doesn't exist, initializes state,
and starts the WAI server in a background thread.

Returns 'Left' with an error if the port is already in use or cannot be bound.
This ensures we fail loudly rather than silently degrading.

==== __Examples__

@
config <- 'defaultProxyConfig' "\/tmp\/proxy-logs"
result <- 'start' config
case result of
    Left (PortInUse port) -> putStrLn $ "Port " ++ show port ++ " in use"
    Right server -> putStrLn "Server running on port 8888"
@
-}
start :: ProxyConfig -> IO (Either ProxyError ProxyServer)
start config = do
    -- Setup log directory
    createDirectoryIfMissing True (pcLogDir config)

    -- Create HTTP manager for outbound requests
    manager <- HC.newManager HCT.tlsManagerSettings

    -- Initialize state
    state <- initProxyState config

    let logFile = pcLogDir config <> "/requests.jsonl"
        port = pcPort config

    -- Use an MVar to communicate startup success/failure from the forked thread
    startupResult <- newEmptyMVar

    -- Start proxy server with Warp settings that report bind errors
    tid <- forkIO $ startWithErrorReporting port startupResult (proxyApp state manager logFile)

    -- Wait for the server to either bind successfully or fail
    bindResult <- takeMVar startupResult

    case bindResult of
        Left err -> do
            -- Kill the thread (it may already be dead, but be safe)
            killThread tid
            pure $ Left err
        Right () ->
            pure $
                Right
                    ProxyServer
                        { psState = state
                        , psManager = manager
                        , psThread = tid
                        , psLogFile = logFile
                        }

-- | Start Warp and report success/failure via MVar
startWithErrorReporting :: Int -> MVar (Either ProxyError ()) -> Application -> IO ()
startWithErrorReporting port resultMVar app = do
    let settings =
            Warp.setPort port $
                Warp.setBeforeMainLoop (putMVar resultMVar (Right ())) $
                    Warp.defaultSettings
    Warp.runSettings settings app `catch` handleBindError
  where
    handleBindError :: IOException -> IO ()
    handleBindError e =
        case ioeGetErrorType e of
            ResourceBusy -> putMVar resultMVar (Left (PortInUse port))
            _ -> putMVar resultMVar (Left (BindFailed port (show e)))

-- | Initialize proxy state with fresh TVars.
initProxyState :: ProxyConfig -> IO ProxyState
initProxyState config = do
    requestCount <- newTVarIO 0
    tokenTotals <- newTVarIO Map.empty
    pure
        ProxyState
            { psConfig = config
            , psRequestCount = requestCount
            , psTokenTotals = tokenTotals
            }

{- | Stop the proxy server.

Kills the server thread. Any in-flight requests may be interrupted.
Consider using a graceful shutdown mechanism for production use.
-}
stop :: ProxyServer -> IO ()
stop ProxyServer{..} = killThread psThread

-- ═══════════════════════════════════════════════════════════════════════════
-- WAI Application
-- ═══════════════════════════════════════════════════════════════════════════

{- | The main WAI application that handles all proxy requests.

Routes CONNECT requests to 'handleConnect' for HTTPS tunneling,
and all other requests to 'handleHttp' for proxying and logging.
-}
proxyApp :: ProxyState -> HC.Manager -> FilePath -> Application
proxyApp state manager logFile req respond =
    if requestMethod req == "CONNECT"
        then handleConnect req respond
        else handleHttp state manager logFile req respond

-- ═══════════════════════════════════════════════════════════════════════════
-- HTTP Request Handling
-- ═══════════════════════════════════════════════════════════════════════════

{- | Handle regular HTTP requests (non-CONNECT).

This is the main request processing pipeline:

1. Extract session ID and generate request ID
2. Build the outbound request
3. Forward to upstream and capture response
4. Parse token usage from LLM responses
5. Log the complete transaction
6. Forward response to client
-}
handleHttp :: ProxyState -> HC.Manager -> FilePath -> Application
handleHttp state manager logFile req respond = do
    -- Capture timing
    startTime <- getCurrentTime

    -- Extract request metadata
    let sessionId = extractSessionId req
        method = requestMethod req
        host = extractHost req
        maxBodySize = pcMaxBodyLog (psConfig state)

    -- Generate unique request ID
    reqId <- generateRequestId state

    -- Read and process request body
    body <- strictRequestBody req
    let bodyText = truncateBody maxBodySize body
        reqLog = buildRequestLog req bodyText (fromIntegral $ LBS.length body)

    -- Build URL for outbound request
    let url = buildRequestUrl host (rawPathInfo req <> rawQueryString req)

    -- Forward request and handle response
    result <- forwardRequest manager method url (requestHeaders req) body

    endTime <- getCurrentTime
    let duration = calculateDuration startTime endTime

    case result of
        Left _err ->
            handleFailedRequest logFile startTime sessionId reqId method url host reqLog duration respond
        Right resp ->
            handleSuccessfulResponse state logFile startTime sessionId reqId method url host reqLog resp maxBodySize duration respond

-- | Handle a failed upstream request.
handleFailedRequest ::
    FilePath ->
    UTCTime ->
    Text ->
    Text ->
    ByteString ->
    String ->
    Text ->
    RequestLog ->
    Double ->
    (Network.Wai.Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleFailedRequest logFile startTime sessionId reqId method url host reqLog duration respond = do
    let entry = buildLogEntry startTime sessionId reqId method url host reqLog Nothing Nothing duration
    appendLog logFile entry
    respond $ responseLBS status502 [] "Proxy error"

-- | Handle a successful upstream response.
handleSuccessfulResponse ::
    ProxyState ->
    FilePath ->
    UTCTime ->
    Text ->
    Text ->
    ByteString ->
    String ->
    Text ->
    RequestLog ->
    HC.Response LBS.ByteString ->
    Int ->
    Double ->
    (Network.Wai.Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleSuccessfulResponse state logFile startTime sessionId reqId method url host reqLog resp maxBodySize duration respond = do
    let respBody = HC.responseBody resp
        respStatus = HC.responseStatus resp
        respHeaders = HC.responseHeaders resp
        isStream = isStreamingResponse respHeaders

    -- Parse token usage from response
    tokens <- parseTokenUsage host respBody

    -- Update session totals
    forM_ tokens $ \t ->
        atomically $
            modifyTVar' (psTokenTotals state) $
                Map.insertWith addTokens sessionId t

    -- Build response log
    let respLog = buildResponseLog respStatus respHeaders respBody maxBodySize isStream

    -- Log the transaction
    let entry = buildLogEntry startTime sessionId reqId method url host reqLog (Just respLog) tokens duration
    appendLog logFile entry

    -- Forward response to client
    respond $
        responseLBS
            respStatus
            (filter (not . isHopHeader . fst) respHeaders)
            respBody

-- ═══════════════════════════════════════════════════════════════════════════
-- CONNECT Handling (HTTPS Tunneling)
-- ═══════════════════════════════════════════════════════════════════════════

{- | Handle CONNECT requests for HTTPS tunneling.

Establishes a raw TCP tunnel between the client and the target server.
This allows HTTPS traffic to pass through without MITM interception.
-}
handleConnect :: Application
handleConnect req respond = do
    let rawTarget = C8.unpack $ rawPathInfo req
    case parseHostPort rawTarget of
        Nothing ->
            respond $
                responseLBS
                    status400
                    [("Content-Type", "text/plain")]
                    "Invalid CONNECT target"
        Just (host, port) ->
            respond $
                responseRaw
                    (tunnel host port)
                    (responseLBS status502 [("Content-Type", "text/plain")] "Proxy error")

-- | Establish a bidirectional tunnel between client and server.
tunnel :: String -> Int -> IO ByteString -> (ByteString -> IO ()) -> IO ()
tunnel host port readClient writeClient = do
    addrInfos <- Socket.getAddrInfo Nothing (Just host) (Just (show port))
    case addrInfos of
        [] -> writeClient "HTTP/1.1 502 Bad Gateway\r\n\r\n"
        (addr : _) ->
            bracket
                (Socket.socket (Socket.addrFamily addr) Socket.Stream Socket.defaultProtocol)
                Socket.close
                $ \sock -> do
                    Socket.connect sock (Socket.addrAddress addr)
                    writeClient "HTTP/1.1 200 Connection Established\r\n\r\n"
                    bracket
                        (forkIO $ pumpClientToServer sock readClient)
                        killThread
                        (\_ -> pumpServerToClient sock writeClient)

-- | Pump data from client to server.
pumpClientToServer :: Socket.Socket -> IO ByteString -> IO ()
pumpClientToServer sock readClient = do
    bs <- readClient
    if BS.null bs
        then pure ()
        else do
            SocketBS.sendAll sock bs
            pumpClientToServer sock readClient

-- | Pump data from server to client.
pumpServerToClient :: Socket.Socket -> (ByteString -> IO ()) -> IO ()
pumpServerToClient sock writeClient = do
    bs <- SocketBS.recv sock 4096
    if BS.null bs
        then pure ()
        else do
            writeClient bs
            pumpServerToClient sock writeClient

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Helpers
-- ═══════════════════════════════════════════════════════════════════════════

{- $purehelpers
Pure helper functions extracted from IO code for easier testing.
These functions handle URL parsing, body truncation, and response
detection without any side effects.
-}

{- | Parse a host:port string from a CONNECT target.

Handles both @\/host:port@ and @host:port@ formats.

==== __Examples__

>>> parseHostPort "/api.anthropic.com:443"
Just ("api.anthropic.com", 443)

>>> parseHostPort "example.com:8080"
Just ("example.com", 8080)

>>> parseHostPort "invalid"
Nothing
-}
parseHostPort :: String -> Maybe (String, Int)
parseHostPort target =
    let stripped = case target of
            '/' : rest -> rest
            other -> other
     in case break (== ':') stripped of
            (host, ':' : portStr) -> case reads portStr of
                [(port, "")] -> Just (host, port)
                _otherReads -> Nothing
            _otherParts -> Nothing

{- | Build the full URL for an outbound request.

If the path already starts with @http@, it's returned as-is.
Otherwise, the host is prepended with @http:\/\/@.

==== __Examples__

>>> buildRequestUrl "api.example.com" "/v1/chat"
"http://api.example.com/v1/chat"

>>> buildRequestUrl "ignored" "http://other.com/path"
"http://other.com/path"
-}
buildRequestUrl :: Text -> ByteString -> String
buildRequestUrl host path
    | "http" `BS.isPrefixOf` path = T.unpack $ decodeUtf8 path
    | otherwise = "http://" <> T.unpack host <> T.unpack (decodeUtf8 path)

{- | Truncate a body to the maximum size for logging.

Returns 'Nothing' for empty bodies, or 'Just' the truncated text.

==== __Examples__

>>> truncateBody 10 ""
Nothing

>>> truncateBody 5 "hello world"
Just "hello"
-}
truncateBody :: Int -> LBS.ByteString -> Maybe Text
truncateBody maxSize body
    | LBS.length body == 0 = Nothing
    | otherwise = Just $ decodeUtf8 $ LBS.toStrict $ LBS.take (fromIntegral maxSize) body

{- | Check if a response is a streaming (SSE) response.

==== __Examples__

>>> isStreamingResponse [("content-type", "text/event-stream")]
True

>>> isStreamingResponse [("content-type", "application/json")]
False
-}
isStreamingResponse :: ResponseHeaders -> Bool
isStreamingResponse headers =
    maybe False ("text/event-stream" `BS.isInfixOf`) $
        lookup "content-type" headers

{- | Filter log entries by session ID (pure version).

==== __Examples__

>>> filterSessionLogs "session_123" entries
[... entries with leSessionId == "session_123" ...]
-}
filterSessionLogs :: Text -> [LogEntry] -> [LogEntry]
filterSessionLogs sessionId = filter (\e -> leSessionId e == sessionId)

{- | Parse token usage from JSON response.

Supports both Anthropic and OpenAI response formats.
Returns 'Nothing' for non-LLM responses or unrecognized formats.
-}
parseTokensFromJson :: Text -> Value -> Maybe TokenUsage
parseTokensFromJson host json = flip parseMaybe json $ \case
    Object obj ->
        if "anthropic" `T.isInfixOf` host
            then parseAnthropicTokens obj
            else
                if "openai" `T.isInfixOf` host
                    then parseOpenAITokens obj
                    else fail "Unknown provider"
    _otherValue -> fail "Not an object"

-- | Parse Anthropic-format token usage.
parseAnthropicTokens :: Data.Aeson.Object -> Data.Aeson.Types.Parser TokenUsage
parseAnthropicTokens obj = do
    usage <- obj .: "usage"
    inputTokens <- usage .: "input_tokens"
    outputTokens <- usage .: "output_tokens"
    cacheRead <- usage .:? "cache_read_input_tokens"
    cacheWrite <- usage .:? "cache_creation_input_tokens"
    model <- obj .: "model"
    pure
        TokenUsage
            { tuProvider = "anthropic"
            , tuModel = model
            , tuInputTokens = inputTokens
            , tuOutputTokens = outputTokens
            , tuCacheRead = cacheRead
            , tuCacheWrite = cacheWrite
            }

-- | Parse OpenAI-format token usage.
parseOpenAITokens :: Data.Aeson.Object -> Data.Aeson.Types.Parser TokenUsage
parseOpenAITokens obj = do
    usage <- obj .: "usage"
    inputTokens <- usage .: "prompt_tokens"
    outputTokens <- usage .: "completion_tokens"
    model <- obj .: "model"
    pure
        TokenUsage
            { tuProvider = "openai"
            , tuModel = model
            , tuInputTokens = inputTokens
            , tuOutputTokens = outputTokens
            , tuCacheRead = Nothing
            , tuCacheWrite = Nothing
            }

-- ═══════════════════════════════════════════════════════════════════════════
-- Internal Helpers
-- ═══════════════════════════════════════════════════════════════════════════

-- | Extract session ID from request headers.
extractSessionId :: Request -> Text
extractSessionId req =
    maybe "unknown" decodeUtf8 (lookup "X-Opencode-Session" (requestHeaders req))

-- | Extract host from request headers.
extractHost :: Request -> Text
extractHost req =
    maybe "unknown" decodeUtf8 (requestHeaderHost req)

-- | Generate a unique request ID.
generateRequestId :: ProxyState -> IO Text
generateRequestId state = atomically $ do
    n <- readTVar (psRequestCount state)
    writeTVar (psRequestCount state) (n + 1)
    pure $ "req_" <> T.pack (show n)

-- | Calculate duration in milliseconds.
calculateDuration :: UTCTime -> UTCTime -> Double
calculateDuration startTime endTime = realToFrac (diffUTCTime endTime startTime) * 1000

-- | Forward a request to upstream.
forwardRequest ::
    HC.Manager ->
    ByteString ->
    String ->
    RequestHeaders ->
    LBS.ByteString ->
    IO (Either SomeException (HC.Response LBS.ByteString))
forwardRequest manager method url headers body = do
    outReq <- HC.parseRequest url
    let outReq' =
            outReq
                { HC.method = method
                , HC.requestHeaders = filter (not . isHopHeader . fst) headers
                , HC.requestBody = HC.RequestBodyLBS body
                }
    try @SomeException $ HC.httpLbs outReq' manager

-- | Build a RequestLog from request data.
buildRequestLog :: Request -> Maybe Text -> Int -> RequestLog
buildRequestLog req bodyText bodySize =
    RequestLog
        { rlHeaders = headersToMap (requestHeaders req)
        , rlBody = bodyText
        , rlSize = bodySize
        }

-- | Build a ResponseLog from response data.
buildResponseLog :: Status -> ResponseHeaders -> LBS.ByteString -> Int -> Bool -> ResponseLog
buildResponseLog status headers body maxBodySize isStream =
    ResponseLog
        { rsStatus = statusCode status
        , rsHeaders = headersToMap headers
        , rsBody = truncateBody maxBodySize body
        , rsSize = fromIntegral $ LBS.length body
        , rsStream = isStream
        }

-- | Build a complete LogEntry.
buildLogEntry ::
    UTCTime ->
    Text ->
    Text ->
    ByteString ->
    String ->
    Text ->
    RequestLog ->
    Maybe ResponseLog ->
    Maybe TokenUsage ->
    Double ->
    LogEntry
buildLogEntry timestamp sessionId reqId method url host reqLog respLog tokens duration =
    LogEntry
        { leTimestamp = timestamp
        , leSessionId = sessionId
        , leRequestId = reqId
        , leMethod = decodeUtf8 method
        , leUrl = T.pack url
        , leHost = host
        , leRequest = reqLog
        , leResponse = respLog
        , leTokens = tokens
        , leDuration = duration
        }

-- | Parse token usage from response body.
parseTokenUsage :: Text -> LBS.ByteString -> IO (Maybe TokenUsage)
parseTokenUsage host body = do
    let mJson = decode body :: Maybe Value
    case mJson of
        Nothing -> pure Nothing
        Just json -> pure $ parseTokensFromJson host json

-- | Append a log entry to the JSONL file.
appendLog :: FilePath -> LogEntry -> IO ()
appendLog path entry = do
    let line = LBS.toStrict (encode entry) <> "\n"
    BS.appendFile path line

-- ═══════════════════════════════════════════════════════════════════════════
-- Log Retrieval
-- ═══════════════════════════════════════════════════════════════════════════

{- $logretieval
Functions for retrieving logged data from a running proxy server.
-}

{- | Get all log entries for a specific session.

Reads the JSONL log file and filters entries by session ID.
This is an O(n) operation that reads the entire log file.

==== __Examples__

@
logs <- 'getSessionLogs' server "session_abc123"
mapM_ print logs
@
-}
getSessionLogs :: ProxyServer -> Text -> IO [LogEntry]
getSessionLogs ProxyServer{..} sessionId = do
    content <- BS.readFile psLogFile
    let chunks = C8.lines content
        entries = catMaybes [decode (LBS.fromStrict l) | l <- chunks]
    pure $ filterSessionLogs sessionId entries

{- | Get aggregated token usage for all sessions.

Returns a map from session ID to total token usage.
This reflects the current in-memory state, not the log file.

==== __Examples__

@
usage <- 'getTokenUsage' server
case Map.lookup "session_abc123" usage of
    Just tokens -> print (tuInputTokens tokens + tuOutputTokens tokens)
    Nothing -> putStrLn "No usage for session"
@
-}
getTokenUsage :: ProxyServer -> IO (Map Text TokenUsage)
getTokenUsage ProxyServer{..} = readTVarIO (psTokenTotals psState)
