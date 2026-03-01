-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                     // weapon-server // main
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The matrix has its roots in primitive arcade games," said the voice-over,
--    "in early graphics programs and military experimentation with cranial
--    jacks."
--
--                                                                — Neuromancer
--
-- Entry point for the Weapon Haskell server. Sets up evring-wai (io_uring)
-- with WebSocket support for PTY connections, CORS middleware, and the Servant API.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
import Bus.Bus qualified as Bus
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (object)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Evring.Wai.MultiCore (ServerSettings (..), defaultServerSettings, runServerMultiCore)
import Global.Event ()
import Handlers
import Katip qualified
import Log qualified
import Middleware (
    addAllowHeader,
    limitJsonDepth,
    rejectDoubleEncodedPaths,
    rejectDuplicateQueryParams,
    rejectEmptyPathSegments,
    rejectHeadMethod,
    rejectInvalidCharset,
    rejectInvalidContentType,
    rejectMethodMismatch,
    rejectNullBytePaths,
    rejectUnknownQueryParams,
    rejectUnsupportedMethods,
    requestLogger,
    requireContentType,
    supplyEmptyBody,
 )
import Network.HTTP.Types (methodOptions, status200)
import Network.Wai (Middleware, mapResponseHeaders, requestHeaders, requestMethod, responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (
    Connection,
    PendingConnection,
    acceptRequest,
    defaultConnectionOptions,
    pendingRequest,
    receiveData,
    requestPath,
    sendBinaryData,
 )
import Pty.Connect ()
import Pty.Pty qualified as Pty
import Servant
import Server.ErrorFormatters (errorFormattersContext)
import State
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Read (readMaybe)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                 // middleware
-- ════════════════════════════════════════════════════════════════════════════

{- | CORS middleware for cross-origin requests.
Only responds to actual CORS preflight requests (OPTIONS with Access-Control-Request-Method).
Non-preflight OPTIONS requests pass through to Servant for proper 405 handling.
-}
enableCors :: Middleware
enableCors app req callback
    | requestMethod req == methodOptions && isCorsPreflightRequest =
        callback $ responseLBS status200 corsHeaders ""
    | otherwise =
        app req $ \response ->
            callback $ mapResponseHeaders (<> corsHeaders) response
  where
    -- CORS preflight requests have Access-Control-Request-Method header
    isCorsPreflightRequest =
        any (\(h, _) -> h == "Access-Control-Request-Method") (requestHeaders req)
    corsHeaders =
        [ ("Access-Control-Allow-Origin", "*")
        , ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS, PATCH")
        , ("Access-Control-Allow-Headers", "Authorization, Content-Type, x-weapon-directory")
        ]

-- ════════════════════════════════════════════════════════════════════════════
--                                                                 // websocket
-- ════════════════════════════════════════════════════════════════════════════

{- | WebSocket handler for PTY connections.

Bridges WebSocket I/O to PTY sessions, enabling terminal access from
browser clients.
-}
ptyWebSocketApp :: AppState -> PendingConnection -> IO ()
ptyWebSocketApp appState pending = do
    let path = requestPath (pendingRequest pending)
        pathParts = BS.split (fromIntegral (fromEnum '/')) path
        -- path should be /pty/{ptyId}/connect
        maybePtyId = case pathParts of
            [_root, "pty", ptyIdBytes, "connect"] -> Just (TE.decodeUtf8 ptyIdBytes)
            _otherParts -> Nothing

    case maybePtyId of
        Nothing -> pure ()
        Just ptyId -> do
            maybeConnection <- Pty.connect (stPtyManager appState) ptyId Nothing
            case maybeConnection of
                Nothing -> pure ()
                Just ptyConnection -> do
                    websocketConnection <- acceptRequest pending
                    bridgePtyToWebSocket ptyConnection websocketConnection

-- | Bidirectional bridge between PTY and WebSocket.
bridgePtyToWebSocket :: Pty.PtyConnection -> Network.WebSockets.Connection -> IO ()
bridgePtyToWebSocket ptyConnection websocketConnection = do
    -- reader thread: pty -> websocket
    void $ forkIO $ Pty.pcOnData ptyConnection $ \bytes -> do
        void $ try @SomeException $ sendBinaryData websocketConnection bytes

    -- writer loop: websocket -> pty
    let loop = do
            result <- try @SomeException $ receiveData websocketConnection
            case result of
                Left _err -> Pty.pcClose ptyConnection
                Right bytes -> do
                    Pty.pcSend ptyConnection bytes
                    loop
    loop

-- ════════════════════════════════════════════════════════════════════════════
--                                                                      // main
-- ════════════════════════════════════════════════════════════════════════════

-- | Default log level based on build mode
defaultLogLevel :: Katip.Severity
#ifdef PRODUCTION
defaultLogLevel = Katip.InfoS
#else
defaultLogLevel = Katip.DebugS
#endif

-- | Default port for the server
defaultPort :: Int
defaultPort = 4096

-- | Parse command line arguments for port
parseArgs :: [String] -> Int
parseArgs [] = defaultPort
parseArgs ("--port" : portStr : _) = fromMaybe defaultPort (readMaybe portStr)
parseArgs ("-p" : portStr : _) = fromMaybe defaultPort (readMaybe portStr)
parseArgs (_ : rest) = parseArgs rest

main :: IO ()
main = Log.withLoggerLevel "weapon" defaultLogLevel $ \logger -> do
    hSetBuffering stdout LineBuffering

    args <- getArgs
    let requestedPort = parseArgs args

    let serverLogger = Log.withNS logger "server"
    Log.logMsg serverLogger Katip.InfoS "initializing weapon server"

    workingDirectory <- getCurrentDirectory
    let storageDirectory = workingDirectory </> ".weapon" </> "storage"
    let projectId = "proj_default"

    appState <- initialState storageDirectory (T.pack projectId) (T.pack workingDirectory) logger
    startPromptAsyncWorker appState

    -- heartbeat thread
    _ <- forkIO $ heartbeatLoop appState

    Log.logMsg serverLogger Katip.InfoS $ "storage: " <> T.pack storageDirectory

    -- Middleware chain (applied outside-in, so rightmost runs first):
    -- 1. serve api (Servant handles request)
    -- 2. supplyEmptyBody (provides {} for empty POST/PUT/PATCH/DELETE)
    -- 3. requireContentType (reject body requests without Content-Type)
    -- 4. rejectInvalidContentType (reject text/json, application/x-invalid, etc.)
    -- 5. limitJsonDepth (reject deeply nested JSON - DoS protection)
    -- 6. rejectInvalidCharset (only UTF-8 allowed)
    -- 7. enableCors (add CORS headers for preflight, pass others through)
    -- 8. rejectUnknownQueryParams (strict parameter validation)
    -- 9. rejectDuplicateQueryParams (prevent parameter pollution)
    -- 10. rejectEmptyPathSegments (reject /session/ style paths)
    -- 11. rejectNullBytePaths (reject null byte injection)
    -- 12. rejectDoubleEncodedPaths (reject path traversal via double encoding)
    -- 13. rejectMethodMismatch (405 for wrong method on known routes)
    -- 14. rejectUnsupportedMethods (405 for OPTIONS, TRACE, CONNECT, etc.)
    -- 15. rejectHeadMethod (HEAD not in OpenAPI spec)
    -- 16. addAllowHeader (RFC 9110: Allow header on 405)
    -- 17. requestLogger (log all requests)
    let servantApp =
            requestLogger logger $
                addAllowHeader $
                    rejectHeadMethod $
                        rejectUnsupportedMethods $
                            rejectMethodMismatch $
                                rejectDoubleEncodedPaths $
                                    rejectNullBytePaths $
                                        rejectEmptyPathSegments $
                                            rejectDuplicateQueryParams $
                                                rejectUnknownQueryParams $
                                                    enableCors $
                                                        rejectInvalidCharset $
                                                            limitJsonDepth $
                                                                rejectInvalidContentType $
                                                                    requireContentType $
                                                                        supplyEmptyBody $
                                                                            serveWithContext api errorFormattersContext (server appState)
        websocketApp = websocketsOr defaultConnectionOptions (ptyWebSocketApp appState) servantApp

    -- Start server with retry logic handled by MultiCore
    Log.logMsg serverLogger Katip.InfoS $ "attempting to listen on port " <> T.pack (show requestedPort)
    let settings = defaultServerSettings{serverPort = requestedPort, serverPortRetry = 10, serverCores = Just 8}
    runServerMultiCore settings websocketApp

-- | Periodic heartbeat to keep SSE connections alive.
heartbeatLoop :: AppState -> IO ()
heartbeatLoop appState = do
    threadDelay 10_000_000 -- 10 seconds
    Bus.publish (stBus appState) "server.heartbeat" (object [])
    heartbeatLoop appState
