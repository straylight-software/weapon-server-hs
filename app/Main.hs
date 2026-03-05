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
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Api
import Bus.Bus qualified as Bus
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (object)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
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
    PendingConnection,
    acceptRequest,
    defaultConnectionOptions,
    pendingRequest,
    receiveData,
    requestPath,
    sendBinaryData,
 )
import Network.WebSockets qualified
import Options.Applicative
import Pty.Connect ()
import Pty.Pty qualified as Pty
import Servant
import Server.ErrorFormatters (errorFormattersContext)
import Server.Run (ServerBackend (..), ServerSettings (..), checkBackendAvailable, defaultServerSettings, runServerWithCleanup)
import State
import System.Directory (XdgDirectory (..), getCurrentDirectory, getXdgDirectory)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import Telemetry.Manager qualified as Telemetry
import Util.Thread (forkLogged)

-- ════════════════════════════════════════════════════════════════════════════
--                                                          // command line args
-- ════════════════════════════════════════════════════════════════════════════

-- | Server options parsed from command line
data ServerOpts = ServerOpts
    { optPort :: !Int
    -- ^ Port to listen on
    , optBackend :: !ServerBackend
    -- ^ HTTP backend (iouring or warp)
    , optPortRetry :: !Int
    -- ^ Number of ports to try if busy (0 = fail immediately)
    , optCores :: !(Maybe Int)
    -- ^ Number of cores (Nothing = all available)
    , optQuiet :: !Bool
    -- ^ Suppress stdout logging (log to file only)
    }
    deriving (Show, Eq)

-- | Parse command line options
parseOpts :: IO ServerOpts
parseOpts =
    execParser $
        info
            (optsParser <**> helper)
            ( fullDesc
                <> header "weapon-server - Weapon HTTP server"
                <> progDesc "Start the Weapon HTTP server with io_uring or warp backend"
            )

optsParser :: Parser ServerOpts
optsParser = do
    optPort <-
        option
            auto
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> value defaultPort
                <> showDefault
                <> help "Port to listen on"
            )
    optBackend <-
        option
            backendReader
            ( long "backend"
                <> metavar "BACKEND"
                <> value BackendIoUring
                <> showDefaultWith showBackend
                <> help "HTTP backend: iouring or warp"
            )
    optPortRetry <-
        option
            auto
            ( long "port-retry"
                <> metavar "N"
                <> value 0
                <> showDefault
                <> help "Number of ports to try if busy (0 = fail immediately)"
            )
    optCores <-
        optional $
            option
                auto
                ( long "cores"
                    <> metavar "N"
                    <> help "Number of cores to use (default: all available)"
                )
    optQuiet <-
        switch
            ( long "quiet"
                <> short 'q'
                <> help "Suppress stdout logging (log to file only)"
            )
    pure ServerOpts{..}

-- | Parse backend from string
backendReader :: ReadM ServerBackend
backendReader = eitherReader $ \s ->
    case s of
        "iouring" -> Right BackendIoUring
        "io_uring" -> Right BackendIoUring
        "warp" -> Right BackendWarp
        _ -> Left $ "Unknown backend: " <> s <> ". Expected: iouring or warp"

-- | Show backend for --help
showBackend :: ServerBackend -> String
showBackend BackendIoUring = "iouring"
showBackend BackendWarp = "warp"

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
                    let wsLogger = Log.withNS (stLogger appState) "websocket"
                    bridgePtyToWebSocket wsLogger ptyConnection websocketConnection

-- | Bidirectional bridge between PTY and WebSocket.
bridgePtyToWebSocket :: Log.Logger -> Pty.PtyConnection -> Network.WebSockets.Connection -> IO ()
bridgePtyToWebSocket logger ptyConnection websocketConnection = do
    -- reader thread: pty -> websocket
    -- If WebSocket send fails, close the PTY - don't continue reading into the void
    _ <- forkLogged logger "pty-websocket-bridge" $ Pty.pcOnData ptyConnection $ \bytes -> do
        result <- try @SomeException $ sendBinaryData websocketConnection bytes
        case result of
            Left _err -> Pty.pcClose ptyConnection
            Right () -> pure ()

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

main :: IO ()
main = do
    ServerOpts{..} <- parseOpts

    -- Configure logging: file always, stdout only if not quiet
    let logConfig =
            (Log.defaultLogConfig "weapon")
                { Log.lcStdout = not optQuiet
                , Log.lcFile = True
                , Log.lcLevel = defaultLogLevel
                }
    Log.withLoggerConfig logConfig $ \logger -> do
        hSetBuffering stdout LineBuffering
        hSetBuffering stderr LineBuffering

        let serverLogger = Log.withNS logger "server"
        Log.logMsg serverLogger Katip.InfoS "initializing weapon server"

        -- Check backend availability FIRST - fail fast if unavailable
        let tempSettings = (defaultServerSettings logger){serverBackend = optBackend}
        available <- checkBackendAvailable tempSettings
        case available of
            Left err -> do
                Log.logMsg serverLogger Katip.ErrorS $ "Backend unavailable: " <> T.pack err
                error err
            Right () -> pure ()

        workingDirectory <- getCurrentDirectory
        -- Use XDG_DATA_HOME/weapon/storage to match TypeScript server
        xdgDataDir <- getXdgDirectory XdgData "weapon"
        let storageDirectory = xdgDataDir </> "storage"
        let projectId = "proj_default"

        appState <- initialState storageDirectory (T.pack projectId) (T.pack workingDirectory) logger
        startPromptAsyncWorker appState

        -- heartbeat thread
        _ <- forkLogged serverLogger "heartbeat-loop" $ heartbeatLoop appState

        Log.logMsg serverLogger Katip.InfoS $ "storage: " <> T.pack storageDirectory

        -- Middleware chain (applied outside-in, so rightmost runs first):
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

        -- Start server with explicit settings (no silent fallback)
        Log.logMsg serverLogger Katip.InfoS $ "starting on port " <> T.pack (show optPort) <> " with " <> T.pack (showBackend optBackend) <> " backend"
        let settings =
                (defaultServerSettings logger)
                    { serverBackend = optBackend
                    , serverPort = optPort
                    , serverPortRetry = optPortRetry
                    , serverCores = optCores
                    }

        -- Cleanup action for graceful shutdown (telemetry flush, etc.)
        let cleanup = case stTelemetry appState of
                Just tm -> do
                    Log.logMsg serverLogger Katip.InfoS "shutting down telemetry"
                    Telemetry.stopManager tm
                    Log.logMsg serverLogger Katip.InfoS "telemetry shutdown complete"
                Nothing -> pure ()

        runServerWithCleanup settings websocketApp cleanup

-- | Periodic heartbeat to keep SSE connections alive.
heartbeatLoop :: AppState -> IO ()
heartbeatLoop appState = do
    threadDelay 10_000_000 -- 10 seconds
    Bus.publish (stBus appState) "server.heartbeat" (object [])
    heartbeatLoop appState
