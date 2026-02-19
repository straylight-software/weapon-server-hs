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
-- Entry point for the Weapon Haskell server. Sets up Warp with WebSocket
-- support for PTY connections, CORS middleware, and the Servant API.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
import Bus.Bus qualified as Bus
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (object)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Global.Event ()
import Handlers
import Katip qualified
import Log qualified
import Middleware (supplyEmptyBody)
import Network.HTTP.Types (methodOptions, status200)
import Network.Wai (Middleware, mapResponseHeaders, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
    ( Connection
    , PendingConnection
    , acceptRequest
    , defaultConnectionOptions
    , pendingRequest
    , receiveData
    , requestPath
    , sendBinaryData
    )
import Pty.Connect ()
import Pty.Pty qualified as Pty
import Servant
import State
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stdout)


-- ════════════════════════════════════════════════════════════════════════════
--                                                                 // middleware
-- ════════════════════════════════════════════════════════════════════════════

-- | CORS middleware for cross-origin requests.
enableCors :: Middleware
enableCors app req callback
    | requestMethod req == methodOptions =
        callback $ responseLBS status200 corsHeaders ""
    | otherwise =
        app req $ \response ->
            callback $ mapResponseHeaders (<> corsHeaders) response
  where
    corsHeaders =
        [ ("Access-Control-Allow-Origin", "*")
        , ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS, PATCH")
        , ("Access-Control-Allow-Headers", "Authorization, Content-Type, x-weapon-directory")
        ]

-- ════════════════════════════════════════════════════════════════════════════
--                                                                 // websocket
-- ════════════════════════════════════════════════════════════════════════════

-- | WebSocket handler for PTY connections.
--
-- Bridges WebSocket I/O to PTY sessions, enabling terminal access from
-- browser clients.
ptyWebSocketApp :: AppState -> PendingConnection -> IO ()
ptyWebSocketApp appState pending = do
    let path = requestPath (pendingRequest pending)
        pathParts = BS.split (fromIntegral (fromEnum '/')) path
        -- path should be /pty/{ptyId}/connect
        maybePtyId = case pathParts of
            [_, "pty", ptyIdBytes, "connect"] -> Just (TE.decodeUtf8 ptyIdBytes)
            _ -> Nothing

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
                Left _ -> Pty.pcClose ptyConnection
                Right bytes -> do
                    Pty.pcSend ptyConnection bytes
                    loop
    loop


-- ════════════════════════════════════════════════════════════════════════════
--                                                                      // main
-- ════════════════════════════════════════════════════════════════════════════

main :: IO ()
main = Log.withLogger "weapon" $ \logger -> do
    hSetBuffering stdout LineBuffering

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
    Log.logMsg serverLogger Katip.InfoS "listening on port 4096"

    let servantApp = enableCors $ supplyEmptyBody $ serve api (server appState)
        websocketApp = websocketsOr defaultConnectionOptions (ptyWebSocketApp appState) servantApp

    run 4096 websocketApp

-- | Periodic heartbeat to keep SSE connections alive.
heartbeatLoop :: AppState -> IO ()
heartbeatLoop appState = do
    threadDelay 10_000_000  -- 10 seconds
    Bus.publish (stBus appState) "server.heartbeat" (object [])
    heartbeatLoop appState
