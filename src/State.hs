{-# LANGUAGE OverloadedStrings #-}

-- | Application state
module State (
    AppState (..),
    initialState,
    initialStateNoProxy,
    initialStateNoProxyWithHome,

    -- * Agent tracking
    registerAgent,
    unregisterAgent,
    abortAgent,
) where

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.STM
import Data.Aeson (Value, toJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Bus.Bus qualified as Bus
import Data.Text qualified as Text
import Katip qualified
import Log qualified
import Prompt.Async (PromptAsyncJob)
import Proxy.Proxy qualified as Proxy
import Proxy.Types (defaultProxyConfig)
import Pty.Pty qualified as Pty
import Storage.Storage qualified as Storage
import Util.Identifier qualified as Identifier

-- | Global Application State
data AppState = AppState
    { stBus :: Bus.Bus
    , stStorage :: Storage.StorageConfig
    , stProjectID :: Text
    , stDirectory :: Text
    , stVersion :: Text
    , stEventChan :: TChan Value -- Raw SSE channel for backwards compat
    , stPtyManager :: Pty.PtyManager -- PTY session manager
    , stProxy :: Maybe Proxy.ProxyServer -- MITM proxy for LLM traffic
    , stLogger :: Log.Logger -- Structured logger
    , stPromptAsyncQueue :: TQueue PromptAsyncJob -- prompt_async worker queue
    , stHomeDir :: Maybe FilePath -- Override home directory for config (tests)
    , stActiveAgents :: TVar (Map Text ThreadId) -- Active agent threads by session ID
    , stIdGen :: Identifier.IdGenState -- Monotonic ID generator state
    }

-- | Initialize a new state with optional proxy and home directory override
mkAppState :: Maybe Proxy.ProxyServer -> Maybe FilePath -> FilePath -> Text -> Text -> Log.Logger -> IO AppState
mkAppState proxy homeDir storageDir projectID directory logger = do
    bus <- Bus.newBus
    eventChan <- newBroadcastTChanIO
    ptyManager <- Pty.newManager (Text.unpack directory)
    promptQueue <- newTQueueIO
    activeAgents <- newTVarIO Map.empty
    idGen <- Identifier.newIdGenState

    -- Subscribe bus to also write to event channel for SSE
    _ <- Bus.subscribeAll bus $ \event -> do
        Log.logMsg logger Katip.InfoS $ "State: bus->eventChan forwarding: " <> Bus.beType event
        atomically $ writeTChan eventChan (toJSON event)

    pure $
        AppState
            { stBus = bus
            , stStorage = Storage.StorageConfig storageDir
            , stProjectID = projectID
            , stDirectory = directory
            , stVersion = "0.1.0"
            , stEventChan = eventChan
            , stPtyManager = ptyManager
            , stProxy = proxy
            , stLogger = logger
            , stPromptAsyncQueue = promptQueue
            , stHomeDir = homeDir
            , stActiveAgents = activeAgents
            , stIdGen = idGen
            }

-- | Initialize a new state with MITM proxy
initialState :: FilePath -> Text -> Text -> Log.Logger -> IO AppState
initialState storageDir projectID directory logger = do
    -- Start MITM proxy for LLM traffic surveillance
    let proxyLogDir = storageDir <> "/proxy"
    proxy <- Proxy.start (defaultProxyConfig proxyLogDir)
    mkAppState (Just proxy) Nothing storageDir projectID directory logger

{- | Initialize state without starting the MITM proxy (for tests)
Takes an optional home directory override for config isolation
-}
initialStateNoProxy :: FilePath -> Text -> Text -> Log.Logger -> IO AppState
initialStateNoProxy = initialStateNoProxyWithHome Nothing

-- | Initialize state without proxy, with optional home directory override
initialStateNoProxyWithHome :: Maybe FilePath -> FilePath -> Text -> Text -> Log.Logger -> IO AppState
initialStateNoProxyWithHome = mkAppState Nothing

-- ═══════════════════════════════════════════════════════════════════════════
-- Agent Thread Tracking
-- ═══════════════════════════════════════════════════════════════════════════

-- | Register an agent thread for a session (allows abort)
registerAgent :: AppState -> Text -> ThreadId -> IO ()
registerAgent st sid tid =
    atomically $ modifyTVar' (stActiveAgents st) (Map.insert sid tid)

-- | Unregister an agent thread when it completes
unregisterAgent :: AppState -> Text -> IO ()
unregisterAgent st sid =
    atomically $ modifyTVar' (stActiveAgents st) (Map.delete sid)

{- | Abort a running agent by killing its thread
Returns True if an agent was running and was killed
-}
abortAgent :: AppState -> Text -> IO Bool
abortAgent st sid = do
    mTid <- atomically $ do
        agents <- readTVar (stActiveAgents st)
        let mTid = Map.lookup sid agents
        -- Remove from map regardless (cleanup)
        modifyTVar' (stActiveAgents st) (Map.delete sid)
        pure mTid
    case mTid of
        Just tid -> do
            killThread tid
            pure True
        Nothing -> pure False
