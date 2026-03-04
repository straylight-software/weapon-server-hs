{-# LANGUAGE OverloadedStrings #-}

-- | Application state
module State (
    AppState (..),
    initialState,
    initialStateNoProxy,
    initialStateNoProxyQuiet,
    initialStateNoProxyWithHome,
    initialStateNoProxyWithCache,
    initialStateNoProxyWithCaches,

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
import Config.Dhall qualified as Dhall
import Config.Types (Config (..))
import Data.Text qualified as Text
import Formatter.Status qualified as Formatter
import Katip qualified
import Log qualified
import Prompt.Async (PromptAsyncJob)
import Proxy.Proxy qualified as Proxy
import Proxy.Types (defaultProxyConfig)
import Pty.Pty qualified as Pty
import Storage.Storage qualified as Storage
import Telemetry.Manager qualified as Telemetry
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
    , stDhallCache :: Dhall.DhallCache -- Cached Dhall config loader
    , stExeCache :: Formatter.ExeCache -- Cached executable lookups
    , stDirCache :: Storage.DirCache -- Cached directory existence checks
    , stTelemetry :: Maybe Telemetry.TelemetryManager -- Full-take telemetry capture
    }

-- | Initialize a new state with optional proxy and home directory override
mkAppState :: Maybe Proxy.ProxyServer -> Maybe FilePath -> FilePath -> Text -> Text -> Log.Logger -> IO AppState
mkAppState proxy homeDir storageDir projectID directory logger = do
    dhallCache <- Dhall.newDhallCache
    mkAppStateWithCache proxy dhallCache homeDir storageDir projectID directory logger

-- | Initialize a new state with optional proxy, home directory override, and shared DhallCache
mkAppStateWithCache :: Maybe Proxy.ProxyServer -> Dhall.DhallCache -> Maybe FilePath -> FilePath -> Text -> Text -> Log.Logger -> IO AppState
mkAppStateWithCache proxy dhallCache homeDir storageDir projectID directory logger = do
    exeCache <- Formatter.newExeCache
    mkAppStateWithCaches proxy dhallCache exeCache homeDir storageDir projectID directory logger

-- | Initialize a new state with optional proxy, home directory override, and shared caches
mkAppStateWithCaches :: Maybe Proxy.ProxyServer -> Dhall.DhallCache -> Formatter.ExeCache -> Maybe FilePath -> FilePath -> Text -> Text -> Log.Logger -> IO AppState
mkAppStateWithCaches = mkAppStateWithCachesQuiet False

-- | Initialize a new state with optional proxy, home directory override, shared caches
-- The Bool parameter is deprecated and ignored - logging is now controlled via Log.LogConfig
mkAppStateWithCachesQuiet :: Bool -> Maybe Proxy.ProxyServer -> Dhall.DhallCache -> Formatter.ExeCache -> Maybe FilePath -> FilePath -> Text -> Text -> Log.Logger -> IO AppState
mkAppStateWithCachesQuiet _quiet proxy dhallCache exeCache homeDir storageDir projectID directory logger = do
    bus <- Bus.newBus
    eventChan <- newBroadcastTChanIO
    ptyManager <- Pty.newManager (Text.unpack directory)
    promptQueue <- newTQueueIO
    activeAgents <- newTVarIO Map.empty
    idGen <- Identifier.newIdGenState
    dirCache <- Storage.newDirCache

    -- Subscribe bus to also write to event channel for SSE
    _ <- Bus.subscribeAll bus $ \event -> do
        Log.logMsg logger Katip.InfoS $ "State: bus->eventChan forwarding: " <> Bus.beType event
        atomically $ writeTChan eventChan (toJSON event)

    -- Load telemetry config from Dhall and start capture (if configured)
    dhallConfig <- Dhall.loadConfigCached dhallCache (Text.unpack directory)
    telemetry <- case cfgTelemetry dhallConfig of
        Nothing -> do
            Log.logMsg logger Katip.InfoS "Telemetry disabled (no config)"
            pure Nothing
        Just telConfig -> do
            let telemetryConfig = Telemetry.defaultManagerConfig telConfig logger
            tm <- Telemetry.startManager telemetryConfig bus projectID directory
            pure (Just tm)

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
            , stDhallCache = dhallCache
            , stExeCache = exeCache
            , stDirCache = dirCache
            , stTelemetry = telemetry
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

-- | Initialize state without proxy, suppressing startup messages (for TUI mode)
initialStateNoProxyQuiet :: FilePath -> Text -> Text -> Log.Logger -> IO AppState
initialStateNoProxyQuiet storageDir projectID directory logger = do
    dhallCache <- Dhall.newDhallCache
    exeCache <- Formatter.newExeCache
    mkAppStateWithCachesQuiet True Nothing dhallCache exeCache Nothing storageDir projectID directory logger

-- | Initialize state without proxy, with optional home directory override
initialStateNoProxyWithHome :: Maybe FilePath -> FilePath -> Text -> Text -> Log.Logger -> IO AppState
initialStateNoProxyWithHome = mkAppState Nothing

-- | Initialize state without proxy, with shared DhallCache (for tests)
initialStateNoProxyWithCache :: Dhall.DhallCache -> Maybe FilePath -> FilePath -> Text -> Text -> Log.Logger -> IO AppState
initialStateNoProxyWithCache = mkAppStateWithCache Nothing

-- | Initialize state without proxy, with shared DhallCache and ExeCache (for tests)
initialStateNoProxyWithCaches :: Dhall.DhallCache -> Formatter.ExeCache -> Maybe FilePath -> FilePath -> Text -> Text -> Log.Logger -> IO AppState
initialStateNoProxyWithCaches = mkAppStateWithCaches Nothing

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
