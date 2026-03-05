{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Telemetry.Manager
Description : Global telemetry manager for all sessions

Manages telemetry capture across all sessions. Each session gets its
own WAL, but the manager coordinates:

* Creating/closing WALs as sessions start/end
* Global bus subscription for event capture
* R2 replication coordination

@since 0.1.0
-}
module Telemetry.Manager (
    -- * Types
    TelemetryManager,
    TelemetryManagerConfig (..),

    -- * Lifecycle
    startManager,
    stopManager,

    -- * Configuration
    defaultManagerConfig,
) where

import Bus.Bus qualified as Bus
import Config.Types (TelemetryConfig (..))
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad (forM_, void)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds, systemSeconds)
import Log qualified
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))
import Telemetry.ParquetWAL qualified as WAL
import Telemetry.R2 qualified as R2
import Telemetry.Types
import Util.Identifier qualified as Identifier
import Util.Thread (forkLogged)

-- | Manager configuration
data TelemetryManagerConfig = TelemetryManagerConfig
    { tmcTelemetryConfig :: TelemetryConfig
    -- ^ Telemetry configuration (R2 credentials etc.)
    , tmcWALDir :: Maybe FilePath
    -- ^ Override WAL directory
    , tmcSyncOnWrite :: Bool
    -- ^ Fsync after every event
    , tmcLogger :: Log.Logger
    -- ^ Logger for telemetry subsystem
    }

-- | Default configuration
defaultManagerConfig :: TelemetryConfig -> Log.Logger -> TelemetryManagerConfig
defaultManagerConfig telConfig logger =
    TelemetryManagerConfig
        { tmcTelemetryConfig = telConfig
        , tmcWALDir = Nothing
        , tmcSyncOnWrite = True
        , tmcLogger = Log.withNS logger "telemetry"
        }

-- | R2 poll interval in seconds
r2PollIntervalSec :: Int
r2PollIntervalSec = 30

-- | Per-session state
data SessionTelemetry = SessionTelemetry
    { stWAL :: WAL.WALHandle
    , stProjectId :: Text
    , stDirectory :: Text
    , stR2Worker :: Maybe R2.ReplicationWorker
    -- ^ R2 replication worker (if enabled)
    }

-- | Global telemetry manager
data TelemetryManager = TelemetryManager
    { tmConfig :: TelemetryManagerConfig
    , tmWALDir :: FilePath
    , tmSessions :: TVar (Map Text SessionTelemetry)
    , tmIdGen :: Identifier.IdGenState
    , tmUnsubscribe :: IO ()
    , tmDefaultProjectId :: Text
    , tmDefaultDirectory :: Text
    , tmR2Handle :: R2.R2Handle
    -- ^ R2 handle (always present - config validated at startup)
    }

{- | Start the telemetry manager

FAILS with error if R2 config validation fails (missing required credentials).
The manager only exists when telemetry is enabled (caller checks Maybe TelemetryConfig).
-}
startManager ::
    TelemetryManagerConfig ->
    Bus.Bus ->
    -- | Default project ID
    Text ->
    -- | Default directory
    Text ->
    IO TelemetryManager
startManager config bus defaultProjectId defaultDirectory = do
    -- Determine WAL directory
    walDir <- case tmcWALDir config of
        Just dir -> pure dir
        Nothing -> do
            xdgData <- getXdgDirectory XdgData "weapon"
            pure $ xdgData </> "wal"

    createDirectoryIfMissing True walDir

    sessions <- newTVarIO Map.empty
    idGen <- Identifier.newIdGenState

    -- Validate and initialize R2 from config
    -- FAIL if validation fails (no silent degradation - config errors are fatal)
    let lg = tmcLogger config
        telConfig = tmcTelemetryConfig config

    r2Handle <- case R2.configFromDhall telConfig of
        Right r2Config -> do
            Log.logInfo lg "R2 replication enabled" ()
            R2.newR2Handle r2Config
        Left (R2.R2ConfigError err) -> do
            let msg = "R2 config error: " <> err
            Log.logError lg msg ()
            throwIO $ userError $ T.unpack msg
        Left err -> do
            -- Other errors (network, upload) shouldn't happen during config
            Log.logError lg ("Unexpected R2 error during config: " <> T.pack (show err)) ()
            throwIO $ userError $ "R2 initialization failed: " <> show err

    -- Subscribe to all bus events with supervised thread
    unsubscribe <- Bus.subscribeAllLogged lg "telemetry-manager-subscriber" bus $ \busEvent ->
        handleEvent walDir config r2Handle sessions idGen defaultProjectId defaultDirectory busEvent

    pure $
        TelemetryManager
            { tmConfig = config
            , tmWALDir = walDir
            , tmSessions = sessions
            , tmIdGen = idGen
            , tmUnsubscribe = unsubscribe
            , tmDefaultProjectId = defaultProjectId
            , tmDefaultDirectory = defaultDirectory
            , tmR2Handle = r2Handle
            }

-- | Stop the telemetry manager
stopManager :: TelemetryManager -> IO ()
stopManager tm = do
    -- Unsubscribe from bus
    tmUnsubscribe tm

    -- Stop all R2 workers and close all session WALs
    sessions <- readTVarIO (tmSessions tm)
    forM_ (Map.elems sessions) $ \sess -> do
        -- Stop R2 worker if running
        for_ (stR2Worker sess) R2.stopReplicationWorker
        -- Close WAL
        WAL.closeWAL (stWAL sess)

-- | Handle a bus event
handleEvent ::
    FilePath ->
    TelemetryManagerConfig ->
    R2.R2Handle ->
    TVar (Map Text SessionTelemetry) ->
    Identifier.IdGenState ->
    Text ->
    Text ->
    Bus.BusEvent ->
    IO ()
handleEvent walDir config r2Handle sessionsVar idGen defaultProjectId defaultDirectory busEvent =
    void $
        forkLogged (tmcLogger config) "telemetry-event-handler" $
            handleEventSync walDir config r2Handle sessionsVar idGen defaultProjectId defaultDirectory busEvent

-- | Synchronous event handler
handleEventSync ::
    FilePath ->
    TelemetryManagerConfig ->
    R2.R2Handle ->
    TVar (Map Text SessionTelemetry) ->
    Identifier.IdGenState ->
    Text ->
    Text ->
    Bus.BusEvent ->
    IO ()
handleEventSync walDir config r2Handle sessionsVar idGen defaultProjectId defaultDirectory busEvent = do
    let eventType = Bus.beType busEvent
        payload = Bus.beProperties busEvent

    -- Extract session ID from payload
    let sessionId = extractSessionId payload

    -- Get or create session telemetry
    sessionTel <- getOrCreateSession walDir config r2Handle sessionsVar sessionId defaultProjectId defaultDirectory

    -- Build and write event
    event <- buildEvent idGen sessionId (stProjectId sessionTel) (stDirectory sessionTel) eventType payload
    if tmcSyncOnWrite config
        then void $ WAL.appendEventSync (stWAL sessionTel) event
        else void $ WAL.appendEvent (stWAL sessionTel) event

-- | Extract session ID from event payload
extractSessionId :: Value -> Text
extractSessionId (Object obj) =
    case KM.lookup (K.fromText "sessionID") obj of
        Just (String sid) -> sid
        _ -> case KM.lookup (K.fromText "session_id") obj of
            Just (String sid) -> sid
            _ -> "global"
extractSessionId _ = "global"

-- | Get or create session telemetry state
getOrCreateSession ::
    FilePath ->
    TelemetryManagerConfig ->
    R2.R2Handle ->
    TVar (Map Text SessionTelemetry) ->
    Text ->
    Text ->
    Text ->
    IO SessionTelemetry
getOrCreateSession walDir config r2Handle sessionsVar sessionId defaultProjectId defaultDirectory = do
    -- Check if exists
    mSession <- Map.lookup sessionId <$> readTVarIO sessionsVar
    case mSession of
        Just s -> pure s
        Nothing -> do
            -- Create new WAL
            let walConfig =
                    WAL.WALConfig
                        { WAL.walBaseDir = walDir
                        , WAL.walSegmentSize = 10000
                        , WAL.walSyncOnWrite = tmcSyncOnWrite config
                        , WAL.walLogger = tmcLogger config
                        }
            wal <- WAL.openWAL walConfig sessionId

            -- Start R2 replication worker
            r2Worker <- R2.startReplicationWorker r2Handle wal sessionId r2PollIntervalSec (tmcLogger config)

            let session =
                    SessionTelemetry
                        { stWAL = wal
                        , stProjectId = defaultProjectId
                        , stDirectory = defaultDirectory
                        , stR2Worker = Just r2Worker
                        }

            atomically $ modifyTVar' sessionsVar (Map.insert sessionId session)
            pure session

-- | Build a telemetry event
buildEvent ::
    Identifier.IdGenState ->
    Text ->
    Text ->
    Text ->
    Text ->
    Value ->
    IO TelemetryEvent
buildEvent idGen sessionId projectId directory eventType payload = do
    eventId <- Identifier.ascendingWithPrefix idGen "evt"
    now <- getCurrentTime
    sysTime <- getSystemTime
    let monoNs =
            fromIntegral (systemSeconds sysTime) * 1_000_000_000
                + fromIntegral (systemNanoseconds sysTime)

    pure $
        TelemetryEvent
            { teId = eventId
            , teSeq = 0
            , teTimestamp = now
            , teMonotonicNs = monoNs
            , teSessionId = sessionId
            , teProjectId = projectId
            , teDirectory = directory
            , teType = eventType
            , tePayload = payload
            , teMeta = extractMeta payload
            }

-- | Extract metadata from payload
extractMeta :: Value -> EventMeta
extractMeta (Object obj) =
    EventMeta
        { emModel = getTextField "modelID" obj
        , emAgent = getTextField "agent" obj
        , emTokensIn = Nothing
        , emTokensOut = Nothing
        , emLatencyMs = Nothing
        , emTotalLatencyMs = Nothing
        , emToolName = getTextField "tool" obj
        , emParentEvent = Nothing
        , emErrorMessage = getTextField "error" obj
        }
  where
    getTextField k o = case KM.lookup (K.fromText k) o of
        Just (String t) -> Just t
        _ -> Nothing
extractMeta _ = emptyMeta
