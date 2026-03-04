{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Telemetry.Telemetry
Description : Full-take telemetry capture for AI coding sessions

This module provides the main telemetry interface that captures every
event from the bus and persists it to the write-ahead log.

== Usage

@
import Telemetry.Telemetry qualified as Telemetry

main = do
    bus <- Bus.newBus
    telemetry <- Telemetry.start config bus sessionId projectId directory
    
    -- Events are now being captured...
    
    Telemetry.stop telemetry
@

@since 0.1.0
-}
module Telemetry.Telemetry (
    -- * Types
    TelemetryConfig (..),
    TelemetryHandle,

    -- * Lifecycle
    start,
    stop,

    -- * Manual capture
    captureEvent,

    -- * Configuration
    defaultConfig,
) where

import Bus.Bus qualified as Bus
import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent qualified as Concurrent
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds, systemSeconds)
import Data.Word (Word64)
import Log qualified
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.FilePath ((</>))
import Telemetry.Types
import Telemetry.WAL qualified as WAL
import Util.Identifier qualified as Identifier
import Util.Thread (forkLogged)

-- | Telemetry configuration
data TelemetryConfig = TelemetryConfig
    { tcWALDir :: Maybe FilePath
    -- ^ Override WAL directory (defaults to XDG_DATA_HOME/weapon/wal)
    , tcSyncOnWrite :: Bool
    -- ^ Fsync after every event (safest)
    , tcEnabled :: Bool
    -- ^ Master enable switch
    , tcLogger :: Log.Logger
    -- ^ Logger for telemetry subsystem
    }

-- | Default configuration (enabled, sync on write)
defaultConfig :: Log.Logger -> TelemetryConfig
defaultConfig logger =
    TelemetryConfig
        { tcWALDir = Nothing
        , tcSyncOnWrite = True
        , tcEnabled = True
        , tcLogger = Log.withNS logger "telemetry"
        }

-- | Handle to a running telemetry capture
data TelemetryHandle = TelemetryHandle
    { thConfig :: TelemetryConfig
    , thWAL :: WAL.WALHandle
    , thSessionId :: Text
    , thProjectId :: Text
    , thDirectory :: Text
    , thIdGen :: Identifier.IdGenState
    , thSubscriberThread :: ThreadId
    , thUnsubscribe :: IO ()
    }

-- | Start telemetry capture for a session
start ::
    TelemetryConfig ->
    Bus.Bus ->
    Text ->
    -- ^ Session ID
    Text ->
    -- ^ Project ID
    Text ->
    -- ^ Working directory
    IO TelemetryHandle
start config bus sessionId projectId directory = do
    -- Determine WAL directory
    walDir <- case tcWALDir config of
        Just dir -> pure dir
        Nothing -> do
            xdgData <- getXdgDirectory XdgData "weapon"
            pure $ xdgData </> "wal"

    -- Open WAL for this session
    let walConfig =
            WAL.WALConfig
                { WAL.walBaseDir = walDir
                , WAL.walSegmentSize = 10000
                , WAL.walSyncOnWrite = tcSyncOnWrite config
                , WAL.walLogger = Log.withNS (tcLogger config) "wal"
                }
    wal <- WAL.openWAL walConfig sessionId

    -- ID generator for event IDs
    idGen <- Identifier.newIdGenState

    -- Create handle (thread will be filled in)
    tidRef <- newIORef (error "TelemetryHandle not initialized")
    unsubRef <- newIORef (pure ())

    let captureEvent' eventType payload = do
            event <- buildEvent idGen sessionId projectId directory eventType payload
            void $ WAL.appendEventSync wal event

    let lg = tcLogger config

    -- Subscribe to all bus events with supervised thread
    unsubscribe <- Bus.subscribeAllLogged lg "telemetry-capture-subscriber" bus $ \busEvent -> do
        let eventType = Bus.beType busEvent
            payload = Bus.beProperties busEvent
        -- Capture asynchronously (don't block bus)
        -- forkLogged provides outer safety net; inner try/catch for finer-grained handling
        void $ forkLogged lg "telemetry-capture" $ do
            result <- try $ captureEvent' eventType payload
            case result of
                Left (e :: SomeException) ->
                    -- Log error but don't crash
                    Log.logError lg ("Capture error: " <> T.pack (show e)) ()
                Right _ -> pure ()

    writeIORef unsubRef unsubscribe

    -- The subscriber thread is managed by Bus.subscribeAll
    -- We just need a placeholder thread ID
    tid <- forkLogged lg "telemetry-placeholder" $ forever $ Concurrent.threadDelay 1000000000
    writeIORef tidRef tid

    pure $
        TelemetryHandle
            { thConfig = config
            , thWAL = wal
            , thSessionId = sessionId
            , thProjectId = projectId
            , thDirectory = directory
            , thIdGen = idGen
            , thSubscriberThread = tid
            , thUnsubscribe = unsubscribe
            }

-- | Stop telemetry capture
stop :: TelemetryHandle -> IO ()
stop th = do
    -- Unsubscribe from bus
    thUnsubscribe th
    -- Kill placeholder thread
    killThread (thSubscriberThread th)
    -- Close WAL (flushes and syncs)
    WAL.closeWAL (thWAL th)

-- | Manually capture an event (for events not on the bus)
captureEvent :: TelemetryHandle -> Text -> Value -> IO Word64
captureEvent th eventType payload = do
    event <-
        buildEvent
            (thIdGen th)
            (thSessionId th)
            (thProjectId th)
            (thDirectory th)
            eventType
            payload
    WAL.appendEventSync (thWAL th) event

-- | Build a telemetry event from raw data
buildEvent ::
    Identifier.IdGenState ->
    Text ->
    Text ->
    Text ->
    Text ->
    Value ->
    IO TelemetryEvent
buildEvent idGen sessionId projectId directory eventType payload = do
    -- Generate ULID-style event ID
    eventId <- Identifier.ascendingWithPrefix idGen "evt"

    -- Get timestamps
    now <- getCurrentTime
    sysTime <- getSystemTime
    let monoNs =
            fromIntegral (systemSeconds sysTime) * 1_000_000_000
                + fromIntegral (systemNanoseconds sysTime)

    -- Extract metadata from payload if available
    let meta = extractMeta payload

    pure $
        TelemetryEvent
            { teId = eventId
            , teSeq = 0 -- Will be set by WAL
            , teTimestamp = now
            , teMonotonicNs = monoNs
            , teSessionId = sessionId
            , teProjectId = projectId
            , teDirectory = directory
            , teType = eventType
            , tePayload = payload
            , teMeta = meta
            }

-- | Extract metadata from event payload
extractMeta :: Value -> EventMeta
extractMeta (Object obj) =
    EventMeta
        { emModel = getTextField "modelID" obj
        , emAgent = getTextField "agent" obj
        , emTokensIn = Nothing -- TODO: extract from usage
        , emTokensOut = Nothing
        , emLatencyMs = Nothing
        , emTotalLatencyMs = Nothing
        , emToolName = getTextField "tool" obj
        , emParentEvent = getTextField "parentId" obj
        , emErrorMessage = getTextField "error" obj
        }
  where
    getTextField k o = case KM.lookup (K.fromText k) o of
        Just (String t) -> Just t
        _ -> Nothing
extractMeta _ = emptyMeta
