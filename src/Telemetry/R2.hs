{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Telemetry.R2
Description : Cloudflare R2 replication for telemetry data

Asynchronous replication of WAL segments to Cloudflare R2 for
durable, off-site storage of training data.

== Architecture

@
Local WAL → Replication Worker → Compress (zstd) → Sign (AWS SigV4) → R2 Upload
                    ↓
            Update high-water mark
                    ↓
            Optionally delete local segment
@

== R2 Layout

@
weapon-telemetry\/
  {org}\/
    {user}\/
      {session_id}\/
        meta.json
        events\/
          0000000000.jsonl.zst
          0000000001.jsonl.zst
        artifacts\/
          {hash}.bin
@

== Dhall Configuration

Configuration comes from @weapon.dhall@:

@
{ telemetry =
    { enabled = Some True
    , r2 = Some
        { accountId = Some "your-cloudflare-account-id"
        , accessKeyId = Some "r2-api-token-access-key"
        , secretKey = Some "r2-api-token-secret-key"
        , bucket = Some "weapon-telemetry"
        , prefix = Some "telemetry"
        , endpoint = None Text
        }
    }
}
@

@since 0.1.0
-}
module Telemetry.R2 (
    -- * Types
    R2Config (..),
    R2Handle,
    R2Error (..),
    ReplicationWorker,

    -- * Configuration
    configFromDhall,

    -- * Operations
    newR2Handle,
    uploadSegment,
    uploadSessionMeta,
    uploadBytes,

    -- * Replication Worker
    startReplicationWorker,
    stopReplicationWorker,
) where

import Control.Concurrent (ThreadId, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (Exception, IOException, SomeException, try)
import Crypto.Hash (SHA256 (..), hashWith)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.Aeson (encode, object, (.=))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Log qualified
import Network.HTTP.Client (
    Manager,
    Request (..),
    RequestBody (..),
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
    responseStatus,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Header, statusCode)
import Util.Thread (forkLogged)

import System.FilePath (takeFileName, (</>))
import Telemetry.ParquetWAL qualified as WAL

import Config.Types (R2StorageConfig (..), TelemetryConfig (..))

-- | R2 configuration
data R2Config = R2Config
    { r2AccountId :: Text
    -- ^ Cloudflare account ID
    , r2AccessKeyId :: Text
    -- ^ R2 access key ID
    , r2SecretAccessKey :: Text
    -- ^ R2 secret access key
    , r2Bucket :: Text
    -- ^ R2 bucket name
    , r2Prefix :: Text
    -- ^ Key prefix (e.g., "org/user")
    , r2Endpoint :: Maybe Text
    -- ^ Custom endpoint (for testing)
    , r2Region :: Text
    -- ^ Region (always "auto" for R2)
    }
    deriving (Show, Eq)

-- | R2 errors
data R2Error
    = -- | Upload failed with status code and response body
      R2UploadError Text Int ByteString
    | -- | Network error
      R2NetworkError Text
    | -- | Configuration error (missing required fields in Dhall config)
      R2ConfigError Text
    deriving (Show, Eq)

instance Exception R2Error

-- | Handle to R2 client
data R2Handle = R2Handle
    { r2Config :: R2Config
    , r2Manager :: Manager
    }

{- | Validate R2 configuration from TelemetryConfig.

All configuration comes from Dhall - no environment variable fallback.
Returns R2ConfigError if required fields are missing.

Required fields in weapon.dhall telemetry.r2:
  accountId
  accessKeyId
  secretKey

Optional fields (with defaults):
  bucket   (default: "weapon-telemetry")
  prefix   (default: "telemetry")
  endpoint (default: none, uses Cloudflare R2)
-}
configFromDhall :: TelemetryConfig -> Either R2Error R2Config
configFromDhall (TelemetryConfig r2cfg) =
    case (r2sAccountId r2cfg, r2sAccessKeyId r2cfg, r2sSecretKey r2cfg) of
        (Just accountId, Just accessKey, Just secretKey) ->
            Right
                R2Config
                    { r2AccountId = accountId
                    , r2AccessKeyId = accessKey
                    , r2SecretAccessKey = secretKey
                    , r2Bucket = fromMaybe "weapon-telemetry" (r2sBucket r2cfg)
                    , r2Prefix = fromMaybe "telemetry" (r2sPrefix r2cfg)
                    , r2Endpoint = r2sEndpoint r2cfg
                    , r2Region = "auto" -- R2 always uses "auto"
                    }
        (Nothing, _, _) -> Left $ R2ConfigError "telemetry.r2.accountId is required"
        (_, Nothing, _) -> Left $ R2ConfigError "telemetry.r2.accessKeyId is required"
        (_, _, Nothing) -> Left $ R2ConfigError "telemetry.r2.secretKey is required"

-- | Create a new R2 handle
newR2Handle :: R2Config -> IO R2Handle
newR2Handle config = do
    manager <- newManager tlsManagerSettings
    pure $ R2Handle{r2Config = config, r2Manager = manager}

-- | Upload a segment file to R2
uploadSegment ::
    R2Handle ->
    -- | Session ID
    Text ->
    -- | Local segment path
    FilePath ->
    IO (Either R2Error ())
uploadSegment h sessionId localPath = do
    -- TODO[b7r6]: add zstd compression for segment uploads
    readResult <- try @IOException $ BS.readFile localPath
    case readResult of
        Left err ->
            pure $
                Left $
                    R2NetworkError $
                        "Failed to read segment file " <> T.pack localPath <> ": " <> T.pack (show err)
        Right content -> do
            -- For now, no compression - just upload raw
            let compressed = content

            let key =
                    T.unpack (r2Prefix (r2Config h))
                        </> T.unpack sessionId
                        </> "events"
                        </> takeFileName localPath

            uploadBytes h (T.pack key) compressed "application/vnd.apache.parquet"

-- | Upload session metadata
uploadSessionMeta ::
    R2Handle ->
    -- | Session ID
    Text ->
    -- | Project ID
    Text ->
    -- | Model ID
    Text ->
    -- | Agent
    Text ->
    IO (Either R2Error ())
uploadSessionMeta h sessionId projectId modelId agent = do
    now <- getCurrentTime
    let meta =
            object
                [ "session_id" .= sessionId
                , "project_id" .= projectId
                , "model_id" .= modelId
                , "agent" .= agent
                , "created_at" .= now
                ]
        key =
            r2Prefix (r2Config h)
                <> "/"
                <> sessionId
                <> "/meta.json"

    uploadBytes h key (LBS.toStrict $ encode meta) "application/json"

-- | Upload raw bytes to R2 with AWS Signature V4
uploadBytes ::
    R2Handle ->
    -- | Object key
    Text ->
    -- | Content
    ByteString ->
    -- | Content-Type
    ByteString ->
    IO (Either R2Error ())
uploadBytes h key content contentType = do
    now <- getCurrentTime
    let config = r2Config h
        host = case r2Endpoint config of
            Just ep -> TE.encodeUtf8 $ T.drop 8 ep -- Remove "https://"
            Nothing ->
                TE.encodeUtf8 (r2AccountId config)
                    <> ".r2.cloudflarestorage.com"
        endpoint = case r2Endpoint config of
            Just ep -> T.unpack ep
            Nothing ->
                "https://"
                    <> T.unpack (r2AccountId config)
                    <> ".r2.cloudflarestorage.com"
        url = endpoint <> "/" <> T.unpack (r2Bucket config) <> "/" <> T.unpack key

    -- Build signed headers
    let headers =
            signRequest
                config
                now
                "PUT"
                ("/" <> TE.encodeUtf8 (r2Bucket config) <> "/" <> TE.encodeUtf8 key)
                host
                content
                contentType

    result <- try $ do
        initReq <- parseRequest url
        let req =
                initReq
                    { method = "PUT"
                    , requestBody = RequestBodyBS content
                    , requestHeaders = headers
                    }
        response <- httpLbs req (r2Manager h)
        let status = responseStatus response
            body = LBS.toStrict $ responseBody response
        if statusCode status >= 200 && statusCode status < 300
            then pure $ Right ()
            else pure $ Left $ R2UploadError (T.pack $ show status) (statusCode status) body

    case result of
        Left (e :: SomeException) -> pure $ Left $ R2NetworkError (T.pack $ show e)
        Right r -> pure r

-- ═══════════════════════════════════════════════════════════════════════════
-- AWS Signature V4 Implementation
-- ═══════════════════════════════════════════════════════════════════════════

-- | Sign a request using AWS Signature V4
signRequest ::
    R2Config ->
    UTCTime ->
    -- | HTTP method
    ByteString ->
    -- | Path (including bucket)
    ByteString ->
    -- | Host
    ByteString ->
    -- | Payload
    ByteString ->
    -- | Content-Type
    ByteString ->
    [Header]
signRequest config now httpMethod reqPath host payload contentType =
    [ ("Host", host)
    , ("Content-Type", contentType)
    , ("X-Amz-Date", amzDate)
    , ("X-Amz-Content-Sha256", payloadHash)
    , ("Authorization", authHeader)
    ]
  where
    -- Date strings
    amzDate = BC.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" now
    dateStamp = BC.pack $ formatTime defaultTimeLocale "%Y%m%d" now

    -- Hash payload
    payloadHash = B16.encode $ convert $ hashWith SHA256 payload

    -- Canonical request
    signedHeaders = "content-type;host;x-amz-content-sha256;x-amz-date"
    canonicalHeaders =
        "content-type:"
            <> contentType
            <> "\n"
            <> "host:"
            <> host
            <> "\n"
            <> "x-amz-content-sha256:"
            <> payloadHash
            <> "\n"
            <> "x-amz-date:"
            <> amzDate
            <> "\n"
    canonicalRequest =
        httpMethod
            <> "\n"
            <> reqPath
            <> "\n"
            <> "\n"
            <> canonicalHeaders -- Query string (empty)
            <> "\n"
            <> signedHeaders
            <> "\n"
            <> payloadHash

    -- String to sign
    algorithm = "AWS4-HMAC-SHA256"
    region = TE.encodeUtf8 $ r2Region config
    service = "s3"
    credentialScope = dateStamp <> "/" <> region <> "/" <> service <> "/aws4_request"
    hashedCanonicalRequest = B16.encode $ convert $ hashWith SHA256 canonicalRequest
    stringToSign =
        algorithm
            <> "\n"
            <> amzDate
            <> "\n"
            <> credentialScope
            <> "\n"
            <> hashedCanonicalRequest

    -- Signing key
    kSecret = "AWS4" <> TE.encodeUtf8 (r2SecretAccessKey config)
    kDate = hmacSHA256 kSecret dateStamp
    kRegion = hmacSHA256 kDate region
    kService = hmacSHA256 kRegion service
    kSigning = hmacSHA256 kService "aws4_request"

    -- Signature
    signature = B16.encode $ hmacSHA256 kSigning stringToSign

    -- Authorization header
    credential = TE.encodeUtf8 (r2AccessKeyId config) <> "/" <> credentialScope
    authHeader =
        algorithm
            <> " Credential="
            <> credential
            <> ", SignedHeaders="
            <> signedHeaders
            <> ", Signature="
            <> signature

-- | HMAC-SHA256
hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 key msg = convert (hmac key msg :: HMAC SHA256)

-- ═══════════════════════════════════════════════════════════════════════════
-- Replication Worker
-- ═══════════════════════════════════════════════════════════════════════════

-- | Replication worker state
data ReplicationWorker = ReplicationWorker
    { rwThread :: ThreadId
    , rwStop :: TVar Bool
    }

-- | Start the replication worker
startReplicationWorker ::
    R2Handle ->
    WAL.WALHandle ->
    -- | Session ID
    Text ->
    -- | Poll interval in seconds
    Int ->
    -- | Logger
    Log.Logger ->
    IO ReplicationWorker
startReplicationWorker r2 wal sessionId pollInterval logger = do
    stopVar <- newTVarIO False
    let lg = Log.withNS logger "r2"

    tid <- forkLogged lg "r2-replication" $ replicationLoop r2 wal sessionId stopVar pollInterval lg

    pure $
        ReplicationWorker
            { rwThread = tid
            , rwStop = stopVar
            }

-- | Stop the replication worker
stopReplicationWorker :: ReplicationWorker -> IO ()
stopReplicationWorker rw = do
    atomically $ writeTVar (rwStop rw) True
    -- Give it time to finish current upload
    threadDelay 1000000
    killThread (rwThread rw)

-- | Maximum consecutive failures before giving up on a batch
maxConsecutiveFailures :: Int
maxConsecutiveFailures = 10

-- | Maximum backoff delay in seconds
maxBackoffSeconds :: Int
maxBackoffSeconds = 60

-- | Calculate backoff delay in seconds: min(60, 2^(failures-1))
calculateBackoffSeconds :: Int -> Int
calculateBackoffSeconds failures =
    min maxBackoffSeconds (2 ^ (failures - 1))

-- | Main replication loop
replicationLoop ::
    R2Handle ->
    WAL.WALHandle ->
    Text ->
    TVar Bool ->
    Int ->
    Log.Logger ->
    IO ()
replicationLoop r2 wal sessionId stopVar pollInterval lg = do
    -- Track consecutive failures for backoff and escalation
    failureCountVar <- newTVarIO (0 :: Int)
    loop failureCountVar
  where
    loop failureCountVar = do
        shouldStop <- readTVarIO stopVar
        if shouldStop
            then pure ()
            else do
                -- Find unreplicated segments
                segments <- WAL.listUnreplicatedSegments wal

                -- Upload each segment
                mapM_ (uploadOne failureCountVar) segments

                -- Wait before next poll
                threadDelay (pollInterval * 1000000)
                loop failureCountVar

    uploadOne failureCountVar (path, _startSeq, _endSeq) = do
        result <- uploadSegment r2 sessionId path
        case result of
            Right () -> do
                -- Reset failure counter on success
                atomically $ writeTVar failureCountVar 0
                -- TODO[b7r6]: HIGH PRIORITY - Implement WAL.setHighWaterMark
                -- Without this, segments may be re-uploaded after restart since we
                -- don't track which segments have been successfully replicated.
                -- WAL.setHighWaterMark wal endSeq
                Log.logDebug lg ("Uploaded: " <> T.pack path) ()
            Left err -> do
                -- Increment failure counter
                failureCount <- atomically $ do
                    count <- readTVar failureCountVar
                    let newCount = count + 1
                    writeTVar failureCountVar newCount
                    pure newCount

                -- Log with escalating severity based on failure count
                let errMsg = "Upload failed: " <> T.pack (show err)
                    backoffSecs = calculateBackoffSeconds failureCount
                    backoffMsg =
                        " (backoff: "
                            <> T.pack (show backoffSecs)
                            <> "s, attempt "
                            <> T.pack (show failureCount)
                            <> "/"
                            <> T.pack (show maxConsecutiveFailures)
                            <> ")"

                if failureCount >= maxConsecutiveFailures
                    then do
                        -- Give up on this batch after max failures
                        Log.logError lg ("CRITICAL: telemetry upload failing repeatedly, giving up on batch: " <> errMsg) ()
                        -- Reset counter so we can try fresh on next batch
                        atomically $ writeTVar failureCountVar 0
                    else
                        if failureCount >= 6
                            then do
                                Log.logError lg ("CRITICAL: telemetry upload failing repeatedly: " <> errMsg <> backoffMsg) ()
                                threadDelay (backoffSecs * 1000000)
                            else
                                if failureCount >= 3
                                    then do
                                        Log.logError lg (errMsg <> backoffMsg) ()
                                        threadDelay (backoffSecs * 1000000)
                                    else do
                                        -- 1-2 failures: WARNING
                                        Log.logWarn lg (errMsg <> backoffMsg) ()
                                        threadDelay (backoffSecs * 1000000)
