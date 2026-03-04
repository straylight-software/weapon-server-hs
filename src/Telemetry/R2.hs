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

== Environment Variables

@
WEAPON_R2_ACCOUNT_ID     - Cloudflare account ID
WEAPON_R2_ACCESS_KEY_ID  - R2 access key ID  
WEAPON_R2_SECRET_KEY     - R2 secret access key
WEAPON_R2_BUCKET         - R2 bucket name (default: weapon-telemetry)
WEAPON_R2_PREFIX         - Key prefix (default: telemetry)
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
    configFromEnv,
    configFromDhall,
    isConfigured,

    -- * Operations
    newR2Handle,
    uploadSegment,
    uploadSessionMeta,
    uploadBytes,

    -- * Replication Worker
    startReplicationWorker,
    stopReplicationWorker,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, try)
import Crypto.Hash (SHA256 (..), hashWith)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.Aeson (encode, object, (.=))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
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
import Log qualified
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)
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
    = R2UploadError Text Int ByteString
    -- ^ Upload failed with status code and response body
    | R2NetworkError Text
    -- ^ Network error
    | R2ConfigError Text
    -- ^ Configuration error (missing env vars)
    | R2NotConfigured
    -- ^ R2 is not configured (env vars not set)
    deriving (Show, Eq)

instance Exception R2Error

-- | Handle to R2 client
data R2Handle = R2Handle
    { r2Config :: R2Config
    , r2Manager :: Manager
    }

-- | Load R2 configuration from environment variables
configFromEnv :: IO (Either R2Error R2Config)
configFromEnv = do
    mAccountId <- lookupEnv "WEAPON_R2_ACCOUNT_ID"
    mAccessKey <- lookupEnv "WEAPON_R2_ACCESS_KEY_ID"
    mSecretKey <- lookupEnv "WEAPON_R2_SECRET_KEY"
    mBucket <- lookupEnv "WEAPON_R2_BUCKET"
    mPrefix <- lookupEnv "WEAPON_R2_PREFIX"
    mEndpoint <- lookupEnv "WEAPON_R2_ENDPOINT"

    case (mAccountId, mAccessKey, mSecretKey) of
        (Just accountId, Just accessKey, Just secretKey) ->
            pure $
                Right
                    R2Config
                        { r2AccountId = T.pack accountId
                        , r2AccessKeyId = T.pack accessKey
                        , r2SecretAccessKey = T.pack secretKey
                        , r2Bucket = T.pack $ maybe "weapon-telemetry" id mBucket
                        , r2Prefix = T.pack $ maybe "telemetry" id mPrefix
                        , r2Endpoint = T.pack <$> mEndpoint
                        , r2Region = "auto" -- R2 always uses "auto"
                        }
        _ ->
            pure $ Left R2NotConfigured

{- | Load R2 configuration from Dhall TelemetryConfig with env var fallback.

This function first checks the Dhall config for R2 settings, then falls back
to environment variables for any missing values. This allows users to either
configure R2 entirely via Dhall, entirely via environment variables, or use
a combination of both.

Priority (highest to lowest):
1. Dhall config values
2. Environment variables
3. Default values (for bucket and prefix)
-}
configFromDhall :: TelemetryConfig -> IO (Either R2Error R2Config)
configFromDhall telConfig = do
    -- Get env vars as fallbacks
    mEnvAccountId <- lookupEnv "WEAPON_R2_ACCOUNT_ID"
    mEnvAccessKey <- lookupEnv "WEAPON_R2_ACCESS_KEY_ID"
    mEnvSecretKey <- lookupEnv "WEAPON_R2_SECRET_KEY"
    mEnvBucket <- lookupEnv "WEAPON_R2_BUCKET"
    mEnvPrefix <- lookupEnv "WEAPON_R2_PREFIX"
    mEnvEndpoint <- lookupEnv "WEAPON_R2_ENDPOINT"

    -- Extract R2 config from Dhall (if present)
    let mDhallR2 = telR2 telConfig

    -- Helper to get value from Dhall or env, preferring Dhall
    let getValue :: (R2StorageConfig -> Maybe Text) -> Maybe String -> Maybe Text
        getValue dhallGetter envVal =
            case mDhallR2 of
                Just r2cfg -> dhallGetter r2cfg <|> (T.pack <$> envVal)
                Nothing -> T.pack <$> envVal

    let mAccountId = getValue r2sAccountId mEnvAccountId
        mAccessKey = getValue r2sAccessKeyId mEnvAccessKey
        mSecretKey = getValue r2sSecretKey mEnvSecretKey
        mBucket = getValue r2sBucket mEnvBucket
        mPrefix = getValue r2sPrefix mEnvPrefix
        mEndpoint = getValue r2sEndpoint mEnvEndpoint

    case (mAccountId, mAccessKey, mSecretKey) of
        (Just accountId, Just accessKey, Just secretKey) ->
            pure $
                Right
                    R2Config
                        { r2AccountId = accountId
                        , r2AccessKeyId = accessKey
                        , r2SecretAccessKey = secretKey
                        , r2Bucket = maybe "weapon-telemetry" id mBucket
                        , r2Prefix = maybe "telemetry" id mPrefix
                        , r2Endpoint = mEndpoint
                        , r2Region = "auto" -- R2 always uses "auto"
                        }
        _ ->
            pure $ Left R2NotConfigured

-- | Check if R2 is configured
isConfigured :: IO Bool
isConfigured = do
    result <- configFromEnv
    pure $ case result of
        Right _ -> True
        Left _ -> False

-- | Create a new R2 handle
newR2Handle :: R2Config -> IO R2Handle
newR2Handle config = do
    manager <- newManager tlsManagerSettings
    pure $ R2Handle{r2Config = config, r2Manager = manager}

-- | Upload a segment file to R2
uploadSegment ::
    R2Handle ->
    Text ->
    -- ^ Session ID
    FilePath ->
    -- ^ Local segment path
    IO (Either R2Error ())
uploadSegment h sessionId localPath = do
    -- Read segment (TODO: add zstd compression)
    content <- BS.readFile localPath
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
    Text ->
    -- ^ Session ID
    Text ->
    -- ^ Project ID
    Text ->
    -- ^ Model ID
    Text ->
    -- ^ Agent
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
    Text ->
    -- ^ Object key
    ByteString ->
    -- ^ Content
    ByteString ->
    -- ^ Content-Type
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
    ByteString ->
    -- ^ HTTP method
    ByteString ->
    -- ^ Path (including bucket)
    ByteString ->
    -- ^ Host
    ByteString ->
    -- ^ Payload
    ByteString ->
    -- ^ Content-Type
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
            <> -- Query string (empty)
            canonicalHeaders
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
    Text ->
    -- ^ Session ID
    Int ->
    -- ^ Poll interval in seconds
    Log.Logger ->
    -- ^ Logger
    IO ReplicationWorker
startReplicationWorker r2 wal sessionId pollInterval logger = do
    stopVar <- newTVarIO False
    let lg = Log.withNS logger "r2"

    tid <- forkIO $ replicationLoop r2 wal sessionId stopVar pollInterval lg

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

-- | Main replication loop
replicationLoop ::
    R2Handle ->
    WAL.WALHandle ->
    Text ->
    TVar Bool ->
    Int ->
    Log.Logger ->
    IO ()
replicationLoop r2 wal sessionId stopVar pollInterval lg = loop
  where
    loop = do
        shouldStop <- readTVarIO stopVar
        if shouldStop
            then pure ()
            else do
                -- Find unreplicated segments
                segments <- WAL.listUnreplicatedSegments wal

                -- Upload each segment
                mapM_ uploadOne segments

                -- Wait before next poll
                threadDelay (pollInterval * 1000000)
                loop

    uploadOne (path, _startSeq, _endSeq) = do
        result <- uploadSegment r2 sessionId path
        case result of
            Right () -> do
                -- Update high water mark
                -- TODO: WAL.setHighWaterMark wal endSeq
                Log.logDebug lg ("Uploaded: " <> T.pack path) ()
            Left err -> do
                -- Log error, will retry next poll
                Log.logWarn lg ("Upload failed: " <> T.pack (show err)) ()
