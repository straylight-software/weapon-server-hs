{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Proxy.Types
Description : Types for the MITM proxy

Types for the MITM (Man-In-The-Middle) proxy that intercepts and logs
LLM API traffic for monitoring, auditing, and token counting.

"Full take" - capture everything, analyze later.

The proxy sits between sandbox environments and LLM APIs, providing:

* Request/response logging to JSONL files
* Token usage extraction from API responses
* Per-session tracking of API calls
* TLS termination with dynamic certificates
-}
module Proxy.Types (
    -- * Log Entry Types
    -- $logentry
    LogEntry (..),
    RequestLog (..),
    ResponseLog (..),

    -- * Token Usage
    -- $tokenusage
    TokenUsage (..),
    addTokens,

    -- * Proxy Configuration
    -- $proxyconfig
    ProxyConfig (..),
    defaultProxyConfig,

    -- * Runtime State
    -- $proxystate
    ProxyState (..),

    -- * Errors
    -- $errors
    ProxyError (..),

    -- * Header Utilities
    -- $headers
    headersToMap,
    isHopHeader,
) where

import Control.Concurrent.STM (TVar)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.ByteString (ByteString)
import Data.CaseInsensitive (original)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.HTTP.Types (HeaderName)

import Data.Map.Strict qualified as Map

{- $logentry
Log entries capture complete request/response cycles through the proxy.
Each entry includes timing information, the originating session, and
any token usage parsed from LLM API responses.
-}

{- | A single logged request/response pair.

Represents a complete HTTP transaction captured by the proxy,
including the request, response (if successful), and any token
usage information extracted from LLM API responses.

Log entries are written to JSONL files for audit and analysis.
-}
data LogEntry = LogEntry
    { leTimestamp :: !UTCTime
    -- ^ When the request was received by the proxy
    , leSessionId :: !Text
    -- ^ Which PTY session made this request (from X-Opencode-Session header)
    , leRequestId :: !Text
    -- ^ Unique request ID (e.g., "req_42")
    , leMethod :: !Text
    -- ^ HTTP method (GET, POST, etc.)
    , leUrl :: !Text
    -- ^ Full URL including query string
    , leHost :: !Text
    -- ^ Target host from Host header
    , leRequest :: !RequestLog
    -- ^ Captured request details
    , leResponse :: !(Maybe ResponseLog)
    -- ^ Response details, or 'Nothing' if request failed
    , leTokens :: !(Maybe TokenUsage)
    -- ^ Token usage parsed from LLM responses
    , leDuration :: !Double
    -- ^ Response time in milliseconds
    }
    deriving (Eq, Show, Generic)

instance ToJSON LogEntry where
    toJSON LogEntry{..} =
        object
            [ "ts" .= leTimestamp
            , "session" .= leSessionId
            , "request_id" .= leRequestId
            , "method" .= leMethod
            , "url" .= leUrl
            , "host" .= leHost
            , "request" .= leRequest
            , "response" .= leResponse
            , "tokens" .= leTokens
            , "duration" .= leDuration
            ]

instance FromJSON LogEntry where
    parseJSON = withObject "LogEntry" $ \v ->
        LogEntry
            <$> v .: "ts"
            <*> v .: "session"
            <*> v .: "request_id"
            <*> v .: "method"
            <*> v .: "url"
            <*> v .: "host"
            <*> v .: "request"
            <*> v .:? "response"
            <*> v .:? "tokens"
            <*> v .: "duration"

{- | Logged request details.

Contains the headers and body of an HTTP request. The body may be
truncated if it exceeds 'pcMaxBodyLog' bytes from 'ProxyConfig'.
-}
data RequestLog = RequestLog
    { rlHeaders :: !(Map Text Text)
    -- ^ Request headers as key-value pairs
    , rlBody :: !(Maybe Text)
    -- ^ Request body (truncated if larger than max), or 'Nothing' if empty
    , rlSize :: !Int
    -- ^ Original body size in bytes (before truncation)
    }
    deriving (Eq, Show, Generic)

instance ToJSON RequestLog where
    toJSON RequestLog{..} =
        object
            [ "headers" .= rlHeaders
            , "body" .= rlBody
            , "size" .= rlSize
            ]

instance FromJSON RequestLog where
    parseJSON = withObject "RequestLog" $ \v ->
        RequestLog
            <$> v .: "headers"
            <*> v .:? "body"
            <*> v .: "size"

{- | Logged response details.

Contains the status, headers, and body of an HTTP response.
The 'rsStream' flag indicates if this was a Server-Sent Events
(SSE) streaming response, common for LLM API completions.
-}
data ResponseLog = ResponseLog
    { rsStatus :: !Int
    -- ^ HTTP status code (e.g., 200, 404, 500)
    , rsHeaders :: !(Map Text Text)
    -- ^ Response headers as key-value pairs
    , rsBody :: !(Maybe Text)
    -- ^ Response body (truncated if large), or 'Nothing' if empty
    , rsSize :: !Int
    -- ^ Original body size in bytes (before truncation)
    , rsStream :: !Bool
    -- ^ 'True' if this was a streaming (SSE) response
    }
    deriving (Eq, Show, Generic)

instance ToJSON ResponseLog where
    toJSON ResponseLog{..} =
        object
            [ "status" .= rsStatus
            , "headers" .= rsHeaders
            , "body" .= rsBody
            , "size" .= rsSize
            , "stream" .= rsStream
            ]

instance FromJSON ResponseLog where
    parseJSON = withObject "ResponseLog" $ \v ->
        ResponseLog
            <$> v .: "status"
            <*> v .: "headers"
            <*> v .:? "body"
            <*> v .: "size"
            <*> v .: "stream"

{- | Token usage from LLM API responses.

Captures token counts from API responses for billing and monitoring.
Different providers have different response formats, but this type
normalizes them to a common structure.

Anthropic responses include cache metrics ('tuCacheRead' and 'tuCacheWrite'),
while OpenAI responses do not (these fields will be 'Nothing').
-}
data TokenUsage = TokenUsage
    { tuProvider :: !Text
    -- ^ Provider name: @"anthropic"@, @"openai"@, or @"openrouter"@
    , tuModel :: !Text
    -- ^ Model identifier (e.g., @"claude-3-opus-20240229"@)
    , tuInputTokens :: !Int
    -- ^ Number of input (prompt) tokens
    , tuOutputTokens :: !Int
    -- ^ Number of output (completion) tokens
    , tuCacheRead :: !(Maybe Int)
    -- ^ Anthropic: tokens read from cache (@cache_read_input_tokens@)
    , tuCacheWrite :: !(Maybe Int)
    -- ^ Anthropic: tokens written to cache (@cache_creation_input_tokens@)
    }
    deriving (Eq, Show, Generic)

instance ToJSON TokenUsage where
    toJSON TokenUsage{..} =
        object
            [ "provider" .= tuProvider
            , "model" .= tuModel
            , "input_tokens" .= tuInputTokens
            , "output_tokens" .= tuOutputTokens
            , "cache_read" .= tuCacheRead
            , "cache_write" .= tuCacheWrite
            ]

instance FromJSON TokenUsage where
    parseJSON = withObject "TokenUsage" $ \v ->
        TokenUsage
            <$> v .: "provider"
            <*> v .: "model"
            <*> v .: "input_tokens"
            <*> v .: "output_tokens"
            <*> v .:? "cache_read"
            <*> v .:? "cache_write"

{- $proxyconfig
Configuration for the MITM proxy. Use 'defaultProxyConfig' to create
a configuration with sensible defaults, then customize as needed.
-}

{- | Proxy configuration.

Controls the proxy's listening port, logging behavior, TLS certificates,
and host filtering. Use 'defaultProxyConfig' for sensible defaults.
-}
data ProxyConfig = ProxyConfig
    { pcPort :: !Int
    -- ^ Listen port (default: 8888)
    , pcLogDir :: !FilePath
    -- ^ Directory for JSONL log files
    , pcCaKeyPath :: !FilePath
    -- ^ Path to CA private key for MITM TLS termination
    , pcCaCertPath :: !FilePath
    -- ^ Path to CA certificate for MITM TLS termination
    , pcMaxBodyLog :: !Int
    -- ^ Maximum body size to log in bytes (default: 1MB). Larger bodies are truncated.
    , pcAllowedHosts :: !(Maybe [Text])
    -- ^ Optional whitelist of allowed hosts. 'Nothing' means allow all hosts.
    }
    deriving (Eq, Show)

{- | Create a 'ProxyConfig' with sensible defaults.

* Port: 8888
* CA key/cert: @\<logDir\>\/ca.key@ and @\<logDir\>\/ca.crt@
* Max body log: 1MB
* Allowed hosts: all (no filtering)

==== __Examples__

>>> let cfg = defaultProxyConfig "/var/log/proxy"
>>> pcPort cfg
8888
>>> pcMaxBodyLog cfg
1048576
-}
defaultProxyConfig :: FilePath -> ProxyConfig
defaultProxyConfig logDir =
    ProxyConfig
        { pcPort = 8888
        , pcLogDir = logDir
        , pcCaKeyPath = logDir <> "/ca.key"
        , pcCaCertPath = logDir <> "/ca.crt"
        , pcMaxBodyLog = 1024 * 1024 -- 1MB
        , pcAllowedHosts = Nothing
        }

{- $proxystate
Runtime state for a running proxy server. This is created by 'Proxy.Proxy.start'
and contains mutable state for request counting and token aggregation.
-}

{- | Runtime proxy state.

Contains configuration and mutable state for a running proxy server.
The 'TVar's provide thread-safe access to counters and aggregated data.
-}
data ProxyState = ProxyState
    { psConfig :: !ProxyConfig
    -- ^ Immutable configuration
    , psRequestCount :: !(TVar Word64)
    -- ^ Monotonically increasing request counter for generating IDs
    , psTokenTotals :: !(TVar (Map Text TokenUsage))
    -- ^ Per-session aggregated token usage
    }

-- ═══════════════════════════════════════════════════════════════════════════
-- Token Usage Operations
-- ═══════════════════════════════════════════════════════════════════════════

{- $tokenusage
Token usage tracking for LLM API calls. The 'TokenUsage' type captures
input and output token counts, plus Anthropic-specific cache metrics.
Use 'addTokens' to combine usage from multiple requests.
-}

{- | Add token counts from two 'TokenUsage' values.

Combines token counts for aggregation across multiple requests.
Cache read/write values are combined if both are present, otherwise
the result is 'Nothing'.

==== __Examples__

>>> let t1 = TokenUsage "anthropic" "claude-3" 100 50 (Just 10) Nothing
>>> let t2 = TokenUsage "anthropic" "claude-3" 200 75 (Just 20) Nothing
>>> addTokens t1 t2
TokenUsage {tuProvider = "anthropic", tuModel = "claude-3", tuInputTokens = 300, ...}

The provider and model from the first argument are preserved:

>>> tuInputTokens (addTokens t1 t2)
300
-}
addTokens :: TokenUsage -> TokenUsage -> TokenUsage
addTokens a b =
    a
        { tuInputTokens = tuInputTokens a + tuInputTokens b
        , tuOutputTokens = tuOutputTokens a + tuOutputTokens b
        , tuCacheRead = (+) <$> tuCacheRead a <*> tuCacheRead b
        , tuCacheWrite = (+) <$> tuCacheWrite a <*> tuCacheWrite b
        }

-- ═══════════════════════════════════════════════════════════════════════════
-- Header Utilities
-- ═══════════════════════════════════════════════════════════════════════════

{- $headers
Utilities for working with HTTP headers. These functions help convert
between WAI/http-types header representations and the 'Map Text Text'
format used for JSON logging.
-}

{- | Convert HTTP headers to a 'Map' for JSON serialization.

Header names are converted to lowercase text, and values are decoded
as UTF-8. This is used for both request and response header logging.

==== __Examples__

>>> headersToMap [("Content-Type", "application/json")]
fromList [("content-type","application/json")]
-}
headersToMap :: [(HeaderName, ByteString)] -> Map Text Text
headersToMap headers =
    Map.fromList [(decodeUtf8 (original k), decodeUtf8 v) | (k, v) <- headers]

{- | Check if a header is a hop-by-hop header that should not be forwarded.

Hop-by-hop headers are specific to a single transport-level connection
and must not be retransmitted by proxies. See RFC 2616 Section 13.5.1.

==== __Examples__

>>> isHopHeader "connection"
True
>>> isHopHeader "content-type"
False
-}
isHopHeader :: HeaderName -> Bool
isHopHeader h =
    h
        `elem` [ "connection"
               , "keep-alive"
               , "proxy-authenticate"
               , "proxy-authorization"
               , "te"
               , "trailer"
               , "transfer-encoding"
               , "upgrade"
               ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Errors
-- ═══════════════════════════════════════════════════════════════════════════

{- $errors
Errors that can occur when starting or running the proxy server.
-}

{- | Proxy startup errors.

These errors indicate that the proxy could not be started and represent
fatal conditions that should be reported to the user.
-}
data ProxyError
    = -- | The configured port is already in use by another process
      PortInUse !Int
    | -- | Failed to bind to the port for another reason
      BindFailed !Int !String
    deriving (Eq, Show)
