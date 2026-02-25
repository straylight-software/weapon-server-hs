{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : LLM.Anthropic
Description : Anthropic Claude API client

This module provides a client for the Anthropic Messages API, supporting
both streaming and non-streaming chat completions with Claude models.

==== Example usage

@
client <- newClient "your-api-key"
let request = ChatRequest
      { crModel = "claude-3-opus-20240229"
      , crMessages = [Message User (SimpleContent "Hello!")]
      , crMaxTokens = 1024
      , crSystem = Nothing
      , crTemperature = Nothing
      , crTools = Nothing
      , crStream = False
      }
response <- chat client request
@
-}
module LLM.Anthropic (
    -- * Client
    AnthropicClient (..),
    newClient,

    -- * API Calls
    chat,
    chatStream,

    -- * Pure Parsing (for testing)
    parseSSE,
    parseEvent,
    parseStreamEvent,
) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson (Value (..), decode, eitherDecode, encode, parseJSON, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS qualified as HCT
import Network.HTTP.Types qualified as HT

import LLM.Types

{- | Anthropic API client configuration.

Holds the API key and HTTP manager for making requests to the
Anthropic Messages API.
-}
data AnthropicClient = AnthropicClient
    { acApiKey :: Text
    -- ^ Anthropic API key (starts with "sk-ant-")
    , acManager :: HC.Manager
    -- ^ HTTP connection manager for connection pooling
    , acBaseUrl :: Text
    -- ^ Base URL for API requests (default: "https://api.anthropic.com")
    }

{- | Create a new Anthropic client with the given API key.

Uses default TLS settings and the standard Anthropic API endpoint.

==== Example

>>> client <- newClient "sk-ant-..."
-}
newClient :: Text -> IO AnthropicClient
newClient apiKey = do
    manager <- HC.newManager HCT.tlsManagerSettings
    pure
        AnthropicClient
            { acApiKey = apiKey
            , acManager = manager
            , acBaseUrl = "https://api.anthropic.com"
            }

{- | Send a non-streaming chat completion request.

Makes a single request and waits for the complete response.
For long responses, consider using 'chatStream' instead.

Returns 'Left' with an error message on failure, or 'Right' with
the response on success.
-}
chat :: AnthropicClient -> ChatRequest -> IO (Either Text ChatResponse)
chat client req = do
    let reqBody = encode req{crStream = False}

    result <- makeRequest client "/v1/messages" reqBody

    case result of
        Left err -> pure $ Left err
        Right body -> case eitherDecode body of
            Left parseErr -> pure $ Left $ "Parse error: " <> T.pack parseErr
            Right resp -> pure $ Right resp

{- | Send a streaming chat completion request.

The response is delivered incrementally via the callback function,
which is called for each 'StreamEvent' as it arrives. The stream
ends when a 'MessageStop' event is received.

==== Example

@
chatStream client request $ \event -> case event of
    ContentBlockDelta _ text -> putStr text
    MessageStop -> putStrLn ""
    _ -> pure ()
@
-}
chatStream :: AnthropicClient -> ChatRequest -> (StreamEvent -> IO ()) -> IO (Either Text ())
chatStream client req onEvent = do
    let reqBody = encode req{crStream = True}

    initReq <- HC.parseRequest $ T.unpack (acBaseUrl client) <> "/v1/messages"
    let httpReq =
            initReq
                { HC.method = "POST"
                , HC.requestHeaders =
                    [ ("Content-Type", "application/json")
                    , ("x-api-key", encodeUtf8 $ acApiKey client)
                    , ("anthropic-version", "2023-06-01")
                    ]
                , HC.requestBody = HC.RequestBodyLBS reqBody
                }

    result <- try @SomeException $ HC.withResponse httpReq (acManager client) $ \resp -> do
        let status = HC.responseStatus resp
        if HT.statusCode status /= 200
            then do
                body <- HC.brConsume $ HC.responseBody resp
                pure $ Left $ "API error: " <> T.pack (show status) <> " " <> T.pack (show body)
            else do
                -- Parse SSE stream
                bufferRef <- newIORef ""
                let loop = do
                        chunk <- HC.brRead $ HC.responseBody resp
                        if BS.null chunk
                            then pure ()
                            else do
                                buffer <- readIORef bufferRef
                                let fullBuffer = buffer <> chunk
                                -- Process complete events
                                (remaining, events) <- parseSSE fullBuffer
                                writeIORef bufferRef remaining
                                mapM_ onEvent events
                                -- Check if we got MessageStop
                                unless (any isMessageStop events) loop
                loop
                pure $ Right ()

    case result of
        Left e -> pure $ Left $ T.pack $ show e
        Right (Left err) -> pure $ Left err
        Right (Right ()) -> pure $ Right ()

-- | Make an HTTP request to Anthropic API
makeRequest :: AnthropicClient -> Text -> LBS.ByteString -> IO (Either Text LBS.ByteString)
makeRequest AnthropicClient{..} path body = do
    initReq <- HC.parseRequest $ T.unpack acBaseUrl <> T.unpack path
    let req =
            initReq
                { HC.method = "POST"
                , HC.requestHeaders =
                    [ ("Content-Type", "application/json")
                    , ("x-api-key", encodeUtf8 acApiKey)
                    , ("anthropic-version", "2023-06-01")
                    ]
                , HC.requestBody = HC.RequestBodyLBS body
                }

    result <- try @SomeException $ HC.httpLbs req acManager

    case result of
        Left e -> pure $ Left $ T.pack $ show e
        Right resp -> do
            let status = HC.responseStatus resp
            if HT.statusCode status == 200
                then pure $ Right $ HC.responseBody resp
                else
                    pure $
                        Left $
                            "API error "
                                <> T.pack (show $ HT.statusCode status)
                                <> ": "
                                <> decodeUtf8 (LBS.toStrict $ HC.responseBody resp)

{- | Parse Server-Sent Events (SSE) from a byte buffer.

Processes the buffer line by line, extracting complete SSE events.
Returns a tuple of (remaining unparsed bytes, list of parsed events).

This is exposed for testing. In normal usage, it's called internally
by 'chatStream'.

==== SSE Format

SSE events have the format:

@
event: message_start
data: {"type": "message_start", ...}

event: content_block_delta
data: {"type": "content_block_delta", ...}
@
-}
parseSSE :: ByteString -> IO (ByteString, [StreamEvent])
parseSSE buffer = do
    let chunks = C8.lines buffer
    go chunks id ""
  where
    go [] events remaining = pure (remaining, events [])
    go (l : ls) events _
        | "data: " `BS.isPrefixOf` l = do
            let jsonPart = BS.drop 6 l
            case parseEvent jsonPart of
                Just event -> go ls (events . (event :)) ""
                Nothing -> go ls events l -- Keep unparsed line
        | "event: " `BS.isPrefixOf` l = go ls events "" -- Skip event type lines
        | BS.null l = go ls events "" -- Empty line = event boundary
        | otherwise = go ls events l -- Incomplete line

{- | Parse a single SSE data line from JSON bytes.

Takes the JSON portion of an SSE data line (after "data: ") and
attempts to parse it as a 'StreamEvent'.
-}
parseEvent :: ByteString -> Maybe StreamEvent
parseEvent bs = do
    json <- decode (LBS.fromStrict bs)
    parseStreamEvent json

{- | Parse a stream event from a JSON 'Value'.

This is the pure parsing logic, separate from the byte decoding.
Useful for testing event parsing in isolation.
-}
parseStreamEvent :: Value -> Maybe StreamEvent
parseStreamEvent json = flip parseMaybe json $ \case
    Object obj -> do
        typ <- obj .: "type"
        case typ :: Text of
            "message_start" -> do
                msg <- obj .: "message"
                MessageStart <$> parseJSON msg
            "content_block_start" -> do
                idx <- obj .: "index"
                block <- obj .: "content_block"
                ContentBlockStart idx <$> parseJSON block
            "content_block_delta" -> do
                idx <- obj .: "index"
                delta <- obj .: "delta"
                txt <- delta .: "text"
                pure $ ContentBlockDelta idx txt
            "content_block_stop" -> do
                idx <- obj .: "index"
                pure $ ContentBlockStop idx
            "message_delta" -> do
                delta <- obj .: "delta"
                stopReason <- delta .: "stop_reason"
                usage <- obj .: "usage"
                pure $ MessageDelta stopReason usage
            "message_stop" -> pure MessageStop
            "ping" -> pure Ping
            _otherType -> fail "Unknown event type"
    _otherValue -> fail "Not an object"
