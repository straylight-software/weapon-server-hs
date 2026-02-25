# Adding an LLM Provider

This tutorial explains how to add a new LLM provider to Weapon Server. The server uses a modular provider system that makes it straightforward to integrate new APIs.

## Provider Architecture

The LLM system consists of:

```
src/LLM/
├── Types.hs      -- Shared types (Message, Role, Content, etc.)
├── Anthropic.hs  -- Anthropic Claude implementation
└── OpenRouter.hs -- OpenRouter (multi-model) implementation
```

All providers use the types defined in `LLM.Types`, which follow the Anthropic Messages API format as the canonical internal representation.

## Step 1: Create the Provider Module

Create a new file `src/LLM/MyProvider.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : LLM.MyProvider
Description : MyProvider API client

Client implementation for the MyProvider LLM API.
-}
module LLM.MyProvider (
    -- * Client
    MyProviderClient (..),
    newClient,

    -- * API Calls
    chat,
    chatStream,
) where

import Data.Text (Text)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS qualified as HCT

import LLM.Types

-- | Client configuration
data MyProviderClient = MyProviderClient
    { mpApiKey :: Text
    , mpManager :: HC.Manager
    , mpBaseUrl :: Text
    }

-- | Create a new client
newClient :: Text -> IO MyProviderClient
newClient apiKey = do
    manager <- HCT.newTlsManager
    pure MyProviderClient
        { mpApiKey = apiKey
        , mpManager = manager
        , mpBaseUrl = "https://api.myprovider.com/v1"
        }
```

## Step 2: Implement Core Types

Define any provider-specific request/response types:

```haskell
-- Provider-specific request format
data MyProviderRequest = MyProviderRequest
    { mprModel :: Text
    , mprMessages :: [MyProviderMessage]
    , mprMaxTokens :: Maybe Int
    , mprStream :: Bool
    }

-- Convert from canonical format
fromChatRequest :: ChatRequest -> MyProviderRequest
fromChatRequest ChatRequest{..} = MyProviderRequest
    { mprModel = crModel
    , mprMessages = map convertMessage crMessages
    , mprMaxTokens = crMaxTokens
    , mprStream = crStream
    }

-- Convert to canonical format
toChatResponse :: MyProviderResponse -> ChatResponse
toChatResponse resp = ChatResponse
    { respId = mprId resp
    , respContent = convertContent (mprContent resp)
    , respModel = mprModel resp
    , respStopReason = convertStopReason (mprStopReason resp)
    , respUsage = convertUsage (mprUsage resp)
    }
```

## Step 3: Implement Non-Streaming Chat

```haskell
-- | Non-streaming chat completion
chat :: MyProviderClient -> ChatRequest -> IO (Either Text ChatResponse)
chat client req = do
    let request = fromChatRequest req
    
    -- Build HTTP request
    initReq <- HC.parseRequest (T.unpack (mpBaseUrl client) <> "/chat")
    let httpReq = initReq
            { HC.method = "POST"
            , HC.requestHeaders =
                [ ("Authorization", "Bearer " <> encodeUtf8 (mpApiKey client))
                , ("Content-Type", "application/json")
                ]
            , HC.requestBody = HC.RequestBodyLBS (encode request)
            }
    
    -- Execute request
    result <- try $ HC.httpLbs httpReq (mpManager client)
    case result of
        Left (e :: SomeException) -> 
            pure $ Left $ "Request failed: " <> T.pack (show e)
        Right response ->
            case eitherDecode (HC.responseBody response) of
                Left err -> pure $ Left $ "Parse error: " <> T.pack err
                Right resp -> pure $ Right $ toChatResponse resp
```

## Step 4: Implement Streaming

Streaming requires parsing Server-Sent Events (SSE):

```haskell
-- | Streaming chat completion
chatStream 
    :: MyProviderClient 
    -> ChatRequest 
    -> (Text -> IO ())  -- Callback for text deltas
    -> IO (Either Text ChatResponse)
chatStream client req onDelta = do
    let request = (fromChatRequest req) { mprStream = True }
    
    initReq <- HC.parseRequest (T.unpack (mpBaseUrl client) <> "/chat")
    let httpReq = initReq
            { HC.method = "POST"
            , HC.requestHeaders =
                [ ("Authorization", "Bearer " <> encodeUtf8 (mpApiKey client))
                , ("Content-Type", "application/json")
                , ("Accept", "text/event-stream")
                ]
            , HC.requestBody = HC.RequestBodyLBS (encode request)
            }
    
    -- Use withResponse for streaming
    HC.withResponse httpReq (mpManager client) $ \response -> do
        -- Parse SSE stream
        processSSEStream (HC.responseBody response) onDelta
```

### SSE Parsing

```haskell
-- | Parse SSE events from response body
processSSEStream :: BodyReader -> (Text -> IO ()) -> IO ()
processSSEStream body onDelta = loop ""
  where
    loop buffer = do
        chunk <- body
        if BS.null chunk
            then pure ()  -- Stream ended
            else do
                let newBuffer = buffer <> chunk
                -- Parse complete events (separated by double newline)
                let (events, remaining) = splitEvents newBuffer
                forM_ events $ \event ->
                    case parseEvent event of
                        Just delta -> onDelta delta
                        Nothing -> pure ()
                loop remaining

-- | Parse a single SSE event
parseEvent :: ByteString -> Maybe Text
parseEvent bs = do
    -- SSE format: "data: {...}\n\n"
    let line = C8.dropWhile (== ' ') $ C8.drop 5 bs  -- Drop "data:"
    json <- decode (LBS.fromStrict line)
    -- Extract text delta from provider-specific format
    extractDelta json
```

## Step 5: Register the Provider

Add your provider to `src/Provider/Types.hs`:

```haskell
data ProviderType
    = Anthropic
    | OpenRouter
    | MyProvider  -- Add your provider
    deriving (Eq, Show)
```

Update the provider registry in `src/Handlers.hs`:

```haskell
getProviderClient :: Text -> AppState -> IO SomeClient
getProviderClient providerId st = case providerId of
    "anthropic" -> do
        apiKey <- getEnv "ANTHROPIC_API_KEY"
        client <- Anthropic.newClient (T.pack apiKey)
        pure $ SomeAnthropicClient client
    "myprovider" -> do
        apiKey <- getEnv "MYPROVIDER_API_KEY"
        client <- MyProvider.newClient (T.pack apiKey)
        pure $ SomeMyProviderClient client
    _ -> error $ "Unknown provider: " <> T.unpack providerId
```

## Step 6: Add Configuration

In `dhall/Types/Provider.dhall`:

```dhall
let ProviderConfig =
      { Type =
          { apiKey : Optional Text
          , baseUrl : Optional Text
          , models : Optional (List Text)
          }
      , default =
          { apiKey = None Text
          , baseUrl = None Text
          , models = None (List Text)
          }
      }
```

Users can then configure in their `weapon.dhall`:

```dhall
let Types = ./dhall/Types.dhall

in Types.Config.default // {
  provider = Some [
    { mapKey = "myprovider"
    , mapValue = Types.Provider.ProviderConfig::{
        baseUrl = Some "https://custom.api.endpoint.com"
      }
    }
  ]
}
```

## Step 7: Add Tests

Create `test/Property/MyProviderProps.hs`:

```haskell
module Property.MyProviderProps (tests) where

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import LLM.MyProvider
import LLM.Types

-- | Test request conversion
prop_requestConversion :: Property
prop_requestConversion = property $ do
    model <- forAll genModelName
    let req = ChatRequest
            { crModel = model
            , crMessages = []
            , crMaxTokens = Just 100
            , crStream = False
            , crTools = Nothing
            }
        converted = fromChatRequest req
    mprModel converted === model

-- | Test SSE parsing
prop_parseEvent_validJson :: Property
prop_parseEvent_validJson = property $ do
    let event = "data: {\"delta\": {\"text\": \"hello\"}}\n\n"
    parseEvent event === Just "hello"

tests :: TestTree
tests = testGroup "MyProvider"
    [ testProperty "request conversion preserves model" prop_requestConversion
    , testProperty "SSE parsing extracts text" prop_parseEvent_validJson
    ]
```

## Best Practices

1. **Follow the canonical types** — Convert to/from `LLM.Types` at the boundary
1. **Handle errors gracefully** — Return `Either Text a`, not exceptions
1. **Support streaming** — Most UIs expect real-time token streaming
1. **Test SSE parsing** — Edge cases abound in streaming responses
1. **Document rate limits** — Note any provider-specific constraints
1. **Use connection pooling** — Reuse the HTTP manager for performance

## Example: Complete Provider

See `src/LLM/Anthropic.hs` for a complete, production-ready implementation that handles:

- Streaming and non-streaming modes
- Tool use (function calling)
- Error handling and retries
- Token usage tracking
- Multiple content block types
