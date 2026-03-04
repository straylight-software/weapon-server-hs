{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : LLM.OpenRouter
Description : OpenRouter API client for multi-provider LLM access

OpenRouter provides a unified API for accessing multiple LLM providers
(OpenAI, Anthropic, Google, etc.) through a single endpoint. This module
implements the OpenAI-compatible chat completions format used by OpenRouter.

==== Key Features

* Non-streaming and streaming chat completions
* Tool/function calling support
* Dynamic model discovery via the models endpoint

==== Example Usage

@
client <- newClient "your-openrouter-key"
let request = ChatRequest
      { crModel = "anthropic/claude-3-opus"
      , crMessages = [simpleMessage User "Hello!"]
      , crMaxTokens = Just 1024
      , crTemperature = Nothing
      , crStream = False
      , crTools = Nothing
      }
response <- chat client request
@

==== Notes

This module uses curl for streaming requests as a workaround for IPv6
timeout issues with the http-client library on some systems.
-}
module LLM.OpenRouter (
    -- * Client
    Client (..),
    newClient,

    -- * API Calls
    chat,
    chatStream,
    chatStreamWithTools,
    fetchModels,

    -- * Request/Response Types
    ChatRequest (..),
    ChatResponse (..),
    Choice (..),
    Usage (..),
    StreamResult (..),

    -- * Message Types
    Message (..),
    ChatMessage (..),
    Role (..),

    -- * Tool Types
    Tool (..),
    ToolFunction (..),
    ToolCall (..),
    ToolCallFunction (..),
    ToolResultMessage (..),

    -- * Message Helpers
    simpleMessage,
    toolResultMessage,
    assistantMessageWithTools,
    toolResultChatMessage,

    -- * Tool Conversion
    toolDefToOpenAI,

    -- * Pure Parsing (for testing)
    extractDelta,
    extractFinishReason,
    parseToolCallDeltas,
    assembleToolCalls,
    ToolCallPart (..),
    ToolCallDelta (..),
    mergeToolCallDelta,

    -- * JSON Utilities
    extractFieldText,
    extractFieldValue,
)
where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS qualified as HCT
import Network.HTTP.Types qualified as HT
import Provider.Types qualified as PT
import Control.Concurrent.Async (withAsync, wait)
import System.Exit (ExitCode (..))
import System.IO (hClose, hGetContents, hGetLine, hIsEOF)
import System.Process (StdStream (..), createProcess, proc, std_err, std_in, std_out, waitForProcess)

-- ============================================================================
-- Message Roles
-- ============================================================================

{- | The role of a message sender in OpenAI-format conversations.

Note: This is a separate type from 'LLM.Types.Role' because OpenRouter
uses the OpenAI format which has slightly different semantics.
-}
data Role
    = -- | Human user input
      User
    | -- | LLM assistant response
      Assistant
    | -- | System instructions/prompt
      System
    deriving (Eq, Show, Generic)

instance ToJSON Role where
    toJSON User = "user"
    toJSON Assistant = "assistant"
    toJSON System = "system"

instance FromJSON Role where
    parseJSON = withText "Role" $ \case
        "user" -> pure User
        "assistant" -> pure Assistant
        "system" -> pure System
        _unknownRole -> fail "Unknown role"

-- ============================================================================
-- Tool Types
-- ============================================================================

{- | A tool call from the assistant in an OpenAI-format response.

When the LLM decides to use a tool, it generates one or more ToolCall
objects specifying which functions to invoke and with what arguments.
-}
data ToolCall = ToolCall
    { tcId :: Text
    -- ^ Unique identifier for matching results to calls
    , tcType :: Text
    -- ^ Type of tool (always "function" currently)
    , tcFunction :: ToolCallFunction
    -- ^ The function to call
    }
    deriving (Eq, Show, Generic)

-- | The function details within a tool call.
data ToolCallFunction = ToolCallFunction
    { tcfName :: Text
    -- ^ Name of the function to call
    , tcfArguments :: Text
    -- ^ JSON string containing the function arguments
    }
    deriving (Eq, Show, Generic)

instance ToJSON ToolCall where
    toJSON ToolCall{..} =
        object
            [ "id" .= tcId
            , "type" .= tcType
            , "function" .= tcFunction
            ]

instance FromJSON ToolCall where
    parseJSON = withObject "ToolCall" $ \v ->
        ToolCall
            <$> v .: "id"
            <*> v .: "type"
            <*> v .: "function"

instance ToJSON ToolCallFunction where
    toJSON ToolCallFunction{..} =
        object
            [ "name" .= tcfName
            , "arguments" .= tcfArguments
            ]

instance FromJSON ToolCallFunction where
    parseJSON = withObject "ToolCallFunction" $ \v ->
        ToolCallFunction
            <$> v .: "name"
            <*> v .: "arguments"

{- | Accumulated tool call parts during streaming.

Tool calls arrive in chunks during streaming. This type accumulates
the pieces until the complete tool call can be assembled.
-}
data ToolCallPart = ToolCallPart
    { tcpIndex :: Int
    -- ^ Index of this tool call in the array
    , tcpId :: Text
    -- ^ Tool call ID (may be empty initially)
    , tcpType :: Text
    -- ^ Tool type (usually "function")
    , tcpName :: Text
    -- ^ Function name (may be empty initially)
    , tcpArgs :: Text
    -- ^ Accumulated JSON arguments string
    }
    deriving (Eq, Show)

{- | A single delta update for a streaming tool call.

During streaming, tool calls arrive as incremental updates.
Each delta may contain partial information that needs to be
accumulated with previous deltas for the same index.
-}
data ToolCallDelta = ToolCallDelta
    { tcdIndex :: Int
    -- ^ Index of the tool call being updated
    , tcdId :: Maybe Text
    -- ^ Tool call ID (present in first delta)
    , tcdType :: Maybe Text
    -- ^ Tool type (present in first delta)
    , tcdName :: Maybe Text
    -- ^ Function name (present in first delta)
    , tcdArgs :: Maybe Text
    -- ^ Partial arguments string (accumulates across deltas)
    }
    deriving (Eq, Show)

{- | A tool result message sent back to the API after executing a tool.

After executing a function requested via 'ToolCall', the result
is sent back using this message type.
-}
data ToolResultMessage = ToolResultMessage
    { trmRole :: Text
    -- ^ Role (always "tool")
    , trmToolCallId :: Text
    -- ^ ID of the tool call this responds to (must match 'tcId')
    , trmContent :: Text
    -- ^ The result content (tool output or error message)
    }
    deriving (Eq, Show, Generic)

instance ToJSON ToolResultMessage where
    toJSON ToolResultMessage{..} =
        object
            [ "role" .= trmRole
            , "tool_call_id" .= trmToolCallId
            , "content" .= trmContent
            ]

-- ============================================================================
-- Message Types
-- ============================================================================

{- | A chat message that can be either a regular message or a tool result.

This sum type allows the messages list to contain both regular
conversation messages and tool result responses in a single list.
-}
data ChatMessage
    = -- | A regular conversation message
      RegularMessage Message
    | -- | A tool execution result
      ToolResult ToolResultMessage
    deriving (Eq, Show)

instance ToJSON ChatMessage where
    toJSON (RegularMessage m) = toJSON m
    toJSON (ToolResult tr) = toJSON tr

{- | A chat message in OpenAI format.

In OpenAI's format, content can be null when tool_calls are present.
This differs from Anthropic's format where content and tool use are
separate content blocks.
-}
data Message = Message
    { msgRole :: Role
    -- ^ Who sent this message
    , msgContent :: Maybe Text
    -- ^ Text content (may be Nothing when tool_calls present)
    , msgToolCalls :: Maybe [ToolCall]
    -- ^ Tool calls requested by the assistant
    }
    deriving (Eq, Show, Generic)

instance ToJSON Message where
    toJSON Message{..} =
        object $
            filter
                ((/= Null) . snd)
                [ "role" .= msgRole
                , "content" .= msgContent
                , "tool_calls" .= msgToolCalls
                ]

instance FromJSON Message where
    parseJSON = withObject "Message" $ \v ->
        Message
            <$> v .: "role"
            <*> v .:? "content"
            <*> v .:? "tool_calls"

-- ============================================================================
-- Message Construction Helpers
-- ============================================================================

{- | Create a simple text message.

==== Example

>>> simpleMessage User "Hello, how are you?"
RegularMessage (Message {msgRole = User, msgContent = Just "Hello, how are you?", msgToolCalls = Nothing})
-}
simpleMessage :: Role -> Text -> ChatMessage
simpleMessage role content =
    RegularMessage $
        Message
            { msgRole = role
            , msgContent = Just content
            , msgToolCalls = Nothing
            }

{- | Create a tool result message.

==== Example

>>> toolResultMessage "call_123" "The file was created successfully"
ToolResultMessage {trmRole = "tool", trmToolCallId = "call_123", trmContent = "The file was created successfully"}
-}
toolResultMessage :: Text -> Text -> ToolResultMessage
toolResultMessage toolCallId content =
    ToolResultMessage
        { trmRole = "tool"
        , trmToolCallId = toolCallId
        , trmContent = content
        }

{- | Create an assistant message that includes tool calls.

This is used when replaying the assistant's response in the
conversation history after tool execution.
-}
assistantMessageWithTools :: Maybe Text -> [ToolCall] -> ChatMessage
assistantMessageWithTools content toolCalls =
    RegularMessage $
        Message
            { msgRole = Assistant
            , msgContent = content
            , msgToolCalls = if null toolCalls then Nothing else Just toolCalls
            }

{- | Create a tool result as a 'ChatMessage'.

Convenience wrapper around 'toolResultMessage' that returns a 'ChatMessage'
for direct inclusion in the messages list.
-}
toolResultChatMessage :: Text -> Text -> ChatMessage
toolResultChatMessage toolCallId content =
    ToolResult $ toolResultMessage toolCallId content

-- ============================================================================
-- Tool Definition Types
-- ============================================================================

{- | A tool definition for the OpenAI-compatible API.

Tools describe functions that the LLM can request to call.
Currently only function tools are supported.
-}
data Tool = Tool
    { toolType :: Text
    -- ^ Type of tool (always "function" currently)
    , toolFunction :: ToolFunction
    -- ^ The function definition
    }
    deriving (Eq, Show, Generic)

-- | Function definition within a tool.
data ToolFunction = ToolFunction
    { tfName :: Text
    -- ^ Name of the function
    , tfDescription :: Text
    -- ^ Description of what the function does
    , tfParameters :: Value
    -- ^ JSON Schema describing the function parameters
    }
    deriving (Eq, Show, Generic)

instance ToJSON Tool where
    toJSON Tool{..} =
        object
            [ "type" .= toolType
            , "function" .= toolFunction
            ]

instance ToJSON ToolFunction where
    toJSON ToolFunction{..} =
        object
            [ "name" .= tfName
            , "description" .= tfDescription
            , "parameters" .= tfParameters
            ]

-- ============================================================================
-- JSON Utilities
-- ============================================================================

{- | Extract a text field from a JSON Value.

Returns an empty string if the value is not an object, the key doesn't
exist, or the field is not a string.

==== Example

>>> extractFieldText "name" (object ["name" .= "test"])
"test"

>>> extractFieldText "missing" (object ["name" .= "test"])
""
-}
extractFieldText :: Text -> Value -> Text
extractFieldText key (Object obj) = case KM.lookup (K.fromText key) obj of
    Just (String s) -> s
    Just (Object _) -> ""
    Just (Array _) -> ""
    Just (Number _) -> ""
    Just (Bool _) -> ""
    Just Null -> ""
    Nothing -> ""
extractFieldText _ (Array _) = ""
extractFieldText _ (String _) = ""
extractFieldText _ (Number _) = ""
extractFieldText _ (Bool _) = ""
extractFieldText _ Null = ""

{- | Extract a JSON Value from an object field with a fallback.

Returns the fallback if the value is not an object or the key doesn't exist.

==== Example

>>> extractFieldValue "params" (object ["params" .= object ["x" .= (1 :: Int)]]) Null
Object (fromList [("x",Number 1.0)])
-}
extractFieldValue :: Text -> Value -> Value -> Value
extractFieldValue key (Object obj) fallback = fromMaybe fallback (KM.lookup (K.fromText key) obj)
extractFieldValue _ _ fallback = fallback

{- | Convert an Anthropic-style tool definition to OpenAI format.

Anthropic uses @input_schema@ for parameters, while OpenAI uses @parameters@.
This function handles the conversion.
-}
toolDefToOpenAI :: Value -> Tool
toolDefToOpenAI v =
    Tool
        { toolType = "function"
        , toolFunction =
            ToolFunction
                { tfName = extractFieldText "name" v
                , tfDescription = extractFieldText "description" v
                , tfParameters = extractFieldValue "input_schema" v (object [])
                }
        }

-- ============================================================================
-- Request/Response Types
-- ============================================================================

{- | A chat completion request in OpenAI format.

This is the request body sent to the OpenRouter chat completions endpoint.
-}
data ChatRequest = ChatRequest
    { crModel :: Text
    -- ^ Model identifier (e.g., "anthropic/claude-3-opus")
    , crMessages :: [ChatMessage]
    -- ^ Conversation history
    , crMaxTokens :: Maybe Int
    -- ^ Maximum tokens to generate (optional)
    , crTemperature :: Maybe Double
    -- ^ Sampling temperature (0.0-2.0, optional)
    , crStream :: Bool
    -- ^ Whether to stream the response
    , crTools :: Maybe [Tool]
    -- ^ Tool definitions for function calling (optional)
    }
    deriving (Eq, Show, Generic)

instance ToJSON ChatRequest where
    toJSON ChatRequest{..} =
        object $
            filter
                ((/= Null) . snd)
                [ "model" .= crModel
                , "messages" .= crMessages
                , "max_tokens" .= crMaxTokens
                , "temperature" .= crTemperature
                , "stream" .= crStream
                , "tools" .= crTools
                ]

-- | Token usage statistics for a request/response.
data Usage = Usage
    { usagePromptTokens :: Int
    -- ^ Tokens in the prompt
    , usageCompletionTokens :: Int
    -- ^ Tokens in the completion
    , usageTotalTokens :: Int
    -- ^ Total tokens (prompt + completion)
    }
    deriving (Eq, Show, Generic)

instance FromJSON Usage where
    parseJSON = withObject "Usage" $ \v ->
        Usage
            <$> v .:? "prompt_tokens" .!= 0
            <*> v .:? "completion_tokens" .!= 0
            <*> v .:? "total_tokens" .!= 0

instance ToJSON Usage where
    toJSON Usage{..} =
        object
            [ "prompt_tokens" .= usagePromptTokens
            , "completion_tokens" .= usageCompletionTokens
            , "total_tokens" .= usageTotalTokens
            ]

{- | A single choice from the chat completion response.

OpenAI-format responses can return multiple choices (n > 1), though
OpenRouter typically returns just one.
-}
data Choice = Choice
    { choiceIndex :: Int
    -- ^ Index of this choice in the choices array
    , choiceMessage :: Message
    -- ^ The generated message
    , choiceFinishReason :: Maybe Text
    -- ^ Why generation stopped (e.g., "stop", "tool_calls", "length")
    }
    deriving (Eq, Show, Generic)

instance FromJSON Choice where
    parseJSON = withObject "Choice" $ \v ->
        Choice
            <$> v .: "index"
            <*> v .: "message"
            <*> v .:? "finish_reason"

-- | A chat completion response in OpenAI format.
data ChatResponse = ChatResponse
    { respId :: Text
    -- ^ Unique identifier for this completion
    , respModel :: Text
    -- ^ Model that generated the response
    , respChoices :: [Choice]
    -- ^ Generated choices (usually just one)
    , respUsage :: Maybe Usage
    -- ^ Token usage statistics (may be absent during streaming)
    }
    deriving (Eq, Show, Generic)

instance FromJSON ChatResponse where
    parseJSON = withObject "ChatResponse" $ \v ->
        ChatResponse
            <$> v .: "id"
            <*> v .: "model"
            <*> v .: "choices"
            <*> v .:? "usage"

-- ============================================================================
-- Client
-- ============================================================================

-- | OpenRouter API client configuration.
data Client = Client
    { clApiKey :: Text
    -- ^ OpenRouter API key
    , clManager :: HC.Manager
    -- ^ HTTP connection manager
    , clBaseUrl :: Text
    -- ^ Base URL for API requests
    }

{- | Create a new OpenRouter client.

Uses the standard OpenRouter hostname. The client is configured with
a 120-second timeout to accommodate long streaming responses.
-}
newClient :: Text -> IO Client
newClient apiKey = do
    let baseUrl = "https://openrouter.ai/api/v1"

    -- Configure TLS manager with extended timeout for streaming
    let settings =
            HCT.tlsManagerSettings
                { HC.managerResponseTimeout = HC.responseTimeoutMicro (120 * 1000000) -- 120s timeout
                }
    manager <- HC.newManager settings
    pure
        Client
            { clApiKey = apiKey
            , clManager = manager
            , clBaseUrl = baseUrl
            }

-- ============================================================================
-- Model Discovery (Internal Types)
-- ============================================================================

{- | Model information from the OpenRouter models endpoint.

This is an internal type used when fetching available models.
It gets converted to our internal 'PT.Model' type.
-}
data OpenRouterModel = OpenRouterModel
    { ormId :: Text
    -- ^ Model identifier (e.g., "anthropic/claude-3-opus")
    , ormName :: Text
    -- ^ Human-readable model name
    , ormContextLength :: Int
    -- ^ Maximum context window size
    , ormPricing :: Maybe OpenRouterPricing
    -- ^ Pricing information
    , ormTopProvider :: Maybe OpenRouterTopProvider
    -- ^ Provider-specific limits
    }
    deriving (Show, Generic)

-- | Pricing information for an OpenRouter model.
data OpenRouterPricing = OpenRouterPricing
    { orpPrompt :: Text
    -- ^ Price per prompt token as a string (e.g., "0.000003")
    , orpCompletion :: Text
    -- ^ Price per completion token as a string
    }
    deriving (Show, Generic)

-- | Provider-specific limits for an OpenRouter model.
data OpenRouterTopProvider = OpenRouterTopProvider
    { ortpContextLength :: Maybe Int
    -- ^ Provider's context length limit
    , ortpMaxCompletionTokens :: Maybe Int
    -- ^ Maximum completion tokens
    }
    deriving (Show, Generic)

instance FromJSON OpenRouterModel where
    parseJSON = withObject "OpenRouterModel" $ \v ->
        OpenRouterModel
            <$> v .: "id"
            <*> v .: "name"
            <*> v .:? "context_length" .!= 0
            <*> v .:? "pricing"
            <*> v .:? "top_provider"

instance FromJSON OpenRouterPricing where
    parseJSON = withObject "OpenRouterPricing" $ \v ->
        OpenRouterPricing
            <$> v .:? "prompt" .!= "0"
            <*> v .:? "completion" .!= "0"

instance FromJSON OpenRouterTopProvider where
    parseJSON = withObject "OpenRouterTopProvider" $ \v ->
        OpenRouterTopProvider
            <$> v .:? "context_length"
            <*> v .:? "max_completion_tokens"

-- | Response wrapper for models endpoint
newtype ModelsResponse = ModelsResponse {mrData :: [OpenRouterModel]}
    deriving (Show, Generic)

instance FromJSON ModelsResponse where
    parseJSON = withObject "ModelsResponse" $ \v ->
        ModelsResponse <$> v .: "data"

-- | Convert OpenRouter pricing (per token) to our format (per million tokens)
parsePrice :: Text -> Double
parsePrice t = case reads (T.unpack t) of
    [(d, "")] -> d * 1000000 -- Convert per-token to per-million
    [] -> 0 -- No parse
    [(_d, _remainder)] -> 0 -- Partial parse with remainder
    (_first : _rest) -> 0 -- Multiple parses (ambiguous)

-- | Convert OpenRouter model to our internal Model type
toProviderModel :: OpenRouterModel -> PT.Model
toProviderModel orm =
    let contextLen = ormContextLength orm
        maxOutput = case ormTopProvider orm of
            Just tp -> fromMaybe (contextLen `div` 4) (ortpMaxCompletionTokens tp)
            Nothing -> contextLen `div` 4
        cost = case ormPricing orm of
            Just p ->
                Just $
                    PT.ModelCost
                        { PT.mcInput = parsePrice (orpPrompt p)
                        , PT.mcOutput = parsePrice (orpCompletion p)
                        , PT.mcCacheRead = Nothing
                        , PT.mcCacheWrite = Nothing
                        , PT.mcContextOver200k = Nothing
                        }
            Nothing -> Nothing
        -- Detect model capabilities from name/id
        isReasoning = any (`T.isInfixOf` T.toLower (ormId orm)) ["o1", "o3", "deepseek-r1", "qwq"]
        modelFamily = case T.splitOn "/" (ormId orm) of
            (provider : _rest) -> Just provider
            [] -> Nothing
     in PT.Model
            { PT.modelId = ormId orm
            , PT.modelName = ormName orm
            , PT.modelReleaseDate = "" -- Not provided by OpenRouter
            , PT.modelAttachment = True -- Most models support attachments
            , PT.modelReasoning = isReasoning
            , PT.modelTemperature = not isReasoning -- Reasoning models typically don't use temperature
            , PT.modelToolCall = True -- Most models support tools
            , PT.modelLimit =
                PT.ModelLimit
                    { PT.mlContext = contextLen
                    , PT.mlInput = Nothing
                    , PT.mlOutput = maxOutput
                    }
            , PT.modelOptions = mempty
            , PT.modelFamily = modelFamily
            , PT.modelInterleaved = Nothing
            , PT.modelCost = cost
            , PT.modelModalities = Just $ PT.ModelModalities ["text", "image"] ["text"]
            , PT.modelExperimental = Nothing
            , PT.modelStatus = Nothing
            , PT.modelHeaders = Nothing
            , PT.modelProvider = Nothing
            , PT.modelVariants = Nothing
            }

-- ============================================================================
-- API Functions
-- ============================================================================

{- | Fetch available models from the OpenRouter API.

Returns a list of models converted to our internal 'PT.Model' format.
This can be used for dynamic model discovery.
-}
fetchModels :: Client -> IO (Either Text [PT.Model])
fetchModels client = do
    result <- makeGetRequest client "/models"
    case result of
        Left err -> pure $ Left err
        Right body -> case eitherDecode body of
            Left parseErr -> pure $ Left $ "Parse error: " <> T.pack parseErr
            Right (ModelsResponse models) -> pure $ Right $ map toProviderModel models

-- | Make a GET request (for models endpoint)
makeGetRequest :: Client -> String -> IO (Either Text LBS.ByteString)
makeGetRequest client path = do
    let url = T.unpack (clBaseUrl client) <> path
    initReq <- HC.parseRequest url
    let req =
            initReq
                { HC.method = "GET"
                , HC.requestHeaders =
                    [ ("Authorization", "Bearer " <> encodeUtf8 (clApiKey client))
                    , ("HTTP-Referer", "https://opencode.ai")
                    , ("X-Title", "opencode")
                    ]
                }
    result <- try @SomeException $ HC.httpLbs req (clManager client)
    case result of
        Left err -> pure $ Left $ "Request failed: " <> T.pack (show err)
        Right resp ->
            if HT.statusIsSuccessful (HC.responseStatus resp)
                then pure $ Right $ HC.responseBody resp
                else pure $ Left $ "HTTP error: " <> T.pack (show (HC.responseStatus resp))

{- | Send a non-streaming chat completion request.

Makes a single request and waits for the complete response.
-}
chat :: Client -> ChatRequest -> IO (Either Text ChatResponse)
chat client req = do
    let reqBody = encode req{crStream = False}

    result <- makeRequest client "/chat/completions" reqBody

    case result of
        Left err -> pure $ Left err
        Right body -> case eitherDecode body of
            Left parseErr -> pure $ Left $ "Parse error: " <> T.pack parseErr <> " body: " <> decodeUtf8 (LBS.toStrict body)
            Right resp -> pure $ Right resp

{- | Send a streaming chat completion request.

Calls the provided handler for each text delta as it arrives.
This uses curl as a subprocess (workaround for IPv6 timeout issues).

Note: Uses @-d \@-@ to read the request body from stdin, avoiding
"Argument list too long" errors with large conversation histories.
-}
chatStream :: Client -> ChatRequest -> (Text -> IO ()) -> IO (Either Text ())
chatStream client req onDelta = do
    let reqBody = LBS.toStrict $ encode req{crStream = True}

    -- Use curl with IPv4 flag to avoid IPv6 timeout issues
    -- Use -d @- to read body from stdin (avoids ARG_MAX limit)
    let curlArgs =
            [ "-4" -- Force IPv4
            , "-s" -- Silent
            , "-N" -- No buffering
            , "-X"
            , "POST"
            , T.unpack (clBaseUrl client) <> "/chat/completions"
            , "-H"
            , "Content-Type: application/json"
            , "-H"
            , "Authorization: Bearer " <> T.unpack (clApiKey client)
            , "-H"
            , "HTTP-Referer: https://opencode.ai"
            , "-H"
            , "X-Title: opencode"
            , "-d"
            , "@-" -- Read body from stdin
            ]

    result <- try @SomeException $ do
        (Just hIn, Just hOut, Just hErr, ph) <-
            createProcess
                (proc "curl" curlArgs)
                    { std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }

        -- Write request body to curl's stdin
        C8.hPut hIn reqBody
        hClose hIn

        -- Read lines from curl output
        let readLoop = do
                eof <- hIsEOF hOut
                if eof
                    then pure ()
                    else do
                        line <- T.pack <$> hGetLine hOut
                        -- Parse SSE line (skip OPENROUTER comments)
                        when (": OPENROUTER" `T.isPrefixOf` line) $ pure ()
                        when ("data: " `T.isPrefixOf` line) $ do
                            let jsonPart = encodeUtf8 $ T.drop 6 line
                            for_ (extractDelta jsonPart) onDelta
                        unless ("data: [DONE]" `T.isPrefixOf` line) readLoop

        -- Drain stderr concurrently to prevent blocking
        withAsync (hGetContents hErr) $ \stderrAsync -> do
            readLoop
            exitCode <- waitForProcess ph
            stderrContent <- wait stderrAsync
            case exitCode of
                ExitSuccess -> pure Nothing
                ExitFailure n -> pure $ Just $ "curl failed with exit code " <> show n <> ": " <> stderrContent

    case result of
        Left e -> pure $ Left $ T.pack $ show e
        Right Nothing -> pure $ Right ()
        Right (Just errMsg) -> pure $ Left $ T.pack errMsg

-- | Make an HTTP request to OpenRouter API
makeRequest :: Client -> Text -> LBS.ByteString -> IO (Either Text LBS.ByteString)
makeRequest Client{..} path body = do
    initReq <- HC.parseRequest $ T.unpack clBaseUrl <> T.unpack path
    let req =
            initReq
                { HC.method = "POST"
                , HC.requestHeaders =
                    [ ("Host", "openrouter.ai") -- Required when using IP address
                    , ("Content-Type", "application/json")
                    , ("Authorization", "Bearer " <> encodeUtf8 clApiKey)
                    , ("HTTP-Referer", "https://opencode.ai")
                    , ("X-Title", "opencode")
                    ]
                , HC.requestBody = HC.RequestBodyLBS body
                }

    result <- try @SomeException $ HC.httpLbs req clManager

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

-- | Result from a streaming chat completion with tool support.
data StreamResult = StreamResult
    { srFinishReason :: Maybe Text
    -- ^ Why generation stopped (e.g., "stop", "tool_calls")
    , srToolCalls :: [ToolCall]
    -- ^ Tool calls requested by the assistant (empty if none)
    }
    deriving (Eq, Show)

{- | Send a streaming chat completion request with tool call support.

Like 'chatStream', but also tracks and returns any tool calls made
by the assistant. This is the primary function used by the agent loop
when tools are enabled.

Note: Uses @-d \@-@ to read the request body from stdin, avoiding
"Argument list too long" errors with large conversation histories.
-}
chatStreamWithTools :: Client -> ChatRequest -> (Text -> IO ()) -> IO (Either Text StreamResult)
chatStreamWithTools client req onDelta = do
    let reqBody = LBS.toStrict $ encode req{crStream = True}

    -- Use curl with IPv4 flag to avoid IPv6 timeout issues
    -- Use -d @- to read body from stdin (avoids ARG_MAX limit)
    let curlArgs =
            [ "-4" -- Force IPv4
            , "-s" -- Silent
            , "-N" -- No buffering
            , "-X"
            , "POST"
            , T.unpack (clBaseUrl client) <> "/chat/completions"
            , "-H"
            , "Content-Type: application/json"
            , "-H"
            , "Authorization: Bearer " <> T.unpack (clApiKey client)
            , "-H"
            , "HTTP-Referer: https://opencode.ai"
            , "-H"
            , "X-Title: opencode"
            , "-d"
            , "@-" -- Read body from stdin
            ]

    -- Track accumulated tool calls (they come in chunks)
    toolCallsRef <- newIORef ([] :: [ToolCallPart])
    finishReasonRef <- newIORef (Nothing :: Maybe Text)

    result <- try @SomeException $ do
        (Just hIn, Just hOut, Just hErr, ph) <-
            createProcess
                (proc "curl" curlArgs)
                    { std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }

        -- Write request body to curl's stdin
        C8.hPut hIn reqBody
        hClose hIn

        -- Read lines from curl output
        let readLoop = do
                eof <- hIsEOF hOut
                if eof
                    then pure ()
                    else do
                        line <- T.pack <$> hGetLine hOut
                        -- Parse SSE line
                        when ("data: " `T.isPrefixOf` line && not ("data: [DONE]" `T.isPrefixOf` line)) $ do
                            let jsonPart = encodeUtf8 $ T.drop 6 line
                            -- Extract text delta
                            for_ (extractDelta jsonPart) onDelta
                            -- Extract tool call deltas
                            extractToolCallDelta jsonPart toolCallsRef
                            -- Extract finish reason
                            for_ (extractFinishReason jsonPart) $ writeIORef finishReasonRef . Just
                        unless ("data: [DONE]" `T.isPrefixOf` line) readLoop

        -- Drain stderr concurrently to prevent blocking
        withAsync (hGetContents hErr) $ \stderrAsync -> do
            readLoop
            exitCode <- waitForProcess ph
            stderrContent <- wait stderrAsync
            case exitCode of
                ExitSuccess -> pure Nothing
                ExitFailure n -> pure $ Just $ "curl failed with exit code " <> show n <> ": " <> stderrContent

    case result of
        Left e -> pure $ Left $ T.pack $ show e
        Right (Just errMsg) -> pure $ Left $ T.pack errMsg
        Right Nothing -> do
            toolCallParts <- readIORef toolCallsRef
            finishReason <- readIORef finishReasonRef
            -- Assemble tool calls from accumulated parts
            let toolCalls = assembleToolCalls toolCallParts
            pure $ Right $ StreamResult finishReason toolCalls

{- | Extract and accumulate tool call deltas into an IORef.

This is the IO wrapper around the pure 'parseToolCallDeltas' and
'mergeToolCallDelta' functions.
-}
extractToolCallDelta :: ByteString -> IORef [ToolCallPart] -> IO ()
extractToolCallDelta bs ref =
    case parseToolCallDeltas bs of
        Just deltas -> forM_ deltas $ \delta ->
            modifyIORef' ref (mergeToolCallDelta delta)
        Nothing -> pure ()

-- ============================================================================
-- Pure Parsing Functions
-- ============================================================================

{- | Assemble complete tool calls from accumulated parts.

Filters out incomplete tool calls (those missing an ID or name) and
constructs 'ToolCall' values from the accumulated parts.

This is a pure function that can be tested independently of IO.
-}
assembleToolCalls :: [ToolCallPart] -> [ToolCall]
assembleToolCalls parts =
    [ ToolCall
        { tcId = tcpId p
        , tcType = tcpType p
        , tcFunction =
            ToolCallFunction
                { tcfName = tcpName p
                , tcfArguments = tcpArgs p
                }
        }
    | p <- parts
    , not (T.null (tcpId p)) && not (T.null (tcpName p))
    ]

{- | Merge a tool call delta into the accumulated parts list.

If a part with the same index exists, updates it with new information.
Otherwise, creates a new part. Arguments are concatenated for existing
parts (since they stream incrementally).

This is a pure function that can be tested independently of IO.
-}
mergeToolCallDelta :: ToolCallDelta -> [ToolCallPart] -> [ToolCallPart]
mergeToolCallDelta delta acc =
    let idx = tcdIndex delta
        mId = tcdId delta
        mType = tcdType delta
        mName = tcdName delta
        mArgs = tcdArgs delta
        existing = filter (\p -> tcpIndex p == idx) acc
     in case existing of
            [] ->
                acc
                    ++ [ ToolCallPart
                            idx
                            (fromMaybe "" mId)
                            (fromMaybe "function" mType)
                            (fromMaybe "" mName)
                            (fromMaybe "" mArgs)
                       ]
            (_first : _rest) ->
                map
                    ( \p ->
                        if tcpIndex p == idx
                            then
                                ToolCallPart
                                    (tcpIndex p)
                                    (fromMaybe (tcpId p) (if T.null (fromMaybe "" mId) then Nothing else mId))
                                    (fromMaybe (tcpType p) mType)
                                    (fromMaybe (tcpName p) mName)
                                    (tcpArgs p <> fromMaybe "" mArgs)
                            else p
                    )
                    acc

{- | Parse tool call deltas from SSE JSON bytes.

Extracts the tool_calls array from a streaming response delta and
parses each element into a 'ToolCallDelta'.

This is a pure function that can be tested independently of IO.
-}
parseToolCallDeltas :: ByteString -> Maybe [ToolCallDelta]
parseToolCallDeltas bs = do
    json <- decode (LBS.fromStrict bs)
    flip parseMaybe json $ \case
        Object obj -> do
            choices <- obj .: "choices"
            case choices of
                (choice : _) -> do
                    delta <- choice .: "delta"
                    toolCalls <- delta .:? "tool_calls" .!= ([] :: [Value])
                    forM toolCalls $ \case
                        Object tcObj -> do
                            idx <- tcObj .:? "index" .!= (0 :: Int)
                            mId <- tcObj .:? "id"
                            mType <- tcObj .:? "type"
                            mFunc <- tcObj .:? "function"
                            let mName = mFunc >>= extractFunctionField "name"
                            let mArgs = mFunc >>= extractFunctionField "arguments"
                            pure (ToolCallDelta idx mId mType mName mArgs)
                        Array _ -> fail "tool call not an object"
                        String _ -> fail "tool call not an object"
                        Number _ -> fail "tool call not an object"
                        Bool _ -> fail "tool call not an object"
                        Null -> fail "tool call not an object"
                [] -> pure []
        Array _ -> fail "not an object"
        String _ -> fail "not an object"
        Number _ -> fail "not an object"
        Bool _ -> fail "not an object"
        Null -> fail "not an object"
  where
    extractFunctionField :: Text -> Value -> Maybe Text
    extractFunctionField field (Object fObj) = parseMaybe (.: K.fromText field) fObj
    extractFunctionField _ _ = Nothing

{- | Extract finish reason from SSE JSON.

Returns the finish_reason from the first choice, if present.
-}
extractFinishReason :: ByteString -> Maybe Text
extractFinishReason bs = do
    json <- decode (LBS.fromStrict bs)
    flip parseMaybe json $ \case
        Object obj -> do
            choices <- obj .: "choices"
            case choices of
                (choice : _rest) -> choice .: "finish_reason"
                [] -> fail "no choices"
        Array _ -> fail "not object"
        String _ -> fail "not object"
        Number _ -> fail "not object"
        Bool _ -> fail "not object"
        Null -> fail "not object"

{- | Extract text delta content from SSE JSON.

Returns the content text from the first choice's delta, or Nothing
if no content is present or it's empty.

This is a pure function that can be tested independently of IO.
-}
extractDelta :: ByteString -> Maybe Text
extractDelta bs = do
    json <- decode (LBS.fromStrict bs)
    flip parseMaybe json $ \case
        Object obj -> do
            choices <- obj .: "choices"
            case choices of
                (choice : _rest) -> do
                    delta <- choice .: "delta"
                    content <- delta .:? "content"
                    case content of
                        Just txt | not (T.null txt) -> pure txt
                        Just _emptyTxt -> fail "empty or no content"
                        Nothing -> fail "empty or no content"
                [] -> fail "no choices"
        Array _ -> fail "not object"
        String _ -> fail "not object"
        Number _ -> fail "not object"
        Bool _ -> fail "not object"
        Null -> fail "not object"
