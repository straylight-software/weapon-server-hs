{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | OpenRouter API client

OpenRouter provides a unified API for multiple LLM providers.
Uses OpenAI-compatible chat completions format.
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

    -- * Types
    ChatRequest (..),
    ChatResponse (..),
    Choice (..),
    Message (..),
    ChatMessage (..),
    Role (..),
    Usage (..),
    Tool (..),
    ToolFunction (..),
    ToolCall (..),
    ToolCallFunction (..),
    ToolResultMessage (..),
    StreamResult (..),
    toolDefToOpenAI,

    -- * Helpers
    simpleMessage,
    toolResultMessage,
    assistantMessageWithTools,
    toolResultChatMessage,
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
import System.IO (hClose, hGetLine, hIsEOF)
import System.Process (StdStream (..), createProcess, proc, std_err, std_in, std_out, waitForProcess)

-- | Message role
data Role = User | Assistant | System
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

-- | Tool call from assistant response (OpenAI format)
data ToolCall = ToolCall
    { tcId :: Text
    , tcType :: Text -- always "function"
    , tcFunction :: ToolCallFunction
    }
    deriving (Eq, Show, Generic)

data ToolCallFunction = ToolCallFunction
    { tcfName :: Text
    , tcfArguments :: Text -- JSON string
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

-- | Accumulated tool call parts during streaming (replaces 5-tuple)
data ToolCallPart = ToolCallPart
    { tcpIndex :: Int
    , tcpId :: Text
    , tcpType :: Text
    , tcpName :: Text
    , tcpArgs :: Text
    }

-- | Parsed tool call delta (intermediate representation during streaming)
data ToolCallDelta = ToolCallDelta
    { tcdIndex :: Int
    , tcdId :: Maybe Text
    , tcdType :: Maybe Text
    , tcdName :: Maybe Text
    , tcdArgs :: Maybe Text
    }

-- | Tool result message (for sending back to API)
data ToolResultMessage = ToolResultMessage
    { trmRole :: Text -- always "tool"
    , trmToolCallId :: Text
    , trmContent :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON ToolResultMessage where
    toJSON ToolResultMessage{..} =
        object
            [ "role" .= trmRole
            , "tool_call_id" .= trmToolCallId
            , "content" .= trmContent
            ]

-- | Chat message that can be either a regular message or tool result
data ChatMessage
    = RegularMessage Message
    | ToolResult ToolResultMessage
    deriving (Eq, Show)

instance ToJSON ChatMessage where
    toJSON (RegularMessage m) = toJSON m
    toJSON (ToolResult tr) = toJSON tr

{- | A chat message (OpenAI format)
Content can be null when tool_calls are present
-}
data Message = Message
    { msgRole :: Role
    , msgContent :: Maybe Text
    , msgToolCalls :: Maybe [ToolCall]
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

-- | Create a simple text message as ChatMessage
simpleMessage :: Role -> Text -> ChatMessage
simpleMessage role content =
    RegularMessage $
        Message
            { msgRole = role
            , msgContent = Just content
            , msgToolCalls = Nothing
            }

-- | Create a tool result message
toolResultMessage :: Text -> Text -> ToolResultMessage
toolResultMessage toolCallId content =
    ToolResultMessage
        { trmRole = "tool"
        , trmToolCallId = toolCallId
        , trmContent = content
        }

-- | Create an assistant message with tool calls as ChatMessage
assistantMessageWithTools :: Maybe Text -> [ToolCall] -> ChatMessage
assistantMessageWithTools content toolCalls =
    RegularMessage $
        Message
            { msgRole = Assistant
            , msgContent = content
            , msgToolCalls = if null toolCalls then Nothing else Just toolCalls
            }

-- | Create a tool result ChatMessage
toolResultChatMessage :: Text -> Text -> ChatMessage
toolResultChatMessage toolCallId content =
    ToolResult $ toolResultMessage toolCallId content

-- | Tool definition for OpenAI-compatible API
data Tool = Tool
    { toolType :: Text -- always "function"
    , toolFunction :: ToolFunction
    }
    deriving (Eq, Show, Generic)

data ToolFunction = ToolFunction
    { tfName :: Text
    , tfDescription :: Text
    , tfParameters :: Value
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

-- | Convert tool definition to OpenAI format
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
  where
    extractFieldText :: Text -> Value -> Text
    extractFieldText key (Object obj) = case KM.lookup (K.fromText key) obj of
        Just (String s) -> s
        Just (Object _) -> ""
        Just (Array _) -> ""
        Just (Number _) -> ""
        Just (Bool _) -> ""
        Just Null -> ""
        Nothing -> ""
    extractFieldText _key (Array _) = ""
    extractFieldText _key (String _) = ""
    extractFieldText _key (Number _) = ""
    extractFieldText _key (Bool _) = ""
    extractFieldText _key Null = ""

    extractFieldValue :: Text -> Value -> Value -> Value
    extractFieldValue key (Object obj) fallback = fromMaybe fallback (KM.lookup (K.fromText key) obj)
    extractFieldValue _key (Array _) fallback = fallback
    extractFieldValue _key (String _) fallback = fallback
    extractFieldValue _key (Number _) fallback = fallback
    extractFieldValue _key (Bool _) fallback = fallback
    extractFieldValue _key Null fallback = fallback

-- | Chat completion request (OpenAI format)
data ChatRequest = ChatRequest
    { crModel :: Text
    , crMessages :: [ChatMessage]
    , crMaxTokens :: Maybe Int
    , crTemperature :: Maybe Double
    , crStream :: Bool
    , crTools :: Maybe [Tool] -- OpenAI-compatible tools
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

-- | Token usage
data Usage = Usage
    { usagePromptTokens :: Int
    , usageCompletionTokens :: Int
    , usageTotalTokens :: Int
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

-- | Choice in response
data Choice = Choice
    { choiceIndex :: Int
    , choiceMessage :: Message
    , choiceFinishReason :: Maybe Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON Choice where
    parseJSON = withObject "Choice" $ \v ->
        Choice
            <$> v .: "index"
            <*> v .: "message"
            <*> v .:? "finish_reason"

-- | Chat completion response (OpenAI format)
data ChatResponse = ChatResponse
    { respId :: Text
    , respModel :: Text
    , respChoices :: [Choice]
    , respUsage :: Maybe Usage
    }
    deriving (Eq, Show, Generic)

instance FromJSON ChatResponse where
    parseJSON = withObject "ChatResponse" $ \v ->
        ChatResponse
            <$> v .: "id"
            <*> v .: "model"
            <*> v .: "choices"
            <*> v .:? "usage"

-- | OpenRouter API client
data Client = Client
    { clApiKey :: Text
    , clManager :: HC.Manager
    , clBaseUrl :: Text
    }

{- | Create a new OpenRouter client
Uses standard hostname - will work if IPv6 is functional or system prefers IPv4
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

-- | OpenRouter model from their API (different from our internal Model type)
data OpenRouterModel = OpenRouterModel
    { ormId :: Text
    , ormName :: Text
    , ormContextLength :: Int
    , ormPricing :: Maybe OpenRouterPricing
    , ormTopProvider :: Maybe OpenRouterTopProvider
    }
    deriving (Show, Generic)

data OpenRouterPricing = OpenRouterPricing
    { orpPrompt :: Text -- Price per token as string (e.g. "0.000003")
    , orpCompletion :: Text
    }
    deriving (Show, Generic)

data OpenRouterTopProvider = OpenRouterTopProvider
    { ortpContextLength :: Maybe Int
    , ortpMaxCompletionTokens :: Maybe Int
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

{- | Fetch available models from OpenRouter API
Returns list of models converted to our internal format
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

-- | Non-streaming chat completion
chat :: Client -> ChatRequest -> IO (Either Text ChatResponse)
chat client req = do
    let reqBody = encode req{crStream = False}

    result <- makeRequest client "/chat/completions" reqBody

    case result of
        Left err -> pure $ Left err
        Right body -> case eitherDecode body of
            Left parseErr -> pure $ Left $ "Parse error: " <> T.pack parseErr <> " body: " <> decodeUtf8 (LBS.toStrict body)
            Right resp -> pure $ Right resp

{- | Streaming chat completion using curl (workaround for IPv6 issues)
Calls handler for each content delta

NOTE: We use `-d @-` to read the request body from stdin instead of passing
it as a command-line argument. This avoids "Argument list too long" errors
when the conversation history contains large tool outputs.
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
        (Just hIn, Just hOut, _, ph) <-
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

        readLoop
        _ <- waitForProcess ph
        pure ()

    case result of
        Left e -> pure $ Left $ T.pack $ show e
        Right () -> pure $ Right ()

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

-- | Streaming result with tool calls
data StreamResult = StreamResult
    { srFinishReason :: Maybe Text
    , srToolCalls :: [ToolCall]
    }
    deriving (Eq, Show)

{- | Streaming chat completion with tool call support
Returns tool calls if any, along with finish reason

NOTE: We use `-d @-` to read the request body from stdin instead of passing
it as a command-line argument. This avoids "Argument list too long" errors
when the conversation history contains large tool outputs.
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
        (Just hIn, Just hOut, _, ph) <-
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

        readLoop
        _ <- waitForProcess ph
        pure ()

    case result of
        Left e -> pure $ Left $ T.pack $ show e
        Right () -> do
            toolCallParts <- readIORef toolCallsRef
            finishReason <- readIORef finishReasonRef
            -- Assemble tool calls from accumulated parts
            let toolCalls = assembleToolCalls toolCallParts
            pure $ Right $ StreamResult finishReason toolCalls

{- | Extract and accumulate tool call delta
Accumulator stores: (index, id, type, name, args)
-}
extractToolCallDelta :: ByteString -> IORef [ToolCallPart] -> IO ()
extractToolCallDelta bs ref = do
    let mParts = do
            json <- decode (LBS.fromStrict bs)
            flip parseMaybe json $ \case
                Object obj -> do
                    choices <- obj .: "choices"
                    case choices of
                        (choice : _rest) -> do
                            delta <- choice .: "delta"
                            toolCalls <- delta .:? "tool_calls" .!= ([] :: [Value])
                            forM toolCalls $ \case
                                Object tcObj -> do
                                    idx <- tcObj .:? "index" .!= (0 :: Int)
                                    mId <- tcObj .:? "id"
                                    mType <- tcObj .:? "type"
                                    mFunc <- tcObj .:? "function"
                                    let mName =
                                            mFunc >>= \case
                                                Object fObj -> parseMaybe (.: "name") fObj
                                                Array _ -> Nothing
                                                String _ -> Nothing
                                                Number _ -> Nothing
                                                Bool _ -> Nothing
                                                Null -> Nothing
                                    let mArgs =
                                            mFunc >>= \case
                                                Object fObj -> parseMaybe (.: "arguments") fObj
                                                Array _ -> Nothing
                                                String _ -> Nothing
                                                Number _ -> Nothing
                                                Bool _ -> Nothing
                                                Null -> Nothing
                                    pure (ToolCallDelta idx mId mType mName mArgs)
                                Array _ -> fail "not object"
                                String _ -> fail "not object"
                                Number _ -> fail "not object"
                                Bool _ -> fail "not object"
                                Null -> fail "not object"
                        [] -> pure []
                Array _ -> fail "not object"
                String _ -> fail "not object"
                Number _ -> fail "not object"
                Bool _ -> fail "not object"
                Null -> fail "not object"
    case mParts of
        Just parts -> forM_ parts $ \delta -> do
            -- Accumulate parts by index (tool calls stream in chunks)
            let idx = tcdIndex delta
            let mId = tcdId delta
            let mType = tcdType delta
            let mName = tcdName delta
            let mArgs = tcdArgs delta
            modifyIORef' ref $ \acc ->
                let existing = filter (\p -> tcpIndex p == idx) acc
                 in case existing of
                        [] -> acc ++ [ToolCallPart idx (fromMaybe "" mId) (fromMaybe "function" mType) (fromMaybe "" mName) (fromMaybe "" mArgs)]
                        (_x : _xs) ->
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
        Nothing -> pure ()

-- | Assemble tool calls from accumulated parts
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

-- | Extract finish reason from SSE JSON
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

{- | Extract delta content from SSE JSON
Returns Nothing for empty or missing content (skip empty deltas)
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
