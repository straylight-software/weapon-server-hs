{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | OpenRouter API client
--
-- OpenRouter provides a unified API for multiple LLM providers.
-- Uses OpenAI-compatible chat completions format.
module LLM.OpenRouter
  ( -- * Client
    Client (..),
    newClient,

    -- * API Calls
    chat,
    chatStream,
    chatStreamWithTools,

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
import Control.Monad (forM, forM_, when)
import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS qualified as HCT
import Network.HTTP.Types qualified as HT
import System.IO (hGetLine, hIsEOF)
import System.Process (StdStream (..), createProcess, proc, std_err, std_out, waitForProcess)

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
    _ -> fail "Unknown role"

-- | Tool call from assistant response (OpenAI format)
data ToolCall = ToolCall
  { tcId :: Text,
    tcType :: Text, -- always "function"
    tcFunction :: ToolCallFunction
  }
  deriving (Eq, Show, Generic)

data ToolCallFunction = ToolCallFunction
  { tcfName :: Text,
    tcfArguments :: Text -- JSON string
  }
  deriving (Eq, Show, Generic)

instance ToJSON ToolCall where
  toJSON ToolCall {..} =
    object
      [ "id" .= tcId,
        "type" .= tcType,
        "function" .= tcFunction
      ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \v ->
    ToolCall
      <$> v .: "id"
      <*> v .: "type"
      <*> v .: "function"

instance ToJSON ToolCallFunction where
  toJSON ToolCallFunction {..} =
    object
      [ "name" .= tcfName,
        "arguments" .= tcfArguments
      ]

instance FromJSON ToolCallFunction where
  parseJSON = withObject "ToolCallFunction" $ \v ->
    ToolCallFunction
      <$> v .: "name"
      <*> v .: "arguments"

-- | Tool result message (for sending back to API)
data ToolResultMessage = ToolResultMessage
  { trmRole :: Text, -- always "tool"
    trmToolCallId :: Text,
    trmContent :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ToolResultMessage where
  toJSON ToolResultMessage {..} =
    object
      [ "role" .= trmRole,
        "tool_call_id" .= trmToolCallId,
        "content" .= trmContent
      ]

-- | Chat message that can be either a regular message or tool result
data ChatMessage
  = RegularMessage Message
  | ToolResult ToolResultMessage
  deriving (Eq, Show)

instance ToJSON ChatMessage where
  toJSON (RegularMessage m) = toJSON m
  toJSON (ToolResult tr) = toJSON tr

-- | A chat message (OpenAI format)
-- Content can be null when tool_calls are present
data Message = Message
  { msgRole :: Role,
    msgContent :: Maybe Text,
    msgToolCalls :: Maybe [ToolCall]
  }
  deriving (Eq, Show, Generic)

instance ToJSON Message where
  toJSON Message {..} =
    object $
      filter
        ((/= Null) . snd)
        [ "role" .= msgRole,
          "content" .= msgContent,
          "tool_calls" .= msgToolCalls
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
      { msgRole = role,
        msgContent = Just content,
        msgToolCalls = Nothing
      }

-- | Create a tool result message
toolResultMessage :: Text -> Text -> ToolResultMessage
toolResultMessage toolCallId content =
  ToolResultMessage
    { trmRole = "tool",
      trmToolCallId = toolCallId,
      trmContent = content
    }

-- | Create an assistant message with tool calls as ChatMessage
assistantMessageWithTools :: Maybe Text -> [ToolCall] -> ChatMessage
assistantMessageWithTools content toolCalls =
  RegularMessage $
    Message
      { msgRole = Assistant,
        msgContent = content,
        msgToolCalls = if null toolCalls then Nothing else Just toolCalls
      }

-- | Create a tool result ChatMessage
toolResultChatMessage :: Text -> Text -> ChatMessage
toolResultChatMessage toolCallId content =
  ToolResult $ toolResultMessage toolCallId content

-- | Tool definition for OpenAI-compatible API
data Tool = Tool
  { toolType :: Text, -- always "function"
    toolFunction :: ToolFunction
  }
  deriving (Eq, Show, Generic)

data ToolFunction = ToolFunction
  { tfName :: Text,
    tfDescription :: Text,
    tfParameters :: Value
  }
  deriving (Eq, Show, Generic)

instance ToJSON Tool where
  toJSON Tool {..} =
    object
      [ "type" .= toolType,
        "function" .= toolFunction
      ]

instance ToJSON ToolFunction where
  toJSON ToolFunction {..} =
    object
      [ "name" .= tfName,
        "description" .= tfDescription,
        "parameters" .= tfParameters
      ]

-- | Convert tool definition to OpenAI format
toolDefToOpenAI :: Value -> Tool
toolDefToOpenAI v =
  Tool
    { toolType = "function",
      toolFunction =
        ToolFunction
          { tfName = case v of
              Object obj -> case lookup "name" (toList obj) of
                Just (String n) -> n
                _ -> ""
              _ -> "",
            tfDescription = case v of
              Object obj -> case lookup "description" (toList obj) of
                Just (String d) -> d
                _ -> ""
              _ -> "",
            tfParameters = case v of
              Object obj -> case lookup "input_schema" (toList obj) of
                Just schema -> schema
                _ -> object []
              _ -> object []
          }
    }
  where
    toList (KM.toList -> kvs) = [(K.toText k, v') | (k, v') <- kvs]

-- | Chat completion request (OpenAI format)
data ChatRequest = ChatRequest
  { crModel :: Text,
    crMessages :: [ChatMessage],
    crMaxTokens :: Maybe Int,
    crTemperature :: Maybe Double,
    crStream :: Bool,
    crTools :: Maybe [Tool] -- OpenAI-compatible tools
  }
  deriving (Eq, Show, Generic)

instance ToJSON ChatRequest where
  toJSON ChatRequest {..} =
    object $
      filter
        ((/= Null) . snd)
        [ "model" .= crModel,
          "messages" .= crMessages,
          "max_tokens" .= crMaxTokens,
          "temperature" .= crTemperature,
          "stream" .= crStream,
          "tools" .= crTools
        ]

-- | Token usage
data Usage = Usage
  { usagePromptTokens :: Int,
    usageCompletionTokens :: Int,
    usageTotalTokens :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v ->
    Usage
      <$> v .:? "prompt_tokens" .!= 0
      <*> v .:? "completion_tokens" .!= 0
      <*> v .:? "total_tokens" .!= 0

instance ToJSON Usage where
  toJSON Usage {..} =
    object
      [ "prompt_tokens" .= usagePromptTokens,
        "completion_tokens" .= usageCompletionTokens,
        "total_tokens" .= usageTotalTokens
      ]

-- | Choice in response
data Choice = Choice
  { choiceIndex :: Int,
    choiceMessage :: Message,
    choiceFinishReason :: Maybe Text
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
  { respId :: Text,
    respModel :: Text,
    respChoices :: [Choice],
    respUsage :: Maybe Usage
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
  { clApiKey :: Text,
    clManager :: HC.Manager,
    clBaseUrl :: Text
  }

-- | Create a new OpenRouter client
-- Uses standard hostname - will work if IPv6 is functional or system prefers IPv4
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
      { clApiKey = apiKey,
        clManager = manager,
        clBaseUrl = baseUrl
      }

-- | Non-streaming chat completion
chat :: Client -> ChatRequest -> IO (Either Text ChatResponse)
chat client req = do
  let reqBody = encode req {crStream = False}

  result <- makeRequest client "/chat/completions" reqBody

  case result of
    Left err -> pure $ Left err
    Right body -> case eitherDecode body of
      Left parseErr -> pure $ Left $ "Parse error: " <> T.pack parseErr <> " body: " <> decodeUtf8 (LBS.toStrict body)
      Right resp -> pure $ Right resp

-- | Streaming chat completion using curl (workaround for IPv6 issues)
-- Calls handler for each content delta
chatStream :: Client -> ChatRequest -> (Text -> IO ()) -> IO (Either Text ())
chatStream client req onDelta = do
  let reqBody = LBS.toStrict $ encode req {crStream = True}

  -- Use curl with IPv4 flag to avoid IPv6 timeout issues
  let curlArgs =
        [ "-4", -- Force IPv4
          "-s", -- Silent
          "-N", -- No buffering
          "-X",
          "POST",
          T.unpack (clBaseUrl client) <> "/chat/completions",
          "-H",
          "Content-Type: application/json",
          "-H",
          "Authorization: Bearer " <> T.unpack (clApiKey client),
          "-H",
          "HTTP-Referer: https://opencode.ai",
          "-H",
          "X-Title: opencode",
          "-d",
          C8.unpack reqBody
        ]

  result <- try @SomeException $ do
    (_, Just hOut, _, ph) <-
      createProcess
        (proc "curl" curlArgs)
          { std_out = CreatePipe,
            std_err = CreatePipe
          }

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
                case extractDelta jsonPart of
                  Just delta -> onDelta delta
                  Nothing -> pure ()
              if "data: [DONE]" `T.isPrefixOf` line
                then pure ()
                else readLoop

    readLoop
    _ <- waitForProcess ph
    pure ()

  case result of
    Left e -> pure $ Left $ T.pack $ show e
    Right () -> pure $ Right ()

-- | Make an HTTP request to OpenRouter API
makeRequest :: Client -> Text -> LBS.ByteString -> IO (Either Text LBS.ByteString)
makeRequest Client {..} path body = do
  initReq <- HC.parseRequest $ T.unpack clBaseUrl <> T.unpack path
  let req =
        initReq
          { HC.method = "POST",
            HC.requestHeaders =
              [ ("Host", "openrouter.ai"), -- Required when using IP address
                ("Content-Type", "application/json"),
                ("Authorization", "Bearer " <> encodeUtf8 clApiKey),
                ("HTTP-Referer", "https://opencode.ai"),
                ("X-Title", "opencode")
              ],
            HC.requestBody = HC.RequestBodyLBS body
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
  { srFinishReason :: Maybe Text,
    srToolCalls :: [ToolCall]
  }
  deriving (Eq, Show)

-- | Streaming chat completion with tool call support
-- Returns tool calls if any, along with finish reason
chatStreamWithTools :: Client -> ChatRequest -> (Text -> IO ()) -> IO (Either Text StreamResult)
chatStreamWithTools client req onDelta = do
  let reqBody = LBS.toStrict $ encode req {crStream = True}

  -- Use curl with IPv4 flag to avoid IPv6 timeout issues
  let curlArgs =
        [ "-4", -- Force IPv4
          "-s", -- Silent
          "-N", -- No buffering
          "-X",
          "POST",
          T.unpack (clBaseUrl client) <> "/chat/completions",
          "-H",
          "Content-Type: application/json",
          "-H",
          "Authorization: Bearer " <> T.unpack (clApiKey client),
          "-H",
          "HTTP-Referer: https://opencode.ai",
          "-H",
          "X-Title: opencode",
          "-d",
          C8.unpack reqBody
        ]

  -- Track accumulated tool calls (they come in chunks)
  toolCallsRef <- newIORef ([] :: [(Int, Text, Text, Text, Text)]) -- [(index, id, type, name, args)]
  finishReasonRef <- newIORef (Nothing :: Maybe Text)

  result <- try @SomeException $ do
    (_, Just hOut, _, ph) <-
      createProcess
        (proc "curl" curlArgs)
          { std_out = CreatePipe,
            std_err = CreatePipe
          }

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
                case extractDelta jsonPart of
                  Just delta -> onDelta delta
                  Nothing -> pure ()
                -- Extract tool call deltas
                extractToolCallDelta jsonPart toolCallsRef
                -- Extract finish reason
                case extractFinishReason jsonPart of
                  Just reason -> writeIORef finishReasonRef (Just reason)
                  Nothing -> pure ()
              if "data: [DONE]" `T.isPrefixOf` line
                then pure ()
                else readLoop

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

-- | Extract and accumulate tool call delta
-- Accumulator stores: (index, id, type, name, args)
extractToolCallDelta :: ByteString -> IORef [(Int, Text, Text, Text, Text)] -> IO ()
extractToolCallDelta bs ref = do
  let mParts = do
        json <- decode (LBS.fromStrict bs)
        flip parseMaybe json $ \case
          Object obj -> do
            choices <- obj .: "choices"
            case choices of
              (choice : _) -> do
                delta <- choice .: "delta"
                toolCalls <- delta .:? "tool_calls" .!= ([] :: [Value])
                forM toolCalls $ \tc -> case tc of
                  Object tcObj -> do
                    idx <- tcObj .:? "index" .!= (0 :: Int)
                    mId <- tcObj .:? "id"
                    mType <- tcObj .:? "type"
                    mFunc <- tcObj .:? "function"
                    let mName =
                          mFunc >>= \f -> case f of
                            Object fObj -> parseMaybe (.: "name") fObj
                            _ -> Nothing
                    let mArgs =
                          mFunc >>= \f -> case f of
                            Object fObj -> parseMaybe (.: "arguments") fObj
                            _ -> Nothing
                    pure (idx, mId, mType, mName, mArgs)
                  _ -> fail "not object"
              [] -> pure []
          _ -> fail "not object"
  case mParts of
    Just parts -> forM_ parts $ \(idx, mId, mType, mName, mArgs) -> do
      -- Accumulate parts by index (tool calls stream in chunks)
      modifyIORef' ref $ \acc ->
        let existing = filter (\(i, _, _, _, _) -> i == idx) acc
         in case existing of
              [] -> acc ++ [(idx, fromMaybe "" mId, fromMaybe "function" mType, fromMaybe "" mName, fromMaybe "" mArgs)]
              _ ->
                map
                  ( \(i, tcid, t, n, a) ->
                      if i == idx
                        then (i, fromMaybe tcid (if T.null (fromMaybe "" mId) then Nothing else mId), fromMaybe t mType, fromMaybe n mName, a <> fromMaybe "" mArgs)
                        else (i, tcid, t, n, a)
                  )
                  acc
    Nothing -> pure ()

-- | Assemble tool calls from accumulated parts
assembleToolCalls :: [(Int, Text, Text, Text, Text)] -> [ToolCall]
assembleToolCalls parts =
  [ ToolCall
      { tcId = tcid,
        tcType = tctype,
        tcFunction =
          ToolCallFunction
            { tcfName = name,
              tcfArguments = args
            }
      }
  | (_idx, tcid, tctype, name, args) <- parts,
    not (T.null tcid) && not (T.null name)
  ]

-- | Extract finish reason from SSE JSON
extractFinishReason :: ByteString -> Maybe Text
extractFinishReason bs = do
  json <- decode (LBS.fromStrict bs)
  flip parseMaybe json $ \case
    Object obj -> do
      choices <- obj .: "choices"
      case choices of
        (choice : _) -> choice .: "finish_reason"
        [] -> fail "no choices"
    _ -> fail "not object"

-- | Extract delta content from SSE JSON
-- Returns Nothing for empty or missing content (skip empty deltas)
extractDelta :: ByteString -> Maybe Text
extractDelta bs = do
  json <- decode (LBS.fromStrict bs)
  flip parseMaybe json $ \case
    Object obj -> do
      choices <- obj .: "choices"
      case choices of
        (choice : _) -> do
          delta <- choice .: "delta"
          content <- delta .:? "content"
          case content of
            Just txt | not (T.null txt) -> pure txt
            _ -> fail "empty or no content"
        [] -> fail "no choices"
    _ -> fail "not object"
