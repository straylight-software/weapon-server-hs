{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : LLM.Types
Description : Core types for LLM API interactions

This module defines the shared types used for communicating with LLM APIs
(Anthropic Claude, OpenRouter, etc.). It provides JSON serialization for
messages, content blocks, tool interactions, and streaming events.

The types follow the Anthropic Messages API format, which is the canonical
format used internally. OpenRouter types are converted to/from this format
when needed.
-}
module LLM.Types (
    -- * Message Roles
    -- $roles
    Role (..),

    -- * Message Content
    -- $content
    Content (..),
    ContentBlock (..),

    -- * Tool Interactions
    -- $tools
    ToolUse (..),
    ToolResult (..),

    -- * Messages
    -- $messages
    Message (..),

    -- * Request/Response
    -- $requests
    ChatRequest (..),
    ChatResponse (..),

    -- * Usage and Stop Reasons
    Usage (..),
    StopReason (..),

    -- * Streaming
    -- $streaming
    StreamEvent (..),

    -- * Utilities
    isMessageStop,
)
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

{- $roles
Roles define who is sending a message in the conversation.
The 'User' role represents human input, 'Assistant' represents
the LLM's responses, and 'System' is used for system prompts.
-}

{- | The role of a message sender in the conversation.

==== Examples

>>> encode User
"\"user\""

>>> encode Assistant
"\"assistant\""
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
        _otherRole -> fail "Unknown role"

{- $content
Content blocks represent the different types of content that can appear
in a message. Text is the most common, but images and tool interactions
are also supported.
-}

{- | A single block of content within a message.

Messages can contain multiple content blocks of different types.
This allows for rich interactions including text, images, and tool use.
-}
data ContentBlock
    = -- | Plain text content
      TextBlock Text
    | -- | Base64-encoded image with media type (e.g., "image/png")
      ImageBlock
        -- | Media type (e.g., "image/png", "image/jpeg")
        Text
        -- | Base64-encoded image data
        Text
    | -- | Tool use request from the assistant
      ToolUseBlock ToolUse
    | -- | Result of a tool execution from the user
      ToolResultBlock ToolResult
    deriving (Eq, Show, Generic)

instance ToJSON ContentBlock where
    toJSON (TextBlock t) = object ["type" .= ("text" :: Text), "text" .= t]
    toJSON (ImageBlock mediaType b64) =
        object
            [ "type" .= ("image" :: Text)
            , "source"
                .= object
                    [ "type" .= ("base64" :: Text)
                    , "media_type" .= mediaType
                    , "data" .= b64
                    ]
            ]
    toJSON (ToolUseBlock tu) =
        object
            [ "type" .= ("tool_use" :: Text)
            , "id" .= tuId tu
            , "name" .= tuName tu
            , "input" .= tuInput tu
            ]
    toJSON (ToolResultBlock tr) =
        object
            [ "type" .= ("tool_result" :: Text)
            , "tool_use_id" .= trToolUseId tr
            , "content" .= trContent tr
            , "is_error" .= trIsError tr
            ]

instance FromJSON ContentBlock where
    parseJSON = withObject "ContentBlock" $ \v -> do
        typ <- v .: "type"
        case typ :: Text of
            "text" -> TextBlock <$> v .: "text"
            "image" -> do
                source <- v .: "source"
                mediaType <- source .: "media_type"
                b64 <- source .: "data"
                pure $ ImageBlock mediaType b64
            "tool_use" -> ToolUseBlock <$> parseJSON (Object v)
            "tool_result" -> ToolResultBlock <$> parseJSON (Object v)
            _otherType -> fail "Unknown content block type"

{- $tools
Tool interactions allow the LLM to request external actions and receive
results. The assistant sends 'ToolUse' blocks to request tool execution,
and the user responds with 'ToolResult' blocks containing the output.
-}

{- | A tool use request from the assistant.

When the LLM wants to use a tool, it generates a ToolUse block
specifying which tool to call and what arguments to pass.
-}
data ToolUse = ToolUse
    { tuId :: Text
    -- ^ Unique identifier for this tool use (used to match results)
    , tuName :: Text
    -- ^ Name of the tool to invoke
    , tuInput :: Value
    -- ^ JSON object containing tool arguments
    }
    deriving (Eq, Show, Generic)

instance ToJSON ToolUse where
    toJSON ToolUse{..} =
        object
            [ "id" .= tuId
            , "name" .= tuName
            , "input" .= tuInput
            ]

instance FromJSON ToolUse where
    parseJSON = withObject "ToolUse" $ \v ->
        ToolUse
            <$> v .: "id"
            <*> v .: "name"
            <*> v .: "input"

{- | The result of executing a tool, sent back to the assistant.

After executing a tool requested via 'ToolUse', the result is sent
back using this type. The 'trToolUseId' must match the 'tuId' from
the original request.
-}
data ToolResult = ToolResult
    { trToolUseId :: Text
    -- ^ ID of the tool use this is responding to
    , trContent :: Text
    -- ^ Text content of the result (output or error message)
    , trIsError :: Bool
    -- ^ Whether the tool execution failed
    }
    deriving (Eq, Show, Generic)

instance ToJSON ToolResult where
    toJSON ToolResult{..} =
        object
            [ "tool_use_id" .= trToolUseId
            , "content" .= trContent
            , "is_error" .= trIsError
            ]

instance FromJSON ToolResult where
    parseJSON = withObject "ToolResult" $ \v ->
        ToolResult
            <$> v .: "tool_use_id"
            <*> v .: "content"
            <*> v .:? "is_error" .!= False

{- | Message content, which can be simple text or structured blocks.

Simple content is just a text string. Block content is a list of
content blocks that can include text, images, and tool interactions.
-}
data Content
    = -- | Simple text content (serializes as a JSON string)
      SimpleContent Text
    | -- | Structured content with multiple blocks (serializes as JSON array)
      BlockContent [ContentBlock]
    deriving (Eq, Show, Generic)

instance ToJSON Content where
    toJSON (SimpleContent t) = toJSON t
    toJSON (BlockContent bs) = toJSON bs

instance FromJSON Content where
    parseJSON (String t) = pure $ SimpleContent t
    parseJSON (Array a) = BlockContent <$> mapM parseJSON (toList a)
      where
        toList = foldr (:) []
    parseJSON _ = fail "Content must be string or array"

{- $messages
Messages are the fundamental unit of conversation. Each message has a
role (who is speaking) and content (what they're saying).
-}

{- | A single message in the conversation.

Messages form the conversation history sent to the LLM. Each message
has a role indicating the sender and content which can be simple text
or structured blocks.
-}
data Message = Message
    { msgRole :: Role
    -- ^ Who sent this message
    , msgContent :: Content
    -- ^ The message content
    }
    deriving (Eq, Show, Generic)

instance ToJSON Message where
    toJSON Message{..} =
        object
            [ "role" .= msgRole
            , "content" .= msgContent
            ]

instance FromJSON Message where
    parseJSON = withObject "Message" $ \v ->
        Message
            <$> v .: "role"
            <*> v .: "content"

{- $requests
Request and response types for the chat completion API.
-}

{- | A request for chat completion from the LLM.

This contains all parameters needed to make a chat completion request,
including the conversation history, model selection, and optional
parameters like temperature and tool definitions.
-}
data ChatRequest = ChatRequest
    { crModel :: Text
    -- ^ Model identifier (e.g., "claude-3-opus-20240229")
    , crMessages :: [Message]
    -- ^ Conversation history
    , crMaxTokens :: Int
    -- ^ Maximum tokens to generate in the response
    , crSystem :: Maybe Text
    -- ^ Optional system prompt
    , crTemperature :: Maybe Double
    -- ^ Sampling temperature (0.0-1.0, lower is more deterministic)
    , crTools :: Maybe [Value]
    -- ^ Optional tool definitions (JSON schema format)
    , crStream :: Bool
    -- ^ Whether to stream the response
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
                , "system" .= crSystem
                , "temperature" .= crTemperature
                , "tools" .= crTools
                , "stream" .= crStream
                ]

{- | The reason why the LLM stopped generating.

Understanding the stop reason helps determine what to do next:
- 'EndTurn': Normal completion, the assistant finished its response
- 'MaxTokens': Hit the token limit, response may be truncated
- 'ToolUseSR': The assistant wants to use a tool (execute and continue)
- 'StopSequence': Hit a stop sequence (custom termination)
-}
data StopReason
    = -- | Normal end of response
      EndTurn
    | -- | Hit maximum token limit
      MaxTokens
    | -- | Requesting tool use (SR suffix to avoid conflict with ToolUse type)
      ToolUseSR
    | -- | Hit a stop sequence
      StopSequence
    deriving (Eq, Show, Generic)

instance FromJSON StopReason where
    parseJSON = withText "StopReason" $ \case
        "end_turn" -> pure EndTurn
        "max_tokens" -> pure MaxTokens
        "tool_use" -> pure ToolUseSR
        "stop_sequence" -> pure StopSequence
        _otherReason -> pure EndTurn

instance ToJSON StopReason where
    toJSON EndTurn = "end_turn"
    toJSON MaxTokens = "max_tokens"
    toJSON ToolUseSR = "tool_use"
    toJSON StopSequence = "stop_sequence"

{- | Token usage statistics for a request/response.

This tracks how many tokens were used, which is important for
cost tracking and staying within context limits.
-}
data Usage = Usage
    { usageInputTokens :: Int
    -- ^ Tokens in the input (prompt)
    , usageOutputTokens :: Int
    -- ^ Tokens in the output (response)
    , usageCacheRead :: Maybe Int
    -- ^ Tokens read from cache (Anthropic prompt caching)
    , usageCacheWrite :: Maybe Int
    -- ^ Tokens written to cache (Anthropic prompt caching)
    }
    deriving (Eq, Show, Generic)

instance FromJSON Usage where
    parseJSON = withObject "Usage" $ \v ->
        Usage
            <$> v .: "input_tokens"
            <*> v .: "output_tokens"
            <*> v .:? "cache_read_input_tokens"
            <*> v .:? "cache_creation_input_tokens"

instance ToJSON Usage where
    toJSON Usage{..} =
        object
            [ "input_tokens" .= usageInputTokens
            , "output_tokens" .= usageOutputTokens
            , "cache_read_input_tokens" .= usageCacheRead
            , "cache_creation_input_tokens" .= usageCacheWrite
            ]

{- | The response from a chat completion request.

Contains the generated content along with metadata about the response
including token usage and why generation stopped.
-}
data ChatResponse = ChatResponse
    { respId :: Text
    -- ^ Unique identifier for this response
    , respModel :: Text
    -- ^ Model that generated this response
    , respRole :: Role
    -- ^ Role of the responder (always 'Assistant')
    , respContent :: [ContentBlock]
    -- ^ Generated content blocks
    , respStopReason :: Maybe StopReason
    -- ^ Why generation stopped (may be Nothing during streaming)
    , respUsage :: Usage
    -- ^ Token usage statistics
    }
    deriving (Eq, Show, Generic)

instance ToJSON ChatResponse where
    toJSON ChatResponse{..} =
        object
            [ "id" .= respId
            , "model" .= respModel
            , "role" .= respRole
            , "content" .= respContent
            , "stop_reason" .= respStopReason
            , "usage" .= respUsage
            ]

instance FromJSON ChatResponse where
    parseJSON = withObject "ChatResponse" $ \v ->
        ChatResponse
            <$> v .: "id"
            <*> v .: "model"
            <*> v .: "role"
            <*> v .: "content"
            <*> v .:? "stop_reason"
            <*> v .: "usage"

{- $streaming
Streaming events allow real-time processing of LLM responses. The API
sends a sequence of events as the response is generated, starting with
'MessageStart' and ending with 'MessageStop'.
-}

{- | Streaming event types for Server-Sent Events (SSE) from LLM APIs.

These events are received during streaming chat completions and allow
real-time processing of the response as it's generated.
-}
data StreamEvent
    = -- | Initial message metadata (id, model, etc.)
      MessageStart ChatResponse
    | -- | A new content block is starting at the given index
      ContentBlockStart Int ContentBlock
    | -- | Delta update for content at the given index
      ContentBlockDelta Int Text
    | -- | Content block at the given index is complete
      ContentBlockStop Int
    | -- | Final message delta with stop reason and usage
      MessageDelta StopReason Usage
    | -- | Message is complete
      MessageStop
    | -- | Keep-alive ping from the server
      Ping
    deriving (Eq, Show, Generic)

{- | Check if a stream event signals the end of the message.

This is useful for determining when to stop reading from an SSE stream.

==== Examples

>>> isMessageStop MessageStop
True

>>> isMessageStop (ContentBlockDelta 0 "hello")
False
-}
isMessageStop :: StreamEvent -> Bool
isMessageStop MessageStop = True
isMessageStop _ = False
