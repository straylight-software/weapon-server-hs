{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- | ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                              // weapon-server // api/message
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Message types and API endpoints. Messages represent the conversation turns
within a session, containing parts that can be text, tool calls, or results.

= Message Discriminated Union

The 'MessageInfo' type is a discriminated union based on the @"role"@ field:

* @"user"@ -> 'UserMessageInfo'
* @"assistant"@ -> 'AssistantMessageInfo'

This matches the OpenAPI specification's oneOf schema for messages.

= Usage

Messages are created via 'CreateMessageInput' and retrieved as 'Message'
objects containing 'MessageInfo' metadata and a list of parts.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-}
module Api.Message (
    -- * Message Types

    -- ** Time and Path
    MessageTime (..),
    MessagePath (..),

    -- ** Token Usage
    TokenCache (..),
    MessageTokens (..),

    -- ** Message Info (Discriminated Union)
    MessageInfo (..),
    UserMessageInfo (..),
    AssistantMessageInfo (..),

    -- ** Full Message
    Message (..),

    -- ** Input Types
    CreateMessageInput (..),
    ModelSelection (..),
    PartInput (..),
    ToolsMap (..),
    OutputFormat (..),

    -- * Message Accessors
    -- $accessors
    messageInfoId,
    messageInfoRole,
    messageInfoSessionId,
    messageInfoCreatedTime,

    -- * Message API Endpoints
    SessionMessageListAPI,
    SessionMessageCreateAPI,
    SessionMessageGetAPI,
    SessionMessagePartDeleteAPI,
    SessionMessagePartUpdateAPI,
    SessionPromptAsyncAPI,
) where

import Api.Internal (buildObject, optField)
import Data.Aeson (
    FromJSON (..),
    Key,
    Object,
    ToJSON (..),
    Value (..),
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import Json.Strict (withStrictObject, (.:!?))
import Servant (
    Capture,
    Delete,
    Get,
    JSON,
    Patch,
    Post,
    PostNoContent,
    QueryParam,
    ReqBody,
    type (:>),
 )

-- ═══════════════════════════════════════════════════════════════════════════
-- // message time //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Timestamp information for messages.

Messages track when they were created and optionally when they completed
(for assistant messages that stream responses).

__Note:__ This differs from 'Session.Types.SessionTime' which tracks
different lifecycle events (created, updated, compacting, archived).

==== Example JSON

@
{ "created": 1708123456.789, "completed": 1708123460.123 }
@
-}
data MessageTime = MessageTime
    { mtimeCreated :: Double
    -- ^ Unix timestamp (seconds with millisecond precision) when message was created
    , mtimeCompleted :: Maybe Double
    -- ^ Unix timestamp when response completed (assistant messages only)
    }
    deriving (Eq, Show, Generic)

instance ToJSON MessageTime where
    toJSON mt =
        object $
            buildObject
                ["created" .= mtimeCreated mt]
                [optField "completed" (mtimeCompleted mt)]

instance FromJSON MessageTime where
    parseJSON = withObject "MessageTime" $ \v ->
        MessageTime
            <$> v .: "created"
            <*> v .:? "completed"

-- ═══════════════════════════════════════════════════════════════════════════
-- // message path //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Path context for assistant messages.

Records the working directory and project root at the time
the assistant message was generated.

==== Example JSON

@
{ "cwd": "/home/user/project/src", "root": "/home/user/project" }
@
-}
data MessagePath = MessagePath
    { mpCwd :: Text
    -- ^ Current working directory when message was created
    , mpRoot :: Text
    -- ^ Project root directory
    }
    deriving (Eq, Show, Generic)

instance ToJSON MessagePath where
    toJSON mp =
        object
            [ "cwd" .= mpCwd mp
            , "root" .= mpRoot mp
            ]

instance FromJSON MessagePath where
    parseJSON = withObject "MessagePath" $ \v ->
        MessagePath
            <$> v .: "cwd"
            <*> v .: "root"

-- ═══════════════════════════════════════════════════════════════════════════
-- // token cache //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Token cache statistics for prompt caching.

Tracks how many tokens were read from and written to the prompt cache.
Used for cost optimization analysis.

==== Example JSON

@
{ "read": 1500.0, "write": 200.0 }
@
-}
data TokenCache = TokenCache
    { tcRead :: Double
    -- ^ Number of tokens read from cache (cache hits)
    , tcWrite :: Double
    -- ^ Number of tokens written to cache (cache misses)
    }
    deriving (Eq, Show, Generic)

instance ToJSON TokenCache where
    toJSON tc =
        object
            [ "read" .= tcRead tc
            , "write" .= tcWrite tc
            ]

instance FromJSON TokenCache where
    parseJSON = withObject "TokenCache" $ \v ->
        TokenCache
            <$> v .: "read"
            <*> v .: "write"

-- ═══════════════════════════════════════════════════════════════════════════
-- // message tokens //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Token usage statistics for an assistant message.

Detailed breakdown of token consumption including input, output,
reasoning tokens (for models that support it), and cache statistics.

==== Example JSON

@
{
  "total": 2500.0,
  "input": 1000.0,
  "output": 500.0,
  "reasoning": 0.0,
  "cache": { "read": 1000.0, "write": 0.0 }
}
@
-}
data MessageTokens = MessageTokens
    { mtTotal :: Maybe Double
    -- ^ Total tokens used (may be omitted if not calculated)
    , mtInput :: Double
    -- ^ Input tokens (prompt)
    , mtOutput :: Double
    -- ^ Output tokens (response)
    , mtReasoning :: Double
    -- ^ Reasoning tokens (for models with chain-of-thought)
    , mtCache :: TokenCache
    -- ^ Cache hit/miss statistics
    }
    deriving (Eq, Show, Generic)

instance ToJSON MessageTokens where
    toJSON mt =
        object $
            buildObject
                [ "input" .= mtInput mt
                , "output" .= mtOutput mt
                , "reasoning" .= mtReasoning mt
                , "cache" .= mtCache mt
                ]
                [optField "total" (mtTotal mt)]

instance FromJSON MessageTokens where
    parseJSON = withObject "MessageTokens" $ \v ->
        MessageTokens
            <$> v .:? "total"
            <*> v .: "input"
            <*> v .: "output"
            <*> v .: "reasoning"
            <*> v .: "cache"

-- ═══════════════════════════════════════════════════════════════════════════
-- // user message info //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Metadata for user messages (@role="user"@).

User messages contain the human's input to the conversation.
They have minimal metadata compared to assistant messages.

==== Example JSON

@
{
  "id": "msg_abc123",
  "sessionID": "ses_xyz789",
  "role": "user",
  "time": { "created": 1708123456.789 },
  "agent": "default"
}
@
-}
data UserMessageInfo = UserMessageInfo
    { umiId :: Text
    -- ^ Unique message identifier (prefixed with "msg_")
    , umiSessionId :: Text
    -- ^ Parent session ID
    , umiTime :: MessageTime
    -- ^ Message timing information
    , umiAgent :: Text
    -- ^ Agent that should handle this message (required)
    , umiModel :: ModelSelection
    -- ^ Model selection for this message (required)
    }
    deriving (Eq, Show, Generic)

instance ToJSON UserMessageInfo where
    toJSON info =
        object
            [ "id" .= umiId info
            , "sessionID" .= umiSessionId info
            , "role" .= ("user" :: Text)
            , "time" .= umiTime info
            , "agent" .= umiAgent info
            , "model" .= umiModel info
            ]

instance FromJSON UserMessageInfo where
    parseJSON = withObject "UserMessageInfo" $ \v -> do
        role <- v .: "role" :: Parser Text
        if role /= "user"
            then fail "Expected role 'user'"
            else
                UserMessageInfo
                    <$> v .: "id"
                    <*> v .: "sessionID"
                    <*> v .: "time"
                    <*> v .: "agent"
                    <*> v .: "model"

-- ═══════════════════════════════════════════════════════════════════════════
-- // assistant message info //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Metadata for assistant messages (@role="assistant"@).

Assistant messages contain the AI's response, including full information
about the model used, token consumption, costs, and completion status.

This type matches the OpenAPI @AssistantMessage@ schema.

==== Required Fields

* 'amiId', 'amiSessionId', 'amiTime' - Identity and timing
* 'amiParentId' - The user message this responds to
* 'amiModelId', 'amiProviderId' - Model identification
* 'amiMode', 'amiAgent' - Execution context
* 'amiPath' - Working directory context
* 'amiCost', 'amiTokens' - Usage statistics

==== Optional Fields

* 'amiSummary' - Whether this is a summary message
* 'amiVariant' - Message variant identifier
* 'amiFinish' - Completion reason (e.g., "stop", "length")
* 'amiError' - Error information if failed
* 'amiStructured' - Structured output data
-}
data AssistantMessageInfo = AssistantMessageInfo
    { amiId :: Text
    -- ^ Unique message identifier (prefixed with "msg_")
    , amiSessionId :: Text
    -- ^ Parent session ID
    , amiTime :: MessageTime
    -- ^ Message timing information
    , amiParentId :: Text
    -- ^ ID of the user message this responds to
    , amiModelId :: Text
    -- ^ Model identifier (e.g., "claude-3-opus-20240229")
    , amiProviderId :: Text
    -- ^ Provider identifier (e.g., "anthropic")
    , amiMode :: Text
    -- ^ Execution mode (e.g., "chat", "edit")
    , amiAgent :: Text
    -- ^ Agent that generated this response
    , amiPath :: MessagePath
    -- ^ Working directory context
    , amiCost :: Double
    -- ^ Estimated cost in USD
    , amiTokens :: MessageTokens
    -- ^ Token usage statistics
    , amiSummary :: Maybe Bool
    -- ^ Whether this is a conversation summary
    , amiVariant :: Maybe Text
    -- ^ Variant identifier for A/B testing
    , amiFinish :: Maybe Text
    -- ^ Completion reason ("stop", "length", "tool_use", etc.)
    , amiError :: Maybe Value
    -- ^ Error information if the message failed
    , amiStructured :: Maybe Value
    -- ^ Structured output data (for structured output mode)
    }
    deriving (Eq, Show, Generic)

instance ToJSON AssistantMessageInfo where
    toJSON info =
        object $
            buildObject
                (encodeAssistantRequiredFields info)
                (encodeAssistantOptionalFields info)

{- | Encode required fields for assistant message.
Separated to reduce complexity of the ToJSON instance.
-}
encodeAssistantRequiredFields :: AssistantMessageInfo -> [(Key, Value)]
encodeAssistantRequiredFields info =
    [ "id" .= amiId info
    , "sessionID" .= amiSessionId info
    , "role" .= ("assistant" :: Text)
    , "time" .= amiTime info
    , "parentID" .= amiParentId info
    , "modelID" .= amiModelId info
    , "providerID" .= amiProviderId info
    , "mode" .= amiMode info
    , "agent" .= amiAgent info
    , "path" .= amiPath info
    , "cost" .= amiCost info
    , "tokens" .= amiTokens info
    ]

{- | Encode optional fields for assistant message.
Separated to reduce complexity of the ToJSON instance.
-}
encodeAssistantOptionalFields :: AssistantMessageInfo -> [[(Key, Value)]]
encodeAssistantOptionalFields info =
    [ optField "summary" (amiSummary info)
    , optField "variant" (amiVariant info)
    , optField "finish" (amiFinish info)
    , optField "error" (amiError info)
    , optField "structured" (amiStructured info)
    ]

instance FromJSON AssistantMessageInfo where
    parseJSON = withObject "AssistantMessageInfo" $ \v -> do
        role <- v .: "role" :: Parser Text
        if role /= "assistant"
            then fail "Expected role 'assistant'"
            else parseAssistantFields v

{- | Parse all fields for AssistantMessageInfo.
Separated to reduce complexity of the FromJSON instance.
-}
parseAssistantFields :: Object -> Parser AssistantMessageInfo
parseAssistantFields v =
    AssistantMessageInfo
        <$> v .: "id"
        <*> v .: "sessionID"
        <*> v .: "time"
        <*> v .: "parentID"
        <*> v .: "modelID"
        <*> v .: "providerID"
        <*> v .: "mode"
        <*> v .: "agent"
        <*> v .: "path"
        <*> v .: "cost"
        <*> v .: "tokens"
        <*> v .:? "summary"
        <*> v .:? "variant"
        <*> v .:? "finish"
        <*> v .:? "error"
        <*> v .:? "structured"

-- ═══════════════════════════════════════════════════════════════════════════
-- // message info (discriminated union) //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Discriminated union of message types based on the @"role"@ field.

When serialized to JSON, the @"role"@ field determines the structure:

* @"user"@ - 'UserInfo' containing 'UserMessageInfo'
* @"assistant"@ - 'AssistantInfo' containing 'AssistantMessageInfo'

Use the accessor functions ('messageInfoId', 'messageInfoRole', etc.)
to extract common fields without pattern matching.
-}
data MessageInfo
    = -- | User message (@role="user"@)
      UserInfo UserMessageInfo
    | -- | Assistant message (@role="assistant"@)
      AssistantInfo AssistantMessageInfo
    deriving (Eq, Show, Generic)

{- $accessors
These accessor functions provide a uniform way to extract common
fields from either message type without pattern matching.
-}

{- | Extract the message ID from any 'MessageInfo'.

>>> messageInfoId (UserInfo userMsg)
"msg_abc123"
-}
messageInfoId :: MessageInfo -> Text
messageInfoId (UserInfo info) = umiId info
messageInfoId (AssistantInfo info) = amiId info

{- | Extract the role string from any 'MessageInfo'.

>>> messageInfoRole (UserInfo _)
"user"
>>> messageInfoRole (AssistantInfo _)
"assistant"
-}
messageInfoRole :: MessageInfo -> Text
messageInfoRole (UserInfo _) = "user"
messageInfoRole (AssistantInfo _) = "assistant"

-- | Extract the session ID from any 'MessageInfo'.
messageInfoSessionId :: MessageInfo -> Text
messageInfoSessionId (UserInfo info) = umiSessionId info
messageInfoSessionId (AssistantInfo info) = amiSessionId info

-- | Extract the creation timestamp from any 'MessageInfo'.
messageInfoCreatedTime :: MessageInfo -> Double
messageInfoCreatedTime (UserInfo info) = mtimeCreated (umiTime info)
messageInfoCreatedTime (AssistantInfo info) = mtimeCreated (amiTime info)

instance ToJSON MessageInfo where
    toJSON (UserInfo info) = toJSON info
    toJSON (AssistantInfo info) = toJSON info

instance FromJSON MessageInfo where
    parseJSON v = withObject "MessageInfo" parseRole v
      where
        parseRole obj = do
            role <- obj .: "role" :: Parser Text
            case role of
                "user" -> UserInfo <$> parseJSON v
                "assistant" -> AssistantInfo <$> parseJSON v
                _ -> fail $ "Unknown role: " ++ show role

-- ═══════════════════════════════════════════════════════════════════════════
-- // message //
-- ═══════════════════════════════════════════════════════════════════════════

{- | A complete message with metadata and content parts.

Messages contain:

* 'msgInfo' - The message metadata (user or assistant info)
* 'msgParts' - List of content parts (text, tool calls, tool results, etc.)

==== Example JSON

@
{
  "info": { "id": "msg_abc", "role": "user", ... },
  "parts": [
    { "type": "text", "text": "Hello, world!" }
  ]
}
@
-}
data Message = Message
    { msgInfo :: MessageInfo
    -- ^ Message metadata (discriminated by role)
    , msgParts :: [Value]
    -- ^ Content parts (text, tool_call, tool_result, etc.)
    }
    deriving (Eq, Show, Generic)

instance ToJSON Message where
    toJSON message =
        object
            [ "info" .= msgInfo message
            , "parts" .= msgParts message
            ]

instance FromJSON Message where
    parseJSON = withObject "Message" $ \v ->
        Message
            <$> v .: "info"
            <*> v .: "parts"

-- ═══════════════════════════════════════════════════════════════════════════
-- // model selection //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Model selection for message creation.

Specifies which provider and model to use for generating a response.
This is the object format sent by the TUI.

==== Example JSON

@
{ "providerID": "anthropic", "modelID": "claude-3-opus-20240229" }
@
-}
data ModelSelection = ModelSelection
    { msProviderID :: Text
    -- ^ Provider identifier (e.g., "anthropic", "openrouter")
    , msModelID :: Text
    -- ^ Model identifier within the provider
    }
    deriving (Eq, Show, Generic)

instance FromJSON ModelSelection where
    parseJSON = withStrictObject "ModelSelection" ["providerID", "modelID"] $ \v ->
        ModelSelection
            <$> v .: "providerID"
            <*> v .: "modelID"

instance ToJSON ModelSelection where
    toJSON ms =
        object
            [ "providerID" .= msProviderID ms
            , "modelID" .= msModelID ms
            ]

-- ═══════════════════════════════════════════════════════════════════════════
-- // part input //
-- ═══════════════════════════════════════════════════════════════════════════

{- | A validated part input for message creation.

Parts must be objects with a @type@ field. This wrapper ensures type validation
at parse time rather than accepting any JSON value.

The inner value is preserved as-is for downstream processing.
-}
newtype PartInput = PartInput {unPartInput :: Value}
    deriving (Eq, Show, Generic)

instance ToJSON PartInput where
    toJSON = unPartInput

instance FromJSON PartInput where
    parseJSON v =
        withObject
            "Part"
            ( \obj -> do
                -- Ensure the part has a "type" field
                _ :: Text <- obj .: "type"
                pure (PartInput v)
            )
            v

-- ═══════════════════════════════════════════════════════════════════════════
-- // tools map //
-- ═══════════════════════════════════════════════════════════════════════════

{- | A validated tools map for message creation.

Tools must be an object with string keys and boolean values. This wrapper
ensures type validation at parse time.

==== Example JSON
@
{
  "web_search": true,
  "code_execution": false
}
@
-}
newtype ToolsMap = ToolsMap {unToolsMap :: Value}
    deriving (Eq, Show, Generic)

instance ToJSON ToolsMap where
    toJSON = unToolsMap

instance FromJSON ToolsMap where
    parseJSON v =
        withObject
            "Tools"
            ( \obj -> do
                -- Validate all values are booleans
                mapM_ validateBoolValue (KM.toList obj)
                pure (ToolsMap v)
            )
            v
      where
        validateBoolValue :: (Key, Value) -> Parser ()
        validateBoolValue (_, Bool _) = pure ()
        validateBoolValue (k, _) = fail $ "tools property " <> show k <> " must be a boolean"

-- ═══════════════════════════════════════════════════════════════════════════
-- // output format //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Output format specification for message creation.

Must be an object with a @type@ field that is either "text" or "json_schema".
For json_schema, a @schema@ field is also required.

==== Example JSON (text)
@
{ "type": "text" }
@

==== Example JSON (json_schema)
@
{
  "type": "json_schema",
  "schema": { "type": "object", "properties": { "name": { "type": "string" } } }
}
@
-}
newtype OutputFormat = OutputFormat {unOutputFormat :: Value}
    deriving (Eq, Show, Generic)

instance ToJSON OutputFormat where
    toJSON = unOutputFormat

instance FromJSON OutputFormat where
    parseJSON v =
        withObject
            "OutputFormat"
            ( \obj -> do
                -- Require a "type" field
                formatType :: Text <- obj .: "type"
                -- Validate the type is one of the allowed values
                case formatType of
                    "text" -> pure (OutputFormat v)
                    "json_schema" -> do
                        -- For json_schema, require a schema field
                        _ :: Value <- obj .: "schema"
                        pure (OutputFormat v)
                    _ -> fail $ "format type must be 'text' or 'json_schema', got: " <> show formatType
            )
            v

-- ═══════════════════════════════════════════════════════════════════════════
-- // message input //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Input for creating a new message in a session.

Used by @POST /session/:sessionID/message@ and @POST /session/:sessionID/prompt_async@.

==== Example JSON

@
{
  "parts": [{ "type": "text", "text": "Hello!" }],
  "model": { "providerID": "anthropic", "modelID": "claude-3-opus" },
  "agent": "default"
}
@
-}
data CreateMessageInput = CreateMessageInput
    { cmiMessageId :: Maybe Text
    -- ^ Optional message ID (generated if not provided)
    , cmiParts :: [PartInput]
    -- ^ Message content parts (must be objects with type field)
    , cmiModel :: Maybe ModelSelection
    -- ^ Optional model selection override
    , cmiAgent :: Maybe Text
    -- ^ Optional agent selection
    , cmiNoReply :: Maybe Bool
    -- ^ Optional noReply flag (not nullable)
    , cmiTools :: Maybe ToolsMap
    -- ^ Optional tools object (deprecated, must be object with boolean values)
    , cmiFormat :: Maybe OutputFormat
    -- ^ Optional output format (must be object with type field)
    , cmiSystem :: Maybe Text
    -- ^ Optional system prompt (not nullable)
    , cmiVariant :: Maybe Text
    -- ^ Optional variant (not nullable)
    }
    deriving (Eq, Show, Generic)

instance FromJSON CreateMessageInput where
    parseJSON = withStrictObject
        "CreateMessageInput"
        ["messageID", "parts", "model", "agent", "noReply", "tools", "format", "system", "variant"]
        $ \v ->
            CreateMessageInput
                <$> v .:!? "messageID"
                <*> v .: "parts"
                <*> v .:!? "model"
                <*> v .:!? "agent"
                <*> v .:!? "noReply"
                <*> v .:!? "tools"
                <*> v .:!? "format"
                <*> v .:!? "system"
                <*> v .:!? "variant"

instance ToJSON CreateMessageInput where
    toJSON cmi =
        object $
            buildObject
                [ "parts" .= cmiParts cmi
                ]
                [ optField "messageID" (cmiMessageId cmi)
                , optField "model" (cmiModel cmi)
                , optField "agent" (cmiAgent cmi)
                , optField "noReply" (cmiNoReply cmi)
                , optField "tools" (cmiTools cmi)
                , optField "format" (cmiFormat cmi)
                , optField "system" (cmiSystem cmi)
                , optField "variant" (cmiVariant cmi)
                ]

-- ═══════════════════════════════════════════════════════════════════════════
-- // api type definitions //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @GET /session/:sessionID/message@ - List messages in a session.

Query parameters:

* @limit@ - Maximum number of messages to return
-}
type SessionMessageListAPI =
    "session"
        :> Capture "sessionID" Text
        :> "message"
        :> QueryParam "limit" Int
        :> Get '[JSON] [Message]

{- | @POST /session/:sessionID/message@ - Create a new message.

Creates a user message and triggers assistant response generation.
-}
type SessionMessageCreateAPI =
    "session"
        :> Capture "sessionID" Text
        :> "message"
        :> ReqBody '[JSON] CreateMessageInput
        :> Post '[JSON] Message

-- | @GET /session/:sessionID/message/:messageID@ - Get a specific message.
type SessionMessageGetAPI =
    "session"
        :> Capture "sessionID" Text
        :> "message"
        :> Capture "messageID" Text
        :> Get '[JSON] Message

-- | @DELETE /session/:sessionID/message/:messageID/part/:partID@ - Delete a message part.
type SessionMessagePartDeleteAPI =
    "session"
        :> Capture "sessionID" Text
        :> "message"
        :> Capture "messageID" Text
        :> "part"
        :> Capture "partID" Text
        :> Delete '[JSON] Bool

-- | @PATCH /session/:sessionID/message/:messageID/part/:partID@ - Update a message part.
type SessionMessagePartUpdateAPI =
    "session"
        :> Capture "sessionID" Text
        :> "message"
        :> Capture "messageID" Text
        :> "part"
        :> Capture "partID" Text
        :> ReqBody '[JSON] Value
        :> Patch '[JSON] Value

{- | @POST /session/:sessionID/prompt_async@ - Create message asynchronously.

Similar to 'SessionMessageCreateAPI' but returns immediately (204 No Content).
The response is streamed via Server-Sent Events.
-}
type SessionPromptAsyncAPI =
    "session"
        :> Capture "sessionID" Text
        :> "prompt_async"
        :> ReqBody '[JSON] CreateMessageInput
        :> PostNoContent
