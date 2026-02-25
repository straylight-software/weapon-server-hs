{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Message.Types
Description : Message type definitions for the agent server

This module defines the core message types that mirror the TypeScript
MessageV2 namespace. Messages are the primary communication unit between
the client and server, containing structured parts like text, tool calls,
files, and reasoning steps.

= Message Structure

A 'Message' consists of:

* 'MessageInfo' - Metadata about the message (id, session, role, time)
* A list of 'Part's - The content of the message

= Part Types

Messages can contain various types of parts:

* 'TextPart' - Plain text content
* 'ToolPart' - Tool invocations and their states
* 'FilePart' - File attachments
* 'ReasoningPart' - AI reasoning/thinking content
* 'StepStartPart' / 'StepFinishPart' - Step boundaries
* 'SnapshotPart' - State snapshots
-}
module Message.Types (
    -- * Message types
    Message (..),
    MessageInfo (..),
    MessageRole (..),
    MessageTime (..),

    -- * Part types
    Part (..),
    PartBase (..),
    TextPart (..),
    ToolPart (..),
    ToolState (..),
    FilePart (..),
    ReasoningPart (..),
    StepStartPart (..),
    StepFinishPart (..),
    SnapshotPart (..),

    -- * Input types
    CreateMessageInput (..),
    TextPartInput (..),
    FilePartInput (..),

    -- * Pure helpers
    roleToText,
    textToRole,
) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Message role indicating whether a message is from the user or the assistant.
data MessageRole = User | Assistant
    deriving (Show, Eq, Generic)

{- | Convert a 'MessageRole' to its text representation.

>>> roleToText User
"user"
>>> roleToText Assistant
"assistant"
-}
roleToText :: MessageRole -> Text
roleToText User = "user"
roleToText Assistant = "assistant"

{- | Parse a text value into a 'MessageRole'.

Returns 'Nothing' for unrecognized role strings.

>>> textToRole "user"
Just User
>>> textToRole "assistant"
Just Assistant
>>> textToRole "unknown"
Nothing
-}
textToRole :: Text -> Maybe MessageRole
textToRole "user" = Just User
textToRole "assistant" = Just Assistant
textToRole _ = Nothing

instance ToJSON MessageRole where
    toJSON = String . roleToText

instance FromJSON MessageRole where
    parseJSON = withText "MessageRole" $ \t ->
        case textToRole t of
            Just role -> pure role
            Nothing -> fail "Invalid message role"

{- | Timestamp information for a message.

Contains the creation time as a Unix epoch timestamp (seconds since 1970).
-}
newtype MessageTime = MessageTime
    { mtCreated :: Double
    -- ^ Unix epoch timestamp when the message was created
    }
    deriving (Show, Eq, Generic)

instance ToJSON MessageTime where
    toJSON mt = object ["created" .= mtCreated mt]

instance FromJSON MessageTime where
    parseJSON = withObject "MessageTime" $ \v ->
        MessageTime
            <$> v .: "created"

{- | Metadata for a message.

Contains identifying information and timing data for a message within a session.
-}
data MessageInfo = MessageInfo
    { miId :: Text
    -- ^ Unique identifier for this message
    , miSessionID :: Text
    -- ^ ID of the session this message belongs to
    , miRole :: MessageRole
    -- ^ Whether this is a user or assistant message
    , miParentID :: Maybe Text
    -- ^ ID of the parent message (for threading)
    , miTime :: MessageTime
    -- ^ Timestamp information
    }
    deriving (Show, Eq, Generic)

instance ToJSON MessageInfo where
    toJSON mi =
        object
            [ "id" .= miId mi
            , "sessionID" .= miSessionID mi
            , "role" .= miRole mi
            , "parentID" .= miParentID mi
            , "time" .= miTime mi
            ]

instance FromJSON MessageInfo where
    parseJSON = withObject "MessageInfo" $ \v ->
        MessageInfo
            <$> v .: "id"
            <*> v .: "sessionID"
            <*> v .: "role"
            <*> v .:? "parentID"
            <*> v .: "time"

{- | Base fields common to all message parts.

Every part has an ID, belongs to a session, and belongs to a message.
-}
data PartBase = PartBase
    { pbId :: Text
    -- ^ Unique identifier for this part
    , pbSessionID :: Text
    -- ^ ID of the session this part belongs to
    , pbMessageID :: Text
    -- ^ ID of the message this part belongs to
    }
    deriving (Show, Eq, Generic)

{- | A text content part within a message.

Represents plain text content that can optionally be marked as synthetic
(generated) or ignored (not sent to the model).
-}
data TextPart = TextPart
    { tpBase :: PartBase
    -- ^ Common part fields
    , tpText :: Text
    -- ^ The text content
    , tpSynthetic :: Maybe Bool
    -- ^ Whether this text was synthetically generated
    , tpIgnored :: Maybe Bool
    -- ^ Whether this text should be ignored in processing
    }
    deriving (Show, Eq, Generic)

{- | State of a tool invocation.

Tool parts progress through states: Pending -> Running -> Completed/Error.
Field names are prefixed with underscores to suppress -Wpartial-fields warnings.
-}
data ToolState
    = ToolPending
        { _tsInput :: Map.Map Text Value
        -- ^ The input parameters for the tool
        , _tsRaw :: Text
        -- ^ Raw input text before parsing
        }
    | ToolRunning
        { _tsrInput :: Map.Map Text Value
        -- ^ The input parameters for the tool
        , _tsrTitle :: Maybe Text
        -- ^ Optional display title for the running tool
        }
    | ToolCompleted
        { _tscInput :: Map.Map Text Value
        -- ^ The input parameters for the tool
        , _tscOutput :: Text
        -- ^ The output produced by the tool
        , _tscTitle :: Text
        -- ^ Display title for the completed tool
        }
    | ToolError
        { _tseInput :: Map.Map Text Value
        -- ^ The input parameters for the tool
        , _tseError :: Text
        -- ^ Error message describing what went wrong
        }
    deriving (Show, Eq, Generic)

instance ToJSON ToolState where
    toJSON (ToolPending input raw) =
        object
            [ "status" .= String "pending"
            , "input" .= input
            , "raw" .= raw
            ]
    toJSON (ToolRunning input title) =
        object
            [ "status" .= String "running"
            , "input" .= input
            , "title" .= title
            ]
    toJSON (ToolCompleted input output title) =
        object
            [ "status" .= String "completed"
            , "input" .= input
            , "output" .= output
            , "title" .= title
            ]
    toJSON (ToolError input err) =
        object
            [ "status" .= String "error"
            , "input" .= input
            , "error" .= err
            ]

{- | A tool invocation part within a message.

Represents a call to an external tool, tracking its state from pending
through completion or error.
-}
data ToolPart = ToolPart
    { toolBase :: PartBase
    -- ^ Common part fields
    , toolCallID :: Text
    -- ^ Unique identifier for this tool call
    , toolName :: Text
    -- ^ Name of the tool being invoked
    , toolState :: ToolState
    -- ^ Current state of the tool invocation
    }
    deriving (Show, Eq, Generic)

{- | A file attachment part within a message.

Represents an attached file with its MIME type and URL location.
-}
data FilePart = FilePart
    { fpBase :: PartBase
    -- ^ Common part fields
    , fpMime :: Text
    -- ^ MIME type of the file (e.g., "image/png", "application/pdf")
    , fpFilename :: Maybe Text
    -- ^ Optional original filename
    , fpUrl :: Text
    -- ^ URL where the file can be accessed
    }
    deriving (Show, Eq, Generic)

{- | A reasoning/thinking part within a message.

Contains the AI's internal reasoning or chain-of-thought content.
-}
data ReasoningPart = ReasoningPart
    { rpBase :: PartBase
    -- ^ Common part fields
    , rpText :: Text
    -- ^ The reasoning text content
    }
    deriving (Show, Eq, Generic)

{- | Marker for the start of a processing step.

Used to delineate distinct phases in message processing.
-}
data StepStartPart = StepStartPart
    { sspBase :: PartBase
    -- ^ Common part fields
    , sspSnapshot :: Maybe Text
    -- ^ Optional state snapshot at step start
    }
    deriving (Show, Eq, Generic)

{- | Marker for the end of a processing step.

Contains information about why the step finished and its cost.
-}
data StepFinishPart = StepFinishPart
    { sfpBase :: PartBase
    -- ^ Common part fields
    , sfpReason :: Text
    -- ^ Reason why the step finished (e.g., "complete", "limit_reached")
    , sfpCost :: Double
    -- ^ Cost incurred during this step
    }
    deriving (Show, Eq, Generic)

{- | A state snapshot part within a message.

Contains a serialized snapshot of system state at a point in time.
-}
data SnapshotPart = SnapshotPart
    { snpBase :: PartBase
    -- ^ Common part fields
    , snpSnapshot :: Text
    -- ^ The serialized snapshot data
    }
    deriving (Show, Eq, Generic)

{- | Union of all message part types.

A message can contain multiple parts of different types, allowing for
rich structured content including text, tool calls, files, and metadata.
-}
data Part
    = -- | Plain text content
      PartText TextPart
    | -- | A tool invocation
      PartTool ToolPart
    | -- | A file attachment
      PartFile FilePart
    | -- | AI reasoning/thinking content
      PartReasoning ReasoningPart
    | -- | Start of a processing step
      PartStepStart StepStartPart
    | -- | End of a processing step
      PartStepFinish StepFinishPart
    | -- | A state snapshot
      PartSnapshot SnapshotPart
    deriving (Show, Eq, Generic)

{- | Convert base fields to JSON key-value pairs.

This helper extracts the common fields from a 'PartBase' for JSON serialization.
-}
baseFields :: PartBase -> [Pair]
baseFields pb =
    [ "id" .= pbId pb
    , "sessionID" .= pbSessionID pb
    , "messageID" .= pbMessageID pb
    ]

instance ToJSON Part where
    toJSON (PartText tp) =
        object $
            ("type" .= String "text")
                : baseFields (tpBase tp)
                ++ [ "text" .= tpText tp
                   , "synthetic" .= tpSynthetic tp
                   , "ignored" .= tpIgnored tp
                   ]
    toJSON (PartTool tp) =
        object $
            ("type" .= String "tool")
                : baseFields (toolBase tp)
                ++ [ "callID" .= toolCallID tp
                   , "tool" .= toolName tp
                   , "state" .= toolState tp
                   ]
    toJSON (PartFile fp) =
        object $
            ("type" .= String "file")
                : baseFields (fpBase fp)
                ++ [ "mime" .= fpMime fp
                   , "filename" .= fpFilename fp
                   , "url" .= fpUrl fp
                   ]
    toJSON (PartReasoning rp) =
        object $
            ("type" .= String "reasoning")
                : baseFields (rpBase rp)
                ++ ["text" .= rpText rp]
    toJSON (PartStepStart ssp) =
        object $
            ("type" .= String "step-start")
                : baseFields (sspBase ssp)
                ++ ["snapshot" .= sspSnapshot ssp]
    toJSON (PartStepFinish sfp) =
        object $
            ("type" .= String "step-finish")
                : baseFields (sfpBase sfp)
                ++ [ "reason" .= sfpReason sfp
                   , "cost" .= sfpCost sfp
                   ]
    toJSON (PartSnapshot snp) =
        object $
            ("type" .= String "snapshot")
                : baseFields (snpBase snp)
                ++ ["snapshot" .= snpSnapshot snp]

{- | A complete message containing metadata and content parts.

Messages are the fundamental unit of communication in a session,
consisting of metadata ('MessageInfo') and a list of content 'Part's.
-}
data Message = Message
    { msgInfo :: MessageInfo
    -- ^ Metadata about this message
    , msgParts :: [Part]
    -- ^ The content parts of this message
    }
    deriving (Show, Eq, Generic)

instance ToJSON Message where
    toJSON m =
        object
            [ "info" .= msgInfo m
            , "parts" .= msgParts m
            ]

{- | Input structure for creating a text part.

Used when receiving text part data from API requests.
-}
data TextPartInput = TextPartInput
    { tpiType :: Text
    -- ^ Part type identifier, should be "text"
    , tpiText :: Text
    -- ^ The text content
    }
    deriving (Show, Eq, Generic)

instance FromJSON TextPartInput where
    parseJSON = withObject "TextPartInput" $ \v ->
        TextPartInput
            <$> v .: "type"
            <*> v .: "text"

{- | Input structure for creating a file part.

Used when receiving file part data from API requests.
-}
data FilePartInput = FilePartInput
    { fpiType :: Text
    -- ^ Part type identifier, should be "file"
    , fpiMime :: Text
    -- ^ MIME type of the file
    , fpiUrl :: Text
    -- ^ URL where the file can be accessed
    , fpiFilename :: Maybe Text
    -- ^ Optional original filename
    }
    deriving (Show, Eq, Generic)

instance FromJSON FilePartInput where
    parseJSON = withObject "FilePartInput" $ \v ->
        FilePartInput
            <$> v .: "type"
            <*> v .: "mime"
            <*> v .: "url"
            <*> v .:? "filename"

{- | Input structure for creating a new message.

Used when receiving message creation requests from the API.
Parts are generic JSON values that are validated at runtime.
-}
data CreateMessageInput = CreateMessageInput
    { cmiMessageID :: Maybe Text
    -- ^ Optional custom message ID (generated if not provided)
    , cmiParts :: [Value]
    -- ^ The message parts as generic JSON values
    }
    deriving (Show, Eq, Generic)

instance FromJSON CreateMessageInput where
    parseJSON = withObject "CreateMessageInput" $ \v ->
        CreateMessageInput
            <$> v .:? "messageID"
            <*> v .: "parts"
