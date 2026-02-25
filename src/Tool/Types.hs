{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Tool.Types
Description : Core type definitions for tool system

This module defines the core types used throughout the tool execution system.
It includes tool identifiers, input types for each tool, output types, and
execution context.

== Tool System Overview

The tool system provides a set of file and shell operations that can be
invoked by AI agents. Each tool has:

* A 'ToolID' for identification
* An input type (e.g., 'ReadInput', 'WriteInput')
* A 'ToolDef' describing its schema for the API

== Usage

@
-- Create a successful tool output
let output = toolSuccess "Read file.txt" "file contents..."

-- Create an error output
let err = toolError "Write Error" "Permission denied"
@
-}
module Tool.Types (
    -- * Tool Identifiers
    -- $toolids
    ToolID (..),

    -- * Tool Definitions
    -- $tooldefs
    ToolDef (..),

    -- * Tool Input Types
    -- $toolinputs
    ReadInput (..),
    WriteInput (..),
    EditInput (..),
    BashInput (..),
    GlobInput (..),
    GrepInput (..),

    -- * Tool Output
    -- $tooloutput
    ToolOutput (..),
    toolSuccess,
    toolError,
    toolSuccessWithMeta,

    -- * Execution Context
    -- $context
    ToolContext (..),

    -- * Streaming Support
    -- $streaming
    StreamingCallback,
    noStreaming,
)
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

{- $toolids
Tool identifiers enumerate all available tools in the system.
These are used for routing tool invocations to the correct handler.
-}

{- | Enumeration of all available tool types.

Each constructor corresponds to a specific tool that can be executed
by the agent. The JSON serialization uses lowercase names (e.g., "read", "write").
-}
data ToolID
    = ReadTool
    | WriteTool
    | EditTool
    | BashTool
    | GlobTool
    | GrepTool
    | TodoWriteTool
    | WebFetchTool
    | QuestionTool
    | TaskTool
    deriving (Eq, Show, Generic)

instance ToJSON ToolID where
    toJSON ReadTool = "read"
    toJSON WriteTool = "write"
    toJSON EditTool = "edit"
    toJSON BashTool = "bash"
    toJSON GlobTool = "glob"
    toJSON GrepTool = "grep"
    toJSON TodoWriteTool = "todowrite"
    toJSON WebFetchTool = "webfetch"
    toJSON QuestionTool = "question"
    toJSON TaskTool = "task"

instance FromJSON ToolID where
    parseJSON = withText "ToolID" $ \case
        "read" -> pure ReadTool
        "write" -> pure WriteTool
        "edit" -> pure EditTool
        "bash" -> pure BashTool
        "glob" -> pure GlobTool
        "grep" -> pure GrepTool
        "todowrite" -> pure TodoWriteTool
        "webfetch" -> pure WebFetchTool
        "question" -> pure QuestionTool
        "task" -> pure TaskTool
        t -> fail $ "Unknown tool: " <> show t

{- $tooldefs
Tool definitions describe tools for the API layer, including their
JSON schema for input validation.
-}

{- | Definition of a tool for the Anthropic API.

This includes the tool's name, description, and a JSON Schema describing
the expected input format.
-}
data ToolDef = ToolDef
    { tdName :: Text
    -- ^ Unique identifier for the tool (e.g., "read", "write", "bash")
    , tdDescription :: Text
    -- ^ Human-readable description of what the tool does
    , tdInputSchema :: Value
    -- ^ JSON Schema describing the expected input structure
    }
    deriving (Eq, Show, Generic)

instance ToJSON ToolDef where
    toJSON ToolDef{..} =
        object
            [ "name" .= tdName
            , "description" .= tdDescription
            , "input_schema" .= tdInputSchema
            ]

{- $toolinputs
Each tool has a corresponding input type that defines the parameters
it accepts. These types have JSON instances for serialization.
-}

{- | Input parameters for the read tool.

The read tool reads file contents or lists directory entries.
-}
data ReadInput = ReadInput
    { riFilePath :: Text
    -- ^ Absolute or relative path to the file or directory
    , riOffset :: Maybe Int
    -- ^ Line number to start from (1-indexed, defaults to 1)
    , riLimit :: Maybe Int
    -- ^ Maximum number of lines to read (defaults to 2000)
    }
    deriving (Eq, Show, Generic)

instance FromJSON ReadInput where
    parseJSON = withObject "ReadInput" $ \v ->
        ReadInput
            <$> v .: "filePath"
            <*> v .:? "offset"
            <*> v .:? "limit"

instance ToJSON ReadInput where
    toJSON ReadInput{..} =
        object
            [ "filePath" .= riFilePath
            , "offset" .= riOffset
            , "limit" .= riLimit
            ]

{- | Input parameters for the write tool.

The write tool creates or overwrites files with the specified content.
Parent directories are created automatically if they don't exist.
-}
data WriteInput = WriteInput
    { wiFilePath :: Text
    -- ^ Absolute or relative path to the file to write
    , wiContent :: Text
    -- ^ Content to write to the file
    }
    deriving (Eq, Show, Generic)

instance FromJSON WriteInput where
    parseJSON = withObject "WriteInput" $ \v ->
        WriteInput
            <$> v .: "filePath"
            <*> v .: "content"

instance ToJSON WriteInput where
    toJSON WriteInput{..} =
        object
            [ "filePath" .= wiFilePath
            , "content" .= wiContent
            ]

{- | Input parameters for the edit tool.

The edit tool performs string replacement in files. By default, it replaces
only the first occurrence; use 'eiReplaceAll' to replace all occurrences.
-}
data EditInput = EditInput
    { eiFilePath :: Text
    -- ^ Path to the file to edit
    , eiOldString :: Text
    -- ^ Text to search for and replace
    , eiNewString :: Text
    -- ^ Replacement text
    , eiReplaceAll :: Maybe Bool
    -- ^ If 'True', replace all occurrences; otherwise replace only the first
    }
    deriving (Eq, Show, Generic)

instance FromJSON EditInput where
    parseJSON = withObject "EditInput" $ \v ->
        EditInput
            <$> v .: "filePath"
            <*> v .: "oldString"
            <*> v .: "newString"
            <*> v .:? "replaceAll"

instance ToJSON EditInput where
    toJSON EditInput{..} =
        object
            [ "filePath" .= eiFilePath
            , "oldString" .= eiOldString
            , "newString" .= eiNewString
            , "replaceAll" .= eiReplaceAll
            ]

{- | Input parameters for the bash tool.

The bash tool executes shell commands with configurable timeout and
working directory. Output is streamed back in real-time.
-}
data BashInput = BashInput
    { biCommand :: Text
    -- ^ The bash command to execute
    , biDescription :: Text
    -- ^ Human-readable description of the command's purpose
    , biTimeout :: Maybe Int
    -- ^ Timeout in milliseconds (defaults to 120000, i.e., 2 minutes)
    , biWorkdir :: Maybe Text
    -- ^ Working directory for command execution
    }
    deriving (Eq, Show, Generic)

instance FromJSON BashInput where
    parseJSON = withObject "BashInput" $ \v ->
        BashInput
            <$> v .: "command"
            <*> v .: "description"
            <*> v .:? "timeout"
            <*> v .:? "workdir"

instance ToJSON BashInput where
    toJSON BashInput{..} =
        object
            [ "command" .= biCommand
            , "description" .= biDescription
            , "timeout" .= biTimeout
            , "workdir" .= biWorkdir
            ]

{- | Input parameters for the glob tool.

The glob tool finds files matching a glob pattern using @fd@.
-}
data GlobInput = GlobInput
    { giPattern :: Text
    -- ^ Glob pattern (e.g., @\"**\/*.hs\"@, @\"src\/**\/*.ts\"@)
    , giPath :: Maybe Text
    -- ^ Directory to search in (defaults to current directory)
    }
    deriving (Eq, Show, Generic)

instance FromJSON GlobInput where
    parseJSON = withObject "GlobInput" $ \v ->
        GlobInput
            <$> v .: "pattern"
            <*> v .:? "path"

instance ToJSON GlobInput where
    toJSON GlobInput{..} =
        object
            [ "pattern" .= giPattern
            , "path" .= giPath
            ]

{- | Input parameters for the grep tool.

The grep tool searches file contents using @ripgrep@ (rg).
-}
data GrepInput = GrepInput
    { grPattern :: Text
    -- ^ Regular expression pattern to search for
    , grPath :: Maybe Text
    -- ^ Directory to search in (defaults to current directory)
    , grInclude :: Maybe Text
    -- ^ File pattern filter (e.g., @\"*.ts\"@, @\"*.{ts,tsx}\"@)
    }
    deriving (Eq, Show, Generic)

instance FromJSON GrepInput where
    parseJSON = withObject "GrepInput" $ \v ->
        GrepInput
            <$> v .: "pattern"
            <*> v .:? "path"
            <*> v .:? "include"

instance ToJSON GrepInput where
    toJSON GrepInput{..} =
        object
            [ "pattern" .= grPattern
            , "path" .= grPath
            , "include" .= grInclude
            ]

{- $tooloutput
Tool output represents the result of executing a tool. It includes
a title, the output content, an error flag, and optional metadata.
-}

{- | Result of executing a tool.

Use 'toolSuccess', 'toolSuccessWithMeta', or 'toolError' to construct
instances rather than using the constructor directly.
-}
data ToolOutput = ToolOutput
    { toTitle :: Text
    -- ^ Short title describing the operation (e.g., "Read file.txt")
    , toOutput :: Text
    -- ^ The output content (file contents, command output, error message)
    , toIsError :: Bool
    -- ^ 'True' if this represents an error result
    , toMetadata :: Maybe Value
    -- ^ Optional JSON metadata about the operation
    }
    deriving (Eq, Show, Generic)

instance ToJSON ToolOutput where
    toJSON ToolOutput{..} =
        object
            [ "title" .= toTitle
            , "output" .= toOutput
            , "is_error" .= toIsError
            , "metadata" .= toMetadata
            ]

instance FromJSON ToolOutput where
    parseJSON = withObject "ToolOutput" $ \v ->
        ToolOutput
            <$> v .: "title"
            <*> v .: "output"
            <*> v .: "is_error"
            <*> v .:? "metadata"

{- $context
Execution context provides information about the current session and
working directory for tool execution.
-}

{- | Context for tool execution.

This is passed to every tool executor and provides information about
the current session and working directory.
-}
data ToolContext = ToolContext
    { tcSessionID :: Text
    -- ^ Unique identifier for the current session
    , tcMessageID :: Text
    -- ^ Unique identifier for the current message/request
    , tcWorkdir :: FilePath
    -- ^ Working directory for relative path resolution
    }
    deriving (Eq, Show)

{- | Smart constructor for successful tool output without metadata.

==== __Examples__

@
toolSuccess "Read file.txt" "line 1\\nline 2\\n"
@
-}
toolSuccess :: Text -> Text -> ToolOutput
toolSuccess title output = ToolOutput title output False Nothing

{- | Smart constructor for successful tool output with metadata.

==== __Examples__

@
toolSuccessWithMeta "Glob *.hs" "file1.hs\\nfile2.hs" (Just (object ["count" .= 2]))
@
-}
toolSuccessWithMeta :: Text -> Text -> Maybe Value -> ToolOutput
toolSuccessWithMeta title output = ToolOutput title output False

{- | Smart constructor for error tool output.

==== __Examples__

@
toolError "Write Error" "Permission denied: /etc/passwd"
@
-}
toolError :: Text -> Text -> ToolOutput
toolError title output = ToolOutput title output True Nothing

{- $streaming
Streaming support allows tools to report incremental output as they execute.
This is particularly useful for long-running commands where the user wants
to see progress.
-}

{- | Callback invoked with accumulated output during streaming tool execution.

The callback receives the total accumulated output so far (not just the delta).
This allows the UI to replace the previous output rather than append.
-}
type StreamingCallback = Text -> IO ()

{- | A no-op streaming callback for when streaming is not needed.

Use this when you don't need real-time output updates.
-}
noStreaming :: StreamingCallback
noStreaming = const (pure ())
