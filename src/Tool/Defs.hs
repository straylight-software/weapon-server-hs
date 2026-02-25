{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Tool.Defs
Description : Tool definitions for the Anthropic API

This module defines the available tools and their JSON schemas for the
Anthropic API. Each tool has a name, description, and input schema that
describes the expected parameters.

== Adding New Tools

To add a new tool:

1. Create a 'ToolDef' using the schema builder helpers
2. Add it to 'allTools'
3. Add execution logic in "Tool.Exec"
-}
module Tool.Defs (
    -- * Tool Lists
    allTools,
    toolDefinitions,

    -- * Schema Builders
    -- $schemabuilders
    mkObjectSchema,
    stringProp,
    numberProp,
    boolProp,
)
where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key (Key)
import Data.Text (Text)
import Tool.Types

{- | All available tools in the system.

This list is used to generate the tool definitions sent to the API
and to validate incoming tool invocations.
-}
allTools :: [ToolDef]
allTools =
    [ readDef
    , writeDef
    , editDef
    , bashDef
    , globDef
    , grepDef
    ]

{- | Tool definitions as JSON values for the API.

This converts 'allTools' into the JSON format expected by the
Anthropic API's tool use feature.
-}
toolDefinitions :: [Value]
toolDefinitions = map toApiDef allTools
  where
    toApiDef ToolDef{..} =
        object
            [ "name" .= tdName
            , "description" .= tdDescription
            , "input_schema" .= tdInputSchema
            ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Schema Builder Helpers
-- ═══════════════════════════════════════════════════════════════════════════

{- $schemabuilders
Helper functions for building JSON Schema objects. These reduce duplication
and make tool definitions more readable.
-}

{- | Build a JSON Schema object type with properties and required fields.

==== __Examples__

@
mkObjectSchema
    [ stringProp "name" "The user's name"
    , numberProp "age" "The user's age"
    ]
    ["name"]  -- required fields
@
-}
mkObjectSchema :: [(Key, Value)] -> [Text] -> Value
mkObjectSchema props required =
    object
        [ "type" .= ("object" :: Text)
        , "properties" .= object props
        , "required" .= required
        ]

-- | Create a string property for a JSON schema.
stringProp :: Key -> Text -> (Key, Value)
stringProp name desc =
    ( name
    , object
        [ "type" .= ("string" :: Text)
        , "description" .= desc
        ]
    )

-- | Create a number property for a JSON schema.
numberProp :: Key -> Text -> (Key, Value)
numberProp name desc =
    ( name
    , object
        [ "type" .= ("number" :: Text)
        , "description" .= desc
        ]
    )

-- | Create a boolean property for a JSON schema.
boolProp :: Key -> Text -> (Key, Value)
boolProp name desc =
    ( name
    , object
        [ "type" .= ("boolean" :: Text)
        , "description" .= desc
        ]
    )

-- ═══════════════════════════════════════════════════════════════════════════
-- Tool Definitions
-- ═══════════════════════════════════════════════════════════════════════════

{- | Read tool definition.

Reads file contents with line numbers, or lists directory entries.
-}
readDef :: ToolDef
readDef =
    ToolDef
        { tdName = "read"
        , tdDescription = "Read a file or directory from the local filesystem. Returns contents with line numbers."
        , tdInputSchema =
            mkObjectSchema
                [ stringProp "filePath" "Absolute path to the file or directory"
                , numberProp "offset" "Line number to start from (1-indexed)"
                , numberProp "limit" "Maximum lines to read (default 2000)"
                ]
                ["filePath"]
        }

{- | Write tool definition.

Writes content to a file, creating parent directories as needed.
-}
writeDef :: ToolDef
writeDef =
    ToolDef
        { tdName = "write"
        , tdDescription = "Write content to a file. Creates the file if it doesn't exist, overwrites if it does."
        , tdInputSchema =
            mkObjectSchema
                [ stringProp "filePath" "Absolute path to the file to write"
                , stringProp "content" "Content to write to the file"
                ]
                ["filePath", "content"]
        }

{- | Edit tool definition.

Performs string replacement in files.
-}
editDef :: ToolDef
editDef =
    ToolDef
        { tdName = "edit"
        , tdDescription = "Edit a file by replacing oldString with newString. Use replaceAll to replace all occurrences."
        , tdInputSchema =
            mkObjectSchema
                [ stringProp "filePath" "Absolute path to the file to edit"
                , stringProp "oldString" "Text to replace"
                , stringProp "newString" "Replacement text"
                , boolProp "replaceAll" "Replace all occurrences (default false)"
                ]
                ["filePath", "oldString", "newString"]
        }

{- | Bash tool definition.

Executes shell commands with timeout support.
-}
bashDef :: ToolDef
bashDef =
    ToolDef
        { tdName = "bash"
        , tdDescription = "Execute a bash command. Returns stdout/stderr and exit code."
        , tdInputSchema =
            mkObjectSchema
                [ stringProp "command" "The command to execute"
                , stringProp "description" "Short description of what this command does"
                , numberProp "timeout" "Timeout in milliseconds"
                , stringProp "workdir" "Working directory for command"
                ]
                ["command", "description"]
        }

{- | Glob tool definition.

Finds files matching glob patterns using @fd@.
-}
globDef :: ToolDef
globDef =
    ToolDef
        { tdName = "glob"
        , tdDescription = "Find files matching a glob pattern. Returns file paths sorted by modification time."
        , tdInputSchema =
            mkObjectSchema
                [ stringProp "pattern" "Glob pattern like **/*.ts or src/**/*.hs"
                , stringProp "path" "Directory to search in"
                ]
                ["pattern"]
        }

{- | Grep tool definition.

Searches file contents using ripgrep.
-}
grepDef :: ToolDef
grepDef =
    ToolDef
        { tdName = "grep"
        , tdDescription = "Search file contents using regex. Returns matching file paths and line numbers."
        , tdInputSchema =
            mkObjectSchema
                [ stringProp "pattern" "Regex pattern to search for"
                , stringProp "path" "Directory to search in"
                , stringProp "include" "File pattern filter like *.ts or *.{ts,tsx}"
                ]
                ["pattern"]
        }
