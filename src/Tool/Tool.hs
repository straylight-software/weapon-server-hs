{- |
Module      : Tool.Tool
Description : Tool system entry point

This module re-exports the tool system's public API. It provides:

* Type definitions from "Tool.Types"
* Tool definitions from "Tool.Defs"
* Execution functions from "Tool.Exec"

== Quick Start

@
import Tool.Tool

-- Execute a tool
result <- execute ctx "read" (object ["filePath" .= "/etc/passwd"])

-- Check result
if toIsError result
    then putStrLn $ "Error: " <> toOutput result
    else putStrLn $ toOutput result
@
-}
module Tool.Tool (
    -- * Types
    module Tool.Types,

    -- * Tool Definitions
    allTools,
    toolDefinitions,
    toolListItems,

    -- * Tool Execution
    execute,
    executeToolUse,
    executeToolUseStreaming,
    executeStreaming,
) where

import Tool.Defs (allTools, toolDefinitions, toolListItems)
import Tool.Exec (execute, executeStreaming, executeToolUse, executeToolUseStreaming)
import Tool.Types
