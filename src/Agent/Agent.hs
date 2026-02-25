{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Agent.Agent
Description : Agent management and built-in agent definitions
Stability   : experimental

This module provides agent management functionality including:

* Built-in agent definitions for common use cases
* Agent lookup and listing operations
* Helper functions for constructing permission rulesets

The module mirrors the TypeScript Agent namespace for API compatibility.

== Built-in Agents

The following agents are provided out of the box:

* @armed@ - The default agent with full tool access
* @locked@ - Read-only mode that disallows edit tools
* @general@ - General-purpose subagent for complex tasks
* @explore@ - Fast subagent for code exploration
* @compaction@ - Hidden agent for conversation compaction
* @title@ - Hidden agent for title generation
* @summary@ - Hidden agent for session summaries
-}
module Agent.Agent (
    -- * Types (re-exported from Agent.Types)
    Agent.Types.Agent (..),
    Agent.Types.AgentMode (..),
    Agent.Types.PermissionRuleset (..),
    Agent.Types.PermissionAction (..),
    Agent.Types.PermissionRule (..),

    -- * Pure Operations
    findAgentByName,
    filterByMode,
    filterVisible,

    -- * IO Operations
    list,
    get,

    -- * Built-in Agents
    builtinAgents,

    -- * Permission Helpers
    mkRuleset,
    mkSimpleRuleset,
    emptyRuleset,
    allowAllRuleset,
    denyAllRuleset,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Agent.Types

-- ============================================================================
-- Permission Ruleset Helpers
-- ============================================================================

{- | Create a permission ruleset from a list of (tool, rules) pairs.

This is the most flexible constructor, allowing multiple rules per tool.

==== __Examples__

@
mkRuleset
  [ ("edit", [PermissionRule Allow (Just "*.hs"), PermissionRule Deny Nothing])
  , ("bash", [PermissionRule Ask Nothing])
  ]
@
-}
mkRuleset :: [(Text, [PermissionRule])] -> PermissionRuleset
mkRuleset = PermissionRuleset . Map.fromList

{- | Create a simple permission ruleset where each tool has one rule without a glob.

This is the most common case for basic permission configuration.

==== __Examples__

@
mkSimpleRuleset [("*", Allow), ("edit", Deny)]
@
-}
mkSimpleRuleset :: [(Text, PermissionAction)] -> PermissionRuleset
mkSimpleRuleset rules =
    PermissionRuleset $ Map.fromList [(k, [PermissionRule v Nothing]) | (k, v) <- rules]

{- | An empty permission ruleset (no rules defined).

With no rules, the default behavior depends on the permission evaluation logic.
-}
emptyRuleset :: PermissionRuleset
emptyRuleset = PermissionRuleset Map.empty

{- | A ruleset that allows all tools.

Equivalent to @mkSimpleRuleset [("*", Allow)]@.
-}
allowAllRuleset :: PermissionRuleset
allowAllRuleset = mkSimpleRuleset [("*", Allow)]

{- | A ruleset that denies all tools.

Equivalent to @mkSimpleRuleset [("*", Deny)]@.
-}
denyAllRuleset :: PermissionRuleset
denyAllRuleset = mkSimpleRuleset [("*", Deny)]

-- ============================================================================
-- Built-in Agent Definitions
-- ============================================================================

{- | The default agent with standard permissions.

Allows most tools, asks before potentially dangerous operations (doom_loop),
and permits questions and plan entry.
-}
armedAgent :: Agent
armedAgent =
    (defaultAgent "armed" Primary (mkSimpleRuleset armedPermissions))
        { agentDescription = Just "The default agent. Executes tools based on configured permissions."
        , agentPrompt =
            Just $
                T.unlines
                    [ "You are an AI coding assistant with full access to tools."
                    , "You help users write, debug, refactor, and improve code."
                    , "You communicate concisely and focus on solving problems."
                    , "You verify your work by reading files and running tests."
                    ]
        }
  where
    armedPermissions =
        [ ("*", Allow)
        , ("doom_loop", Ask)
        , ("question", Allow)
        , ("plan_enter", Allow)
        ]

{- | Locked (read-only) agent.

Disallows edit and write tools while permitting read operations.
-}
lockedAgent :: Agent
lockedAgent =
    (defaultAgent "locked" Primary (mkSimpleRuleset lockedPermissions))
        { agentDescription = Just "Locked mode. Disallows all edit tools."
        , agentPrompt =
            Just $
                T.unlines
                    [ "You are an AI coding assistant in read-only mode."
                    , "You can read and analyze code but cannot make changes."
                    , "Focus on explaining, reviewing, and answering questions."
                    ]
        }
  where
    lockedPermissions =
        [ ("*", Allow)
        , ("edit", Deny)
        , ("write", Deny)
        , ("question", Allow)
        , ("plan_exit", Allow)
        ]

{- | General-purpose subagent for complex tasks.

Has broad permissions but cannot manage todos (to avoid confusion with parent).
-}
generalAgent :: Agent
generalAgent =
    (defaultAgent "general" Subagent (mkSimpleRuleset generalPermissions))
        { agentDescription = Just "General-purpose agent for researching complex questions and executing multi-step tasks."
        , agentPrompt =
            Just $
                T.unlines
                    [ "You are a general-purpose subagent for complex multi-step tasks."
                    , "Break problems into steps, execute thoroughly, and report results."
                    , "You have access to most tools - use them proactively to verify your work."
                    , "Be thorough but concise in your final report to the parent agent."
                    ]
        }
  where
    generalPermissions =
        [ ("*", Allow)
        , ("todoread", Deny)
        , ("todowrite", Deny)
        ]

{- | Fast exploration subagent.

Limited to read-only tools for quick codebase navigation.
-}
exploreAgent :: Agent
exploreAgent =
    (defaultAgent "explore" Subagent (mkSimpleRuleset explorePermissions))
        { agentDescription = Just "Fast agent specialized for exploring codebases."
        , agentPrompt =
            Just $
                T.unlines
                    [ "You are a fast exploration agent for searching and analyzing codebases."
                    , "Use Glob for file patterns, Grep for content search, Read for file contents."
                    , "Return concise, actionable findings. Don't speculate - verify with tools."
                    , "Focus on speed and accuracy. Report what you find, not what you assume."
                    ]
        }
  where
    explorePermissions =
        [ ("*", Deny)
        , ("grep", Allow)
        , ("glob", Allow)
        , ("read", Allow)
        , ("bash", Allow)
        , ("webfetch", Allow)
        ]

-- | Hidden agent for conversation compaction.
compactionAgent :: Agent
compactionAgent =
    (defaultAgent "compaction" Primary denyAllRuleset)
        { agentHidden = Just True
        , agentPrompt = Just "You are a compaction agent. Summarize the conversation concisely."
        }

-- | Hidden agent for title generation.
titleAgent :: Agent
titleAgent =
    (defaultAgent "title" Primary denyAllRuleset)
        { agentHidden = Just True
        , agentTemperature = Just 0.5
        , agentPrompt = Just "Generate a concise title for this conversation."
        }

-- | Hidden agent for session summaries.
summaryAgent :: Agent
summaryAgent =
    (defaultAgent "summary" Primary denyAllRuleset)
        { agentHidden = Just True
        , agentPrompt = Just "Summarize this session."
        }

{- | All built-in agent definitions.

This list is used by 'list' and 'get' to provide the default set of agents.
Custom agents can be added by loading them from configuration files.
-}
builtinAgents :: [Agent]
builtinAgents =
    [ armedAgent
    , lockedAgent
    , generalAgent
    , exploreAgent
    , compactionAgent
    , titleAgent
    , summaryAgent
    ]

-- ============================================================================
-- Pure Query Functions
-- ============================================================================

{- | Find an agent by name from a list of agents.

This is a pure function that can be used for testing or when you have
a custom list of agents.

==== __Examples__

@
findAgentByName "armed" builtinAgents  -- Just armedAgent
findAgentByName "unknown" builtinAgents  -- Nothing
@
-}
findAgentByName :: Text -> [Agent] -> Maybe Agent
findAgentByName name agents =
    lookup name [(agentName a, a) | a <- agents]

{- | Filter agents by their mode.

==== __Examples__

@
filterByMode Primary builtinAgents  -- Only primary agents
filterByMode Subagent builtinAgents  -- Only subagents
@
-}
filterByMode :: AgentMode -> [Agent] -> [Agent]
filterByMode mode = filter (\a -> agentMode a == mode)

{- | Filter out hidden agents, returning only visible ones.

==== __Examples__

@
filterVisible builtinAgents  -- Excludes compaction, title, summary
@
-}
filterVisible :: [Agent] -> [Agent]
filterVisible = filter (not . isHidden)

-- ============================================================================
-- IO Operations
-- ============================================================================

{- | List all available agents.

Currently returns only built-in agents. In the future, this may also
load agents from configuration files.
-}
list :: IO [Agent]
list = pure builtinAgents

{- | Get an agent by name.

Looks up an agent in the list of all available agents.
Returns 'Nothing' if no agent with the given name exists.

==== __Examples__

@
get "armed"    -- IO (Just armedAgent)
get "unknown"  -- IO Nothing
@
-}
get :: Text -> IO (Maybe Agent)
get name = findAgentByName name <$> list
