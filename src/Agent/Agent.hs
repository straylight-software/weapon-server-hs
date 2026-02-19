{-# LANGUAGE OverloadedStrings #-}

{- | Agent module - agent management
Mirrors the TypeScript Agent namespace
-}
module Agent.Agent (
    -- * Types
    Agent.Types.Agent (..),
    Agent.Types.AgentMode (..),
    Agent.Types.PermissionRuleset (..),
    Agent.Types.PermissionAction (..),

    -- * Operations
    list,
    get,

    -- * Built-in agents
    builtinAgents,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Agent.Types

-- | Helper to build permission rulesets
ruleset :: [(Text, PermissionAction)] -> PermissionRuleset
ruleset rules = PermissionRuleset $ Map.fromList [(k, [PermissionRule v Nothing]) | (k, v) <- rules]

-- | Built-in agent definitions
builtinAgents :: [Agent]
builtinAgents =
    [ (defaultAgent "armed" Primary (ruleset [("*", Allow), ("doom_loop", Ask), ("question", Allow), ("plan_enter", Allow)]))
        { agentDescription = Just "The default agent. Executes tools based on configured permissions."
        }
    , (defaultAgent "locked" Primary (ruleset [("*", Allow), ("edit", Deny), ("write", Deny), ("question", Allow), ("plan_exit", Allow)]))
        { agentDescription = Just "Locked mode. Disallows all edit tools."
        }
    , (defaultAgent "general" Subagent (ruleset [("*", Allow), ("todoread", Deny), ("todowrite", Deny)]))
        { agentDescription = Just "General-purpose agent for researching complex questions and executing multi-step tasks."
        }
    , (defaultAgent "explore" Subagent (ruleset [("*", Deny), ("grep", Allow), ("glob", Allow), ("read", Allow), ("bash", Allow), ("webfetch", Allow)]))
        { agentDescription = Just "Fast agent specialized for exploring codebases."
        , agentPrompt = Just "You are an explore agent. Your job is to quickly search and analyze codebases."
        }
    , (defaultAgent "compaction" Primary (ruleset [("*", Deny)]))
        { agentHidden = Just True
        , agentPrompt = Just "You are a compaction agent. Summarize the conversation concisely."
        }
    , (defaultAgent "title" Primary (ruleset [("*", Deny)]))
        { agentHidden = Just True
        , agentTemperature = Just 0.5
        , agentPrompt = Just "Generate a concise title for this conversation."
        }
    , (defaultAgent "summary" Primary (ruleset [("*", Deny)]))
        { agentHidden = Just True
        , agentPrompt = Just "Summarize this session."
        }
    ]

-- | List all agents
list :: IO [Agent]
list = pure builtinAgents

-- | Get an agent by name
get :: Text -> IO (Maybe Agent)
get name = do
    agents <- list
    pure $ lookup name [(agentName a, a) | a <- agents]
