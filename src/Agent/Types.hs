{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Agent.Types
Description : Core type definitions for AI agents
Stability   : experimental

This module defines the core types for representing AI agents in the system.
It mirrors the TypeScript Agent namespace and provides:

* 'Agent' - The main agent configuration type
* 'AgentMode' - Whether an agent is primary or a subagent
* 'PermissionRuleset' - Tool permission configurations
* 'PermissionRule' - Individual permission rules with optional glob patterns
* 'PermissionAction' - Allow, Deny, or Ask for tool access

All types have JSON serialization instances compatible with the TypeScript API.
-}
module Agent.Types (
    -- * Core Types
    Agent (..),
    AgentMode (..),
    PermissionRuleset (..),
    PermissionRule (..),
    PermissionAction (..),

    -- * Smart Constructors
    defaultAgent,

    -- * Pure Query Functions
    lookupPermission,
    hasPermissionFor,
    isHidden,
    isNative,
    getEffectiveTemperature,
) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Action to take when a tool permission is evaluated.

* 'Allow' - The tool can be executed without user confirmation
* 'Deny' - The tool cannot be executed
* 'Ask' - The user must confirm before the tool is executed
-}
data PermissionAction
    = -- | Tool execution is allowed automatically
      Allow
    | -- | Tool execution is denied
      Deny
    | -- | User confirmation is required before execution
      Ask
    deriving (Show, Eq, Generic)

instance ToJSON PermissionAction where
    toJSON Allow = String "allow"
    toJSON Deny = String "deny"
    toJSON Ask = String "ask"

instance FromJSON PermissionAction where
    parseJSON = withText "PermissionAction" $ \case
        "allow" -> pure Allow
        "deny" -> pure Deny
        "ask" -> pure Ask
        _otherAction -> fail "Invalid permission action"

{- | A single permission rule that determines tool access.

Each rule specifies an action to take, and optionally a glob pattern
to match against file paths or tool arguments.

Internal representation uses (action, glob) pairs, but the API
representation uses the flattened format with (permission, pattern, action).

==== __Examples__

@
-- Allow all file operations
PermissionRule Allow Nothing

-- Allow only .hs files
PermissionRule Allow (Just "*.hs")

-- Deny access to /etc
PermissionRule Deny (Just "/etc/*")
@
-}
data PermissionRule = PermissionRule
    { prAction :: PermissionAction
    -- ^ The action to take when this rule matches
    , prGlob :: Maybe Text
    -- ^ Optional glob pattern to filter matches
    }
    deriving (Show, Eq, Generic)

-- | Internal ToJSON for map-based format (used in config files)
permissionRuleToMapJSON :: PermissionRule -> Value
permissionRuleToMapJSON pr =
    object
        [ "action" .= prAction pr
        , "glob" .= prGlob pr
        ]

instance ToJSON PermissionRule where
    toJSON = permissionRuleToMapJSON

instance FromJSON PermissionRule where
    parseJSON = withObject "PermissionRule" $ \v ->
        PermissionRule
            <$> v .: "action"
            <*> v .:? "glob"

{- | A collection of permission rules keyed by tool name.

The map keys are tool names (e.g., "edit", "bash", "read") or wildcards ("*").
Each key maps to a list of 'PermissionRule' values that are evaluated in order.

When checking permissions, rules are typically evaluated as:

1. Look for exact tool name match
2. Fall back to wildcard ("*") if no exact match
3. For each matching rule, check glob pattern if present
4. First matching rule determines the action

NOTE: The API representation is an array of { permission, pattern, action }
objects, while the internal representation is a map. The ToJSON instance
converts from map to array format to match the OpenAPI schema.
-}
newtype PermissionRuleset = PermissionRuleset
    { unRuleset :: Map.Map Text [PermissionRule]
    -- ^ The underlying map from tool names to permission rules
    }
    deriving (Show, Eq, Generic)

{- | Convert internal map format to OpenAPI array format
{ "read": [{ action: "allow", glob: "*.hs" }] }
becomes: [{ permission: "read", pattern: "*.hs", action: "allow" }]
-}
instance ToJSON PermissionRuleset where
    toJSON (PermissionRuleset m) =
        toJSON
            [ object
                [ "permission" .= permName
                , "pattern" .= fromMaybe "*" (prGlob rule)
                , "action" .= prAction rule
                ]
            | (permName, rules) <- Map.toList m
            , rule <- rules
            ]

-- | FromJSON supports both array format (API) and map format (config)
instance FromJSON PermissionRuleset where
    parseJSON v = parseArray v <|> parseMap v
      where
        -- Parse array format: [{ permission, pattern, action }]
        parseArray = withArray "PermissionRuleset" $ \arr -> do
            rules <- mapM parseRule (toList arr)
            let grouped = foldr (\(perm, rule) acc -> Map.insertWith (++) perm [rule] acc) Map.empty rules
            pure (PermissionRuleset grouped)

        parseRule = withObject "PermissionRule" $ \obj -> do
            perm <- obj .: "permission"
            pat <- obj .: "pattern"
            action <- obj .: "action"
            let glob = if pat == "*" then Nothing else Just pat
            pure (perm, PermissionRule action glob)

        -- Parse map format: { "read": [{ action, glob }] }
        parseMap val = PermissionRuleset <$> parseJSON val

{- | The operational mode of an agent.

* 'Primary' - A top-level agent that can be directly invoked by users
* 'Subagent' - An agent that can only be spawned by other agents
* 'AllModes' - An agent that can operate in any mode
-}
data AgentMode
    = -- | Can only be spawned by other agents
      Subagent
    | -- | Top-level agent for direct user interaction
      Primary
    | -- | Can operate in any mode
      AllModes
    deriving (Show, Eq, Generic)

instance ToJSON AgentMode where
    toJSON Subagent = String "subagent"
    toJSON Primary = String "primary"
    toJSON AllModes = String "all"

instance FromJSON AgentMode where
    parseJSON = withText "AgentMode" $ \case
        "subagent" -> pure Subagent
        "primary" -> pure Primary
        "all" -> pure AllModes
        _otherMode -> fail "Invalid agent mode"

{- | Configuration for an AI agent.

An agent represents a configured AI assistant with specific capabilities,
permissions, and behavior settings. Agents can be primary (user-facing)
or subagents (spawned by other agents for specific tasks).

==== __Examples__

@
let explorer = (defaultAgent "explore" Subagent readOnlyPerms)
      { agentDescription = Just "Code exploration agent"
      , agentPrompt = Just "You are an expert at navigating codebases."
      }
@
-}
data Agent = Agent
    { agentName :: Text
    -- ^ Unique identifier for this agent
    , agentDescription :: Maybe Text
    -- ^ Human-readable description of the agent's purpose
    , agentMode :: AgentMode
    -- ^ Whether this is a primary or sub-agent
    , agentNative :: Maybe Bool
    -- ^ Whether this is a built-in (native) agent
    , agentHidden :: Maybe Bool
    -- ^ Whether to hide this agent from listings
    , agentTopP :: Maybe Double
    -- ^ Top-p (nucleus) sampling parameter for the LLM
    , agentTemperature :: Maybe Double
    -- ^ Temperature parameter for LLM randomness (0.0-2.0)
    , agentColor :: Maybe Text
    -- ^ Display color for UI purposes
    , agentPermission :: PermissionRuleset
    -- ^ Tool permission rules for this agent
    , agentModel :: Maybe (Text, Text)
    -- ^ Specific model to use: (providerID, modelID)
    , agentVariant :: Maybe Text
    -- ^ Model variant (e.g., "fast", "accurate")
    , agentPrompt :: Maybe Text
    -- ^ Custom system prompt for the agent
    , agentOptions :: Map.Map Text Value
    -- ^ Additional configuration options
    , agentSteps :: Maybe Int
    -- ^ Maximum number of steps before forcing completion
    }
    deriving (Show, Eq, Generic)

instance ToJSON Agent where
    toJSON a =
        object $
            [ "name" .= agentName a
            , "mode" .= agentMode a
            , "permission" .= agentPermission a
            , "options" .= agentOptions a
            ]
                ++ catMaybes
                    [ ("description" .=) <$> agentDescription a
                    , ("native" .=) <$> agentNative a
                    , ("hidden" .=) <$> agentHidden a
                    , ("topP" .=) <$> agentTopP a
                    , ("temperature" .=) <$> agentTemperature a
                    , ("color" .=) <$> agentColor a
                    , fmap (\(p, m) -> "model" .= object ["providerID" .= p, "modelID" .= m]) (agentModel a)
                    , ("variant" .=) <$> agentVariant a
                    , ("prompt" .=) <$> agentPrompt a
                    , ("steps" .=) <$> agentSteps a
                    ]

instance FromJSON Agent where
    parseJSON = withObject "Agent" $ \v ->
        Agent
            <$> v .: "name"
            <*> v .:? "description"
            <*> v .: "mode"
            <*> v .:? "native"
            <*> v .:? "hidden"
            <*> v .:? "topP"
            <*> v .:? "temperature"
            <*> v .:? "color"
            <*> v .:? "permission" .!= PermissionRuleset Map.empty
            <*> v .:? "model"
            <*> v .:? "variant"
            <*> v .:? "prompt"
            <*> v .:? "options" .!= Map.empty
            <*> v .:? "steps"

{- | Smart constructor for Agent with sensible defaults.

Creates an agent with the given name, mode, and permissions.
All optional fields are set to 'Nothing' or empty, except:

* 'agentNative' is set to @Just True@
* 'agentOptions' is set to @Map.empty@

==== __Examples__

@
let perms = PermissionRuleset Map.empty
let agent = defaultAgent "my-agent" Primary perms
agentName agent  -- "my-agent"
agentNative agent -- Just True
@
-}
defaultAgent :: Text -> AgentMode -> PermissionRuleset -> Agent
defaultAgent name mode permission =
    Agent
        { agentName = name
        , agentDescription = Nothing
        , agentMode = mode
        , agentNative = Just True
        , agentHidden = Nothing
        , agentTopP = Nothing
        , agentTemperature = Nothing
        , agentColor = Nothing
        , agentPermission = permission
        , agentModel = Nothing
        , agentVariant = Nothing
        , agentPrompt = Nothing
        , agentOptions = Map.empty
        , agentSteps = Nothing
        }

-- ============================================================================
-- Pure Query Functions
-- ============================================================================

{- | Look up permission rules for a specific tool in an agent's permission set.

Returns 'Nothing' if no rules are defined for the given tool name.
The tool name can include wildcards (e.g., "*" for all tools).

==== __Examples__

@
lookupPermission "edit" agent  -- Maybe [PermissionRule]
@
-}
lookupPermission :: Text -> Agent -> Maybe [PermissionRule]
lookupPermission toolName agent =
    Map.lookup toolName (unRuleset $ agentPermission agent)

{- | Check if an agent has any permission rules for a specific tool.

Returns 'True' if rules exist (regardless of whether they allow or deny).
-}
hasPermissionFor :: Text -> Agent -> Bool
hasPermissionFor toolName agent =
    Map.member toolName (unRuleset $ agentPermission agent)

{- | Check if an agent is marked as hidden.

Hidden agents are not shown in agent listings but can still be invoked.
Returns 'False' if the hidden field is not set.
-}
isHidden :: Agent -> Bool
isHidden = fromMaybe False . agentHidden

{- | Check if an agent is a native (built-in) agent.

Returns 'False' if the native field is not set.
-}
isNative :: Agent -> Bool
isNative = fromMaybe False . agentNative

{- | Get the effective temperature for an agent.

Returns the agent's temperature if set, otherwise returns the default (1.0).
Temperature controls randomness in LLM responses (0.0 = deterministic, 2.0 = very random).
-}
getEffectiveTemperature :: Agent -> Double
getEffectiveTemperature = fromMaybe 1.0 . agentTemperature
