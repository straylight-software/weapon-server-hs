{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- | ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                // weapon-server // api/types
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Shared data models used across multiple API domains. Core types for health,
paths, projects, providers, and VCS that form the foundation of the API.

= Overview

This module exports:

* 'Health' - Server health status for monitoring
* 'PathInfo' - System path configuration information
* 'Project' - Project workspace metadata
* 'ProviderList' / 'ConfigProviderList' - LLM provider information
* 'VcsInfo' - Version control system state
* 'ChatInput' - Simple chat request input

= API Type Definitions

The API type synonyms define Servant routes matching the OpenAPI specification.
Each type is named with an @API@ suffix (e.g., 'HealthAPI', 'ProjectListAPI').

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-}
module Api.Types (
    -- * Core Types

    -- ** Health Status
    Health (..),

    -- ** Path Information
    PathInfo (..),

    -- ** Project Management
    Project (..),
    ProjectTime (..),

    -- ** Provider Management
    ProviderList (..),
    ConfigProviderList (..),

    -- ** Version Control
    VcsInfo (..),

    -- ** Chat Input
    ChatInput (..),

    -- * Core API Endpoints

    -- ** Health and Global
    HealthAPI,
    PathAPI,
    GlobalConfigAPI,

    -- ** Project Management
    ProjectListAPI,
    ProjectGetAPI,
    ProjectUpdateAPI,
    ProjectCurrentAPI,

    -- ** Provider and Authentication
    ProviderListAPI,
    ProviderAuthAPI,
    ProviderAPI,
    ProviderOauthAuthorizeAPI,
    ProviderOauthCallbackAPI,
    AuthCreateAPI,
    AuthUpdateAPI,
    AuthDeleteAPI,

    -- ** Agent and Configuration
    AgentAPI,
    ConfigAPI,
    CommandAPI,

    -- ** Language Server and VCS
    LspAPI,
    VcsAPI,

    -- ** Permissions and Questions
    PermissionAPI,
    PermissionReplyAPI,
    PermissionReplyInput (..),
    QuestionAPI,
    QuestionReplyAPI,
    QuestionRejectAPI,

    -- ** File Search
    FindAPI,
    FindFileAPI,
    FindSymbolAPI,

    -- ** Events
    GlobalEventAPI,
    EventAPI,

    -- ** Lifecycle
    InstanceDisposeAPI,
    GlobalDisposeAPI,

    -- ** Logging
    LogAPI,

    -- ** Skills and Formatters
    SkillAPI,
    FormatterAPI,

    -- ** Chat
    ChatAPI,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Text (Text)
import Formatter.Status (FormatterStatus)
import GHC.Generics (Generic)
import Json.Strict (withStrictObject)
import Servant (
    Capture,
    Delete,
    Get,
    JSON,
    Patch,
    Post,
    Put,
    QueryParam,
    Raw,
    ReqBody,
    type (:>),
 )
import Skill.Skill (SkillInfo)

-- ═══════════════════════════════════════════════════════════════════════════
-- // health //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Server health status for monitoring and load balancers.

Used by @GET /global/health@ to verify server availability.

==== Example JSON

@
{ "healthy": true, "version": "0.1.0" }
@
-}
data Health = Health
    { healthy :: Bool
    -- ^ Whether the server is operating normally
    , version :: Text
    -- ^ Server version string (e.g., "0.1.0")
    }
    deriving (Eq, Show, Generic)

instance ToJSON Health

instance FromJSON Health

-- ═══════════════════════════════════════════════════════════════════════════
-- // path //
-- ═══════════════════════════════════════════════════════════════════════════

{- | System path configuration for the server.

Provides clients with the resolved paths for home directory, state storage,
configuration files, and workspace directories.

==== Example JSON

@
{
  "home": "/home/user",
  "state": "/home/user/.local/state/opencode",
  "config": "/home/user/.config/opencode",
  "worktree": "/home/user/projects/myapp",
  "directory": "/home/user/projects/myapp"
}
@
-}
data PathInfo = PathInfo
    { home :: Text
    -- ^ User's home directory path
    , state :: Text
    -- ^ Application state directory (XDG_STATE_HOME)
    , config :: Text
    -- ^ Application config directory (XDG_CONFIG_HOME)
    , worktree :: Text
    -- ^ Git worktree root (or project root if not in git)
    , directory :: Text
    -- ^ Current working directory
    }
    deriving (Eq, Show, Generic)

instance ToJSON PathInfo

instance FromJSON PathInfo

-- ═══════════════════════════════════════════════════════════════════════════
-- // project //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Project workspace metadata.

Represents a distinct project workspace identified by its worktree path.
Projects group sessions and configuration together.

==== Example JSON

@
{
  "id": "proj_abc123",
  "worktree": "/home/user/myproject",
  "name": "My Project",
  "time": { "created": 1709312000, "updated": 1709312100 },
  "sandboxes": []
}
@
-}
data Project = Project
    { id :: Text
    -- ^ Unique project identifier (prefixed with "proj_")
    , worktree :: Text
    -- ^ Absolute path to the project worktree
    , name :: Maybe Text
    -- ^ Optional human-readable project name
    , time :: ProjectTime
    -- ^ Project timestamps
    , sandboxes :: [Text]
    -- ^ List of sandbox directory paths
    }
    deriving (Eq, Show, Generic)

instance ToJSON Project

instance FromJSON Project

-- | Project timestamps
data ProjectTime = ProjectTime
    { created :: Double
    -- ^ Unix timestamp when project was created
    , updated :: Double
    -- ^ Unix timestamp when project was last updated
    , initialized :: Maybe Double
    -- ^ Unix timestamp when project was initialized (optional)
    }
    deriving (Eq, Show, Generic)

instance ToJSON ProjectTime where
    toJSON pt =
        object $
            [ "created" .= created pt
            , "updated" .= updated pt
            ]
                ++ maybe [] (\i -> ["initialized" .= i]) (initialized pt)

instance FromJSON ProjectTime

-- ═══════════════════════════════════════════════════════════════════════════
-- // provider //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Provider list response for @GET /provider@.

Returns all available LLM providers, the default model selection,
and which providers are currently authenticated.

==== Fields

* 'plAll' - Complete list of provider definitions
* 'plDefault' - Map of provider ID to default model ID
* 'plConnected' - List of provider IDs that are authenticated
-}
data ProviderList = ProviderList
    { plAll :: [Value]
    -- ^ All available providers (opaque JSON objects)
    , plDefault :: Value
    -- ^ Default model selection map (@{providerId: modelId}@)
    , plConnected :: [Text]
    -- ^ List of connected/authenticated provider IDs
    }
    deriving (Eq, Show, Generic)

instance ToJSON ProviderList where
    toJSON (ProviderList allProviders defaultSelection connected) =
        object
            [ "all" .= allProviders
            , "default" .= defaultSelection
            , "connected" .= connected
            ]

instance FromJSON ProviderList where
    parseJSON = withObject "ProviderList" $ \v ->
        ProviderList
            <$> v .: "all"
            <*> v .: "default"
            <*> v .: "connected"

{- | Config providers response for @GET /config/providers@.

Similar to 'ProviderList' but uses "providers" key instead of "all"
to match the OpenAPI specification for the config endpoint.
-}
data ConfigProviderList = ConfigProviderList
    { cplProviders :: [Value]
    -- ^ All available providers
    , cplDefault :: Value
    -- ^ Default model selection map
    }
    deriving (Eq, Show, Generic)

instance ToJSON ConfigProviderList where
    toJSON (ConfigProviderList providers defaultSelection) =
        object
            [ "providers" .= providers
            , "default" .= defaultSelection
            ]

instance FromJSON ConfigProviderList where
    parseJSON = withObject "ConfigProviderList" $ \v ->
        ConfigProviderList
            <$> v .: "providers"
            <*> v .: "default"

-- ═══════════════════════════════════════════════════════════════════════════
-- // vcs //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Version control system information.

Currently provides Git branch information for the workspace.
Returns 'Nothing' for the branch if not in a git repository.

==== Example JSON

@
{ "branch": "main" }
@

Returns 404 if not in a git repository.
-}
newtype VcsInfo = VcsInfo
    { branch :: Text
    -- ^ Current git branch name
    }
    deriving (Eq, Show, Generic)

instance ToJSON VcsInfo

instance FromJSON VcsInfo

-- ═══════════════════════════════════════════════════════════════════════════
-- // chat //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Simple chat request input.

Used for basic chat interactions outside of the full session/message flow.
Supports an optional model override.

==== Example JSON

@
{ "message": "Hello, how are you?", "model": "claude-3-opus" }
@
-}
data ChatInput = ChatInput
    { ciMessage :: Text
    -- ^ The user's message text
    , ciModel :: Maybe Text
    -- ^ Optional model ID override
    }
    deriving (Eq, Show, Generic)

instance FromJSON ChatInput where
    parseJSON = withObject "ChatInput" $ \v ->
        ChatInput
            <$> v .: "message"
            <*> v .:? "model"

-- ═══════════════════════════════════════════════════════════════════════════
-- // api type definitions //
-- ═══════════════════════════════════════════════════════════════════════════

-- ─────────────────────────────────────────────────────────────────────────────
-- Health and Global
-- ─────────────────────────────────────────────────────────────────────────────

-- | @GET /global/health@ - Server health check endpoint.
type HealthAPI = "global" :> "health" :> Get '[JSON] Health

-- | @GET /path@ - Get system path information.
type PathAPI = "path" :> Get '[JSON] PathInfo

-- | @GET /global/config@ - Get global configuration.
type GlobalConfigAPI = "global" :> "config" :> Get '[JSON] Value

-- ─────────────────────────────────────────────────────────────────────────────
-- Project Management
-- ─────────────────────────────────────────────────────────────────────────────

-- | @GET /project@ - List all projects.
type ProjectListAPI = "project" :> Get '[JSON] [Project]

-- | @GET /project/:projectID@ - Get a specific project.
type ProjectGetAPI = "project" :> Capture "projectID" Text :> Get '[JSON] Project

-- | @PATCH /project/:projectID@ - Update a project.
type ProjectUpdateAPI = "project" :> Capture "projectID" Text :> ReqBody '[JSON] Value :> Patch '[JSON] Project

-- | @GET /project/current@ - Get the current project for a directory.
type ProjectCurrentAPI = "project" :> "current" :> QueryParam "directory" Text :> Get '[JSON] Project

-- ─────────────────────────────────────────────────────────────────────────────
-- Provider and Authentication
-- ─────────────────────────────────────────────────────────────────────────────

-- | @GET /config/providers@ - List providers from config endpoint.
type ProviderListAPI = "config" :> "providers" :> QueryParam "directory" Text :> Get '[JSON] ConfigProviderList

-- | @GET /provider/auth@ - Get provider authentication status.
type ProviderAuthAPI = "provider" :> "auth" :> Get '[JSON] Value

-- | @GET /provider@ - List all providers with connection status.
type ProviderAPI = "provider" :> QueryParam "directory" Text :> Get '[JSON] ProviderList

-- | @POST /provider/:providerID/oauth/authorize@ - Initiate OAuth flow.
type ProviderOauthAuthorizeAPI =
    "provider" :> Capture "providerID" Text :> "oauth" :> "authorize" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- | @POST /provider/:providerID/oauth/callback@ - Complete OAuth flow.
type ProviderOauthCallbackAPI =
    "provider" :> Capture "providerID" Text :> "oauth" :> "callback" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- | @POST /auth/:providerID@ - Create provider authentication.
type AuthCreateAPI = "auth" :> Capture "providerID" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- | @PUT /auth/:providerID@ - Update provider authentication.
type AuthUpdateAPI = "auth" :> Capture "providerID" Text :> ReqBody '[JSON] Value :> Put '[JSON] Bool

-- | @DELETE /auth/:providerID@ - Delete provider authentication.
type AuthDeleteAPI = "auth" :> Capture "providerID" Text :> Delete '[JSON] Bool

-- ─────────────────────────────────────────────────────────────────────────────
-- Agent and Configuration
-- ─────────────────────────────────────────────────────────────────────────────

-- | @GET /agent@ - List available agents.
type AgentAPI = "agent" :> Get '[JSON] [Value]

-- | @GET /config@ - Get current configuration.
type ConfigAPI = "config" :> Get '[JSON] Value

-- | @GET /command@ - List available commands.
type CommandAPI = "command" :> QueryParam "directory" Text :> Get '[JSON] [Value]

-- ─────────────────────────────────────────────────────────────────────────────
-- Language Server and VCS
-- ─────────────────────────────────────────────────────────────────────────────

-- | @GET /lsp@ - List LSP server statuses.
type LspAPI = "lsp" :> Get '[JSON] [Value]

-- | @GET /vcs@ - Get VCS information.
type VcsAPI = "vcs" :> Get '[JSON] VcsInfo

-- ─────────────────────────────────────────────────────────────────────────────
-- Permissions and Questions
-- ─────────────────────────────────────────────────────────────────────────────

-- | @GET /permission@ - List pending permission requests.
type PermissionAPI = "permission" :> QueryParam "directory" Text :> Get '[JSON] [Value]

-- | @POST /permission/:requestID/reply@ - Reply to a permission request.
type PermissionReplyAPI = "permission" :> Capture "requestID" Text :> "reply" :> QueryParam "directory" Text :> ReqBody '[JSON] PermissionReplyInput :> Post '[JSON] Bool

-- | Input for permission reply endpoint (strict JSON parsing)
data PermissionReplyInput = PermissionReplyInput
    { priReply :: Text
    -- ^ Reply type: "once", "always", or "reject"
    , priMessage :: Maybe Text
    -- ^ Optional message
    }
    deriving (Show, Eq, Generic)

instance FromJSON PermissionReplyInput where
    parseJSON = withStrictObject "PermissionReplyInput" ["reply", "message"] $ \v ->
        PermissionReplyInput
            <$> v .: "reply"
            <*> v .:? "message"

instance ToJSON PermissionReplyInput where
    toJSON pri =
        object
            [ "reply" .= priReply pri
            , "message" .= priMessage pri
            ]

-- | @GET /question@ - List pending questions.
type QuestionAPI = "question" :> QueryParam "directory" Text :> Get '[JSON] [Value]

-- | @POST /question/:requestID/reply@ - Reply to a question.
type QuestionReplyAPI = "question" :> Capture "requestID" Text :> "reply" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- | @POST /question/:requestID/reject@ - Reject a question.
type QuestionRejectAPI = "question" :> Capture "requestID" Text :> "reject" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- ─────────────────────────────────────────────────────────────────────────────
-- File Search
-- ─────────────────────────────────────────────────────────────────────────────

{- | @GET /find@ - Search for text patterns in project files.
Note: directory parameter removed - always searches project directory.
-}
type FindAPI = "find" :> QueryParam "query" Text :> QueryParam "pattern" Text :> Get '[JSON] [Value]

{- | @GET /find/file@ - Search for files with advanced options.
Note: directory parameter removed - always searches project directory.
-}
type FindFileAPI = "find" :> "file" :> QueryParam "query" Text :> QueryParam "dirs" Bool :> QueryParam "type" Text :> QueryParam "limit" Int :> Get '[JSON] [Value]

{- | @GET /find/symbol@ - Search for symbols in the codebase.
Note: directory parameter removed - always searches project directory.
-}
type FindSymbolAPI = "find" :> "symbol" :> QueryParam "query" Text :> Get '[JSON] [Value]

-- ─────────────────────────────────────────────────────────────────────────────
-- Events
-- ─────────────────────────────────────────────────────────────────────────────

-- | @GET /global/event@ - Server-Sent Events stream for global events.
type GlobalEventAPI = "global" :> "event" :> Raw

-- | @GET /event@ - Server-Sent Events stream for project events.
type EventAPI = "event" :> Raw

-- ─────────────────────────────────────────────────────────────────────────────
-- Lifecycle
-- ─────────────────────────────────────────────────────────────────────────────

-- | @POST /instance/dispose@ - Dispose the current instance.
type InstanceDisposeAPI = "instance" :> "dispose" :> Post '[JSON] Bool

-- | @POST /global/dispose@ - Dispose all instances and shutdown.
type GlobalDisposeAPI = "global" :> "dispose" :> Post '[JSON] Bool

-- ─────────────────────────────────────────────────────────────────────────────
-- Logging
-- ─────────────────────────────────────────────────────────────────────────────

-- | @POST /log@ - Submit a log entry.
type LogAPI = "log" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- ─────────────────────────────────────────────────────────────────────────────
-- Skills and Formatters
-- ─────────────────────────────────────────────────────────────────────────────

-- | @GET /skill@ - List available skills.
type SkillAPI = "skill" :> QueryParam "directory" Text :> Get '[JSON] [SkillInfo]

-- | @GET /formatter@ - List formatter statuses.
type FormatterAPI = "formatter" :> QueryParam "directory" Text :> Get '[JSON] [FormatterStatus]

-- ─────────────────────────────────────────────────────────────────────────────
-- Chat
-- ─────────────────────────────────────────────────────────────────────────────

-- | @POST /chat@ - Send a simple chat message.
type ChatAPI = "chat" :> ReqBody '[JSON] ChatInput :> Post '[JSON] Value
