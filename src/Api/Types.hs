-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                 // weapon-server // api/types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- Shared data models used across multiple API domains. Core types for health,
-- paths, projects, providers, and VCS that form the foundation of the API.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Types
    ( -- * Core Types
      Health (..)
    , PathInfo (..)
    , Project (..)
    , ProviderList (..)
    , VcsInfo (..)
    , ChatInput (..)

      -- * Core API Endpoints
    , HealthAPI
    , PathAPI
    , GlobalConfigAPI
    , GlobalConfigUpdateAPI
    , ProjectListAPI
    , ProjectGetAPI
    , ProjectUpdateAPI
    , ProjectCurrentAPI
    , ProviderListAPI
    , ProviderAuthAPI
    , ProviderAPI
    , ProviderOauthAuthorizeAPI
    , ProviderOauthCallbackAPI
    , AuthCreateAPI
    , AuthUpdateAPI
    , AuthDeleteAPI
    , AgentAPI
    , ConfigAPI
    , ConfigUpdateAPI
    , CommandAPI
    , LspAPI
    , VcsAPI
    , PermissionAPI
    , PermissionReplyAPI
    , QuestionAPI
    , QuestionReplyAPI
    , QuestionRejectAPI
    , FindAPI
    , FindFileAPI
    , FindSymbolAPI
    , GlobalEventAPI
    , EventAPI
    , InstanceDisposeAPI
    , GlobalDisposeAPI
    , LogAPI
    , SkillAPI
    , FormatterAPI
    , ChatAPI
    ) where

import Data.Aeson
import Data.Text (Text)
import Formatter.Status (FormatterStatus)
import GHC.Generics
import Servant
import Skill.Skill (SkillInfo)


-- ═══════════════════════════════════════════════════════════════════════════
-- // health //
-- ═══════════════════════════════════════════════════════════════════════════

data Health = Health
    { healthy :: Bool
    , version :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON Health

instance FromJSON Health


-- ═══════════════════════════════════════════════════════════════════════════
-- // path //
-- ═══════════════════════════════════════════════════════════════════════════

data PathInfo = PathInfo
    { home :: Text
    , state :: Text
    , config :: Text
    , worktree :: Text
    , directory :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON PathInfo

instance FromJSON PathInfo


-- ═══════════════════════════════════════════════════════════════════════════
-- // project //
-- ═══════════════════════════════════════════════════════════════════════════

data Project = Project
    { id :: Text
    , worktree :: Text
    , name :: Maybe Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON Project

instance FromJSON Project


-- ═══════════════════════════════════════════════════════════════════════════
-- // provider //
-- ═══════════════════════════════════════════════════════════════════════════

data ProviderList = ProviderList
    { providers :: [Value]
    , default_ :: Value
    }
    deriving (Eq, Show, Generic)

instance ToJSON ProviderList where
    toJSON (ProviderList providersList defaultProvider) =
        object ["providers" .= providersList, "default" .= defaultProvider]

instance FromJSON ProviderList where
    parseJSON = withObject "ProviderList" $ \v ->
        ProviderList
            <$> v .: "providers"
            <*> v .: "default"


-- ═══════════════════════════════════════════════════════════════════════════
-- // vcs //
-- ═══════════════════════════════════════════════════════════════════════════

data VcsInfo = VcsInfo
    { branch :: Maybe Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON VcsInfo

instance FromJSON VcsInfo where
    parseJSON = withObject "VcsInfo" $ \v ->
        VcsInfo
            <$> v .:? "branch"


-- ═══════════════════════════════════════════════════════════════════════════
-- // chat //
-- ═══════════════════════════════════════════════════════════════════════════

data ChatInput = ChatInput
    { ciMessage :: Text
    , ciModel :: Maybe Text
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

-- health and global
type HealthAPI = "global" :> "health" :> Get '[JSON] Health
type PathAPI = "path" :> Get '[JSON] PathInfo
type GlobalConfigAPI = "global" :> "config" :> Get '[JSON] Value
type GlobalConfigUpdateAPI = "global" :> "config" :> ReqBody '[JSON] Value :> Patch '[JSON] Value

-- project
type ProjectListAPI = "project" :> Get '[JSON] [Project]
type ProjectGetAPI = "project" :> Capture "projectID" Text :> Get '[JSON] Project
type ProjectUpdateAPI = "project" :> Capture "projectID" Text :> ReqBody '[JSON] Value :> Patch '[JSON] Project
type ProjectCurrentAPI = "project" :> "current" :> QueryParam "directory" Text :> Get '[JSON] Project

-- provider and auth
type ProviderListAPI = "config" :> "providers" :> Get '[JSON] ProviderList
type ProviderAuthAPI = "provider" :> "auth" :> Get '[JSON] Value
type ProviderAPI = "provider" :> Get '[JSON] [Value]
type ProviderOauthAuthorizeAPI =
    "provider" :> Capture "providerID" Text :> "oauth" :> "authorize" :> ReqBody '[JSON] Value :> Post '[JSON] Value
type ProviderOauthCallbackAPI =
    "provider" :> Capture "providerID" Text :> "oauth" :> "callback" :> ReqBody '[JSON] Value :> Post '[JSON] Value
type AuthCreateAPI = "auth" :> Capture "providerID" Text :> ReqBody '[JSON] Value :> Post '[JSON] Value
type AuthUpdateAPI = "auth" :> Capture "providerID" Text :> ReqBody '[JSON] Value :> Put '[JSON] Value
type AuthDeleteAPI = "auth" :> Capture "providerID" Text :> Delete '[JSON] Value

-- agent and config
type AgentAPI = "agent" :> Get '[JSON] [Value]
type ConfigAPI = "config" :> Get '[JSON] Value
type ConfigUpdateAPI = "config" :> ReqBody '[JSON] Value :> Patch '[JSON] Value
type CommandAPI = "command" :> Get '[JSON] [Value]

-- lsp and vcs
type LspAPI = "lsp" :> Get '[JSON] [Value]
type VcsAPI = "vcs" :> Get '[JSON] VcsInfo

-- permission and question
type PermissionAPI = "permission" :> Get '[JSON] [Value]
type PermissionReplyAPI = "permission" :> Capture "requestID" Text :> "reply" :> ReqBody '[JSON] Value :> Post '[JSON] Value
type QuestionAPI = "question" :> Get '[JSON] [Value]
type QuestionReplyAPI = "question" :> Capture "requestID" Text :> "reply" :> ReqBody '[JSON] Value :> Post '[JSON] Value
type QuestionRejectAPI = "question" :> Capture "requestID" Text :> "reject" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- find
type FindAPI = "find" :> QueryParam "query" Text :> QueryParam "pattern" Text :> QueryParam "directory" Text :> Get '[JSON] [Value]
type FindFileAPI = "find" :> "file" :> QueryParam "pattern" Text :> QueryParam "directory" Text :> QueryParam "dirs" Bool :> QueryParam "type" Text :> QueryParam "limit" Int :> Get '[JSON] [Value]
type FindSymbolAPI = "find" :> "symbol" :> QueryParam "query" Text :> QueryParam "directory" Text :> Get '[JSON] [Value]

-- events
type GlobalEventAPI = "global" :> "event" :> Raw
type EventAPI = "event" :> Raw

-- lifecycle
type InstanceDisposeAPI = "instance" :> "dispose" :> Post '[JSON] Value
type GlobalDisposeAPI = "global" :> "dispose" :> Post '[JSON] Value

-- logging
type LogAPI = "log" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- skill and formatter
type SkillAPI = "skill" :> QueryParam "directory" Text :> Get '[JSON] [SkillInfo]
type FormatterAPI = "formatter" :> QueryParam "directory" Text :> Get '[JSON] [FormatterStatus]

-- chat
type ChatAPI = "chat" :> ReqBody '[JSON] ChatInput :> Post '[JSON] Value
