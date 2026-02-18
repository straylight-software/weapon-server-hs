-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                       // weapon-server // api
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- Servant type-level API definition. Every endpoint is expressed as a type,
-- enabling compile-time verification of routing and handler signatures.
--
-- This module re-exports domain-specific types from:
--   - Api.Types        Core types (health, path, project, provider, vcs)
--   - Api.Session      Session management
--   - Api.Message      Message handling
--   - Api.File         File system operations
--   - Api.Pty          Pseudo-terminal management
--   - Api.Tui          Terminal UI controls
--   - Api.Experimental Unstable features
--
-- See docs/API.md for endpoint documentation.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( -- * Combined API
      OpencodeAPI
    , api

      -- * Re-exported Domain Types
    , module Api.Types
    , module Api.Session
    , module Api.Message
    , module Api.File
    , module Api.Pty
    , module Api.Tui
    , module Api.Experimental
    ) where

import Data.Proxy
import Servant

-- domain modules
import Api.Experimental
import Api.File
import Api.Message
import Api.Pty
import Api.Session
import Api.Tui
import Api.Types


-- ═══════════════════════════════════════════════════════════════════════════
-- // combined api //
-- ═══════════════════════════════════════════════════════════════════════════

type OpencodeAPI =
    -- core
    HealthAPI
        :<|> PathAPI
        :<|> GlobalConfigAPI
        :<|> GlobalConfigUpdateAPI
        -- project
        :<|> ProjectListAPI
        :<|> ProjectGetAPI
        :<|> ProjectUpdateAPI
        :<|> ProjectCurrentAPI
        -- provider and auth
        :<|> ProviderListAPI
        :<|> ProviderAuthAPI
        :<|> ProviderAPI
        :<|> ProviderOauthAuthorizeAPI
        :<|> ProviderOauthCallbackAPI
        :<|> AuthCreateAPI
        :<|> AuthUpdateAPI
        :<|> AuthDeleteAPI
        -- agent and config
        :<|> AgentAPI
        :<|> ConfigAPI
        :<|> ConfigUpdateAPI
        :<|> CommandAPI
        -- session
        :<|> SessionStatusAPI
        :<|> SessionListAPI
        :<|> SessionCreateAPI
        :<|> SessionGetAPI
        :<|> SessionDeleteAPI
        :<|> SessionUpdateAPI
        :<|> SessionChildrenAPI
        :<|> SessionTodoAPI
        :<|> SessionInitAPI
        :<|> SessionForkAPI
        :<|> SessionAbortAPI
        :<|> SessionShareCreateAPI
        :<|> SessionShareDeleteAPI
        :<|> SessionDiffAPI
        :<|> SessionSummarizeAPI
        :<|> SessionCommandAPI
        :<|> SessionShellAPI
        :<|> SessionRevertAPI
        :<|> SessionUnrevertAPI
        :<|> SessionPermissionAPI
        -- message
        :<|> SessionMessageListAPI
        :<|> SessionMessageCreateAPI
        :<|> SessionMessageGetAPI
        :<|> SessionMessagePartDeleteAPI
        :<|> SessionMessagePartUpdateAPI
        :<|> SessionPromptAsyncAPI
        -- infrastructure
        :<|> LspAPI
        :<|> VcsAPI
        :<|> PermissionAPI
        :<|> PermissionReplyAPI
        :<|> QuestionAPI
        :<|> QuestionReplyAPI
        :<|> QuestionRejectAPI
        -- find
        :<|> FindAPI
        :<|> FindFileAPI
        :<|> FindSymbolAPI
        -- file
        :<|> FileListAPI
        :<|> FileReadAPI
        :<|> FileStatusAPI
        -- events
        :<|> GlobalEventAPI
        -- pty
        :<|> PtyListAPI
        :<|> PtyCreateAPI
        :<|> PtyGetAPI
        :<|> PtyUpdateAPI
        :<|> PtyDeleteAPI
        :<|> PtyConnectAPI
        :<|> PtyCommitAPI
        :<|> PtyChangesAPI
        -- tui
        :<|> TuiAppendPromptAPI
        :<|> TuiOpenHelpAPI
        :<|> TuiOpenSessionsAPI
        :<|> TuiOpenThemesAPI
        :<|> TuiOpenModelsAPI
        :<|> TuiSubmitPromptAPI
        :<|> TuiClearPromptAPI
        :<|> TuiExecuteCommandAPI
        :<|> TuiShowToastAPI
        :<|> TuiPublishAPI
        :<|> TuiSelectSessionAPI
        :<|> TuiControlNextAPI
        :<|> TuiControlResponseAPI
        -- lifecycle
        :<|> InstanceDisposeAPI
        :<|> GlobalDisposeAPI
        :<|> EventAPI
        :<|> LogAPI
        :<|> SkillAPI
        :<|> FormatterAPI
        -- experimental
        :<|> ExperimentalToolIdsAPI
        :<|> ExperimentalToolListAPI
        :<|> ExperimentalToolAPI
        :<|> ExperimentalWorktreeGetAPI
        :<|> ExperimentalWorktreePostAPI
        :<|> ExperimentalWorktreeResetAPI
        :<|> ExperimentalWorktreeDeleteAPI
        -- llm
        :<|> ChatAPI


api :: Proxy OpencodeAPI
api = Proxy
