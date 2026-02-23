-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                          // weapon-server // api/experimental
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- Experimental API endpoints. Unstable features for tool execution, worktree
-- management, and other capabilities under active development.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Experimental (
    -- * Experimental API Endpoints
    ExperimentalToolIdsAPI,
    ExperimentalToolAPI,
    ExperimentalToolListAPI,
    ExperimentalWorktreeGetAPI,
    ExperimentalWorktreePostAPI,
    ExperimentalWorktreeResetAPI,
    ExperimentalWorktreeDeleteAPI,
    ExperimentalSessionListAPI,
) where

import Data.Aeson (Value)
import Data.Text (Text)
import Servant

import Session.Types (GlobalSession)

-- ═══════════════════════════════════════════════════════════════════════════
-- // api type definitions //
-- ═══════════════════════════════════════════════════════════════════════════

-- ─────────────────────────────────────────────────────────────────────────────
-- tool endpoints
-- ─────────────────────────────────────────────────────────────────────────────

-- get list of tool ids
type ExperimentalToolIdsAPI = "experimental" :> "tool" :> "ids" :> Get '[JSON] [Text]

-- execute a tool
type ExperimentalToolAPI = "experimental" :> "tool" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- list available tools for a provider/model
type ExperimentalToolListAPI =
    "experimental"
        :> "tool"
        :> QueryParam' '[Required] "provider" Text
        :> QueryParam' '[Required] "model" Text
        :> QueryParam "directory" Text
        :> Get '[JSON] [Value]

-- ─────────────────────────────────────────────────────────────────────────────
-- worktree endpoints
-- ─────────────────────────────────────────────────────────────────────────────

-- get worktree list (returns array of directory strings)
type ExperimentalWorktreeGetAPI = "experimental" :> "worktree" :> QueryParam "directory" Text :> Get '[JSON] [Text]

-- create/update worktree
type ExperimentalWorktreePostAPI = "experimental" :> "worktree" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- reset worktree
type ExperimentalWorktreeResetAPI = "experimental" :> "worktree" :> "reset" :> QueryParam "directory" Text :> Post '[JSON] Bool

-- delete worktree
type ExperimentalWorktreeDeleteAPI = "experimental" :> "worktree" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Delete '[JSON] Bool

-- ─────────────────────────────────────────────────────────────────────────────
-- session endpoints (global cross-project session listing)
-- ─────────────────────────────────────────────────────────────────────────────

{- | List sessions globally across all projects
GET /experimental/session
Query params:
  directory: Filter by project directory
  roots: Only return root sessions (no parentID)
  start: Filter sessions updated on or after this timestamp (ms since epoch)
  cursor: Return sessions updated before this timestamp (ms since epoch)
  search: Filter by title (case-insensitive)
  limit: Maximum sessions to return
  archived: Include archived sessions (default false)
-}
type ExperimentalSessionListAPI =
    "experimental"
        :> "session"
        :> QueryParam "directory" Text
        :> QueryParam "roots" Bool
        :> QueryParam "start" Double
        :> QueryParam "cursor" Double
        :> QueryParam "search" Text
        :> QueryParam "limit" Int
        :> QueryParam "archived" Bool
        :> Get '[JSON] [GlobalSession]
