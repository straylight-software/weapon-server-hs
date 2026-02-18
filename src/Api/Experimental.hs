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

module Api.Experimental
    ( -- * Experimental API Endpoints
      ExperimentalToolIdsAPI
    , ExperimentalToolAPI
    , ExperimentalToolListAPI
    , ExperimentalWorktreeGetAPI
    , ExperimentalWorktreePostAPI
    , ExperimentalWorktreeResetAPI
    , ExperimentalWorktreeDeleteAPI
    ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Servant


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

-- get worktree state
type ExperimentalWorktreeGetAPI = "experimental" :> "worktree" :> Get '[JSON] Value

-- create/update worktree
type ExperimentalWorktreePostAPI = "experimental" :> "worktree" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- reset worktree
type ExperimentalWorktreeResetAPI = "experimental" :> "worktree" :> "reset" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- delete worktree
type ExperimentalWorktreeDeleteAPI = "experimental" :> "worktree" :> ReqBody '[JSON] Value :> Delete '[JSON] Bool
