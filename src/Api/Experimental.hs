{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- | ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                         // weapon-server // api/experimental
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Experimental API endpoints. Unstable features for tool execution, worktree
management, and other capabilities under active development.

= Stability Warning

These endpoints are experimental and may change or be removed without
notice. They are provided for testing and development purposes.

= Features

* __Tool Endpoints__ - Direct tool execution and listing
* __Worktree Endpoints__ - Git worktree management
* __Session Endpoints__ - Global cross-project session listing

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-}
module Api.Experimental (
    -- * Experimental API Endpoints

    -- ** Tool Execution
    ExperimentalToolIdsAPI,
    ExperimentalToolAPI,
    ExperimentalToolListAPI,

    -- ** Worktree Management
    ExperimentalWorktreeGetAPI,
    ExperimentalWorktreePostAPI,
    ExperimentalWorktreeResetAPI,
    ExperimentalWorktreeDeleteAPI,
    WorktreeRemoveInput (..),
    Worktree (..),

    -- ** Global Sessions
    ExperimentalSessionListAPI,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Json.Strict (withStrictObject)
import Servant (
    Delete,
    Get,
    JSON,
    Post,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    type (:>),
 )

import Session.Types (GlobalSession)

-- ═══════════════════════════════════════════════════════════════════════════
-- // tool endpoints //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @GET /experimental/tool/ids@ - Get list of all tool IDs.

Returns an array of available tool identifiers.
Useful for discovering what tools are available.
-}
type ExperimentalToolIdsAPI = "experimental" :> "tool" :> "ids" :> Get '[JSON] [Text]

{- | @POST /experimental/tool@ - Execute a tool directly.

Executes a tool with the provided parameters and returns the result.
This bypasses the normal message/session flow.

==== Request Body

@
{
  "tool": "read_file",
  "params": { "path": "src/Main.hs" }
}
@
-}
type ExperimentalToolAPI = "experimental" :> "tool" :> ReqBody '[JSON] Value :> Post '[JSON] Value

{- | @GET /experimental/tool@ - List available tools for a provider/model.

Returns the list of tools that would be available for the specified
provider and model combination.

__Required query parameters:__

* @provider@ - Provider ID (e.g., "anthropic")
* @model@ - Model ID (e.g., "claude-3-opus-20240229")

__Optional query parameters:__

* @directory@ - Project directory context
-}
type ExperimentalToolListAPI =
    "experimental"
        :> "tool"
        :> QueryParam' '[Required] "provider" Text
        :> QueryParam' '[Required] "model" Text
        :> QueryParam "directory" Text
        :> Get '[JSON] [Value]

-- ═══════════════════════════════════════════════════════════════════════════
-- // worktree endpoints //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @GET /experimental/worktree@ - Get worktree list.

Returns an array of worktree directory paths for the current project.
-}
type ExperimentalWorktreeGetAPI = "experimental" :> "worktree" :> QueryParam "directory" Text :> Get '[JSON] [Text]

{- | @POST /experimental/worktree@ - Create or update a worktree.

Creates a new git worktree. Returns the worktree info with name, branch, and directory.
-}
type ExperimentalWorktreePostAPI = "experimental" :> "worktree" :> ReqBody '[JSON] Value :> Post '[JSON] Worktree

{- | @POST /experimental/worktree/reset@ - Reset a worktree.

Resets the worktree to a clean state, discarding local changes.
-}
type ExperimentalWorktreeResetAPI = "experimental" :> "worktree" :> "reset" :> QueryParam "directory" Text :> Post '[JSON] Bool

{- | @DELETE /experimental/worktree@ - Delete a worktree.

Removes a git worktree and cleans up associated resources.
-}
type ExperimentalWorktreeDeleteAPI = "experimental" :> "worktree" :> QueryParam "directory" Text :> ReqBody '[JSON] WorktreeRemoveInput :> Delete '[JSON] Bool

-- | Input for worktree remove endpoint (strict JSON parsing)
newtype WorktreeRemoveInput = WorktreeRemoveInput
    { wriDirectory :: Text
    -- ^ Directory of the worktree to remove
    }
    deriving (Show, Eq, Generic)

instance FromJSON WorktreeRemoveInput where
    parseJSON = withStrictObject "WorktreeRemoveInput" ["directory"] $ \v ->
        WorktreeRemoveInput <$> v .: "directory"

instance ToJSON WorktreeRemoveInput where
    toJSON wri = object ["directory" .= wriDirectory wri]

-- | Worktree response type matching the OpenAPI Worktree schema
data Worktree = Worktree
    { wtName :: Text
    -- ^ Name of the worktree
    , wtBranch :: Text
    -- ^ Git branch for this worktree
    , wtDirectory :: Text
    -- ^ Absolute path to the worktree directory
    }
    deriving (Show, Eq, Generic)

instance FromJSON Worktree where
    parseJSON = withObject "Worktree" $ \v ->
        Worktree
            <$> v .: "name"
            <*> v .: "branch"
            <*> v .: "directory"

instance ToJSON Worktree where
    toJSON (Worktree nm branch dir) =
        object
            [ "name" .= nm
            , "branch" .= branch
            , "directory" .= dir
            ]

-- ═══════════════════════════════════════════════════════════════════════════
-- // session endpoints //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @GET /experimental/session@ - List sessions globally across all projects.

Unlike the standard session list endpoint, this returns sessions from
all projects with project information included.

__Query parameters:__

* @directory@ - Filter by project directory
* @roots@ - Only return root sessions (no parentID)
* @start@ - Filter sessions updated on or after this timestamp (ms since epoch)
* @cursor@ - Return sessions updated before this timestamp (ms since epoch)
* @search@ - Filter by title (case-insensitive)
* @limit@ - Maximum sessions to return
* @archived@ - Include archived sessions (default false)
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
