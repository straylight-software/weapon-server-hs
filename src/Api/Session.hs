{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- | ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                              // weapon-server // api/session
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Session management types and API endpoints. Sessions are the primary unit
of conversation state, tracking messages, diffs, and sharing status.

= Overview

Sessions group messages into coherent conversations. Each session:

* Has a unique identifier (prefixed with "ses_")
* Belongs to a project
* Can have a parent session (for forks/branches)
* Tracks file changes (diffs) made during the conversation
* Can be shared via a public URL

= Type Re-exports

Core session types are defined in "Session.Types" and re-exported here
for convenience. API-specific types like 'UpdateSessionInput' and
'FileDiff' are defined in this module.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-}
module Api.Session (
    -- * Session Types (re-exported from Session.Types)
    Session (..),
    SessionTime (..),
    SessionSummary (..),
    SessionShare (..),
    SessionRevert (..),
    CreateSessionInput (..),

    -- * API-specific Types

    -- ** Session Updates
    UpdateSessionInput (..),
    ForkSessionInput (..),

    -- ** File Diffs
    FileDiff (..),
    FileDiffStatus (..),

    -- * Session API Endpoints

    -- ** Session CRUD
    SessionStatusAPI,
    SessionListAPI,
    SessionCreateAPI,
    SessionGetAPI,
    SessionDeleteAPI,
    SessionUpdateAPI,

    -- ** Session Navigation
    SessionChildrenAPI,
    SessionTodoAPI,

    -- ** Session Lifecycle
    SessionInitAPI,
    SessionForkAPI,
    SessionAbortAPI,

    -- ** Sharing
    SessionShareCreateAPI,
    SessionShareDeleteAPI,

    -- ** Diffs and Revert
    SessionDiffAPI,
    SessionSummarizeAPI,
    SessionRevertAPI,
    SessionUnrevertAPI,

    -- ** Commands
    SessionCommandAPI,
    SessionShellAPI,
    SessionPermissionAPI,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value,
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
 )
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (
    Capture,
    Delete,
    Get,
    JSON,
    Patch,
    Post,
    QueryParam,
    ReqBody,
    type (:>),
 )

-- Re-export canonical session types from Session.Types
import Session.Types (
    CreateSessionInput (..),
    Session (..),
    SessionRevert (..),
    SessionShare (..),
    SessionSummary (..),
    SessionTime (..),
 )

-- ═══════════════════════════════════════════════════════════════════════════
-- // file diff status //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Status of a file in a diff.

Indicates whether a file was added, deleted, or modified during a session.
-}
data FileDiffStatus
    = -- | File was created
      Added
    | -- | File was removed
      Deleted
    | -- | File was changed
      Modified
    deriving (Eq, Show, Generic)

instance ToJSON FileDiffStatus where
    toJSON Added = "added"
    toJSON Deleted = "deleted"
    toJSON Modified = "modified"

instance FromJSON FileDiffStatus where
    parseJSON = withText "FileDiffStatus" $ \case
        "added" -> pure Added
        "deleted" -> pure Deleted
        "modified" -> pure Modified
        other -> fail $ "Invalid FileDiffStatus: " ++ show other

-- ═══════════════════════════════════════════════════════════════════════════
-- // file diff //
-- ═══════════════════════════════════════════════════════════════════════════

{- | A file diff showing changes made during a session.

Contains the file path, before/after content, and statistics about
lines added and removed.

==== Example JSON

@
{
  "file": "src/Main.hs",
  "before": "module Main where\\n...",
  "after": "module Main where\\n-- Updated\\n...",
  "additions": 5,
  "deletions": 2,
  "status": "modified"
}
@
-}
data FileDiff = FileDiff
    { fdFile :: Text
    -- ^ Relative file path from project root
    , fdBefore :: Text
    -- ^ File content before changes (empty for new files)
    , fdAfter :: Text
    -- ^ File content after changes (empty for deleted files)
    , fdAdditions :: Int
    -- ^ Number of lines added
    , fdDeletions :: Int
    -- ^ Number of lines deleted
    , fdStatus :: Maybe FileDiffStatus
    -- ^ Change status (added, deleted, modified)
    }
    deriving (Eq, Show, Generic)

instance ToJSON FileDiff where
    toJSON fd =
        object
            [ "file" .= fdFile fd
            , "before" .= fdBefore fd
            , "after" .= fdAfter fd
            , "additions" .= fdAdditions fd
            , "deletions" .= fdDeletions fd
            , "status" .= fdStatus fd
            ]

instance FromJSON FileDiff where
    parseJSON = withObject "FileDiff" $ \v ->
        FileDiff
            <$> v .: "file"
            <*> v .: "before"
            <*> v .: "after"
            <*> v .: "additions"
            <*> v .: "deletions"
            <*> v .:? "status"

-- ═══════════════════════════════════════════════════════════════════════════
-- // session update input //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Input for updating a session via @PATCH /session/:sessionID@.

All fields are optional. Only provided fields will be updated.

==== Example JSON

@
{ "title": "New Session Title" }
@

Or to update sharing:

@
{ "share": { "url": "https://share.example.com/abc123" } }
@
-}
data UpdateSessionInput = UpdateSessionInput
    { usiTitle :: Maybe Text
    -- ^ New session title
    , usiSummary :: Maybe SessionSummary
    -- ^ Updated summary statistics
    , usiShare :: Maybe SessionShare
    -- ^ Sharing configuration
    , usiRevert :: Maybe SessionRevert
    -- ^ Revert configuration
    }
    deriving (Eq, Show, Generic)

instance FromJSON UpdateSessionInput where
    parseJSON = withObject "UpdateSessionInput" $ \v ->
        UpdateSessionInput
            <$> v .:? "title"
            <*> v .:? "summary"
            <*> v .:? "share"
            <*> v .:? "revert"

instance ToJSON UpdateSessionInput where
    toJSON input =
        object
            [ "title" .= usiTitle input
            , "summary" .= usiSummary input
            , "share" .= usiShare input
            , "revert" .= usiRevert input
            ]

-- ═══════════════════════════════════════════════════════════════════════════
-- // fork session input //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Input for forking a session via @POST /session/:sessionID/fork@.

Creates a new session branching from the specified message point.

==== Example JSON

@
{ "messageID": "msg_abc123" }
@

If @messageID@ is omitted, forks from the latest message.
-}
newtype ForkSessionInput = ForkSessionInput
    { fsiMessageId :: Maybe Text
    -- ^ Message ID to fork from (defaults to latest)
    }
    deriving (Eq, Show, Generic)

instance FromJSON ForkSessionInput where
    parseJSON = withObject "ForkSessionInput" $ \v ->
        ForkSessionInput <$> v .:? "messageID"

instance ToJSON ForkSessionInput where
    toJSON fsi = object ["messageID" .= fsiMessageId fsi]

-- ═══════════════════════════════════════════════════════════════════════════
-- // api type definitions //
-- ═══════════════════════════════════════════════════════════════════════════

-- ─────────────────────────────────────────────────────────────────────────────
-- Session CRUD
-- ─────────────────────────────────────────────────────────────────────────────

-- | @GET /session/status@ - Get session status for a directory.
type SessionStatusAPI = "session" :> "status" :> QueryParam "directory" Text :> Get '[JSON] Value

{- | @GET /session@ - List sessions with filtering options.

Query parameters:

* @directory@ - Filter by project directory
* @roots@ - Only return root sessions (no parent)
* @limit@ - Maximum number of sessions
* @start@ - Filter by minimum updated timestamp
* @search@ - Filter by title (case-insensitive)
-}
type SessionListAPI =
    "session"
        :> QueryParam "directory" Text
        :> QueryParam "roots" Bool
        :> QueryParam "limit" Double
        :> QueryParam "start" Double
        :> QueryParam "search" Text
        :> Get '[JSON] [Session]

-- | @POST /session@ - Create a new session.
type SessionCreateAPI =
    "session"
        :> QueryParam "directory" Text
        :> ReqBody '[JSON] CreateSessionInput
        :> Post '[JSON] Session

-- | @GET /session/:sessionID@ - Get a specific session.
type SessionGetAPI = "session" :> Capture "sessionID" Text :> Get '[JSON] Session

-- | @DELETE /session/:sessionID@ - Delete a session.
type SessionDeleteAPI = "session" :> Capture "sessionID" Text :> Delete '[JSON] Bool

-- | @PATCH /session/:sessionID@ - Update a session.
type SessionUpdateAPI =
    "session"
        :> Capture "sessionID" Text
        :> ReqBody '[JSON] UpdateSessionInput
        :> Patch '[JSON] Session

-- ─────────────────────────────────────────────────────────────────────────────
-- Session Navigation
-- ─────────────────────────────────────────────────────────────────────────────

-- | @GET /session/:sessionID/children@ - Get child sessions (forks).
type SessionChildrenAPI =
    "session" :> Capture "sessionID" Text :> "children" :> QueryParam "directory" Text :> Get '[JSON] [Session]

-- | @GET /session/:sessionID/todo@ - Get TODO items from the session.
type SessionTodoAPI =
    "session" :> Capture "sessionID" Text :> "todo" :> Get '[JSON] [Value]

-- ─────────────────────────────────────────────────────────────────────────────
-- Session Lifecycle
-- ─────────────────────────────────────────────────────────────────────────────

-- | @POST /session/:sessionID/init@ - Initialize a session.
type SessionInitAPI =
    "session" :> Capture "sessionID" Text :> "init" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

{- | @POST /session/:sessionID/fork@ - Fork a session.

Creates a new session branching from this one at the specified message.
-}
type SessionForkAPI =
    "session" :> Capture "sessionID" Text :> "fork" :> ReqBody '[JSON] ForkSessionInput :> Post '[JSON] Session

-- | @POST /session/:sessionID/abort@ - Abort a running session.
type SessionAbortAPI =
    "session" :> Capture "sessionID" Text :> "abort" :> QueryParam "directory" Text :> Post '[JSON] Bool

-- ─────────────────────────────────────────────────────────────────────────────
-- Sharing
-- ─────────────────────────────────────────────────────────────────────────────

-- | @POST /session/:sessionID/share@ - Create a share link for a session.
type SessionShareCreateAPI =
    "session" :> Capture "sessionID" Text :> "share" :> Post '[JSON] Session

-- | @DELETE /session/:sessionID/share@ - Delete a share link.
type SessionShareDeleteAPI =
    "session" :> Capture "sessionID" Text :> "share" :> Delete '[JSON] Session

-- ─────────────────────────────────────────────────────────────────────────────
-- Diffs and Revert
-- ─────────────────────────────────────────────────────────────────────────────

{- | @GET /session/:sessionID/diff@ - Get file diffs for a session.

Query parameters:

* @messageID@ - Get diff up to a specific message (default: all)
-}
type SessionDiffAPI =
    "session"
        :> Capture "sessionID" Text
        :> "diff"
        :> QueryParam "directory" Text
        :> QueryParam "messageID" Text
        :> Get '[JSON] [FileDiff]

-- | @POST /session/:sessionID/summarize@ - Generate a session summary.
type SessionSummarizeAPI =
    "session" :> Capture "sessionID" Text :> "summarize" :> Post '[JSON] Bool

-- | @POST /session/:sessionID/revert@ - Revert file changes.
type SessionRevertAPI =
    "session"
        :> Capture "sessionID" Text
        :> "revert"
        :> ReqBody '[JSON] SessionRevert
        :> Post '[JSON] Session

-- | @POST /session/:sessionID/unrevert@ - Undo a revert operation.
type SessionUnrevertAPI =
    "session" :> Capture "sessionID" Text :> "unrevert" :> Post '[JSON] Session

-- ─────────────────────────────────────────────────────────────────────────────
-- Commands
-- ─────────────────────────────────────────────────────────────────────────────

-- | @POST /session/:sessionID/command@ - Execute a command in session context.
type SessionCommandAPI =
    "session"
        :> Capture "sessionID" Text
        :> "command"
        :> ReqBody '[JSON] Value
        :> Post '[JSON] Value

-- | @POST /session/:sessionID/shell@ - Execute a shell command.
type SessionShellAPI =
    "session"
        :> Capture "sessionID" Text
        :> "shell"
        :> ReqBody '[JSON] Value
        :> Post '[JSON] Value

-- | @POST /session/:sessionID/permissions/:permissionID@ - Handle permission request.
type SessionPermissionAPI =
    "session"
        :> Capture "sessionID" Text
        :> "permissions"
        :> Capture "permissionID" Text
        :> QueryParam "directory" Text
        :> ReqBody '[JSON] Value
        :> Post '[JSON] Bool
