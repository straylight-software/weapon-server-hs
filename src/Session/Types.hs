{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Session.Types
Description : Session type definitions for the AI coding agent server

This module defines the core session types used throughout the application.
It mirrors the TypeScript Session.Info schema from the reference implementation.

= Session Lifecycle

Sessions represent a single conversation or work unit with the AI agent.
Each session has:

* A unique ID (descending for sorted listing)
* A human-readable slug
* Association with a project and directory
* Time tracking (created, updated, archived)
* Optional summary, share, and revert state

= JSON Serialization

All types use custom ToJSON/FromJSON instances that:

* Omit null optional fields (no @null@ values in output)
* Use camelCase field names matching the TypeScript API
-}
module Session.Types (
    -- * Core Session Types
    Session (..),
    SessionTime (..),
    SessionSummary (..),
    SessionShare (..),
    SessionRevert (..),
    CreateSessionInput (..),

    -- * Global Session Types

    {- | Types for the @\/experimental\/session@ endpoint that supports
    cross-project session listing with embedded project info.
    -}
    GlobalSession (..),
    ProjectSummary (..),
    toGlobalSession,
) where

import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Time tracking for a session.

All timestamps are in milliseconds since Unix epoch (matching JavaScript's @Date.now()@).

[@stCreated@]: When the session was first created
[@stUpdated@]: When the session was last modified (automatically updated on any change)
[@stCompacting@]: When message compaction started (if in progress)
[@stArchived@]: When the session was archived (if archived)
-}
data SessionTime = SessionTime
    { stCreated :: Double
    -- ^ Creation timestamp in milliseconds
    , stUpdated :: Double
    -- ^ Last update timestamp in milliseconds
    , stCompacting :: Maybe Double
    -- ^ Compaction start timestamp (if compacting)
    , stArchived :: Maybe Double
    -- ^ Archive timestamp (if archived)
    }
    deriving (Show, Eq, Generic)

instance ToJSON SessionTime where
    toJSON st =
        object $
            [ "created" .= stCreated st
            , "updated" .= stUpdated st
            ]
                ++ catMaybes
                    [ ("compacting" .=) <$> stCompacting st
                    , ("archived" .=) <$> stArchived st
                    ]

instance FromJSON SessionTime where
    parseJSON = withObject "SessionTime" $ \v ->
        SessionTime
            <$> v .: "created"
            <*> v .: "updated"
            <*> v .:? "compacting"
            <*> v .:? "archived"

{- | Summary statistics for changes made during a session.

Tracks the cumulative diff statistics for all file modifications.
This is displayed in the UI to give users a quick overview of session impact.
-}
data SessionSummary = SessionSummary
    { ssAdditions :: Int
    -- ^ Total lines added across all files
    , ssDeletions :: Int
    -- ^ Total lines deleted across all files
    , ssFiles :: Maybe Int
    -- ^ Number of files modified (optional for backwards compatibility)
    }
    deriving (Show, Eq, Generic)

instance ToJSON SessionSummary where
    toJSON ss =
        object $
            [ "additions" .= ssAdditions ss
            , "deletions" .= ssDeletions ss
            ]
                ++ catMaybes
                    [ ("files" .=) <$> ssFiles ss
                    ]

instance FromJSON SessionSummary where
    parseJSON = withObject "SessionSummary" $ \v ->
        SessionSummary
            <$> v .: "additions"
            <*> v .: "deletions"
            <*> v .:? "files"

{- | Information about a shared session.

When a session is shared publicly, this contains the URL where it can be viewed.
-}
newtype SessionShare = SessionShare
    { shareUrl :: Text
    -- ^ Public URL for viewing the shared session
    }
    deriving (Show, Eq, Generic)

instance ToJSON SessionShare where
    toJSON ss = object ["url" .= shareUrl ss]

instance FromJSON SessionShare where
    parseJSON = withObject "SessionShare" $ \v ->
        SessionShare
            <$> v .: "url"

{- | State for an in-progress or completed revert operation.

When a user reverts changes from a specific message, this tracks which
message and what state to restore. The snapshot and diff are used to
show the user what will be (or was) reverted.
-}
data SessionRevert = SessionRevert
    { revertMessageID :: Text
    -- ^ ID of the message being reverted
    , revertPartID :: Maybe Text
    -- ^ Specific part within the message (for multi-part messages)
    , revertSnapshot :: Maybe Text
    -- ^ File state snapshot before the change
    , revertDiff :: Maybe Text
    -- ^ Diff showing what was reverted
    }
    deriving (Show, Eq, Generic)

instance ToJSON SessionRevert where
    toJSON sr =
        object $
            ("messageID" .= revertMessageID sr)
                : catMaybes
                    [ ("partID" .=) <$> revertPartID sr
                    , ("snapshot" .=) <$> revertSnapshot sr
                    , ("diff" .=) <$> revertDiff sr
                    ]

instance FromJSON SessionRevert where
    parseJSON = withObject "SessionRevert" $ \v ->
        SessionRevert
            <$> v .: "messageID"
            <*> v .:? "partID"
            <*> v .:? "snapshot"
            <*> v .:? "diff"

{- | Full session information.

This is the primary session record containing all metadata about a
conversation with the AI agent. Sessions are stored in the project's
storage directory and can be listed, filtered, and searched.

= Session Identity

Each session has two identifiers:

* @sessionId@: A unique, descending ID for efficient sorted listing (e.g., @ses_abc123@)
* @sessionSlug@: A human-readable identifier for URLs

= Hierarchy

Sessions can form parent-child relationships through @sessionParentID@,
allowing for branching conversations or continuation sessions.
-}
data Session = Session
    { sessionId :: Text
    -- ^ Unique session identifier (descending for sorted listing)
    , sessionSlug :: Text
    -- ^ Human-readable slug for URLs
    , sessionProjectID :: Text
    -- ^ ID of the project this session belongs to
    , sessionDirectory :: Text
    -- ^ Working directory for this session
    , sessionParentID :: Maybe Text
    -- ^ Parent session ID (for branched/continued sessions)
    , sessionTitle :: Text
    -- ^ User-visible session title
    , sessionVersion :: Text
    -- ^ Server version that created this session
    , sessionTime :: SessionTime
    -- ^ Time tracking information
    , sessionSummary :: Maybe SessionSummary
    -- ^ Cumulative change statistics
    , sessionShare :: Maybe SessionShare
    -- ^ Public sharing information
    , sessionRevert :: Maybe SessionRevert
    -- ^ In-progress or completed revert state
    }
    deriving (Show, Eq, Generic)

instance ToJSON Session where
    toJSON s =
        object $
            [ "id" .= sessionId s
            , "slug" .= sessionSlug s
            , "projectID" .= sessionProjectID s
            , "directory" .= sessionDirectory s
            , "title" .= sessionTitle s
            , "version" .= sessionVersion s
            , "time" .= sessionTime s
            ]
                ++ catMaybes
                    [ ("parentID" .=) <$> sessionParentID s
                    , ("summary" .=) <$> sessionSummary s
                    , ("share" .=) <$> sessionShare s
                    , ("revert" .=) <$> sessionRevert s
                    ]

instance FromJSON Session where
    parseJSON = withObject "Session" $ \v ->
        Session
            <$> v .: "id"
            <*> v .: "slug"
            <*> v .: "projectID"
            <*> v .: "directory"
            <*> v .:? "parentID"
            <*> v .: "title"
            <*> v .: "version"
            <*> v .: "time"
            <*> v .:? "summary"
            <*> v .:? "share"
            <*> v .:? "revert"

{- | Input parameters for creating a new session.

Both fields are optional:

* If @csiTitle@ is @Nothing@, a default title with timestamp is generated
* If @csiParentID@ is @Nothing@, a root session is created
-}
data CreateSessionInput = CreateSessionInput
    { csiTitle :: Maybe Text
    -- ^ Optional custom title (default: "New session - <timestamp>")
    , csiParentID :: Maybe Text
    -- ^ Optional parent session ID for branching
    }
    deriving (Show, Eq, Generic)

instance FromJSON CreateSessionInput where
    parseJSON = withObject "CreateSessionInput" $ \v ->
        CreateSessionInput
            <$> v .:? "title"
            <*> v .:? "parentID"

instance ToJSON CreateSessionInput where
    toJSON csi =
        object
            [ "title" .= csiTitle csi
            , "parentID" .= csiParentID csi
            ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Global Session Types (for /experimental/session endpoint)
-- ═══════════════════════════════════════════════════════════════════════════

{- | Summary of a project, embedded in 'GlobalSession' responses.

This provides enough information to identify which project a session
belongs to without requiring a separate project lookup.
-}
data ProjectSummary = ProjectSummary
    { psId :: Text
    -- ^ Project identifier
    , psName :: Maybe Text
    -- ^ Human-readable project name (if configured)
    , psWorktree :: Text
    -- ^ Filesystem path to the project worktree
    }
    deriving (Show, Eq, Generic)

instance ToJSON ProjectSummary where
    toJSON ps =
        object
            [ "id" .= psId ps
            , "name" .= psName ps
            , "worktree" .= psWorktree ps
            ]

instance FromJSON ProjectSummary where
    parseJSON = withObject "ProjectSummary" $ \v ->
        ProjectSummary
            <$> v .: "id"
            <*> v .:? "name"
            <*> v .: "worktree"

{- | Session with embedded project information.

This extends 'Session' with project metadata for the @\/experimental\/session@
endpoint, which lists sessions across all projects. The embedded project
summary allows clients to display project context without additional API calls.

All fields mirror 'Session' except for the additional @gsProject@ field.
-}
data GlobalSession = GlobalSession
    { gsId :: Text
    -- ^ Unique session identifier
    , gsSlug :: Text
    -- ^ Human-readable slug
    , gsProjectID :: Text
    -- ^ Project identifier
    , gsDirectory :: Text
    -- ^ Working directory
    , gsParentID :: Maybe Text
    -- ^ Parent session ID
    , gsTitle :: Text
    -- ^ Session title
    , gsVersion :: Text
    -- ^ Server version
    , gsTime :: SessionTime
    -- ^ Time tracking
    , gsSummary :: Maybe SessionSummary
    -- ^ Change statistics
    , gsShare :: Maybe SessionShare
    -- ^ Sharing info
    , gsRevert :: Maybe SessionRevert
    -- ^ Revert state
    , gsProject :: Maybe ProjectSummary
    -- ^ Embedded project summary
    }
    deriving (Show, Eq, Generic)

instance ToJSON GlobalSession where
    toJSON gs =
        object $
            [ "id" .= gsId gs
            , "slug" .= gsSlug gs
            , "projectID" .= gsProjectID gs
            , "directory" .= gsDirectory gs
            , "title" .= gsTitle gs
            , "version" .= gsVersion gs
            , "time" .= gsTime gs
            ]
                ++ catMaybes
                    [ ("parentID" .=) <$> gsParentID gs
                    , ("summary" .=) <$> gsSummary gs
                    , ("share" .=) <$> gsShare gs
                    , ("revert" .=) <$> gsRevert gs
                    , ("project" .=) <$> gsProject gs
                    ]

instance FromJSON GlobalSession where
    parseJSON = withObject "GlobalSession" $ \v ->
        GlobalSession
            <$> v .: "id"
            <*> v .: "slug"
            <*> v .: "projectID"
            <*> v .: "directory"
            <*> v .:? "parentID"
            <*> v .: "title"
            <*> v .: "version"
            <*> v .: "time"
            <*> v .:? "summary"
            <*> v .:? "share"
            <*> v .:? "revert"
            <*> v .:? "project"

{- | Convert a 'Session' to 'GlobalSession' with optional project info.

This is a pure transformation that embeds project metadata into the session
record. Used when preparing responses for the @\/experimental\/session@ endpoint.

==== __Example__

@
let proj = ProjectSummary "proj_123" (Just "My Project") "/home/user/project"
    globalSession = toGlobalSession session (Just proj)
@
-}
toGlobalSession :: Session -> Maybe ProjectSummary -> GlobalSession
toGlobalSession s mProj =
    GlobalSession
        { gsId = sessionId s
        , gsSlug = sessionSlug s
        , gsProjectID = sessionProjectID s
        , gsDirectory = sessionDirectory s
        , gsParentID = sessionParentID s
        , gsTitle = sessionTitle s
        , gsVersion = sessionVersion s
        , gsTime = sessionTime s
        , gsSummary = sessionSummary s
        , gsShare = sessionShare s
        , gsRevert = sessionRevert s
        , gsProject = mProj
        }
