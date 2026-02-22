-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                               // weapon-server // api/session
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- Session management types and API endpoints. Sessions are the primary unit
-- of conversation state, tracking messages, diffs, and sharing status.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Session
    ( -- * Session Types (re-exported from Session.Types)
      Session (..)
    , SessionTime (..)
    , SessionSummary (..)
    , SessionShare (..)
    , SessionRevert (..)
    , CreateSessionInput (..)

      -- * API-specific Types
    , UpdateSessionInput (..)
    , ForkSessionInput (..)
    , FileDiff (..)
    , FileDiffStatus (..)

      -- * Session API Endpoints
    , SessionStatusAPI
    , SessionListAPI
    , SessionCreateAPI
    , SessionGetAPI
    , SessionDeleteAPI
    , SessionUpdateAPI
    , SessionChildrenAPI
    , SessionTodoAPI
    , SessionInitAPI
    , SessionForkAPI
    , SessionAbortAPI
    , SessionShareCreateAPI
    , SessionShareDeleteAPI
    , SessionDiffAPI
    , SessionSummarizeAPI
    , SessionCommandAPI
    , SessionShellAPI
    , SessionRevertAPI
    , SessionUnrevertAPI
    , SessionPermissionAPI
    ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant

-- Re-export canonical session types from Session.Types
import Session.Types
    ( Session (..)
    , SessionTime (..)
    , SessionSummary (..)
    , SessionShare (..)
    , SessionRevert (..)
    , CreateSessionInput (..)
    )


-- ═══════════════════════════════════════════════════════════════════════════
-- // file diff //
-- ═══════════════════════════════════════════════════════════════════════════

data FileDiffStatus = Added | Deleted | Modified
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
        _ -> fail "Invalid FileDiffStatus"

data FileDiff = FileDiff
    { fdFile :: Text
    , fdBefore :: Text
    , fdAfter :: Text
    , fdAdditions :: Int
    , fdDeletions :: Int
    , fdStatus :: Maybe FileDiffStatus
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
-- // session input //
-- ═══════════════════════════════════════════════════════════════════════════

data UpdateSessionInput = UpdateSessionInput
    { usiTitle :: Maybe Text
    , usiSummary :: Maybe SessionSummary
    , usiShare :: Maybe SessionShare
    , usiRevert :: Maybe SessionRevert
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
-- // api type definitions //
-- ═══════════════════════════════════════════════════════════════════════════

type SessionStatusAPI = "session" :> "status" :> QueryParam "directory" Text :> Get '[JSON] Value

type SessionListAPI =
    "session"
        :> QueryParam "directory" Text
        :> QueryParam "roots" Bool
        :> QueryParam "limit" Int
        :> QueryParam "start" Int
        :> QueryParam "search" Text
        :> Get '[JSON] [Session]

type SessionCreateAPI =
    "session"
        :> QueryParam "directory" Text
        :> ReqBody '[JSON] CreateSessionInput
        :> Post '[JSON] Session

type SessionGetAPI = "session" :> Capture "sessionID" Text :> Get '[JSON] Session

type SessionDeleteAPI = "session" :> Capture "sessionID" Text :> Delete '[JSON] Bool

type SessionUpdateAPI =
    "session"
        :> Capture "sessionID" Text
        :> ReqBody '[JSON] UpdateSessionInput
        :> Patch '[JSON] Session

type SessionChildrenAPI =
    "session" :> Capture "sessionID" Text :> "children" :> Get '[JSON] [Session]

type SessionTodoAPI =
    "session" :> Capture "sessionID" Text :> "todo" :> Get '[JSON] [Value]

type SessionInitAPI =
    "session" :> Capture "sessionID" Text :> "init" :> Post '[JSON] Bool

-- | Input for forking a session
data ForkSessionInput = ForkSessionInput
    { fsiMessageId :: Maybe Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON ForkSessionInput where
    parseJSON = withObject "ForkSessionInput" $ \v ->
        ForkSessionInput <$> v .:? "messageID"

instance ToJSON ForkSessionInput where
    toJSON fsi = object ["messageID" .= fsiMessageId fsi]

type SessionForkAPI =
    "session" :> Capture "sessionID" Text :> "fork" :> ReqBody '[JSON] ForkSessionInput :> Post '[JSON] Session

type SessionAbortAPI =
    "session" :> Capture "sessionID" Text :> "abort" :> QueryParam "directory" Text :> Post '[JSON] Bool

type SessionShareCreateAPI =
    "session" :> Capture "sessionID" Text :> "share" :> Post '[JSON] Session

type SessionShareDeleteAPI =
    "session" :> Capture "sessionID" Text :> "share" :> Delete '[JSON] Session

type SessionDiffAPI =
    "session"
        :> Capture "sessionID" Text
        :> "diff"
        :> QueryParam "messageID" Text
        :> Get '[JSON] [FileDiff]

type SessionSummarizeAPI =
    "session" :> Capture "sessionID" Text :> "summarize" :> Post '[JSON] Bool

type SessionCommandAPI =
    "session"
        :> Capture "sessionID" Text
        :> "command"
        :> ReqBody '[JSON] Value
        :> Post '[JSON] Value

type SessionShellAPI =
    "session"
        :> Capture "sessionID" Text
        :> "shell"
        :> ReqBody '[JSON] Value
        :> Post '[JSON] Value

type SessionRevertAPI =
    "session"
        :> Capture "sessionID" Text
        :> "revert"
        :> ReqBody '[JSON] SessionRevert
        :> Post '[JSON] Session

type SessionUnrevertAPI =
    "session" :> Capture "sessionID" Text :> "unrevert" :> Post '[JSON] Session

type SessionPermissionAPI =
    "session"
        :> Capture "sessionID" Text
        :> "permissions"
        :> Capture "permissionID" Text
        :> QueryParam "directory" Text
        :> ReqBody '[JSON] Value
        :> Post '[JSON] Bool
