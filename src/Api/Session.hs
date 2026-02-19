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
    ( -- * Session Types
      SessionTime (..)
    , SessionSummary (..)
    , SessionShare (..)
    , SessionRevert (..)
    , Session (..)
    , UpdateSessionInput (..)
    , CreateSessionInput (..)

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


-- ═══════════════════════════════════════════════════════════════════════════
-- // session time //
-- ═══════════════════════════════════════════════════════════════════════════

data SessionTime = SessionTime
    { stCreated :: Double
    , stUpdated :: Double
    , stArchived :: Maybe Double
    }
    deriving (Eq, Show, Generic)

instance ToJSON SessionTime where
    toJSON sessionTime =
        object
            [ "created" .= stCreated sessionTime
            , "updated" .= stUpdated sessionTime
            , "archived" .= stArchived sessionTime
            ]

instance FromJSON SessionTime where
    parseJSON = withObject "SessionTime" $ \v ->
        SessionTime
            <$> v .: "created"
            <*> v .: "updated"
            <*> v .:? "archived"


-- ═══════════════════════════════════════════════════════════════════════════
-- // session summary //
-- ═══════════════════════════════════════════════════════════════════════════

data SessionSummary = SessionSummary
    { ssAdditions :: Int
    , ssDeletions :: Int
    , ssFiles :: Maybe Int
    }
    deriving (Eq, Show, Generic)

instance ToJSON SessionSummary where
    toJSON summary =
        object
            [ "additions" .= ssAdditions summary
            , "deletions" .= ssDeletions summary
            , "files" .= ssFiles summary
            ]

instance FromJSON SessionSummary where
    parseJSON = withObject "SessionSummary" $ \v ->
        SessionSummary
            <$> v .: "additions"
            <*> v .: "deletions"
            <*> v .:? "files"


-- ═══════════════════════════════════════════════════════════════════════════
-- // session share //
-- ═══════════════════════════════════════════════════════════════════════════

newtype SessionShare = SessionShare
    { shareUrl :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON SessionShare where
    toJSON share = object ["url" .= shareUrl share]

instance FromJSON SessionShare where
    parseJSON = withObject "SessionShare" $ \v ->
        SessionShare
            <$> v .: "url"


-- ═══════════════════════════════════════════════════════════════════════════
-- // session revert //
-- ═══════════════════════════════════════════════════════════════════════════

data SessionRevert = SessionRevert
    { srMessageId :: Text
    , srPartId :: Maybe Text
    , srSnapshot :: Maybe Text
    , srDiff :: Maybe Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON SessionRevert where
    toJSON revert =
        object
            [ "messageID" .= srMessageId revert
            , "partID" .= srPartId revert
            , "snapshot" .= srSnapshot revert
            , "diff" .= srDiff revert
            ]

instance FromJSON SessionRevert where
    parseJSON = withObject "SessionRevert" $ \v ->
        SessionRevert
            <$> v .: "messageID"
            <*> v .:? "partID"
            <*> v .:? "snapshot"
            <*> v .:? "diff"


-- ═══════════════════════════════════════════════════════════════════════════
-- // session //
-- ═══════════════════════════════════════════════════════════════════════════

data Session = Session
    { sesId :: Text
    , sesSlug :: Text
    , sesProjectId :: Text
    , sesDirectory :: Text
    , sesTitle :: Text
    , sesVersion :: Text
    , sesTime :: SessionTime
    , sesParentId :: Maybe Text
    , sesSummary :: Maybe SessionSummary
    , sesShare :: Maybe SessionShare
    , sesRevert :: Maybe SessionRevert
    }
    deriving (Eq, Show, Generic)

instance ToJSON Session where
    toJSON session =
        object
            [ "id" .= sesId session
            , "slug" .= sesSlug session
            , "projectID" .= sesProjectId session
            , "directory" .= sesDirectory session
            , "title" .= sesTitle session
            , "version" .= sesVersion session
            , "time" .= sesTime session
            , "parentID" .= sesParentId session
            , "summary" .= sesSummary session
            , "share" .= sesShare session
            , "revert" .= sesRevert session
            ]

instance FromJSON Session where
    parseJSON = withObject "Session" $ \v ->
        Session
            <$> v .: "id"
            <*> v .: "slug"
            <*> v .: "projectID"
            <*> v .: "directory"
            <*> v .: "title"
            <*> v .: "version"
            <*> v .: "time"
            <*> v .:? "parentID"
            <*> v .:? "summary"
            <*> v .:? "share"
            <*> v .:? "revert"


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

data CreateSessionInput = CreateSessionInput
    { csiTitle :: Maybe Text
    , csiParentId :: Maybe Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON CreateSessionInput where
    parseJSON = withObject "CreateSessionInput" $ \v ->
        CreateSessionInput
            <$> v .:? "title"
            <*> v .:? "parentID"

instance ToJSON CreateSessionInput where
    toJSON input =
        object
            [ "title" .= csiTitle input
            , "parentID" .= csiParentId input
            ]


-- ═══════════════════════════════════════════════════════════════════════════
-- // api type definitions //
-- ═══════════════════════════════════════════════════════════════════════════

type SessionStatusAPI = "session" :> "status" :> Get '[JSON] Value

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
    "session" :> Capture "sessionID" Text :> "init" :> Post '[JSON] Value

type SessionForkAPI =
    "session" :> Capture "sessionID" Text :> "fork" :> Post '[JSON] Session

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
        :> Get '[JSON] Value

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
