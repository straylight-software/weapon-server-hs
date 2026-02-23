{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Session type definitions
Mirrors the TypeScript Session.Info schema
-}
module Session.Types (
    Session (..),
    SessionTime (..),
    SessionSummary (..),
    SessionShare (..),
    SessionRevert (..),
    CreateSessionInput (..),

    -- * Global session types (for /experimental/session)
    GlobalSession (..),
    ProjectSummary (..),
    toGlobalSession,
) where

import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Session time information
data SessionTime = SessionTime
    { stCreated :: Double
    , stUpdated :: Double
    , stCompacting :: Maybe Double
    , stArchived :: Maybe Double
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

-- | Session summary (diff stats)
data SessionSummary = SessionSummary
    { ssAdditions :: Int
    , ssDeletions :: Int
    , ssFiles :: Maybe Int
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

-- | Session share info
newtype SessionShare = SessionShare
    { shareUrl :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON SessionShare where
    toJSON ss = object ["url" .= shareUrl ss]

instance FromJSON SessionShare where
    parseJSON = withObject "SessionShare" $ \v ->
        SessionShare
            <$> v .: "url"

-- | Session revert state
data SessionRevert = SessionRevert
    { revertMessageID :: Text
    , revertPartID :: Maybe Text
    , revertSnapshot :: Maybe Text
    , revertDiff :: Maybe Text
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

-- | Full session info
data Session = Session
    { sessionId :: Text
    , sessionSlug :: Text
    , sessionProjectID :: Text
    , sessionDirectory :: Text
    , sessionParentID :: Maybe Text
    , sessionTitle :: Text
    , sessionVersion :: Text
    , sessionTime :: SessionTime
    , sessionSummary :: Maybe SessionSummary
    , sessionShare :: Maybe SessionShare
    , sessionRevert :: Maybe SessionRevert
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

-- | Input for creating a session
data CreateSessionInput = CreateSessionInput
    { csiTitle :: Maybe Text
    , csiParentID :: Maybe Text
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

-- | Project summary info included in GlobalSession
data ProjectSummary = ProjectSummary
    { psId :: Text
    , psName :: Maybe Text
    , psWorktree :: Text
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

{- | Global session info (Session + project summary)
Used by /experimental/session endpoint for cross-project session listing
-}
data GlobalSession = GlobalSession
    { gsId :: Text
    , gsSlug :: Text
    , gsProjectID :: Text
    , gsDirectory :: Text
    , gsParentID :: Maybe Text
    , gsTitle :: Text
    , gsVersion :: Text
    , gsTime :: SessionTime
    , gsSummary :: Maybe SessionSummary
    , gsShare :: Maybe SessionShare
    , gsRevert :: Maybe SessionRevert
    , gsProject :: Maybe ProjectSummary
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

-- | Convert a Session to GlobalSession with optional project info
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
