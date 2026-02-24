{-# LANGUAGE OverloadedStrings #-}

{- | Session module - CRUD operations
Mirrors the TypeScript Session namespace
-}
module Session.Session (
    -- * Types
    Session.Types.Session (..),
    Session.Types.SessionTime (..),
    Session.Types.CreateSessionInput (..),
    Session.Types.GlobalSession (..),
    Session.Types.ProjectSummary (..),

    -- * Operations
    create,
    get,
    update,
    delete,
    list,
    listGlobal,
    touch,

    -- * Context
    SessionContext (..),
    withSessionContext,
) where

import Control.Monad (forM)
import Data.Aeson (object, (.=))
import Data.List (sortOn)
import Data.List qualified as List
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import System.Random (randomIO)

import Bus.Bus qualified as Bus
import Session.Types
import Storage.Storage qualified as Storage
import Util.Identifier qualified as Identifier
import Util.StorageKeys (sessionKey, sessionPrefix)

-- | Context for session operations
data SessionContext = SessionContext
    { scStorage :: Storage.StorageConfig
    , scBus :: Bus.Bus
    , scProjectID :: Text
    , scDirectory :: Text
    , scVersion :: Text
    , scIdGen :: Identifier.IdGenState
    }

-- | Run operations with a session context
withSessionContext :: Storage.StorageConfig -> Bus.Bus -> Text -> Text -> Text -> Identifier.IdGenState -> (SessionContext -> IO a) -> IO a
withSessionContext storage bus projectID directory version idGen action =
    action (SessionContext storage bus projectID directory version idGen)

{- | Generate a unique session ID (descending for sorted listing)
Uses monotonic counter for sub-millisecond ordering
-}
generateSessionID :: Identifier.IdGenState -> IO Text
generateSessionID idGen = Identifier.descendingWithPrefix idGen "ses"

-- | Generate a random slug (12 hex characters)
generateSlug :: IO Text
generateSlug = do
    w1 <- randomIO :: IO Word64
    w2 <- randomIO :: IO Word64
    -- Use base62 encoding for shorter, URL-safe slugs
    pure $ T.take 12 $ T.pack $ Identifier.encodeTimeBytes (w1 `mod` 0xFFFFFFFFFFFF) <> Identifier.encodeTimeBytes (w2 `mod` 0xFFFFFFFFFFFF)

-- | Get current timestamp in milliseconds
nowMs :: IO Double
nowMs = do
    now <- getCurrentTime
    pure $ realToFrac (utcTimeToPOSIXSeconds now) * 1000

-- | Create a new session
create :: SessionContext -> CreateSessionInput -> IO Session
create ctx input = do
    sid <- generateSessionID (scIdGen ctx)
    slug <- generateSlug
    now <- nowMs

    let session =
            Session
                { sessionId = sid
                , sessionSlug = slug
                , sessionProjectID = scProjectID ctx
                , sessionDirectory = scDirectory ctx
                , sessionParentID = csiParentID input
                , sessionTitle = fromMaybe (defaultTitle now) (csiTitle input)
                , sessionVersion = scVersion ctx
                , sessionTime = SessionTime now now Nothing Nothing
                , sessionSummary = Nothing
                , sessionShare = Nothing
                , sessionRevert = Nothing
                }

    -- Write to storage
    Storage.write (scStorage ctx) (sessionKey (scProjectID ctx) sid) session

    -- Publish event
    Bus.publish (scBus ctx) "session.created" (object ["info" .= session])

    pure session
  where
    defaultTitle now = "New session - " <> T.pack (show (round now :: Integer))

-- | Get a session by ID
get :: SessionContext -> Text -> IO (Maybe Session)
get ctx sid = Storage.readMaybe (scStorage ctx) (sessionKey (scProjectID ctx) sid)

-- | Update a session
update :: SessionContext -> Text -> (Session -> Session) -> IO (Maybe Session)
update ctx sid fn = do
    msession <- get ctx sid
    case msession of
        Nothing -> pure Nothing
        Just session -> do
            now <- nowMs
            let updated = fn session{sessionTime = (sessionTime session){stUpdated = now}}
            Storage.write (scStorage ctx) (sessionKey (scProjectID ctx) sid) updated
            Bus.publish (scBus ctx) "session.updated" (object ["info" .= updated])
            pure (Just updated)

-- | Delete a session
delete :: SessionContext -> Text -> IO Bool
delete ctx sid = do
    msession <- get ctx sid
    case msession of
        Nothing -> pure False
        Just session -> do
            Storage.remove (scStorage ctx) (sessionKey (scProjectID ctx) sid)
            Bus.publish (scBus ctx) "session.deleted" (object ["info" .= session])
            pure True

{- | List all sessions for the current project
Parameters:
  mDir: Filter by directory (must match session's directory)
  mRoots: If True, only return sessions without a parent
  mLimit: Maximum number of sessions to return
  mStart: Filter sessions updated on or after this timestamp
  mSearch: Filter by title (case-insensitive)
-}
list ::
    SessionContext ->
    Maybe Text -> -- directory filter
    Maybe Bool -> -- roots only
    Maybe Int -> -- limit
    Maybe Double -> -- start timestamp
    Maybe Text -> -- search
    IO [Session]
list ctx mDir mRoots mLimit mStart mSearch = do
    keys <- Storage.list (scStorage ctx) (sessionPrefix (scProjectID ctx))
    sessions <- forM keys $ \key -> do
        case List.unsnoc key of
            Nothing -> pure Nothing
            Just (_prefix, sid) -> get ctx sid
    let valid = catMaybes sessions
    -- Filter by directory if specified
    let dirFiltered = case mDir of
            Just dir -> filter (\s -> sessionDirectory s == dir) valid
            Nothing -> valid
    -- Filter by roots (no parent)
    let rootFiltered = case mRoots of
            Just True -> filter (isNothing . sessionParentID) dirFiltered
            Just False -> dirFiltered
            Nothing -> dirFiltered
    -- Filter by start timestamp (sessions updated on or after)
    let startFiltered = case mStart of
            Just ts -> filter (\s -> stUpdated (sessionTime s) >= ts) rootFiltered
            Nothing -> rootFiltered
    -- Filter by search (case-insensitive title match)
    let searchFiltered = case mSearch of
            Just q -> filter (\s -> T.toLower q `T.isInfixOf` T.toLower (sessionTitle s)) startFiltered
            Nothing -> startFiltered
    -- Sort by most recently updated for deterministic ordering
    let sorted = sortOn (Down . stUpdated . sessionTime) searchFiltered
    -- Apply limit
    let limited = case mLimit of
            Just n -> take n sorted
            Nothing -> sorted
    pure limited

-- | Touch a session (update timestamp)
touch :: SessionContext -> Text -> IO ()
touch ctx sid = do
    _ <- update ctx sid id
    pure ()

{- | List sessions globally (for /experimental/session endpoint)
This is similar to `list` but returns GlobalSession with project info
and supports additional filtering (cursor, archived)
-}
listGlobal ::
    SessionContext ->
    Maybe Text -> -- directory filter
    Maybe Bool -> -- roots only
    Maybe Double -> -- start timestamp
    Maybe Double -> -- cursor (sessions before this timestamp)
    Maybe Text -> -- search
    Maybe Int -> -- limit
    Maybe Bool -> -- include archived
    IO [GlobalSession]
listGlobal ctx mDir mRoots mStart mCursor mSearch mLimit mArchived = do
    -- Get all sessions for the project
    keys <- Storage.list (scStorage ctx) (sessionPrefix (scProjectID ctx))
    sessions <- forM keys $ \key -> do
        case List.unsnoc key of
            Nothing -> pure Nothing
            Just (_prefix, sid) -> get ctx sid
    let valid = catMaybes sessions

    -- Filter by directory if specified
    let dirFiltered = case mDir of
            Just dir -> filter (\s -> sessionDirectory s == dir) valid
            Nothing -> valid

    -- Filter by roots (no parent)
    let rootFiltered = case mRoots of
            Just True -> filter (isNothing . sessionParentID) dirFiltered
            Just False -> dirFiltered
            Nothing -> dirFiltered

    -- Filter by start timestamp (sessions updated on or after)
    let startFiltered = case mStart of
            Just ts -> filter (\s -> stUpdated (sessionTime s) >= ts) rootFiltered
            Nothing -> rootFiltered

    -- Filter by cursor (sessions updated before this timestamp)
    let cursorFiltered = case mCursor of
            Just ts -> filter (\s -> stUpdated (sessionTime s) < ts) startFiltered
            Nothing -> startFiltered

    -- Filter by archived status (default: exclude archived)
    let archivedFiltered = case mArchived of
            Just True -> cursorFiltered -- Include all (archived and non-archived)
            Just False -> filter (isNothing . stArchived . sessionTime) cursorFiltered
            Nothing -> filter (isNothing . stArchived . sessionTime) cursorFiltered

    -- Filter by search (case-insensitive title match)
    let searchFiltered = case mSearch of
            Just q -> filter (\s -> T.toLower q `T.isInfixOf` T.toLower (sessionTitle s)) archivedFiltered
            Nothing -> archivedFiltered

    -- Sort by most recently updated
    let sorted = sortOn (Down . stUpdated . sessionTime) searchFiltered

    -- Apply limit (default 100)
    let limit = fromMaybe 100 mLimit
    let limited = take limit sorted

    -- Convert to GlobalSession with project info
    -- For now, we create a simple ProjectSummary from the session's project info
    let toGlobal s =
            let projSummary =
                    Just
                        ProjectSummary
                            { psId = sessionProjectID s
                            , psName = Nothing -- We don't have project name in storage yet
                            , psWorktree = sessionDirectory s
                            }
             in toGlobalSession s projSummary

    pure $ map toGlobal limited
