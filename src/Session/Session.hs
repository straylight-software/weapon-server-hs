{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Session.Session
Description : CRUD operations for AI agent sessions

This module provides the primary API for managing sessions in the AI coding agent.
It mirrors the TypeScript Session namespace from the reference implementation.

= Session Lifecycle

1. 'create' - Create a new session with optional title and parent
2. 'get' - Retrieve a session by ID
3. 'update' - Modify session fields (automatically updates timestamp)
4. 'delete' - Remove a session from storage
5. 'list' - Query sessions with filtering and pagination
6. 'touch' - Update the session's timestamp without other changes

= Architecture

All operations require a 'SessionContext' which provides:

* Storage configuration for persistence
* Event bus for publishing session events
* Project and directory context
* ID generator for creating unique session IDs

= Events

Session mutations publish events to the bus:

* @session.created@ - When a new session is created
* @session.updated@ - When a session is modified
* @session.deleted@ - When a session is removed
-}
module Session.Session (
    -- * Types
    Session.Types.Session (..),
    Session.Types.SessionTime (..),
    Session.Types.CreateSessionInput (..),
    Session.Types.GlobalSession (..),
    Session.Types.ProjectSummary (..),

    -- * CRUD Operations
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

    -- * Pure Filtering (for testing)
    SessionFilter (..),
    defaultFilter,
    applyFilters,
    applySortAndLimit,
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

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Filtering Types and Functions
-- ═══════════════════════════════════════════════════════════════════════════

{- | Filter parameters for session queries.

This type captures all the filtering options available when listing sessions.
Using a record type makes it easy to add new filters without breaking
existing code.

All filters are optional (Nothing means "don't filter on this field").
-}
data SessionFilter = SessionFilter
    { sfDirectory :: Maybe Text
    -- ^ Filter by exact directory match
    , sfRootsOnly :: Maybe Bool
    -- ^ If @Just True@, only return sessions without a parent
    , sfStartTime :: Maybe Double
    -- ^ Filter sessions updated on or after this timestamp
    , sfCursorTime :: Maybe Double
    -- ^ Filter sessions updated before this timestamp (for pagination)
    , sfSearch :: Maybe Text
    -- ^ Case-insensitive title search
    , sfIncludeArchived :: Maybe Bool
    -- ^ If @Just True@, include archived sessions (default: exclude)
    }
    deriving (Show, Eq)

-- | Default filter with no restrictions.
defaultFilter :: SessionFilter
defaultFilter =
    SessionFilter
        { sfDirectory = Nothing
        , sfRootsOnly = Nothing
        , sfStartTime = Nothing
        , sfCursorTime = Nothing
        , sfSearch = Nothing
        , sfIncludeArchived = Nothing
        }

{- | Apply all filters to a list of sessions.

This is a pure function that applies each filter predicate in sequence.
Filters that are @Nothing@ are skipped (no filtering on that field).

The order of filter application doesn't affect the result since all
filters are conjunctive (AND).

==== __Example__

@
let sessions = [session1, session2, session3]
    filter = defaultFilter { sfRootsOnly = Just True, sfSearch = Just "refactor" }
    filtered = applyFilters filter sessions
@
-}
applyFilters :: SessionFilter -> [Session] -> [Session]
applyFilters sf = applyArchived . applySearch . applyCursor . applyStart . applyRoots . applyDir
  where
    applyDir = case sfDirectory sf of
        Just dir -> filter (\s -> sessionDirectory s == dir)
        Nothing -> id

    applyRoots = case sfRootsOnly sf of
        Just True -> filter (isNothing . sessionParentID)
        Just False -> id
        Nothing -> id

    applyStart = case sfStartTime sf of
        Just ts -> filter (\s -> stUpdated (sessionTime s) >= ts)
        Nothing -> id

    applyCursor = case sfCursorTime sf of
        Just ts -> filter (\s -> stUpdated (sessionTime s) < ts)
        Nothing -> id

    applySearch = case sfSearch sf of
        Just q -> filter (\s -> T.toLower q `T.isInfixOf` T.toLower (sessionTitle s))
        Nothing -> id

    applyArchived = case sfIncludeArchived sf of
        Just True -> id -- Include all
        Just False -> filter (isNothing . stArchived . sessionTime) -- Exclude archived
        Nothing -> filter (isNothing . stArchived . sessionTime) -- Exclude archived

{- | Sort sessions by updated time (descending) and apply limit.

This is a pure function that first sorts sessions by their updated timestamp
in descending order (most recent first), then takes at most @limit@ sessions.

If @limit@ is @Nothing@, all sessions are returned.

==== __Example__

@
let sorted = applySortAndLimit (Just 10) sessions
-- Returns at most 10 sessions, most recently updated first
@
-}
applySortAndLimit :: Maybe Int -> [Session] -> [Session]
applySortAndLimit mLimit sessions =
    let sorted = sortOn (Down . stUpdated . sessionTime) sessions
     in case mLimit of
            Just n -> take n sorted
            Nothing -> sorted

-- ═══════════════════════════════════════════════════════════════════════════
-- Context and Configuration
-- ═══════════════════════════════════════════════════════════════════════════

{- | Context required for session operations.

This bundles all dependencies needed by session CRUD operations:

* Storage for persistence
* Event bus for notifications
* Project context (ID, directory, version)
* ID generator for creating unique session IDs

Create a context using 'withSessionContext' or construct directly.
-}
data SessionContext = SessionContext
    { scStorage :: Storage.StorageConfig
    -- ^ Storage configuration for reading/writing sessions
    , scBus :: Bus.Bus
    -- ^ Event bus for publishing session events
    , scProjectID :: Text
    -- ^ Current project identifier
    , scDirectory :: Text
    -- ^ Working directory for new sessions
    , scVersion :: Text
    -- ^ Server version string
    , scIdGen :: Identifier.IdGenState
    -- ^ ID generator state for creating unique IDs
    }

{- | Run operations with a session context.

This is a convenience function that constructs a 'SessionContext' and
passes it to the provided action. Useful for bracketing session operations.

==== __Example__

@
withSessionContext storage bus "proj_123" "/home/user/project" "1.0.0" idGen $ \\ctx -> do
    session <- create ctx CreateSessionInput { csiTitle = Just "My Session", csiParentID = Nothing }
    -- ... use session
@
-}
withSessionContext :: Storage.StorageConfig -> Bus.Bus -> Text -> Text -> Text -> Identifier.IdGenState -> (SessionContext -> IO a) -> IO a
withSessionContext storage bus projectID directory version idGen action =
    action (SessionContext storage bus projectID directory version idGen)

-- ═══════════════════════════════════════════════════════════════════════════
-- Internal Helpers
-- ═══════════════════════════════════════════════════════════════════════════

{- | Generate a unique session ID (descending for sorted listing).

Uses a monotonic counter for sub-millisecond ordering to ensure IDs are
unique even when multiple sessions are created in rapid succession.
The descending format means newer sessions sort first alphabetically.
-}
generateSessionID :: Identifier.IdGenState -> IO Text
generateSessionID idGen = Identifier.descendingWithPrefix idGen "ses"

{- | Generate a random slug (12 characters).

Creates a URL-safe, human-readable identifier using base62 encoding.
The slug is shorter and more memorable than the full session ID.
-}
generateSlug :: IO Text
generateSlug = do
    w1 <- randomIO :: IO Word64
    w2 <- randomIO :: IO Word64
    -- Use base62 encoding for shorter, URL-safe slugs
    pure $ T.take 12 $ T.pack $ Identifier.encodeTimeBytes (w1 `mod` 0xFFFFFFFFFFFF) <> Identifier.encodeTimeBytes (w2 `mod` 0xFFFFFFFFFFFF)

{- | Get current timestamp in milliseconds since Unix epoch.

This matches JavaScript's @Date.now()@ for API compatibility.
-}
nowMs :: IO Double
nowMs = do
    now <- getCurrentTime
    pure $ realToFrac (utcTimeToPOSIXSeconds now) * 1000

{- | Load all sessions from storage for the current project.

This is the IO portion of session listing, separated from the pure
filtering logic for testability.
-}
loadAllSessions :: SessionContext -> IO [Session]
loadAllSessions ctx = do
    keys <- Storage.list (scStorage ctx) (sessionPrefix (scProjectID ctx))
    sessions <- forM keys $ \key -> do
        case List.unsnoc key of
            Nothing -> pure Nothing
            Just (_prefix, sid) -> get ctx sid
    pure $ catMaybes sessions

-- ═══════════════════════════════════════════════════════════════════════════
-- CRUD Operations
-- ═══════════════════════════════════════════════════════════════════════════

{- | Create a new session.

Generates a unique session ID and slug, initializes time tracking,
persists to storage, and publishes a @session.created@ event.

If no title is provided in the input, a default title with the current
timestamp is generated.

==== __Example__

@
session <- create ctx CreateSessionInput
    { csiTitle = Just "Implement feature X"
    , csiParentID = Nothing
    }
@
-}
create :: SessionContext -> CreateSessionInput -> IO Session
create ctx input = do
    sid <- generateSessionID (scIdGen ctx)
    slug <- generateSlug
    now <- nowMs

    let session = buildSession ctx input sid slug now

    -- Write to storage
    Storage.write (scStorage ctx) (sessionKey (scProjectID ctx) sid) session

    -- Publish event
    Bus.publish (scBus ctx) "session.created" (object ["info" .= session])

    pure session

{- | Build a session record from context and input (pure).

This separates the pure session construction from IO operations
like ID generation and storage.
-}
buildSession :: SessionContext -> CreateSessionInput -> Text -> Text -> Double -> Session
buildSession ctx input sid slug now =
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
  where
    defaultTitle ts = "New session - " <> T.pack (show (round ts :: Integer))

{- | Get a session by ID.

Returns @Nothing@ if the session doesn't exist.

==== __Example__

@
mSession <- get ctx "ses_abc123"
case mSession of
    Just session -> putStrLn $ "Found: " <> sessionTitle session
    Nothing -> putStrLn "Session not found"
@
-}
get :: SessionContext -> Text -> IO (Maybe Session)
get ctx sid = Storage.readMaybe (scStorage ctx) (sessionKey (scProjectID ctx) sid)

{- | Update a session with a transformation function.

The transformation is applied to the session after updating the
@stUpdated@ timestamp. Returns @Nothing@ if the session doesn't exist.

Publishes a @session.updated@ event on success.

==== __Example__

@
-- Update session title
result <- update ctx "ses_abc123" $ \\s -> s { sessionTitle = "New title" }

-- Add a summary
result <- update ctx "ses_abc123" $ \\s ->
    s { sessionSummary = Just (SessionSummary 10 5 (Just 3)) }
@
-}
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

{- | Delete a session.

Removes the session from storage and publishes a @session.deleted@ event.
Returns @True@ if the session was deleted, @False@ if it didn't exist.

==== __Example__

@
deleted <- delete ctx "ses_abc123"
when deleted $ putStrLn "Session deleted"
@
-}
delete :: SessionContext -> Text -> IO Bool
delete ctx sid = do
    msession <- get ctx sid
    case msession of
        Nothing -> pure False
        Just session -> do
            Storage.remove (scStorage ctx) (sessionKey (scProjectID ctx) sid)
            Bus.publish (scBus ctx) "session.deleted" (object ["info" .= session])
            pure True

{- | List all sessions for the current project with filtering.

This function loads all sessions from storage, applies the specified
filters, sorts by updated time (descending), and applies the limit.

The filtering and sorting logic is implemented in pure functions
('applyFilters', 'applySortAndLimit') for testability.

==== Parameters

* @mDir@ - Filter by exact directory match
* @mRoots@ - If @Just True@, only return root sessions (no parent)
* @mLimit@ - Maximum number of sessions to return
* @mStart@ - Filter sessions updated on or after this timestamp
* @mSearch@ - Case-insensitive title search

==== __Example__

@
-- Get latest 10 root sessions matching "refactor"
sessions <- list ctx Nothing (Just True) (Just 10) Nothing (Just "refactor")
@
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
    allSessions <- loadAllSessions ctx
    let sessionFilter =
            defaultFilter
                { sfDirectory = mDir
                , sfRootsOnly = mRoots
                , sfStartTime = mStart
                , sfSearch = mSearch
                , sfIncludeArchived = Just True -- list doesn't filter archived by default
                }
    pure $ applySortAndLimit mLimit $ applyFilters sessionFilter allSessions

{- | Touch a session (update its timestamp).

This is equivalent to calling 'update' with an identity function.
Useful for marking a session as recently accessed without changing
any other fields.
-}
touch :: SessionContext -> Text -> IO ()
touch ctx sid = do
    _ <- update ctx sid id
    pure ()

{- | List sessions globally with project info embedded.

This is similar to 'list' but returns 'GlobalSession' with embedded
project information. It's designed for the @\/experimental\/session@
endpoint which lists sessions across all projects.

Additional features compared to 'list':

* Cursor-based pagination via @mCursor@ (sessions updated before timestamp)
* Archived session filtering (default: exclude archived)
* Embedded project summary in each result
* Default limit of 100 if not specified

==== Parameters

* @mDir@ - Filter by exact directory match
* @mRoots@ - If @Just True@, only return root sessions
* @mStart@ - Filter sessions updated on or after this timestamp
* @mCursor@ - Filter sessions updated before this timestamp (pagination)
* @mSearch@ - Case-insensitive title search
* @mLimit@ - Maximum results (default: 100)
* @mArchived@ - If @Just True@, include archived sessions

==== __Example__

@
-- Paginate through sessions, 20 at a time
page1 <- listGlobal ctx Nothing Nothing Nothing Nothing Nothing (Just 20) Nothing
let lastTime = stUpdated . gsTime $ last page1
page2 <- listGlobal ctx Nothing Nothing Nothing (Just lastTime) Nothing (Just 20) Nothing
@
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
    allSessions <- loadAllSessions ctx
    let sessionFilter =
            defaultFilter
                { sfDirectory = mDir
                , sfRootsOnly = mRoots
                , sfStartTime = mStart
                , sfCursorTime = mCursor
                , sfSearch = mSearch
                , sfIncludeArchived = mArchived
                }
    let filtered = applyFilters sessionFilter allSessions
    let limited = applySortAndLimit (Just $ fromMaybe 100 mLimit) filtered
    pure $ map toGlobalWithProject limited
  where
    -- Convert a Session to GlobalSession with embedded project info
    toGlobalWithProject s =
        let projSummary =
                Just
                    ProjectSummary
                        { psId = sessionProjectID s
                        , psName = Nothing -- Project name not in storage yet
                        , psWorktree = sessionDirectory s
                        }
         in toGlobalSession s projSummary
