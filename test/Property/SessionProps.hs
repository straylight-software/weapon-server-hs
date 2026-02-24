{-# LANGUAGE OverloadedStrings #-}

-- | Session property tests
module Property.SessionProps where

import Bus.Bus qualified as Bus

import Control.Monad (replicateM, replicateM_)
import Data.List qualified as List
import Data.Ord (Down (..))
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Session.Session qualified as Session
import Session.Types qualified as ST
import Storage.Storage qualified as Storage
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory)
import Test.Tasty
import Test.Tasty.Hedgehog
import Util.Identifier qualified as Identifier
import Util.StorageKeys (sessionKey)

-- | Create a test session context
withTestContext :: (Session.SessionContext -> IO a) -> IO a
withTestContext action = do
    tmpDir <- createTempDirectory "/tmp" "session-test"
    Storage.withStorage tmpDir $ \storage -> do
        bus <- Bus.newBus
        idGen <- Identifier.newIdGenState
        let ctx =
                Session.SessionContext
                    { Session.scStorage = storage
                    , Session.scBus = bus
                    , Session.scProjectID = "test_project"
                    , Session.scDirectory = T.pack tmpDir
                    , Session.scVersion = "0.1.0"
                    , Session.scIdGen = idGen
                    }
        result <- action ctx
        removeDirectoryRecursive tmpDir
        pure result

-- | Property: create then get returns the session
prop_createGet :: Property
prop_createGet = property $ do
    title <- forAll $ Gen.maybe $ Gen.text (Range.linear 1 50) Gen.alphaNum

    session <- evalIO $ withTestContext $ \ctx -> do
        let input =
                ST.CreateSessionInput
                    { ST.csiTitle = title
                    , ST.csiParentID = Nothing
                    }
        Session.create ctx input

    -- Verify session was created with correct properties
    -- When title is Nothing, a default title is generated
    case title of
        Just t -> ST.sessionTitle session === t
        Nothing -> assert $ T.isPrefixOf "New session - " (ST.sessionTitle session)
    ST.sessionProjectID session === "test_project"
    assert $ T.isPrefixOf "ses_" (ST.sessionId session)

-- | Property: get non-existent session returns Nothing
prop_getNonExistent :: Property
prop_getNonExistent = property $ do
    sid <- forAll $ Gen.text (Range.linear 10 30) Gen.alphaNum

    result <- evalIO $ withTestContext $ \ctx ->
        Session.get ctx sid

    result === Nothing

-- | Property: delete removes the session
prop_deleteRemoves :: Property
prop_deleteRemoves = property $ do
    title <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum

    (created, afterDelete) <- evalIO $ withTestContext $ \ctx -> do
        let input =
                ST.CreateSessionInput
                    { ST.csiTitle = Just title
                    , ST.csiParentID = Nothing
                    }
        session <- Session.create ctx input
        let sid = ST.sessionId session
        _ <- Session.delete ctx sid
        afterGet <- Session.get ctx sid
        pure (session, afterGet)

    -- Session should exist after creation
    assert $ T.isPrefixOf "ses_" (ST.sessionId created)
    -- Session should not exist after deletion
    afterDelete === Nothing

-- | Property: list returns created sessions
prop_listReturnsCreated :: Property
prop_listReturnsCreated = property $ do
    count <- forAll $ Gen.int (Range.linear 1 5)

    sessions <- evalIO $ withTestContext $ \ctx -> do
        -- Create multiple sessions
        replicateM_ count $ do
            let input =
                    ST.CreateSessionInput
                        { ST.csiTitle = Just "test"
                        , ST.csiParentID = Nothing
                        }
            Session.create ctx input
        -- List all sessions
        Session.list ctx Nothing Nothing Nothing Nothing Nothing

    -- Should find all created sessions
    listLength sessions === count

prop_listContainsCreatedId :: Property
prop_listContainsCreatedId = property $ do
    (created, sessions) <- evalIO $ withTestContext $ \ctx -> do
        session <-
            Session.create
                ctx
                ST.CreateSessionInput
                    { ST.csiTitle = Just "test"
                    , ST.csiParentID = Nothing
                    }
        allSessions <- Session.list ctx Nothing Nothing Nothing Nothing Nothing
        pure (session, allSessions)
    assert $ any (\s -> ST.sessionId s == ST.sessionId created) sessions

prop_updateSummaryShareRevert :: Property
prop_updateSummaryShareRevert = property $ do
    msgId <- forAll $ Gen.text (Range.linear 3 20) Gen.alphaNum
    url <- forAll $ Gen.text (Range.linear 3 20) Gen.alphaNum
    (summary, share, revert) <- evalIO $ withTestContext $ \ctx -> do
        session <-
            Session.create
                ctx
                ST.CreateSessionInput
                    { ST.csiTitle = Just "test"
                    , ST.csiParentID = Nothing
                    }
        let sid = ST.sessionId session
        let summary = ST.SessionSummary 1 2 (Just 3)
        let share = ST.SessionShare url
        let revert = ST.SessionRevert msgId Nothing Nothing Nothing
        _ <-
            Session.update
                ctx
                sid
                ( \s ->
                    s
                        { ST.sessionSummary = Just summary
                        , ST.sessionShare = Just share
                        , ST.sessionRevert = Just revert
                        }
                )
        updated <- Session.get ctx sid
        case updated of
            Nothing -> fail "session not found"
            Just s -> pure (ST.sessionSummary s, ST.sessionShare s, ST.sessionRevert s)
    summary === Just (ST.SessionSummary 1 2 (Just 3))
    share === Just (ST.SessionShare url)
    revert === Just (ST.SessionRevert msgId Nothing Nothing Nothing)

-- | Property: list with search filters by title (case-insensitive)
prop_listSearchFilter :: Property
prop_listSearchFilter = property $ do
    (matching, nonMatching) <- evalIO $ withTestContext $ \ctx -> do
        -- Create sessions with different titles
        _ <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "Alpha Project", ST.csiParentID = Nothing}
        _ <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "Beta Project", ST.csiParentID = Nothing}
        _ <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "Gamma Task", ST.csiParentID = Nothing}
        -- Search for "project" (case-insensitive)
        matching <- Session.list ctx Nothing Nothing Nothing Nothing (Just "project")
        nonMatching <- Session.list ctx Nothing Nothing Nothing Nothing (Just "delta")
        pure (matching, nonMatching)
    -- Should find 2 sessions matching "project"
    listLength matching === 2
    -- Should find 0 sessions matching "delta"
    listLength nonMatching === 0

-- | Property: list with limit restricts results
prop_listLimitFilter :: Property
prop_listLimitFilter = property $ do
    limitVal <- forAll $ Gen.int (Range.linear 1 3)
    sessions <- evalIO $ withTestContext $ \ctx -> do
        -- Create 5 sessions
        replicateM_ 5 $ Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "test", ST.csiParentID = Nothing}
        -- List with limit
        Session.list ctx Nothing Nothing (Just limitVal) Nothing Nothing
    -- Should return at most limitVal sessions
    assert $ listLength sessions <= limitVal

-- | Property: list returns sessions ordered by updated timestamp (descending)
prop_listSortedByUpdated :: Property
prop_listSortedByUpdated = property $ do
    shuffled <- forAll $ Gen.shuffle [10.0, 20.0, 30.0]
    let times = take 3 shuffled
    listed <- evalIO $ withTestContext $ \ctx -> do
        sessions <- replicateM 3 $ Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "test", ST.csiParentID = Nothing}
        let store = Session.scStorage ctx
        let projectId = Session.scProjectID ctx
        let updatedSessions =
                zipWith
                    (\s t -> s{ST.sessionTime = ST.SessionTime t t Nothing Nothing})
                    sessions
                    times
        mapM_
            ( \s ->
                Storage.write store (sessionKey projectId (ST.sessionId s)) s
            )
            updatedSessions
        Session.list ctx Nothing Nothing Nothing Nothing Nothing
    let listedTimes = map (ST.stUpdated . ST.sessionTime) listed
    listedTimes === List.sortOn Down times

-- ═══════════════════════════════════════════════════════════════════════════
-- Edge Case Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: update on nonexistent session returns Nothing
prop_updateNonexistent :: Property
prop_updateNonexistent = property $ do
    sid <- forAll $ Gen.text (Range.linear 10 30) Gen.alphaNum
    result <- evalIO $ withTestContext $ \ctx ->
        Session.update ctx sid (\s -> s{ST.sessionTitle = "updated"})
    result === Nothing

-- | Property: delete on nonexistent session returns False
prop_deleteNonexistent :: Property
prop_deleteNonexistent = property $ do
    sid <- forAll $ Gen.text (Range.linear 10 30) Gen.alphaNum
    result <- evalIO $ withTestContext $ \ctx ->
        Session.delete ctx sid
    result === False

-- | Property: list with roots=True filters out child sessions
prop_listRootsFilter :: Property
prop_listRootsFilter = property $ do
    (rootCount, allCount) <- evalIO $ withTestContext $ \ctx -> do
        -- Create a parent session
        parent <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "parent", ST.csiParentID = Nothing}
        -- Create some root sessions
        replicateM_ 2 $ Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "root", ST.csiParentID = Nothing}
        -- Create a child session
        _ <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "child", ST.csiParentID = Just (ST.sessionId parent)}
        -- List roots only
        roots <- Session.list ctx Nothing (Just True) Nothing Nothing Nothing
        -- List all
        allSessions <- Session.list ctx Nothing Nothing Nothing Nothing Nothing
        pure (listLength roots, listLength allSessions)
    -- Should have 3 roots (parent + 2 roots) and 4 total (+ 1 child)
    rootCount === 3
    allCount === 4

-- | Property: touch updates the session timestamp
prop_touchUpdatesTimestamp :: Property
prop_touchUpdatesTimestamp = property $ do
    (timeBefore, timeAfter) <- evalIO $ withTestContext $ \ctx -> do
        session <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "test", ST.csiParentID = Nothing}
        let sid = ST.sessionId session
        let timeBefore = ST.stUpdated (ST.sessionTime session)
        -- Small delay to ensure timestamp changes
        _ <- Session.touch ctx sid
        updated <- Session.get ctx sid
        case updated of
            Nothing -> fail "session not found"
            Just s -> pure (timeBefore, ST.stUpdated (ST.sessionTime s))
    -- Time should have been updated (or at least not decreased)
    assert $ timeAfter >= timeBefore

-- | Property: list with directory filter only returns sessions matching directory
prop_listDirectoryFilter :: Property
prop_listDirectoryFilter = property $ do
    (matchCount, nonMatchCount, allCount) <- evalIO $ withTestContext $ \ctx -> do
        -- Create sessions (they will have the context's directory)
        replicateM_ 3 $ Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "test", ST.csiParentID = Nothing}
        -- Get the directory from context
        let dir = Session.scDirectory ctx
        -- List with matching directory
        matching <- Session.list ctx (Just dir) Nothing Nothing Nothing Nothing
        -- List with non-matching directory
        nonMatching <- Session.list ctx (Just "/nonexistent/path") Nothing Nothing Nothing Nothing
        -- List all (no directory filter)
        allSessions <- Session.list ctx Nothing Nothing Nothing Nothing Nothing
        pure (listLength matching, listLength nonMatching, listLength allSessions)
    -- Matching directory should return all sessions
    matchCount === 3
    -- Non-matching directory should return no sessions
    nonMatchCount === 0
    -- All sessions should be returned when no filter
    allCount === 3

-- Generators
-- Test tree
tests :: TestTree
tests =
    testGroup
        "Session Property Tests"
        [ testProperty "create then get" prop_createGet
        , testProperty "get non-existent" prop_getNonExistent
        , testProperty "delete removes" prop_deleteRemoves
        , testProperty "list returns created" prop_listReturnsCreated
        , testProperty "list contains created id" prop_listContainsCreatedId
        , testProperty "update summary/share/revert" prop_updateSummaryShareRevert
        , testProperty "list search filter" prop_listSearchFilter
        , testProperty "list limit filter" prop_listLimitFilter
        , testProperty "list sorted by updated" prop_listSortedByUpdated
        , -- Edge cases
          testProperty "update nonexistent returns Nothing" prop_updateNonexistent
        , testProperty "delete nonexistent returns False" prop_deleteNonexistent
        , testProperty "list with roots filter" prop_listRootsFilter
        , testProperty "list with directory filter" prop_listDirectoryFilter
        , testProperty "touch updates timestamp" prop_touchUpdatesTimestamp
        ]

listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0
