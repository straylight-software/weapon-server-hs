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
import Test.Fixture (propertyWithTempDir)
import Test.Helpers (listLength)
import Test.Tasty
import Test.Tasty.Hedgehog
import Util.Identifier qualified as Identifier
import Util.StorageKeys (sessionKey)

-- | Create a session context from a temp directory
mkContext :: FilePath -> IO Session.SessionContext
mkContext tmpDir = do
    Storage.withStorage tmpDir $ \storage -> do
        bus <- Bus.newBus
        idGen <- Identifier.newIdGenState
        pure
            Session.SessionContext
                { Session.scStorage = storage
                , Session.scBus = bus
                , Session.scProjectID = "test_project"
                , Session.scDirectory = T.pack tmpDir
                , Session.scVersion = "0.1.0"
                , Session.scIdGen = idGen
                }

-- | Property: create then get returns the session
prop_createGet :: Property
prop_createGet = propertyWithTempDir $ \tmpDir -> do
    title <- forAll $ Gen.maybe $ Gen.text (Range.linear 1 50) Gen.alphaNum
    session <- evalIO $ do
        ctx <- mkContext tmpDir
        Session.create ctx ST.CreateSessionInput{ST.csiTitle = title, ST.csiParentID = Nothing}
    case title of
        Just t -> ST.sessionTitle session === t
        Nothing -> assert $ T.isPrefixOf "New session - " (ST.sessionTitle session)
    ST.sessionProjectID session === "test_project"
    assert $ T.isPrefixOf "ses_" (ST.sessionId session)

-- | Property: get non-existent session returns Nothing
prop_getNonExistent :: Property
prop_getNonExistent = propertyWithTempDir $ \tmpDir -> do
    sid <- forAll $ Gen.text (Range.linear 10 30) Gen.alphaNum
    result <- evalIO $ do
        ctx <- mkContext tmpDir
        Session.get ctx sid
    result === Nothing

-- | Property: delete removes the session
prop_deleteRemoves :: Property
prop_deleteRemoves = propertyWithTempDir $ \tmpDir -> do
    title <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    (created, afterDelete) <- evalIO $ do
        ctx <- mkContext tmpDir
        session <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just title, ST.csiParentID = Nothing}
        _ <- Session.delete ctx (ST.sessionId session)
        afterGet <- Session.get ctx (ST.sessionId session)
        pure (session, afterGet)
    assert $ T.isPrefixOf "ses_" (ST.sessionId created)
    afterDelete === Nothing

-- | Property: list returns created sessions
prop_listReturnsCreated :: Property
prop_listReturnsCreated = propertyWithTempDir $ \tmpDir -> do
    count <- forAll $ Gen.int (Range.linear 1 5)
    sessions <- evalIO $ do
        ctx <- mkContext tmpDir
        replicateM_ count $ Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "test", ST.csiParentID = Nothing}
        Session.list ctx Nothing Nothing Nothing Nothing Nothing
    listLength sessions === count

-- | Property: list contains the created session ID
prop_listContainsCreatedId :: Property
prop_listContainsCreatedId = propertyWithTempDir $ \tmpDir -> do
    (created, sessions) <- evalIO $ do
        ctx <- mkContext tmpDir
        session <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "test", ST.csiParentID = Nothing}
        allSessions <- Session.list ctx Nothing Nothing Nothing Nothing Nothing
        pure (session, allSessions)
    let createdId = ST.sessionId created
    annotate $ "Created ID: " ++ T.unpack createdId
    annotate $ "Listed count: " ++ show (listLength sessions)
    assert $ any (\s -> ST.sessionId s == createdId) sessions

-- | Property: update summary/share/revert
prop_updateSummaryShareRevert :: Property
prop_updateSummaryShareRevert = propertyWithTempDir $ \tmpDir -> do
    msgId <- forAll $ Gen.text (Range.linear 3 20) Gen.alphaNum
    url <- forAll $ Gen.text (Range.linear 3 20) Gen.alphaNum
    (summary, share, revert) <- evalIO $ do
        ctx <- mkContext tmpDir
        session <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "test", ST.csiParentID = Nothing}
        let sid = ST.sessionId session
        let s = ST.SessionSummary 1 2 (Just 3)
        let sh = ST.SessionShare url
        let r = ST.SessionRevert msgId Nothing Nothing Nothing
        _ <- Session.update ctx sid (\sess -> sess{ST.sessionSummary = Just s, ST.sessionShare = Just sh, ST.sessionRevert = Just r})
        updated <- Session.get ctx sid
        case updated of
            Nothing -> fail "session not found"
            Just sess -> pure (ST.sessionSummary sess, ST.sessionShare sess, ST.sessionRevert sess)
    summary === Just (ST.SessionSummary 1 2 (Just 3))
    share === Just (ST.SessionShare url)
    revert === Just (ST.SessionRevert msgId Nothing Nothing Nothing)

-- | Property: list with search filters by title (case-insensitive)
prop_listSearchFilter :: Property
prop_listSearchFilter = propertyWithTempDir $ \tmpDir -> do
    (matching, nonMatching) <- evalIO $ do
        ctx <- mkContext tmpDir
        _ <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "Alpha Project", ST.csiParentID = Nothing}
        _ <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "Beta Project", ST.csiParentID = Nothing}
        _ <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "Gamma Task", ST.csiParentID = Nothing}
        m <- Session.list ctx Nothing Nothing Nothing Nothing (Just "project")
        nm <- Session.list ctx Nothing Nothing Nothing Nothing (Just "delta")
        pure (m, nm)
    listLength matching === 2
    listLength nonMatching === 0

-- | Property: list with limit restricts results
prop_listLimitFilter :: Property
prop_listLimitFilter = propertyWithTempDir $ \tmpDir -> do
    limitVal <- forAll $ Gen.int (Range.linear 1 3)
    sessions <- evalIO $ do
        ctx <- mkContext tmpDir
        replicateM_ 5 $ Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "test", ST.csiParentID = Nothing}
        Session.list ctx Nothing Nothing (Just limitVal) Nothing Nothing
    assert $ listLength sessions <= limitVal

-- | Property: list returns sessions ordered by updated timestamp (descending)
prop_listSortedByUpdated :: Property
prop_listSortedByUpdated = propertyWithTempDir $ \tmpDir -> do
    shuffled <- forAll $ Gen.shuffle [10.0, 20.0, 30.0]
    let times = take 3 shuffled
    listed <- evalIO $ do
        ctx <- mkContext tmpDir
        sessions <- replicateM 3 $ Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "test", ST.csiParentID = Nothing}
        let store = Session.scStorage ctx
        let projectId = Session.scProjectID ctx
        let updatedSessions = zipWith (\s t -> s{ST.sessionTime = ST.SessionTime t t Nothing Nothing}) sessions times
        mapM_ (\s -> Storage.write store (sessionKey projectId (ST.sessionId s)) s) updatedSessions
        Session.list ctx Nothing Nothing Nothing Nothing Nothing
    let listedTimes = map (ST.stUpdated . ST.sessionTime) listed
    listedTimes === List.sortOn Down times

-- | Property: update on nonexistent session returns Nothing
prop_updateNonexistent :: Property
prop_updateNonexistent = propertyWithTempDir $ \tmpDir -> do
    sid <- forAll $ Gen.text (Range.linear 10 30) Gen.alphaNum
    result <- evalIO $ do
        ctx <- mkContext tmpDir
        Session.update ctx sid (\s -> s{ST.sessionTitle = "updated"})
    result === Nothing

-- | Property: delete on nonexistent session returns False
prop_deleteNonexistent :: Property
prop_deleteNonexistent = propertyWithTempDir $ \tmpDir -> do
    sid <- forAll $ Gen.text (Range.linear 10 30) Gen.alphaNum
    result <- evalIO $ do
        ctx <- mkContext tmpDir
        Session.delete ctx sid
    result === False

-- | Property: list with roots=True filters out child sessions
prop_listRootsFilter :: Property
prop_listRootsFilter = propertyWithTempDir $ \tmpDir -> do
    (rootCount, allCount) <- evalIO $ do
        ctx <- mkContext tmpDir
        parent <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "parent", ST.csiParentID = Nothing}
        replicateM_ 2 $ Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "root", ST.csiParentID = Nothing}
        _ <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "child", ST.csiParentID = Just (ST.sessionId parent)}
        roots <- Session.list ctx Nothing (Just True) Nothing Nothing Nothing
        allSessions <- Session.list ctx Nothing Nothing Nothing Nothing Nothing
        pure (listLength roots, listLength allSessions)
    rootCount === 3
    allCount === 4

-- | Property: list with directory filter only returns sessions matching directory
prop_listDirectoryFilter :: Property
prop_listDirectoryFilter = propertyWithTempDir $ \tmpDir -> do
    (matchCount, nonMatchCount, allCount) <- evalIO $ do
        ctx <- mkContext tmpDir
        replicateM_ 3 $ Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "test", ST.csiParentID = Nothing}
        let dir = Session.scDirectory ctx
        matching <- Session.list ctx (Just dir) Nothing Nothing Nothing Nothing
        nonMatching <- Session.list ctx (Just "/nonexistent/path") Nothing Nothing Nothing Nothing
        allSessions <- Session.list ctx Nothing Nothing Nothing Nothing Nothing
        pure (listLength matching, listLength nonMatching, listLength allSessions)
    matchCount === 3
    nonMatchCount === 0
    allCount === 3

-- | Property: touch updates the session timestamp
prop_touchUpdatesTimestamp :: Property
prop_touchUpdatesTimestamp = propertyWithTempDir $ \tmpDir -> do
    (timeBefore, timeAfter) <- evalIO $ do
        ctx <- mkContext tmpDir
        session <- Session.create ctx ST.CreateSessionInput{ST.csiTitle = Just "test", ST.csiParentID = Nothing}
        let sid = ST.sessionId session
        let tb = ST.stUpdated (ST.sessionTime session)
        _ <- Session.touch ctx sid
        updated <- Session.get ctx sid
        case updated of
            Nothing -> fail "session not found"
            Just s -> pure (tb, ST.stUpdated (ST.sessionTime s))
    assert $ timeAfter >= timeBefore

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
        , testProperty "update nonexistent returns Nothing" prop_updateNonexistent
        , testProperty "delete nonexistent returns False" prop_deleteNonexistent
        , testProperty "list with roots filter" prop_listRootsFilter
        , testProperty "list with directory filter" prop_listDirectoryFilter
        , testProperty "touch updates timestamp" prop_touchUpdatesTimestamp
        ]
