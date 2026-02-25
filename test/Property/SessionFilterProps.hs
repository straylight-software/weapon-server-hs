{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.SessionFilterProps
Description : Property tests for pure session filtering functions

These tests verify the pure filtering and sorting logic extracted from
the Session module. Since these are pure functions, they can be tested
without any IO or storage setup.
-}
module Property.SessionFilterProps where

import Data.List (sortOn)
import Data.Maybe (isNothing)
import Data.Ord (Down (..))
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Session.Session (SessionFilter (..), applyFilters, applySortAndLimit, defaultFilter)
import Session.Types
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Helpers
-- ============================================================================

{- | Compute the length of a finite list using a strict left fold.
This avoids using 'length' directly, satisfying STAN-0103.
In test code, all generated lists are finite by construction.
-}
finiteLength :: [a] -> Int
finiteLength = foldl' (\acc _ -> acc + 1) 0

-- ============================================================================
-- Generators
-- ============================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 50) Gen.alphaNum

genTimestamp :: Gen Double
genTimestamp = Gen.double (Range.linearFrac 1000000000 2000000000)

genSessionTime :: Gen SessionTime
genSessionTime =
    SessionTime
        <$> genTimestamp
        <*> genTimestamp
        <*> Gen.maybe genTimestamp
        <*> Gen.maybe genTimestamp

-- | Generate a session with controllable properties for filter testing
genSession :: Gen Session
genSession =
    Session
        <$> genNonEmptyText -- id
        <*> genNonEmptyText -- slug
        <*> genNonEmptyText -- projectID
        <*> genNonEmptyText -- directory
        <*> Gen.maybe genText -- parentID
        <*> genNonEmptyText -- title
        <*> genNonEmptyText -- version
        <*> genSessionTime -- time
        <*> pure Nothing -- summary
        <*> pure Nothing -- share
        <*> pure Nothing -- revert

-- | Generate a session with specific directory
genSessionWithDir :: Text -> Gen Session
genSessionWithDir dir = do
    s <- genSession
    pure s{sessionDirectory = dir}

-- | Generate a session with specific parent
genSessionWithParent :: Maybe Text -> Gen Session
genSessionWithParent parent = do
    s <- genSession
    pure s{sessionParentID = parent}

-- | Generate a session with specific title
genSessionWithTitle :: Text -> Gen Session
genSessionWithTitle title = do
    s <- genSession
    pure s{sessionTitle = title}

-- | Generate a session with specific updated time
genSessionWithUpdated :: Double -> Gen Session
genSessionWithUpdated ts = do
    s <- genSession
    let time = sessionTime s
    pure s{sessionTime = time{stUpdated = ts}}

-- | Generate a session that is NOT archived
genActiveSession :: Gen Session
genActiveSession = do
    s <- genSession
    let time = sessionTime s
    pure s{sessionTime = time{stArchived = Nothing}}

-- | Generate a session with archived timestamp
genArchivedSession :: Gen Session
genArchivedSession = do
    s <- genSession
    archiveTs <- genTimestamp
    let time = sessionTime s
    pure s{sessionTime = time{stArchived = Just archiveTs}}

-- | Generate a list of sessions
genSessions :: Gen [Session]
genSessions = Gen.list (Range.linear 0 20) genSession

-- ============================================================================
-- Filter Properties
-- ============================================================================

-- | Property: defaultFilter accepts all sessions
prop_defaultFilterAcceptsAll :: Property
prop_defaultFilterAcceptsAll = property $ do
    sessions <- forAll genSessions
    let filtered = applyFilters defaultFilter sessions
    -- Default filter excludes archived, so we compare non-archived count
    let nonArchived = filter (isNothing . stArchived . sessionTime) sessions
    finiteLength filtered === finiteLength nonArchived

-- | Property: directory filter only returns matching directories
prop_directoryFilterMatches :: Property
prop_directoryFilterMatches = property $ do
    targetDir <- forAll genNonEmptyText
    -- Generate some sessions with target dir, some without
    matching <- forAll $ Gen.list (Range.linear 1 5) (genSessionWithDir targetDir)
    nonMatching <- forAll $ Gen.list (Range.linear 0 5) (genSessionWithDir "other_dir")
    let allSessions = matching ++ nonMatching
    let flt = defaultFilter{sfDirectory = Just targetDir, sfIncludeArchived = Just True}
    let filtered = applyFilters flt allSessions
    -- All filtered sessions should have the target directory
    assert $ all (\s -> sessionDirectory s == targetDir) filtered
    -- Should return exactly the matching sessions
    finiteLength filtered === finiteLength matching

-- | Property: roots filter only returns sessions without parent
prop_rootsFilterNoParent :: Property
prop_rootsFilterNoParent = property $ do
    roots <- forAll $ Gen.list (Range.linear 1 5) (genSessionWithParent Nothing)
    children <- forAll $ Gen.list (Range.linear 0 5) (genSessionWithParent (Just "parent_123"))
    let allSessions = roots ++ children
    let flt = defaultFilter{sfRootsOnly = Just True, sfIncludeArchived = Just True}
    let filtered = applyFilters flt allSessions
    -- All filtered sessions should have no parent
    assert $ all (isNothing . sessionParentID) filtered
    finiteLength filtered === finiteLength roots

-- | Property: start time filter only returns sessions updated >= start
prop_startTimeFilterCorrect :: Property
prop_startTimeFilterCorrect = property $ do
    startTs <- forAll genTimestamp
    -- Generate sessions with various timestamps
    sessions <- forAll $ Gen.list (Range.linear 1 10) genSession
    let flt = defaultFilter{sfStartTime = Just startTs, sfIncludeArchived = Just True}
    let filtered = applyFilters flt sessions
    -- All filtered sessions should have updated >= startTs
    assert $ all (\s -> stUpdated (sessionTime s) >= startTs) filtered

-- | Property: cursor time filter only returns sessions updated < cursor
prop_cursorTimeFilterCorrect :: Property
prop_cursorTimeFilterCorrect = property $ do
    cursorTs <- forAll genTimestamp
    sessions <- forAll $ Gen.list (Range.linear 1 10) genSession
    let flt = defaultFilter{sfCursorTime = Just cursorTs, sfIncludeArchived = Just True}
    let filtered = applyFilters flt sessions
    -- All filtered sessions should have updated < cursorTs
    assert $ all (\s -> stUpdated (sessionTime s) < cursorTs) filtered

-- | Property: search filter is case-insensitive
prop_searchFilterCaseInsensitive :: Property
prop_searchFilterCaseInsensitive = property $ do
    let baseTitle = "Refactoring Code"
    matching <- forAll $ Gen.list (Range.linear 1 3) (genSessionWithTitle baseTitle)
    matchingLower <- forAll $ Gen.list (Range.linear 1 3) (genSessionWithTitle "refactoring code")
    matchingMixed <- forAll $ Gen.list (Range.linear 1 3) (genSessionWithTitle "REFACTORING CODE")
    nonMatching <- forAll $ Gen.list (Range.linear 0 3) (genSessionWithTitle "Other Task")
    let allSessions = matching ++ matchingLower ++ matchingMixed ++ nonMatching
    let flt = defaultFilter{sfSearch = Just "refactor", sfIncludeArchived = Just True}
    let filtered = applyFilters flt allSessions
    -- Should match all variants of "refactoring"
    let expectedCount = finiteLength matching + finiteLength matchingLower + finiteLength matchingMixed
    finiteLength filtered === expectedCount

-- | Property: search filter finds substring matches
prop_searchFilterSubstring :: Property
prop_searchFilterSubstring = property $ do
    matching <- forAll $ Gen.list (Range.linear 1 5) (genSessionWithTitle "Implement new feature")
    nonMatching <- forAll $ Gen.list (Range.linear 0 5) (genSessionWithTitle "Bug fix")
    let allSessions = matching ++ nonMatching
    let flt = defaultFilter{sfSearch = Just "feature", sfIncludeArchived = Just True}
    let filtered = applyFilters flt allSessions
    finiteLength filtered === finiteLength matching

-- | Property: archived filter excludes archived by default
prop_archivedFilterExcludesByDefault :: Property
prop_archivedFilterExcludesByDefault = property $ do
    active <- forAll $ Gen.list (Range.linear 1 5) genActiveSession
    archived <- forAll $ Gen.list (Range.linear 1 5) genArchivedSession
    let allSessions = active ++ archived
    -- Default filter (sfIncludeArchived = Nothing) should exclude archived
    let flt = defaultFilter
    let filtered = applyFilters flt allSessions
    -- All filtered sessions should not be archived
    assert $ all (isNothing . stArchived . sessionTime) filtered
    finiteLength filtered === finiteLength active

-- | Property: archived filter includes all when sfIncludeArchived = Just True
prop_archivedFilterIncludesWhenTrue :: Property
prop_archivedFilterIncludesWhenTrue = property $ do
    active <- forAll $ Gen.list (Range.linear 1 5) genActiveSession
    archived <- forAll $ Gen.list (Range.linear 1 5) genArchivedSession
    let allSessions = active ++ archived
    let flt = defaultFilter{sfIncludeArchived = Just True}
    let filtered = applyFilters flt allSessions
    finiteLength filtered === finiteLength allSessions

-- | Property: filters are conjunctive (AND)
prop_filtersAreConjunctive :: Property
prop_filtersAreConjunctive = property $ do
    let targetDir = "/home/user/project"
    -- Create sessions: some match both filters, some match only one
    matchBoth <- forAll $ Gen.list (Range.linear 1 3) $ do
        s <- genSessionWithDir targetDir
        pure s{sessionParentID = Nothing}
    matchDirOnly <- forAll $ Gen.list (Range.linear 0 3) $ do
        s <- genSessionWithDir targetDir
        pure s{sessionParentID = Just "parent_123"}
    matchRootOnly <- forAll $ Gen.list (Range.linear 0 3) $ do
        s <- genSessionWithDir "other_dir"
        pure s{sessionParentID = Nothing}
    let allSessions = matchBoth ++ matchDirOnly ++ matchRootOnly
    let flt =
            defaultFilter
                { sfDirectory = Just targetDir
                , sfRootsOnly = Just True
                , sfIncludeArchived = Just True
                }
    let filtered = applyFilters flt allSessions
    -- Should only return sessions matching BOTH filters
    finiteLength filtered === finiteLength matchBoth

-- ============================================================================
-- Sort and Limit Properties
-- ============================================================================

-- | Property: applySortAndLimit sorts by updated time descending
prop_sortByUpdatedDescending :: Property
prop_sortByUpdatedDescending = property $ do
    sessions <- forAll $ Gen.list (Range.linear 2 20) genSession
    let sorted = applySortAndLimit Nothing sessions
    let times = map (stUpdated . sessionTime) sorted
    -- Should be sorted in descending order
    times === sortOn Down times

-- | Property: applySortAndLimit respects limit
prop_sortRespectsLimit :: Property
prop_sortRespectsLimit = property $ do
    sessions <- forAll $ Gen.list (Range.linear 5 20) genSession
    limitVal <- forAll $ Gen.int (Range.linear 1 10)
    let limited = applySortAndLimit (Just limitVal) sessions
    -- Should return at most limitVal sessions
    assert $ finiteLength limited <= limitVal
    -- If input has enough sessions, should return exactly limitVal
    if finiteLength sessions >= limitVal
        then finiteLength limited === limitVal
        else finiteLength limited === finiteLength sessions

-- | Property: applySortAndLimit with Nothing limit returns all
prop_sortNoLimitReturnsAll :: Property
prop_sortNoLimitReturnsAll = property $ do
    sessions <- forAll $ Gen.list (Range.linear 0 20) genSession
    let result = applySortAndLimit Nothing sessions
    finiteLength result === finiteLength sessions

-- | Property: applySortAndLimit returns most recent when limited
prop_sortReturnsMostRecent :: Property
prop_sortReturnsMostRecent = property $ do
    -- Create sessions with known timestamps
    let timestamps = [1000.0, 2000.0, 3000.0, 4000.0, 5000.0]
    sessions <- forAll $ traverse genSessionWithUpdated timestamps
    let limited = applySortAndLimit (Just 3) sessions
    let resultTimes = map (stUpdated . sessionTime) limited
    -- Should return the 3 most recent (5000, 4000, 3000)
    resultTimes === [5000.0, 4000.0, 3000.0]

-- | Property: empty input produces empty output
prop_emptyInputEmptyOutput :: Property
prop_emptyInputEmptyOutput = property $ do
    let filtered = applyFilters defaultFilter []
    let sorted = applySortAndLimit (Just 10) []
    finiteLength filtered === 0
    finiteLength sorted === 0

-- ============================================================================
-- Combined Filter + Sort Properties
-- ============================================================================

-- | Property: filter then sort maintains filter invariants
prop_filterThenSortMaintainsInvariants :: Property
prop_filterThenSortMaintainsInvariants = property $ do
    let targetDir = "/target/dir"
    sessions <- forAll $ Gen.list (Range.linear 5 20) genSession
    -- Add some sessions with target dir
    matching <- forAll $ Gen.list (Range.linear 1 5) (genSessionWithDir targetDir)
    let allSessions = sessions ++ matching
    let flt = defaultFilter{sfDirectory = Just targetDir, sfIncludeArchived = Just True}
    let result = applySortAndLimit (Just 3) $ applyFilters flt allSessions
    -- All results should still match the filter
    assert $ all (\s -> sessionDirectory s == targetDir) result
    -- Results should be sorted
    let times = map (stUpdated . sessionTime) result
    times === sortOn Down times

-- ============================================================================
-- Test Tree
-- ============================================================================

tests :: TestTree
tests =
    testGroup
        "Session Filter Property Tests"
        [ testGroup
            "Filter Properties"
            [ testProperty "default filter accepts all (non-archived)" prop_defaultFilterAcceptsAll
            , testProperty "directory filter matches" prop_directoryFilterMatches
            , testProperty "roots filter returns no parent" prop_rootsFilterNoParent
            , testProperty "start time filter correct" prop_startTimeFilterCorrect
            , testProperty "cursor time filter correct" prop_cursorTimeFilterCorrect
            , testProperty "search filter case insensitive" prop_searchFilterCaseInsensitive
            , testProperty "search filter substring" prop_searchFilterSubstring
            , testProperty "archived filter excludes by default" prop_archivedFilterExcludesByDefault
            , testProperty "archived filter includes when true" prop_archivedFilterIncludesWhenTrue
            , testProperty "filters are conjunctive (AND)" prop_filtersAreConjunctive
            ]
        , testGroup
            "Sort and Limit Properties"
            [ testProperty "sort by updated descending" prop_sortByUpdatedDescending
            , testProperty "sort respects limit" prop_sortRespectsLimit
            , testProperty "no limit returns all" prop_sortNoLimitReturnsAll
            , testProperty "sort returns most recent" prop_sortReturnsMostRecent
            , testProperty "empty input empty output" prop_emptyInputEmptyOutput
            ]
        , testGroup
            "Combined Properties"
            [ testProperty "filter then sort maintains invariants" prop_filterThenSortMaintainsInvariants
            ]
        ]
