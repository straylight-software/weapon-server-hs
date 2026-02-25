{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for Util.ExeCache

Tests the pure cache lookup/insertion logic. IO behavior (actual PATH
lookup and caching) is tested via the IO properties.
-}
module Property.ExeCacheProps where

import Control.Concurrent.Async (async, wait)
import Control.Monad (forM_, replicateM, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Util.ExeCache

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Cache Properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: lookupCache returns Nothing for empty cache
prop_lookupEmptyCache :: Property
prop_lookupEmptyCache = property $ do
    name <- forAll genExeName
    let cache = Map.empty
    lookupCache name cache === Nothing

-- | Property: insertCache followed by lookupCache returns the value
prop_insertThenLookup :: Property
prop_insertThenLookup = property $ do
    name <- forAll genExeName
    mPath <- forAll genMaybePath
    let cache = Map.empty
    let cache' = insertCache name mPath cache
    lookupCache name cache' === Just mPath

-- | Property: insertCache preserves existing entries
prop_insertPreservesOthers :: Property
prop_insertPreservesOthers = property $ do
    name1 <- forAll genExeName
    name2 <- forAll genExeName
    -- Ensure names are different
    when (name1 == name2) discard

    path1 <- forAll genMaybePath
    path2 <- forAll genMaybePath

    let cache = insertCache name1 path1 Map.empty
    let cache' = insertCache name2 path2 cache

    -- Both entries should be present
    lookupCache name1 cache' === Just path1
    lookupCache name2 cache' === Just path2

-- | Property: insertCache overwrites existing entry
prop_insertOverwrites :: Property
prop_insertOverwrites = property $ do
    name <- forAll genExeName
    path1 <- forAll genMaybePath
    path2 <- forAll genMaybePath

    let cache = insertCache name path1 Map.empty
    let cache' = insertCache name path2 cache

    lookupCache name cache' === Just path2

-- | Property: multiple inserts don't affect lookup correctness
prop_multipleInserts :: Property
prop_multipleInserts = property $ do
    entries <- forAll $ Gen.list (Range.linear 1 20) genCacheEntry
    let cache = foldl' (\c (n, p) -> insertCache n p c) Map.empty entries

    -- Check that the last entry for each name is what we get
    let lastEntries = Map.fromList entries -- Map keeps last for duplicate keys
    forM_ (Map.toList lastEntries) $ \(name, expectedPath) -> do
        lookupCache name cache === Just expectedPath

-- ═══════════════════════════════════════════════════════════════════════════
-- IO Cache Properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: findExecutableCached returns same result on repeated calls
prop_ioCacheConsistency :: Property
prop_ioCacheConsistency = property $ do
    exeName <- forAll $ Gen.element ["ls", "cat", "nonexistent_exe_12345"]
    (result1, result2) <- evalIO $ do
        cache <- newExeCache
        r1 <- findExecutableCached cache exeName
        r2 <- findExecutableCached cache exeName
        pure (r1, r2)
    result1 === result2

-- | Property: looking up known executables works (if they exist)
prop_ioKnownExecutables :: Property
prop_ioKnownExecutables = property $ do
    -- These are very likely to exist on any Unix system
    results <- evalIO $ do
        cache <- newExeCache
        mapM (findExecutableCached cache) ["sh", "env"]

    -- At least one of these should be found (sh and env are POSIX required)
    assert $ any isJust results

-- | Property: looking up nonexistent executable returns Nothing consistently
prop_ioNonexistentExecutable :: Property
prop_ioNonexistentExecutable = property $ do
    -- Use a name that definitely doesn't exist
    result <- evalIO $ do
        cache <- newExeCache
        findExecutableCached cache "definitely_nonexistent_exe_xyz_12345"
    assert $ isNothing result

-- | Property: concurrent lookups for same executable return same result
prop_ioConcurrentSameExe :: Property
prop_ioConcurrentSameExe = property $ do
    numThreads <- forAll $ Gen.int (Range.linear 2 10)
    results <- evalIO $ do
        cache <- newExeCache
        asyncs <-
            replicateM numThreads $
                async $
                    findExecutableCached cache "sh"
        mapM wait asyncs

    -- All results should be identical
    let uniqueResults = Set.fromList results
    Set.size uniqueResults === 1

-- | Property: concurrent lookups for different executables work
prop_ioConcurrentDifferentExes :: Property
prop_ioConcurrentDifferentExes = property $ do
    let exes = ["sh", "env", "cat", "ls", "nonexistent_99999"]
    results <- evalIO $ do
        cache <- newExeCache
        asyncs <- mapM (async . findExecutableCached cache) exes
        mapM wait asyncs

    -- Results should be consistent with individual lookups
    expected <- evalIO $ do
        cache <- newExeCache
        mapM (findExecutableCached cache) exes

    results === expected

-- | Property: new cache starts empty (all lookups miss initially)
prop_ioNewCacheEmpty :: Property
prop_ioNewCacheEmpty = property $ do
    -- We can't directly inspect the cache, but we can verify that
    -- looking up a nonexistent executable doesn't find anything
    result <- evalIO $ do
        cache <- newExeCache
        findExecutableCached cache "definitely_not_installed_xyz"
    assert $ isNothing result

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

-- | Generate an executable name
genExeName :: Gen String
genExeName = Gen.string (Range.linear 1 20) Gen.alphaNum

-- | Generate a maybe file path
genMaybePath :: Gen (Maybe FilePath)
genMaybePath = Gen.maybe genFilePath

-- | Generate a file path
genFilePath :: Gen FilePath
genFilePath = Gen.string (Range.linear 1 50) $ Gen.element $ ['a' .. 'z'] ++ ['0' .. '9'] ++ "/_-."

-- | Generate a cache entry
genCacheEntry :: Gen (String, Maybe FilePath)
genCacheEntry = (,) <$> genExeName <*> genMaybePath

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "ExeCache Property Tests"
        [ testGroup
            "Pure Cache"
            [ testProperty "empty cache returns Nothing" prop_lookupEmptyCache
            , testProperty "insert then lookup" prop_insertThenLookup
            , testProperty "insert preserves other entries" prop_insertPreservesOthers
            , testProperty "insert overwrites existing" prop_insertOverwrites
            , testProperty "multiple inserts consistent" prop_multipleInserts
            ]
        , testGroup
            "IO Cache"
            [ testProperty "cache consistency" prop_ioCacheConsistency
            , testProperty "known executables found" prop_ioKnownExecutables
            , testProperty "nonexistent returns Nothing" prop_ioNonexistentExecutable
            , testProperty "concurrent same exe" prop_ioConcurrentSameExe
            , testProperty "concurrent different exes" prop_ioConcurrentDifferentExes
            , testProperty "new cache is empty" prop_ioNewCacheEmpty
            ]
        ]
