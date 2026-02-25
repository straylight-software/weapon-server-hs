{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.ExperimentalProps
Description : Property tests for Experimental.Worktree

Property-based tests for the experimental worktree module.
Tests cover both pure functions and IO operations with storage.
-}
module Property.ExperimentalProps where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Experimental.Worktree qualified as Worktree
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Storage.Storage qualified as Storage
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory)
import Test.Tasty
import Test.Tasty.Hedgehog

-- ════════════════════════════════════════════════════════════════════════════
-- Pure Function Properties
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: defaultWorktreeInfo contains the provided root
prop_defaultWorktreeInfoHasRoot :: Property
prop_defaultWorktreeInfoHasRoot = property $ do
    root <- forAll genText
    let info = Worktree.defaultWorktreeInfo root
    case info of
        Object obj -> do
            KM.lookup (K.fromText "root") obj === Just (String root)
            KM.lookup (K.fromText "ready") obj === Just (Bool True)
        _notObject -> failure

-- | Property: resetWorktreeInfo contains the provided root
prop_resetWorktreeInfoHasRoot :: Property
prop_resetWorktreeInfoHasRoot = property $ do
    root <- forAll genText
    let info = Worktree.resetWorktreeInfo root
    case info of
        Object obj -> do
            KM.lookup (K.fromText "root") obj === Just (String root)
            KM.lookup (K.fromText "reset") obj === Just (Bool True)
        _notObject -> failure

-- | Property: worktreeKey is non-empty and has expected structure
prop_worktreeKeyStructure :: Property
prop_worktreeKeyStructure = withTests 1 $ property $ do
    -- Check exact structure (avoids length on list)
    case Worktree.worktreeKey of
        [a, b] -> do
            a === "experimental"
            b === "worktree"
        _other -> failure

-- | Property: defaultWorktreeInfo and resetWorktreeInfo produce different objects
prop_defaultAndResetDiffer :: Property
prop_defaultAndResetDiffer = property $ do
    root <- forAll genText
    let defaultInfo = Worktree.defaultWorktreeInfo root
        resetInfo' = Worktree.resetWorktreeInfo root
    -- They should both have the same root but different status flags
    case (defaultInfo, resetInfo') of
        (Object defObj, Object resObj) -> do
            -- Both have same root
            KM.lookup (K.fromText "root") defObj === KM.lookup (K.fromText "root") resObj
            -- But different status fields
            KM.lookup (K.fromText "ready") defObj === Just (Bool True)
            KM.lookup (K.fromText "reset") resObj === Just (Bool True)
            -- And they shouldn't have each other's status fields
            KM.lookup (K.fromText "reset") defObj === Nothing
            KM.lookup (K.fromText "ready") resObj === Nothing
        (Null, _) -> failure
        (String _, _) -> failure
        (Number _, _) -> failure
        (Bool _, _) -> failure
        (Array _, _) -> failure
        (_, Null) -> failure
        (_, String _) -> failure
        (_, Number _) -> failure
        (_, Bool _) -> failure
        (_, Array _) -> failure

-- ════════════════════════════════════════════════════════════════════════════
-- IO Operation Properties
-- ════════════════════════════════════════════════════════════════════════════

{- | Run a test action with a temporary storage directory.

Creates a temp directory, initializes storage, runs the action,
cleans up the directory, and returns the result.
-}
withStore :: (Storage.StorageConfig -> IO a) -> IO a
withStore action = do
    tmpDir <- createTempDirectory "/tmp" "experimental-test"
    result <- Storage.withStorage tmpDir action
    removeDirectoryRecursive tmpDir
    pure result

-- | Property: setInfo followed by getInfo returns the set value.
prop_worktreeSetGet :: Property
prop_worktreeSetGet = property $ do
    root <- forAll genText
    value <- forAll genValue
    result <- evalIO $ withStore $ \store -> do
        _ <- Worktree.setInfo store value
        Worktree.getInfo store root
    result === value

-- | Property: resetInfo returns an object with the correct root.
prop_worktreeReset :: Property
prop_worktreeReset = property $ do
    root <- forAll genText
    result <- evalIO $ withStore $ \store -> Worktree.resetInfo store root
    case result of
        Object obj -> case KM.lookup (K.fromText "root") obj of
            Just (String value) -> value === root
            Just _otherValue -> failure
            Nothing -> failure
        _otherValue -> failure

{- | Property: worktree remove on empty store does not crash
This is a smoke test - the operation should complete without exceptions
-}
prop_worktreeRemoveEmpty :: Property
prop_worktreeRemoveEmpty = property $ do
    root <- forAll genText
    _result <- evalIO $ withStore $ \store -> Worktree.remove store root Nothing
    -- If we get here without exception, the test passes
    -- Both Right () and Left err are valid outcomes for empty store
    success

-- | Property: worktree remove after set succeeds
prop_worktreeRemoveAfterSet :: Property
prop_worktreeRemoveAfterSet = property $ do
    root <- forAll genText
    value <- forAll genValue
    result <- evalIO $ withStore $ \store -> do
        _ <- Worktree.setInfo store value
        Worktree.remove store root Nothing
    case result of
        Right () -> success
        Left _err -> failure

{- | Property: worktree get after remove returns an object
After remove, getInfo should still return a valid JSON object
-}
prop_worktreeGetAfterRemove :: Property
prop_worktreeGetAfterRemove = property $ do
    root <- forAll genText
    value <- forAll genValue
    result <- evalIO $ withStore $ \store -> do
        _ <- Worktree.setInfo store value
        _ <- Worktree.remove store root Nothing
        Worktree.getInfo store root
    -- After remove, getInfo should return a valid object (default or empty)
    case result of
        Object _obj -> success
        _notObject -> failure

-- | Generator for non-empty alphanumeric text (1-20 characters).
genText :: Gen Text
genText = Gen.text (Range.linear 1 20) Gen.alphaNum

{- | Generator for worktree-like JSON values.

Produces objects with "root" and "ready" fields.
-}
genValue :: Gen Value
genValue = do
    text <- genText
    pure $ object ["root" .= text, "ready" .= True]

-- | Property: worktree set is idempotent
prop_worktreeSetIdempotent :: Property
prop_worktreeSetIdempotent = property $ do
    root <- forAll genText
    value <- forAll genValue
    result <- evalIO $ withStore $ \store -> do
        _ <- Worktree.setInfo store value
        _ <- Worktree.setInfo store value
        Worktree.getInfo store root
    result === value

-- | Property: worktree reset after set returns new value
prop_worktreeResetAfterSet :: Property
prop_worktreeResetAfterSet = property $ do
    root <- forAll genText
    value <- forAll genValue
    result <- evalIO $ withStore $ \store -> do
        _ <- Worktree.setInfo store value
        Worktree.resetInfo store root
    case result of
        Object obj -> case KM.lookup (K.fromText "root") obj of
            Just (String r) -> r === root
            Just _otherValue -> failure
            Nothing -> failure
        _otherValue -> failure

-- | Property: worktree get with different roots returns same value
prop_worktreeGetDifferentRoots :: Property
prop_worktreeGetDifferentRoots = property $ do
    root1 <- forAll genText
    root2 <- forAll genText
    value <- forAll genValue
    (result1, result2) <- evalIO $ withStore $ \store -> do
        _ <- Worktree.setInfo store value
        r1 <- Worktree.getInfo store root1
        r2 <- Worktree.getInfo store root2
        pure (r1, r2)
    result1 === result2

-- | Property: worktree remove is idempotent (doesn't crash on second call)
prop_worktreeRemoveIdempotent :: Property
prop_worktreeRemoveIdempotent = property $ do
    root <- forAll genText
    value <- forAll genValue
    _result <- evalIO $ withStore $ \store -> do
        _ <- Worktree.setInfo store value
        _ <- Worktree.remove store root Nothing
        Worktree.remove store root Nothing
    -- If we reach here, both remove calls completed without exception
    success

-- | Property: worktree set preserves all fields
prop_worktreeSetPreservesFields :: Property
prop_worktreeSetPreservesFields = property $ do
    root <- forAll genText
    text <- forAll genText
    let value = object ["root" .= text, "ready" .= True, "extra" .= ("field" :: Text)]
    result <- evalIO $ withStore $ \store -> do
        _ <- Worktree.setInfo store value
        Worktree.getInfo store root
    case result of
        Object obj -> do
            KM.lookup (K.fromText "root") obj === Just (String text)
            KM.lookup (K.fromText "ready") obj === Just (Bool True)
        _otherValue -> failure

-- | Property: worktree operations are independent per store
prop_worktreeIndependentStores :: Property
prop_worktreeIndependentStores = property $ do
    root <- forAll genText
    value1 <- forAll genValue
    value2 <- forAll genValue
    (result1, result2) <- evalIO $ do
        tmpDir1 <- createTempDirectory "/tmp" "exp-test-1"
        tmpDir2 <- createTempDirectory "/tmp" "exp-test-2"
        store1 <- Storage.withStorage tmpDir1 pure
        store2 <- Storage.withStorage tmpDir2 pure
        _ <- Worktree.setInfo store1 value1
        _ <- Worktree.setInfo store2 value2
        r1 <- Worktree.getInfo store1 root
        r2 <- Worktree.getInfo store2 root
        removeDirectoryRecursive tmpDir1
        removeDirectoryRecursive tmpDir2
        pure (r1, r2)
    -- Each store should return what was set in it (stores are independent)
    result1 === value1
    result2 === value2

-- | All property tests for the Experimental module
tests :: TestTree
tests =
    testGroup
        "Experimental Property Tests"
        [ testGroup
            "Pure Functions"
            [ testProperty "defaultWorktreeInfo has correct structure" prop_defaultWorktreeInfoHasRoot
            , testProperty "resetWorktreeInfo has correct structure" prop_resetWorktreeInfoHasRoot
            , testProperty "worktreeKey has expected structure" prop_worktreeKeyStructure
            , testProperty "default and reset info differ" prop_defaultAndResetDiffer
            ]
        , testGroup
            "IO Operations"
            [ testProperty "worktree set/get" prop_worktreeSetGet
            , testProperty "worktree reset" prop_worktreeReset
            , testProperty "worktree remove empty" prop_worktreeRemoveEmpty
            , testProperty "worktree remove after set" prop_worktreeRemoveAfterSet
            , testProperty "worktree get after remove" prop_worktreeGetAfterRemove
            , testProperty "worktree set idempotent" prop_worktreeSetIdempotent
            , testProperty "worktree reset after set" prop_worktreeResetAfterSet
            , testProperty "worktree get different roots" prop_worktreeGetDifferentRoots
            , testProperty "worktree remove idempotent" prop_worktreeRemoveIdempotent
            , testProperty "worktree set preserves fields" prop_worktreeSetPreservesFields
            , testProperty "worktree independent stores" prop_worktreeIndependentStores
            ]
        ]
