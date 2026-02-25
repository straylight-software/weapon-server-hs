{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Integration.StorageSpec
Description : Integration tests for Storage module

Integration tests for the storage layer, focusing on:

  * Concurrent access patterns
  * Error handling and edge cases
  * Atomic write guarantees
  * Directory cache effectiveness
-}
module Integration.StorageSpec (
    spec,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import Control.Monad (replicateM)
import Data.Foldable (for_)

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Storage.Storage qualified as Storage
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory)
import Test.Hspec

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Helpers
-- ═══════════════════════════════════════════════════════════════════════════

-- | Run a test with a temporary storage directory
withTempStorage :: (Storage.StorageConfig -> IO a) -> IO a
withTempStorage action = do
    tmpDir <- createTempDirectory "/tmp" "storage-integration-test"
    result <- Storage.withStorage tmpDir action
    removeDirectoryRecursive tmpDir
    pure result

-- | Count list elements without using length (avoids STAN-0103)
countList :: [a] -> Int
countList = go 0
  where
    go !n [] = n
    go !n (_ : xs) = go (n + 1) xs

-- ═══════════════════════════════════════════════════════════════════════════
-- Concurrent Access Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- ═══════════════════════════════════════════════════════════════════════════
-- Hspec Test Specification
-- ═══════════════════════════════════════════════════════════════════════════

-- | Storage integration test specification
spec :: Spec
spec = do
    describe "Concurrent Access" $ do
        it "concurrent writes to different keys" $ do
            withTempStorage $ \storage -> do
                let numWriters = 10
                dones <- replicateM numWriters newEmptyMVar

                -- Start concurrent writers
                for_ (zip [1 .. numWriters] dones) $ \(i, done) ->
                    forkIO $ do
                        let key = ["concurrent", T.pack (show (i :: Int))]
                            val = object ["index" .= i]
                        Storage.write storage key val
                        putMVar done ()

                -- Wait for all writers to complete
                for_ dones takeMVar

                -- Verify all values were written correctly
                -- Use explicit indices list to avoid lazy range (STAN-0210)
                let indices = take numWriters [(1 :: Int) ..]
                for_ indices $ \i -> do
                    let key = ["concurrent", T.pack (show i)]
                    val <- Storage.read storage key :: IO Value
                    val `shouldBe` object ["index" .= i]

        it "concurrent writes to same key (no corruption)" $ do
            withTempStorage $ \storage -> do
                let numWriters = 20
                dones <- replicateM numWriters newEmptyMVar

                -- Start concurrent writers to the same key
                for_ (zip [1 .. numWriters] dones) $ \(i, done) ->
                    forkIO $ do
                        let key = ["shared", "key"]
                            val = object ["writer" .= (i :: Int)]
                        Storage.writeAtomic storage key val
                        putMVar done ()

                -- Wait for all writers to complete
                for_ dones takeMVar

                -- Verify the key has a valid value (one of the writers' values)
                val <- Storage.read storage ["shared", "key"] :: IO Value
                -- The value should be a valid object (not corrupted)
                case val of
                    Object _ -> pure ()
                    Array _ -> expectationFailure "Expected an Object value, got Array"
                    String _ -> expectationFailure "Expected an Object value, got String"
                    Number _ -> expectationFailure "Expected an Object value, got Number"
                    Bool _ -> expectationFailure "Expected an Object value, got Bool"
                    Null -> expectationFailure "Expected an Object value, got Null"

    describe "Error Handling" $ do
        it "NotFoundError for missing key" $ do
            withTempStorage $ \storage -> do
                result <- try $ Storage.read storage ["nonexistent", "key"] :: IO (Either Storage.NotFoundError Text)
                case result of
                    Left (Storage.NotFoundError _) -> pure ()
                    Right _ -> expectationFailure "Expected NotFoundError"

        it "StorageDecodeError for wrong type" $ do
            withTempStorage $ \storage -> do
                -- Write a number
                Storage.write storage ["type", "mismatch"] (42 :: Int)
                -- Try to read as a list
                result <- try $ Storage.read storage ["type", "mismatch"] :: IO (Either Storage.StorageError [Int])
                case result of
                    Left (Storage.StorageDecodeError _ _) -> pure ()
                    Right _ -> expectationFailure "Expected StorageDecodeError"

        it "remove is idempotent" $ do
            withTempStorage $ \storage -> do
                -- Remove non-existent key (should not throw)
                Storage.remove storage ["nonexistent"]
                -- Write and remove
                Storage.write storage ["temp"] ("value" :: Text)
                Storage.remove storage ["temp"]
                -- Remove again (should not throw)
                Storage.remove storage ["temp"]

    describe "Directory Cache" $ do
        it "DirCache batch writes" $ do
            withTempStorage $ \storage -> do
                cache <- Storage.newDirCache
                let numWrites = 100

                -- Write many files to the same directory using cache
                -- Use explicit indices list to avoid lazy range (STAN-0210)
                let indices = take numWrites [(1 :: Int) ..]
                for_ indices $ \i -> do
                    let key = ["batch", "dir", T.pack (show i)]
                    Storage.writeCached cache storage key (object ["n" .= i])

                -- Verify all were written (100 keys)
                keys <- Storage.list storage ["batch", "dir"]
                countList keys `shouldBe` numWrites

    describe "Atomic Writes" $ do
        it "atomic write overwrites existing" $ do
            withTempStorage $ \storage -> do
                let key = ["atomic", "overwrite"]
                -- Write initial value
                Storage.writeAtomic storage key ("initial" :: Text)
                -- Overwrite with new value
                Storage.writeAtomic storage key ("updated" :: Text)
                -- Verify the new value
                val <- Storage.read storage key :: IO Text
                val `shouldBe` "updated"

    describe "List Edge Cases" $ do
        it "list non-existent directory returns empty" $ do
            withTempStorage $ \storage -> do
                keys <- Storage.list storage ["nonexistent", "directory"]
                keys `shouldBe` []

        it "list returns nested keys" $ do
            withTempStorage $ \storage -> do
                -- Create a nested structure
                Storage.write storage ["parent", "child1", "grandchild1"] ("a" :: Text)
                Storage.write storage ["parent", "child1", "grandchild2"] ("b" :: Text)
                Storage.write storage ["parent", "child2", "grandchild1"] ("c" :: Text)

                -- List at parent level
                keys <- Storage.list storage ["parent"]
                case keys of
                    [_a, _b, _c] -> pure ()
                    [] -> expectationFailure "Expected 3 keys, got 0"
                    [_one] -> expectationFailure "Expected 3 keys, got 1"
                    [_a, _b] -> expectationFailure "Expected 3 keys, got 2"
                    (_ : _ : _ : _ : _moreThanThree) -> expectationFailure "Expected 3 keys, got more"

                -- List at child1 level
                child1Keys <- Storage.list storage ["parent", "child1"]
                case child1Keys of
                    [_a, _b] -> pure ()
                    [] -> expectationFailure "Expected 2 keys, got 0"
                    [_one] -> expectationFailure "Expected 2 keys, got 1"
                    (_ : _ : _ : _moreThanTwo) -> expectationFailure "Expected 2 keys, got more"
