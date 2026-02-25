{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.StorageProps
Description : Property tests for Storage module

Property-based tests for the storage layer, covering both:

  * Pure functions (keyPath, keyToRelativeParts) - fast, no IO
  * IO operations (read, write, update, list, remove) - using temp directories

The tests verify key invariants like:

  * Write/read round-trip identity
  * Key path construction is reversible
  * List operations respect prefixes
-}
module Property.StorageProps where

import Control.Exception (catch)
import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Storage.Storage qualified as Storage
import System.Directory (removeDirectoryRecursive)
import System.FilePath (pathSeparator, takeExtension, (</>))
import System.IO.Temp (createTempDirectory)
import Test.Helpers (listLength)
import Test.Tasty
import Test.Tasty.Hedgehog

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

-- | Generate a key segment (alphanumeric, 1-20 chars)
genKeyPart :: Gen Text
genKeyPart = Gen.text (Range.linear 1 20) Gen.alphaNum

-- | Generate test value content (alphanumeric, 0-100 chars)
genTestValue :: Gen Text
genTestValue = Gen.text (Range.linear 0 100) Gen.alphaNum

-- | Generate a base directory path
genBaseDir :: Gen FilePath
genBaseDir = Gen.element ["/tmp", "/data", "/var/lib"]

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Function Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: keyPath produces paths ending in .json
prop_keyPathEndsWithJson :: Property
prop_keyPathEndsWithJson = property $ do
    baseDir <- forAll genBaseDir
    keyParts <- forAll $ Gen.list (Range.linear 1 5) genKeyPart
    let cfg = Storage.StorageConfig baseDir
        path = Storage.keyPath cfg keyParts
    assert $ takeExtension path == ".json"

-- | Property: keyPath starts with the storage directory
prop_keyPathStartsWithDir :: Property
prop_keyPathStartsWithDir = property $ do
    baseDir <- forAll genBaseDir
    keyParts <- forAll $ Gen.list (Range.linear 1 5) genKeyPart
    let cfg = Storage.StorageConfig baseDir
        path = Storage.keyPath cfg keyParts
    assert $ take (length baseDir) path == baseDir

-- | Property: keyPath contains all key parts as path segments
prop_keyPathContainsAllParts :: Property
prop_keyPathContainsAllParts = property $ do
    baseDir <- forAll genBaseDir
    keyParts <- forAll $ Gen.list (Range.linear 1 3) genKeyPart
    let cfg = Storage.StorageConfig baseDir
        path = Storage.keyPath cfg keyParts
    -- Each key part should appear in the path
    forM_ keyParts $ \part ->
        assert $ T.unpack part `isInfixOfStr` path
  where
    isInfixOfStr needle haystack = any (needle `isPrefixOfStr`) (tails haystack)
    isPrefixOfStr [] _ = True
    isPrefixOfStr _ [] = False
    isPrefixOfStr (x : xs) (y : ys) = x == y && isPrefixOfStr xs ys
    tails [] = [[]]
    tails lst@(_ : xs) = lst : tails xs

-- | Property: keyToRelativeParts is inverse of key construction for list results
prop_keyToRelativePartsRoundtrip :: Property
prop_keyToRelativePartsRoundtrip = property $ do
    prefix <- forAll $ Gen.list (Range.linear 1 2) genKeyPart
    suffix <- forAll $ Gen.list (Range.linear 1 3) genKeyPart
    let baseDir = [pathSeparator] <> "storage" </> joinParts prefix
        fullPath = baseDir </> joinParts suffix <> ".json"
        reconstructed = Storage.keyToRelativeParts prefix baseDir fullPath
    -- The reconstructed key should be prefix ++ suffix
    reconstructed === prefix ++ suffix
  where
    -- Safe path joining for finite test lists
    joinParts [] = ""
    joinParts [x] = T.unpack x
    joinParts (x : xs) = T.unpack x </> joinParts xs

-- ═══════════════════════════════════════════════════════════════════════════
-- IO Operation Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: write then read returns the same value
prop_writeReadIdentity :: Property
prop_writeReadIdentity = property $ do
    keyParts <- forAll $ Gen.list (Range.linear 1 5) genKeyPart
    val <- forAll genTestValue

    result <- evalIO $ withTempStorage $ \storage -> do
        Storage.write storage keyParts val
        Storage.read storage keyParts

    val === result

-- | Property: update modifies the value correctly
prop_updateModifies :: Property
prop_updateModifies = property $ do
    keyParts <- forAll $ Gen.list (Range.linear 1 3) genKeyPart
    initial <- forAll genTestValue
    newContent <- forAll genTestValue

    result <- evalIO $ withTempStorage $ \storage -> do
        Storage.write storage keyParts initial
        Storage.update storage keyParts (const newContent)

    result === newContent

-- | Property: list returns keys with the given prefix
prop_listWithPrefix :: Property
prop_listWithPrefix = property $ do
    prefix <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    count <- forAll $ Gen.int (Range.linear 1 10)

    keys <- evalIO $ withTempStorage $ \storage -> do
        -- Write multiple values with the same prefix
        mapM_
            ( \i -> do
                let key = [prefix, T.pack (show i)]
                Storage.write storage key (object ["index" .= i])
            )
            [1 .. count]
        -- List all keys with prefix
        Storage.list storage [prefix]

    -- Should find all the keys we created
    listLength keys === count

-- | Property: remove deletes the value
prop_removeDeletes :: Property
prop_removeDeletes = property $ do
    keyParts <- forAll $ Gen.list (Range.linear 1 3) genKeyPart
    val <- forAll genTestValue

    (foundBefore, foundAfter) <- evalIO $ withTempStorage $ \storage -> do
        Storage.write storage keyParts (val :: Text)
        before <-
            (Just <$> Storage.read storage keyParts)
                `catch` \(Storage.NotFoundError _) -> pure Nothing
        Storage.remove storage keyParts
        afterValue <-
            (Just <$> Storage.read storage keyParts)
                `catch` \(Storage.NotFoundError _) -> pure Nothing
        pure (before :: Maybe Text, afterValue :: Maybe Text)

    -- Value should exist before removal
    assert $ isJust foundBefore
    -- Value should not exist after removal
    foundAfter === Nothing

-- | Property: remove then list returns empty
prop_removeListEmpty :: Property
prop_removeListEmpty = property $ do
    prefix <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    val <- forAll genTestValue
    keys <- evalIO $ withTempStorage $ \storage -> do
        Storage.write storage [prefix, "a"] (val :: Text)
        Storage.remove storage [prefix, "a"]
        Storage.list storage [prefix]
    keys === []

-- | Property: list returns only keys with the given prefix
prop_listRespectsPrefix :: Property
prop_listRespectsPrefix = property $ do
    prefix <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    key1 <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    key2 <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    val <- forAll genTestValue
    keys <- evalIO $ withTempStorage $ \storage -> do
        Storage.write storage [prefix, key1] (val :: Text)
        Storage.write storage [prefix, key2] (val :: Text)
        Storage.list storage [prefix]
    assert $ all (\k -> take 1 k == [prefix]) keys

-- | Property: writeAtomic then read returns the same value
prop_writeAtomicReadIdentity :: Property
prop_writeAtomicReadIdentity = property $ do
    keyParts <- forAll $ Gen.list (Range.linear 1 5) genKeyPart
    val <- forAll genTestValue

    result <- evalIO $ withTempStorage $ \storage -> do
        Storage.writeAtomic storage keyParts val
        Storage.read storage keyParts

    val === result

-- | Property: writeCached then read returns the same value
prop_writeCachedReadIdentity :: Property
prop_writeCachedReadIdentity = property $ do
    keyParts <- forAll $ Gen.list (Range.linear 1 5) genKeyPart
    val <- forAll genTestValue

    result <- evalIO $ withTempStorage $ \storage -> do
        cache <- Storage.newDirCache
        Storage.writeCached cache storage keyParts val
        Storage.read storage keyParts

    val === result

-- | Property: readMaybe returns Nothing for non-existent key
prop_readMaybeNotFound :: Property
prop_readMaybeNotFound = property $ do
    keyParts <- forAll $ Gen.list (Range.linear 1 3) genKeyPart

    result <- evalIO $ withTempStorage $ \storage ->
        Storage.readMaybe storage keyParts :: IO (Maybe Text)

    result === Nothing

-- | Property: readMaybe returns Just for existing key
prop_readMaybeFound :: Property
prop_readMaybeFound = property $ do
    keyParts <- forAll $ Gen.list (Range.linear 1 3) genKeyPart
    val <- forAll genTestValue

    result <- evalIO $ withTempStorage $ \storage -> do
        Storage.write storage keyParts val
        Storage.readMaybe storage keyParts

    result === Just val

-- | Property: writeCached with same directory is faster (uses cache)
prop_writeCachedBatch :: Property
prop_writeCachedBatch = property $ do
    prefix <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    count <- forAll $ Gen.int (Range.linear 5 20)

    indices <- forAll $ pure [1 :: Int .. count]
    keys <- evalIO $ withTempStorage $ \storage -> do
        cache <- Storage.newDirCache
        -- Write multiple values with the same prefix using cache
        mapM_
            ( \i -> do
                let key = [prefix, T.pack (show i)]
                Storage.writeCached cache storage key (object ["index" .= i])
            )
            indices
        -- List all keys with prefix
        Storage.list storage [prefix]

    -- Should find all the keys we created
    listLength keys === count

-- ═══════════════════════════════════════════════════════════════════════════
-- Helper Functions
-- ═══════════════════════════════════════════════════════════════════════════

withTempStorage :: (Storage.StorageConfig -> IO a) -> IO a
withTempStorage action = do
    tmpDir <- createTempDirectory "/tmp" "storage-test"
    result <- Storage.withStorage tmpDir action
    removeDirectoryRecursive tmpDir
    pure result

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

-- | All storage property tests
tests :: TestTree
tests =
    testGroup
        "Storage Property Tests"
        [ testGroup
            "Pure Functions"
            [ testProperty "keyPath ends with .json" prop_keyPathEndsWithJson
            , testProperty "keyPath starts with storage dir" prop_keyPathStartsWithDir
            , testProperty "keyPath contains all key parts" prop_keyPathContainsAllParts
            , testProperty "keyToRelativeParts roundtrip" prop_keyToRelativePartsRoundtrip
            ]
        , testGroup
            "Write Operations"
            [ testProperty "write/read identity" prop_writeReadIdentity
            , testProperty "writeAtomic/read identity" prop_writeAtomicReadIdentity
            , testProperty "writeCached/read identity" prop_writeCachedReadIdentity
            , testProperty "writeCached batch" prop_writeCachedBatch
            ]
        , testGroup
            "Read Operations"
            [ testProperty "readMaybe returns Nothing for missing" prop_readMaybeNotFound
            , testProperty "readMaybe returns Just for existing" prop_readMaybeFound
            ]
        , testGroup
            "Update and Delete"
            [ testProperty "update modifies value" prop_updateModifies
            , testProperty "remove deletes value" prop_removeDeletes
            , testProperty "remove leaves no keys" prop_removeListEmpty
            ]
        , testGroup
            "List Operations"
            [ testProperty "list with prefix" prop_listWithPrefix
            , testProperty "list respects prefix" prop_listRespectsPrefix
            ]
        ]
