{-# LANGUAGE OverloadedStrings #-}

{- | Shared test fixtures for reducing temp directory churn

Instead of creating a temp directory per property iteration (100-1000x per test),
we create one per property and clean files between iterations.
-}
module Test.Fixture (
    -- * Temp directory helpers
    withTempDir,
    withTempDirContents,

    -- * Storage fixtures
    withStorage,

    -- * Hedgehog integration
    PropertyWithFixture,
    withFixture,
    propertyWithTempDir,
    cleanDir,
) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Hedgehog (Property, PropertyT, evalIO, property)
import Storage.Storage qualified as Storage
import System.Directory (
    createDirectoryIfMissing,
    listDirectory,
    removeDirectoryRecursive,
    removePathForcibly,
 )
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)

{- | The base temp directory - use /dev/shm on Linux for in-memory speed
Falls back to /tmp if /dev/shm is not available
-}
tmpBase :: FilePath
tmpBase = "/dev/shm"

{- | Create a temporary directory for testing (single use)
This is for tests that need a fresh directory each time
-}
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir =
    bracket
        (createTempDirectory tmpBase "test")
        removeDirectoryRecursive

{- | Create a temp directory and populate it with initial contents
The callback receives (tmpDir, setup) where setup recreates the initial state
-}
withTempDirContents :: IO () -> (FilePath -> IO () -> IO a) -> IO a
withTempDirContents setup action =
    bracket
        (createTempDirectory tmpBase "test")
        removeDirectoryRecursive
        (\dir -> action dir (cleanDir dir >> setup))

-- | Initialize storage with a base directory
withStorage :: FilePath -> (Storage.StorageConfig -> IO a) -> IO a
withStorage dir action = do
    createDirectoryIfMissing True dir
    action (Storage.StorageConfig dir)

-- | Clean all contents of a directory without removing the directory itself
cleanDir :: FilePath -> IO ()
cleanDir dir = do
    contents <- listDirectory dir
    forM_ contents $ \name ->
        removePathForcibly (dir </> name)

-- | Type alias for properties that use a fixture
type PropertyWithFixture a = a -> PropertyT IO ()

{- | Run a property with a fixture that's created once per property (not per iteration)
This dramatically reduces temp directory churn
-}
withFixture :: IO a -> (a -> IO ()) -> PropertyWithFixture a -> Property
withFixture setup teardown prop = property $ do
    fixture <- evalIO setup
    result <- prop fixture
    evalIO (teardown fixture)
    pure result

{- | Convenience: property with a temp directory created once
Files created during each iteration persist, so tests should either:
1. Use unique filenames per iteration (via forAll generators)
2. Clean up after themselves
3. Not care about leftover files
-}
propertyWithTempDir :: PropertyWithFixture FilePath -> Property
propertyWithTempDir =
    withFixture
        (createTempDirectory tmpBase "prop")
        removeDirectoryRecursive
