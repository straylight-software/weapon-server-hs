{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for Util.FileSystem

Tests the pure helpers for path building and flattening.
IO behavior (actual filesystem access) is tested via integration tests
since it requires real filesystem operations.
-}
module Property.FileSystemProps where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.FilePath ((</>))
import Test.Fixture (propertyWithTempDir)
import Test.Helpers (listLength)
import Test.Tasty
import Test.Tasty.Hedgehog
import Util.FileSystem

import Control.Monad (forM_)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.IO (IOMode (..), hClose, openFile)

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Helper Properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: buildPath combines parent and name correctly
prop_buildPath :: Property
prop_buildPath = property $ do
    parent <- forAll genDirPath
    name <- forAll genFileName
    let result = buildPath parent name
    result === parent </> name

-- | Property: buildPath with empty name
prop_buildPathEmptyName :: Property
prop_buildPathEmptyName = property $ do
    parent <- forAll genDirPath
    let result = buildPath parent ""
    -- FilePath semantics: "foo" </> "" = "foo/"
    result === parent </> ""

-- | Property: flattenPaths with empty list
prop_flattenPathsEmpty :: Property
prop_flattenPathsEmpty = property $ do
    let result = flattenPaths []
    result === []

-- | Property: flattenPaths with single list
prop_flattenPathsSingle :: Property
prop_flattenPathsSingle = property $ do
    paths <- forAll $ Gen.list (Range.linear 0 10) genFilePath
    let result = flattenPaths [paths]
    result === paths

-- | Property: flattenPaths preserves all elements
prop_flattenPathsPreservesAll :: Property
prop_flattenPathsPreservesAll = property $ do
    pathLists <-
        forAll $
            Gen.list (Range.linear 0 5) $
                Gen.list (Range.linear 0 5) genFilePath
    let result = flattenPaths pathLists
    let expected = concat pathLists
    result === expected

-- | Property: flattenPaths preserves order
prop_flattenPathsOrder :: Property
prop_flattenPathsOrder = property $ do
    list1 <- forAll $ Gen.list (Range.linear 1 3) genFilePath
    list2 <- forAll $ Gen.list (Range.linear 1 3) genFilePath
    let result = flattenPaths [list1, list2]
    -- First elements should be from list1, then list2
    take (listLength list1) result === list1
    drop (listLength list1) result === list2

-- | Property: flattenPaths handles empty sublists
prop_flattenPathsEmptySublists :: Property
prop_flattenPathsEmptySublists = property $ do
    list1 <- forAll $ Gen.list (Range.linear 1 3) genFilePath
    list2 <- forAll $ Gen.list (Range.linear 1 3) genFilePath
    let result = flattenPaths [list1, [], list2, []]
    result === list1 ++ list2

-- | Property: flattenPaths length equals sum of sublists lengths
prop_flattenPathsLength :: Property
prop_flattenPathsLength = property $ do
    pathLists <-
        forAll $
            Gen.list (Range.linear 0 5) $
                Gen.list (Range.linear 0 5) genFilePath
    let result = flattenPaths pathLists
    -- Use foldl' to sum the lengths (avoids sum on lists)
    let expectedLen = foldl' (+) 0 (map listLength pathLists)
    listLength result === expectedLen

-- ═══════════════════════════════════════════════════════════════════════════
-- DirectoryEntry Properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: FileEntry equality
prop_fileEntryEquality :: Property
prop_fileEntryEquality = property $ do
    path1 <- forAll genFilePath
    path2 <- forAll genFilePath
    (FileEntry path1 == FileEntry path2) === (path1 == path2)

-- | Property: DirEntry equality
prop_dirEntryEquality :: Property
prop_dirEntryEquality = property $ do
    path1 <- forAll genDirPath
    path2 <- forAll genDirPath
    (DirEntry path1 == DirEntry path2) === (path1 == path2)

-- | Property: FileEntry /= DirEntry for same path
prop_entryTypeDistinct :: Property
prop_entryTypeDistinct = property $ do
    path <- forAll genFilePath
    assert $ FileEntry path /= DirEntry path

-- ═══════════════════════════════════════════════════════════════════════════
-- IO Properties (with temp directories)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: listDirectoryRecursive returns empty for empty directory
prop_ioEmptyDir :: Property
prop_ioEmptyDir = propertyWithTempDir $ \tmpDir -> do
    result <- evalIO $ listDirectoryRecursive tmpDir
    result === []

-- | Property: listDirectoryRecursive finds a single file
prop_ioSingleFile :: Property
prop_ioSingleFile = propertyWithTempDir $ \tmpDir -> do
    fileName <- forAll genFileName
    let filePath = tmpDir </> fileName

    evalIO $ do
        h <- openFile filePath WriteMode
        hClose h

    result <- evalIO $ listDirectoryRecursive tmpDir
    result === [filePath]

-- | Property: listDirectoryRecursive finds files in subdirectories
prop_ioNestedFiles :: Property
prop_ioNestedFiles = propertyWithTempDir $ \tmpDir -> do
    let subDir = tmpDir </> "subdir"
    let file1 = tmpDir </> "file1.txt"
    let file2 = subDir </> "file2.txt"

    evalIO $ do
        createDirectoryIfMissing True subDir
        h1 <- openFile file1 WriteMode
        hClose h1
        h2 <- openFile file2 WriteMode
        hClose h2

    result <- evalIO $ listDirectoryRecursive tmpDir

    -- Should contain both files (order may vary)
    assert $ listLength result == 2
    assert $ file1 `elem` result
    assert $ file2 `elem` result

-- | Property: listDirectoryRecursive finds all created files
prop_ioAllFilesFound :: Property
prop_ioAllFilesFound = propertyWithTempDir $ \tmpDir -> do
    fileNames <- forAll $ Gen.list (Range.linear 1 5) genFileName
    let filePaths = map (tmpDir </>) fileNames

    evalIO $ forM_ filePaths $ \fp -> do
        h <- openFile fp WriteMode
        hClose h

    result <- evalIO $ listDirectoryRecursive tmpDir

    -- All files should be found
    forM_ filePaths $ \fp -> do
        assert $ fp `elem` result

-- | Property: listDirectoryRecursive returns only files, not directories
prop_ioOnlyFiles :: Property
prop_ioOnlyFiles = propertyWithTempDir $ \tmpDir -> do
    let subDir = tmpDir </> "subdir"
    let file1 = tmpDir </> "file.txt"

    evalIO $ do
        createDirectoryIfMissing True subDir
        h <- openFile file1 WriteMode
        hClose h

    result <- evalIO $ listDirectoryRecursive tmpDir

    -- Only file1 should be in result, not subDir
    assert $ file1 `elem` result
    assert $ subDir `notElem` result

    -- Verify all results are actually files
    allFiles <- evalIO $ mapM doesFileExist result
    assert $ and allFiles

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

-- | Generate a directory path
genDirPath :: Gen FilePath
genDirPath = Gen.string (Range.linear 1 30) $ Gen.element $ ['a' .. 'z'] ++ ['0' .. '9'] ++ "/_-"

-- | Generate a file path
genFilePath :: Gen FilePath
genFilePath = do
    dir <- genDirPath
    name <- genFileName
    pure $ dir </> name

-- | Generate a simple file name (no path separators)
genFileName :: Gen String
genFileName = do
    base <- Gen.string (Range.linear 1 12) $ Gen.element $ ['a' .. 'z'] ++ ['0' .. '9']
    ext <- Gen.element ["", ".txt", ".hs", ".json"]
    pure $ base ++ ext

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "FileSystem Property Tests"
        [ testGroup
            "Pure Helpers"
            [ testProperty "buildPath combines correctly" prop_buildPath
            , testProperty "buildPath with empty name" prop_buildPathEmptyName
            , testProperty "flattenPaths empty" prop_flattenPathsEmpty
            , testProperty "flattenPaths single" prop_flattenPathsSingle
            , testProperty "flattenPaths preserves all" prop_flattenPathsPreservesAll
            , testProperty "flattenPaths preserves order" prop_flattenPathsOrder
            , testProperty "flattenPaths handles empty sublists" prop_flattenPathsEmptySublists
            , testProperty "flattenPaths length" prop_flattenPathsLength
            ]
        , testGroup
            "DirectoryEntry"
            [ testProperty "FileEntry equality" prop_fileEntryEquality
            , testProperty "DirEntry equality" prop_dirEntryEquality
            , testProperty "entry types distinct" prop_entryTypeDistinct
            ]
        , testGroup
            "IO Operations"
            [ testProperty "empty directory" prop_ioEmptyDir
            , testProperty "single file" prop_ioSingleFile
            , testProperty "nested files" prop_ioNestedFiles
            , testProperty "all files found" prop_ioAllFilesFound
            , testProperty "returns only files" prop_ioOnlyFiles
            ]
        ]
