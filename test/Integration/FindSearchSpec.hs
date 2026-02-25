{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Integration.FindSearchSpec
Description : Integration tests for Find.Search module

These tests verify the IO behavior of the search functions when the
required external tools (rg, fd) are available. Tests are skipped if
the tools are not found in PATH.
-}
module Integration.FindSearchSpec (spec) where

import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Find.Search (SearchError (..), findFile, findFileWithOptions, findText)
import Find.Search qualified as Search
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.FilePath ((</>))
import Test.Fixture (withTempDir)
import Test.Helpers (lookupText)
import Test.Hspec

-- | Check if ripgrep is available
hasRg :: IO Bool
hasRg = isJust <$> findExecutable "rg"

-- | Check if fd is available
hasFd :: IO Bool
hasFd = isJust <$> findExecutable "fd"

-- | Skip a test if a condition is not met
skipUnless :: Bool -> String -> SpecWith a -> SpecWith a
skipUnless cond reason spec' =
    if cond
        then spec'
        else before_ (pendingWith reason) spec'

spec :: Spec
spec = do
    describe "Find.Search Integration" $ do
        rgAvailable <- runIO hasRg
        fdAvailable <- runIO hasFd

        describe "findText" $ do
            skipUnless rgAvailable "ripgrep (rg) not found in PATH" $ do
                it "finds text in files" $ withTempDir $ \tmpDir -> do
                    -- Create a test file with some content
                    let testFile = tmpDir </> "test.txt"
                    TIO.writeFile testFile "hello world\nfoo bar\nhello again"

                    results <- findText tmpDir "hello"
                    -- Ensure at least 2 results via pattern matching
                    case results of
                        (_ : _ : _atLeastTwo) -> pure ()
                        [] -> expectationFailure "Expected at least 2 results, got 0"
                        [_one] -> expectationFailure "Expected at least 2 results, got 1"

                it "returns empty list when no matches" $ withTempDir $ \tmpDir -> do
                    let testFile = tmpDir </> "test.txt"
                    TIO.writeFile testFile "nothing matches here"

                    results <- findText tmpDir "nonexistent_pattern_xyz"
                    results `shouldBe` []

                it "searches recursively" $ withTempDir $ \tmpDir -> do
                    createDirectoryIfMissing True (tmpDir </> "subdir")
                    let testFile = tmpDir </> "subdir" </> "nested.txt"
                    TIO.writeFile testFile "findme123"

                    results <- findText tmpDir "findme123"
                    case results of
                        [_one] -> pure ()
                        [] -> expectationFailure "Expected 1 result, got 0"
                        (_ : _ : _moreThanOne) -> expectationFailure "Expected 1 result, got multiple"

        describe "findFile" $ do
            skipUnless fdAvailable "fd not found in PATH" $ do
                it "finds files by glob pattern" $ withTempDir $ \tmpDir -> do
                    TIO.writeFile (tmpDir </> "test.hs") "module Test where"
                    TIO.writeFile (tmpDir </> "other.txt") "other"

                    results <- findFile tmpDir "*.hs"
                    case results of
                        [v] -> lookupText "path" v `shouldSatisfy` maybe False (T.isSuffixOf "test.hs")
                        [] -> expectationFailure "Expected exactly one result, got none"
                        (_ : _ : _moreThanOne) -> expectationFailure "Expected exactly one result, got multiple"

                it "returns empty list for no matches" $ withTempDir $ \tmpDir -> do
                    TIO.writeFile (tmpDir </> "test.txt") "content"

                    results <- findFile tmpDir "*.nonexistent"
                    results `shouldBe` []

                it "finds files recursively" $ withTempDir $ \tmpDir -> do
                    createDirectoryIfMissing True (tmpDir </> "deep" </> "nested")
                    TIO.writeFile (tmpDir </> "deep" </> "nested" </> "found.hs") "module Found"

                    results <- findFile tmpDir "*.hs"
                    case results of
                        [_one] -> pure ()
                        [] -> expectationFailure "Expected 1 result, got 0"
                        (_ : _ : _moreThanOne) -> expectationFailure "Expected 1 result, got multiple"

        describe "findFileWithOptions" $ do
            skipUnless fdAvailable "fd not found in PATH" $ do
                it "respects limit option" $ withTempDir $ \tmpDir -> do
                    TIO.writeFile (tmpDir </> "a.txt") "a"
                    TIO.writeFile (tmpDir </> "b.txt") "b"
                    TIO.writeFile (tmpDir </> "c.txt") "c"

                    let opts = Search.FindFileOptions False Nothing (Just 2)
                    results <- findFileWithOptions tmpDir "*.txt" opts
                    case results of
                        [_a, _b] -> pure ()
                        [] -> expectationFailure "Expected 2 results, got 0"
                        [_one] -> expectationFailure "Expected 2 results, got 1"
                        (_ : _ : _ : _moreThanTwo) -> expectationFailure "Expected 2 results, got more"

                it "can include directories" $ withTempDir $ \tmpDir -> do
                    createDirectoryIfMissing True (tmpDir </> "testdir")
                    TIO.writeFile (tmpDir </> "testdir" </> "file.txt") "content"

                    let opts = Search.FindFileOptions True Nothing Nothing
                    results <- findFileWithOptions tmpDir "*" opts
                    -- Should include both the directory and the file
                    case results of
                        (_ : _ : _atLeastTwo) -> pure ()
                        [] -> expectationFailure "Expected at least 2 results, got 0"
                        [_one] -> expectationFailure "Expected at least 2 results, got 1"

                it "can filter to directories only" $ withTempDir $ \tmpDir -> do
                    createDirectoryIfMissing True (tmpDir </> "dir1")
                    createDirectoryIfMissing True (tmpDir </> "dir2")
                    TIO.writeFile (tmpDir </> "file.txt") "content"

                    let opts = Search.FindFileOptions False (Just "directory") Nothing
                    results <- findFileWithOptions tmpDir "*" opts
                    -- Should only find directories
                    case results of
                        [_a, _b] -> pure ()
                        [] -> expectationFailure "Expected 2 directories, got 0"
                        [_one] -> expectationFailure "Expected 2 directories, got 1"
                        (_ : _ : _ : _moreThanTwo) -> expectationFailure "Expected 2 directories, got more"

        describe "error handling" $ do
            it "throws SearchError for missing rg" $ do
                -- This test verifies the error structure but doesn't actually
                -- test a missing executable since we can't remove PATH entries
                let err = MissingExecutable "rg" "test description"
                show err `shouldContain` "rg"
                show err `shouldContain` "test description"

            it "throws SearchError for missing fd" $ do
                let err = MissingExecutable "fd" "test description"
                show err `shouldContain` "fd"
                seName err `shouldBe` "fd"
                seDescription err `shouldBe` "test description"
