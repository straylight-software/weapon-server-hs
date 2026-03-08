{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Integration.VcsDiffSpec
Description : Integration tests for Vcs.Diff IO behavior

These tests verify that loadFileDiffs runs git and parses unified diffs.
Tests are skipped if git is not available in PATH.
-}
module Integration.VcsDiffSpec (spec) where

import Data.List (find)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Formatter.Status qualified as Formatter
import System.Directory (removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Fixture (withTempDir)
import Test.Hspec
import Vcs.Diff qualified as Diff

runGit :: FilePath -> [String] -> IO ()
runGit root args = do
    (code, _out, err) <- readProcessWithExitCode "git" ("-C" : root : args) ""
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> expectationFailure ("git failed: " <> err)

spec :: Spec
spec = do
    describe "Vcs.Diff Integration" $ do
        describe "loadFileDiffs" $ do
            it "returns empty list outside a git repo" $ withTempDir $ \tmpDir -> do
                exeCache <- Formatter.newExeCache
                diffs <- Diff.loadFileDiffs exeCache tmpDir
                diffs `shouldBe` []

            it "parses modified and deleted files from git diff output" $ withTempDir $ \tmpDir -> do
                exeCache <- Formatter.newExeCache
                -- Initialize repo
                (code, _out, _err) <- readProcessWithExitCode "git" ["-C", tmpDir, "init", "-b", "main"] ""
                case code of
                    ExitSuccess -> pure ()
                    ExitFailure _ -> runGit tmpDir ["init"]
                runGit tmpDir ["config", "user.email", "test@test.com"]
                runGit tmpDir ["config", "user.name", "Test"]

                let modifyFile = tmpDir </> "modify.txt"
                let deleteFile = tmpDir </> "delete.txt"
                TIO.writeFile modifyFile "alpha\nbeta\n"
                TIO.writeFile deleteFile "gone\nline2\n"
                runGit tmpDir ["add", "modify.txt", "delete.txt"]
                runGit tmpDir ["commit", "-m", "init"]

                -- Modify and delete files (unstaged changes)
                TIO.writeFile modifyFile "alpha\nbeta\ncharlie\n"
                removeFile deleteFile

                diffs <- Diff.loadFileDiffs exeCache tmpDir
                length diffs `shouldBe` 2

                let findDiff name = find (\fd -> Diff.fdiFile fd == name) diffs
                case findDiff "modify.txt" of
                    Nothing -> expectationFailure "Expected diff for modify.txt"
                    Just fd -> do
                        Diff.fdiStatus fd `shouldBe` Diff.FileModified
                        Diff.fdiAdditions fd `shouldBe` 1
                        Diff.fdiDeletions fd `shouldBe` 0
                        T.isInfixOf "diff --git" (Diff.fdiPatch fd) `shouldBe` True

                case findDiff "delete.txt" of
                    Nothing -> expectationFailure "Expected diff for delete.txt"
                    Just fd -> do
                        Diff.fdiStatus fd `shouldBe` Diff.FileDeleted
                        Diff.fdiAdditions fd `shouldBe` 0
                        Diff.fdiDeletions fd `shouldBe` 2
                        T.isInfixOf "diff --git" (Diff.fdiPatch fd) `shouldBe` True
