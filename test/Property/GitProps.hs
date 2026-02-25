{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for Util.Git

Tests the pure helpers for git argument building and result parsing.
IO behavior (actual git execution) is tested via integration tests
since it requires a real git repository.
-}
module Property.GitProps where

import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Exit (ExitCode (..))
import Test.Tasty
import Test.Tasty.Hedgehog
import Util.Git

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Helper Properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: buildGitArgs prepends -C and root to args
prop_buildGitArgsFormat :: Property
prop_buildGitArgsFormat = property $ do
    root <- forAll genFilePath
    args <- forAll $ Gen.list (Range.linear 0 5) genArg

    let result = buildGitArgs root args
    -- First two elements should be -C and root
    take 2 result === ["-C", root]
    -- Rest should be the original args
    drop 2 result === args

-- | Property: buildGitArgs with empty args returns just -C and root
prop_buildGitArgsEmptyArgs :: Property
prop_buildGitArgsEmptyArgs = property $ do
    root <- forAll genFilePath
    let result = buildGitArgs root []
    result === ["-C", root]

-- | Property: parseGitResult returns Just on ExitSuccess
prop_parseGitResultSuccess :: Property
prop_parseGitResultSuccess = property $ do
    output <- forAll genOutput
    let result = parseGitResult ExitSuccess output
    result === Just (T.pack output)

-- | Property: parseGitResult returns Nothing on ExitFailure
prop_parseGitResultFailure :: Property
prop_parseGitResultFailure = property $ do
    exitCode <- forAll $ Gen.int (Range.linear 1 255)
    output <- forAll genOutput
    let result = parseGitResult (ExitFailure exitCode) output
    result === Nothing

-- | Property: parseGitResult preserves output exactly on success
prop_parseGitResultPreservesOutput :: Property
prop_parseGitResultPreservesOutput = property $ do
    output <- forAll genOutput
    let result = parseGitResult ExitSuccess output
    case result of
        Just t -> T.unpack t === output
        Nothing -> failure

-- | Property: parseGitResult handles empty output
prop_parseGitResultEmpty :: Property
prop_parseGitResultEmpty = property $ do
    let result = parseGitResult ExitSuccess ""
    result === Just ""

-- | Property: parseGitResult handles multi-line output
prop_parseGitResultMultiline :: Property
prop_parseGitResultMultiline = property $ do
    lines' <- forAll $ Gen.list (Range.linear 1 10) genLine
    let output = unlines lines'
    let result = parseGitResult ExitSuccess output
    case result of
        Just t -> T.unpack t === output
        Nothing -> failure

-- | Property: buildGitArgs handles paths with spaces
prop_buildGitArgsPathsWithSpaces :: Property
prop_buildGitArgsPathsWithSpaces = property $ do
    -- Generate a path that includes spaces
    root <- forAll $ Gen.string (Range.linear 5 30) $ Gen.element $ ['a' .. 'z'] ++ [' ', '/', '-']
    args <- forAll $ Gen.list (Range.linear 1 3) genArg

    let result = buildGitArgs root args
    -- The path should be preserved exactly (spaces included) - use pattern matching instead of !!
    case result of
        (_ : pathArg : _) -> pathArg === root
        _other -> failure

-- ═══════════════════════════════════════════════════════════════════════════
-- Property: buildGitArgs and common git commands
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: status command format
prop_gitStatusCommand :: Property
prop_gitStatusCommand = property $ do
    root <- forAll genFilePath
    let result = buildGitArgs root ["status", "--porcelain"]
    result === ["-C", root, "status", "--porcelain"]

-- | Property: branch command format
prop_gitBranchCommand :: Property
prop_gitBranchCommand = property $ do
    root <- forAll genFilePath
    let result = buildGitArgs root ["branch", "--show-current"]
    result === ["-C", root, "branch", "--show-current"]

-- | Property: rev-parse command format
prop_gitRevParseCommand :: Property
prop_gitRevParseCommand = property $ do
    root <- forAll genFilePath
    let result = buildGitArgs root ["rev-parse", "HEAD"]
    result === ["-C", root, "rev-parse", "HEAD"]

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

-- | Generate a file path
genFilePath :: Gen FilePath
genFilePath = Gen.string (Range.linear 1 50) $ Gen.element $ ['a' .. 'z'] ++ ['0' .. '9'] ++ "/_-."

-- | Generate a git argument
genArg :: Gen String
genArg = Gen.string (Range.linear 1 20) $ Gen.element $ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"

-- | Generate command output
genOutput :: Gen String
genOutput = Gen.string (Range.linear 0 200) $ Gen.element $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " \n\t-_/"

-- | Generate a single line of output
genLine :: Gen String
genLine = Gen.string (Range.linear 1 80) $ Gen.element $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " -_/"

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Git Property Tests"
        [ testGroup
            "buildGitArgs"
            [ testProperty "prepends -C and root" prop_buildGitArgsFormat
            , testProperty "empty args" prop_buildGitArgsEmptyArgs
            , testProperty "handles paths with spaces" prop_buildGitArgsPathsWithSpaces
            , testProperty "status command" prop_gitStatusCommand
            , testProperty "branch command" prop_gitBranchCommand
            , testProperty "rev-parse command" prop_gitRevParseCommand
            ]
        , testGroup
            "parseGitResult"
            [ testProperty "returns Just on success" prop_parseGitResultSuccess
            , testProperty "returns Nothing on failure" prop_parseGitResultFailure
            , testProperty "preserves output exactly" prop_parseGitResultPreservesOutput
            , testProperty "handles empty output" prop_parseGitResultEmpty
            , testProperty "handles multi-line output" prop_parseGitResultMultiline
            ]
        ]
