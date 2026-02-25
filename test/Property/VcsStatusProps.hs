{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.VcsStatusProps
Description : Property tests for Vcs.Status module

Tests the pure parsing functions in the VCS Status module using
property-based testing with Hedgehog.
-}
module Property.VcsStatusProps where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Helpers (listLength)
import Test.Tasty
import Test.Tasty.Hedgehog
import Vcs.Status qualified as VcsStatus

prop_parseStatusMapping :: Property
prop_parseStatusMapping = property $ do
    status <- forAll genStatus
    path <- forAll genPath
    let line = status <> " " <> path
    let parsed = VcsStatus.parsePorcelain line
    case parsed of
        [s] -> do
            VcsStatus.fsPath s === path
            VcsStatus.fsStatus s === expected status
        _otherResults -> failure
  where
    expected code
        | code == "??" = "untracked"
        | "U" `T.isInfixOf` code = "unmerged"
        | "A" `T.isInfixOf` code = "added"
        | "D" `T.isInfixOf` code = "deleted"
        | "R" `T.isInfixOf` code = "renamed"
        | "C" `T.isInfixOf` code = "copied"
        | "M" `T.isInfixOf` code = "modified"
        | otherwise = "unknown"

prop_parseRenamePath :: Property
prop_parseRenamePath = property $ do
    oldPath <- forAll genPath
    newPath <- forAll genPath
    let line = "R  " <> oldPath <> " -> " <> newPath
    let parsed = VcsStatus.parsePorcelain line
    case parsed of
        [s] -> VcsStatus.fsPath s === newPath
        _otherResults -> failure

prop_parseCountMatchesLines :: Property
prop_parseCountMatchesLines = property $ do
    count <- forAll $ Gen.int (Range.linear 1 20)
    let lines' = replicate count "?? file.txt"
    let input = T.intercalate "\n" lines'
    let result = VcsStatus.parsePorcelain input
    listLength result === count

prop_statusInAllowedSet :: Property
prop_statusInAllowedSet = property $ do
    code <- forAll $ Gen.element ["??", "U ", "A ", "D ", "R ", "C ", "M ", "XY"]
    let input = code <> " file.txt"
    let result = VcsStatus.parsePorcelain input
    let allowed = ["untracked", "unmerged", "added", "deleted", "renamed", "copied", "modified", "unknown"]
    assert $ all (\status -> VcsStatus.fsStatus status `elem` allowed) result

prop_parsePorcelainEmpty :: Property
prop_parsePorcelainEmpty = property $ do
    VcsStatus.parsePorcelain "" === []

-- ═══════════════════════════════════════════════════════════════════════════
-- parseStatusCode properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: parseStatusCode is idempotent for known codes
prop_parseStatusCodeIdempotent :: Property
prop_parseStatusCodeIdempotent = property $ do
    code <- forAll genStatus
    let result = VcsStatus.parseStatusCode code
    -- Result should be one of the known statuses
    assert $ result `elem` ["untracked", "unmerged", "added", "deleted", "renamed", "copied", "modified", "unknown"]

-- | Property: parseStatusCode returns "untracked" only for "??"
prop_parseStatusCodeUntracked :: Property
prop_parseStatusCodeUntracked = property $ do
    VcsStatus.parseStatusCode "??" === "untracked"

-- | Property: Any code containing 'M' gives "modified" (unless it's "??" or contains higher-priority chars)
prop_parseStatusCodeModified :: Property
prop_parseStatusCodeModified = property $ do
    -- Use codes that only contain 'M' without higher-priority characters
    code <- forAll $ Gen.element ["M ", " M", "MM"]
    VcsStatus.parseStatusCode code === "modified"

-- ═══════════════════════════════════════════════════════════════════════════
-- extractFinalPath properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: extractFinalPath preserves paths without arrows
prop_extractFinalPathNoArrow :: Property
prop_extractFinalPathNoArrow = property $ do
    path <- forAll genPath
    -- Ensure no arrow in path
    assert $ not (" -> " `T.isInfixOf` path)
    VcsStatus.extractFinalPath path === path

-- | Property: extractFinalPath extracts the right side of arrow
prop_extractFinalPathWithArrow :: Property
prop_extractFinalPathWithArrow = property $ do
    oldPath <- forAll genPath
    newPath <- forAll genPath
    let combined = oldPath <> " -> " <> newPath
    VcsStatus.extractFinalPath combined === newPath

-- | Property: extractFinalPath handles multiple arrows (takes last segment)
prop_extractFinalPathMultipleArrows :: Property
prop_extractFinalPathMultipleArrows = property $ do
    path1 <- forAll genPath
    path2 <- forAll genPath
    path3 <- forAll genPath
    let combined = path1 <> " -> " <> path2 <> " -> " <> path3
    VcsStatus.extractFinalPath combined === path3

-- ═══════════════════════════════════════════════════════════════════════════
-- parseBranchName properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: parseBranchName returns Nothing for empty string
prop_parseBranchNameEmpty :: Property
prop_parseBranchNameEmpty = property $ do
    VcsStatus.parseBranchName "" === Nothing

-- | Property: parseBranchName returns Nothing for "HEAD"
prop_parseBranchNameHead :: Property
prop_parseBranchNameHead = property $ do
    VcsStatus.parseBranchName "HEAD" === Nothing
    VcsStatus.parseBranchName "  HEAD  " === Nothing
    VcsStatus.parseBranchName "\nHEAD\n" === Nothing

-- | Property: parseBranchName returns Just for valid branch names
prop_parseBranchNameValid :: Property
prop_parseBranchNameValid = property $ do
    branchName <- forAll genBranchName
    -- Ensure not empty and not HEAD
    assert $ not (T.null branchName)
    assert $ branchName /= "HEAD"
    let result = VcsStatus.parseBranchName branchName
    assert $ isJust result
    result === Just branchName

-- | Property: parseBranchName strips whitespace
prop_parseBranchNameStrips :: Property
prop_parseBranchNameStrips = property $ do
    branchName <- forAll genBranchName
    assert $ not (T.null branchName) && branchName /= "HEAD"
    let withWhitespace = "  " <> branchName <> "  \n"
    VcsStatus.parseBranchName withWhitespace === Just branchName

-- | Property: parseBranchName handles whitespace-only as empty
prop_parseBranchNameWhitespaceOnly :: Property
prop_parseBranchNameWhitespaceOnly = property $ do
    whitespace <- forAll $ Gen.element ["", " ", "  ", "\n", "\t", "  \n  "]
    assert $ isNothing (VcsStatus.parseBranchName whitespace)

-- ═══════════════════════════════════════════════════════════════════════════
-- parseLine properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: parseLine correctly splits code and path
prop_parseLineStructure :: Property
prop_parseLineStructure = property $ do
    code <- forAll genStatus
    path <- forAll genPath
    let line = code <> " " <> path
    let result = VcsStatus.parseLine line
    VcsStatus.fsPath result === path
    VcsStatus.fsStatus result === VcsStatus.parseStatusCode code

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

genStatus :: Gen Text
genStatus = Gen.element ["??", " M", "M ", "A ", " D", "R ", "C ", "U "]

genPath :: Gen Text
genPath = do
    name <- Gen.text (Range.linear 1 12) Gen.alphaNum
    ext <- Gen.text (Range.linear 1 3) Gen.alphaNum
    pure (name <> "." <> ext)

-- | Generate valid branch names (excluding HEAD and empty)
genBranchName :: Gen Text
genBranchName = do
    -- Start with a letter to avoid special names
    first <- Gen.text (Range.singleton 1) Gen.alpha
    rest <- Gen.text (Range.linear 0 20) (Gen.element $ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['-', '_', '/'])
    let name = first <> rest
    -- Filter out HEAD
    if name == "HEAD" then genBranchName else pure name

-- ═══════════════════════════════════════════════════════════════════════════
-- Test tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "VCS Status Property Tests"
        [ testGroup
            "parsePorcelain"
            [ testProperty "parse status mapping" prop_parseStatusMapping
            , testProperty "parse rename path" prop_parseRenamePath
            , testProperty "parse count matches lines" prop_parseCountMatchesLines
            , testProperty "status in allowed set" prop_statusInAllowedSet
            , testProperty "parse empty" prop_parsePorcelainEmpty
            ]
        , testGroup
            "parseStatusCode"
            [ testProperty "idempotent known codes" prop_parseStatusCodeIdempotent
            , testProperty "untracked for ??" prop_parseStatusCodeUntracked
            , testProperty "modified for M codes" prop_parseStatusCodeModified
            ]
        , testGroup
            "extractFinalPath"
            [ testProperty "preserves paths without arrow" prop_extractFinalPathNoArrow
            , testProperty "extracts right side of arrow" prop_extractFinalPathWithArrow
            , testProperty "handles multiple arrows" prop_extractFinalPathMultipleArrows
            ]
        , testGroup
            "parseBranchName"
            [ testProperty "empty returns Nothing" prop_parseBranchNameEmpty
            , testProperty "HEAD returns Nothing" prop_parseBranchNameHead
            , testProperty "valid names return Just" prop_parseBranchNameValid
            , testProperty "strips whitespace" prop_parseBranchNameStrips
            , testProperty "whitespace-only returns Nothing" prop_parseBranchNameWhitespaceOnly
            ]
        , testGroup
            "parseLine"
            [ testProperty "correctly splits code and path" prop_parseLineStructure
            ]
        ]
