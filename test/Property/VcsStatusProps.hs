{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.VcsStatusProps
Description : Property tests for Vcs.Status module

Tests the pure parsing functions in the VCS Status module using
property-based testing with Hedgehog.
-}
module Property.VcsStatusProps where

import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Vcs.Status qualified as VcsStatus

-- ═══════════════════════════════════════════════════════════════════════════
-- parseNumstat properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: parseNumstat parses valid numstat lines correctly
prop_parseNumstatValid :: Property
prop_parseNumstatValid = property $ do
    added <- forAll $ Gen.int (Range.linear 0 1000)
    removed <- forAll $ Gen.int (Range.linear 0 1000)
    path <- forAll genPath
    let line = T.pack (show added) <> "\t" <> T.pack (show removed) <> "\t" <> path
    let result = VcsStatus.parseNumstat line
    Map.lookup path result === Just (added, removed)

-- | Property: parseNumstat returns empty for empty input
prop_parseNumstatEmpty :: Property
prop_parseNumstatEmpty = property $ do
    VcsStatus.parseNumstat "" === Map.empty

-- | Property: parseNumstatLine returns Nothing for binary files (- - path)
prop_parseNumstatLineBinary :: Property
prop_parseNumstatLineBinary = property $ do
    path <- forAll genPath
    let line = "-\t-\t" <> path
    VcsStatus.parseNumstatLine line === Nothing

-- | Property: parseNumstatLine returns Nothing for malformed lines
prop_parseNumstatLineMalformed :: Property
prop_parseNumstatLineMalformed = property $ do
    -- Missing tabs
    VcsStatus.parseNumstatLine "10 5 file.txt" === Nothing
    -- Not enough parts
    VcsStatus.parseNumstatLine "10\tfile.txt" === Nothing
    -- Non-numeric values
    VcsStatus.parseNumstatLine "abc\tdef\tfile.txt" === Nothing

-- | Property: parseNumstat handles multiple lines
prop_parseNumstatMultipleLines :: Property
prop_parseNumstatMultipleLines = property $ do
    let input = "10\t5\tfile1.txt\n20\t15\tfile2.txt\n0\t100\tfile3.txt"
    let result = VcsStatus.parseNumstat input
    Map.size result === 3
    Map.lookup "file1.txt" result === Just (10, 5)
    Map.lookup "file2.txt" result === Just (20, 15)
    Map.lookup "file3.txt" result === Just (0, 100)

-- ═══════════════════════════════════════════════════════════════════════════
-- parseStatusCode properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: parseStatusCode is idempotent for known codes
prop_parseStatusCodeIdempotent :: Property
prop_parseStatusCodeIdempotent = property $ do
    code <- forAll genStatus
    let result = VcsStatus.parseStatusCode code
    -- Result should be one of the allowed statuses (OpenAPI schema)
    assert $ result `elem` ["added", "deleted", "modified"]

-- | Property: parseStatusCode returns "added" for "??" (untracked)
prop_parseStatusCodeUntracked :: Property
prop_parseStatusCodeUntracked = property $ do
    VcsStatus.parseStatusCode "??" === "added"

-- | Property: parseStatusCode returns "added" for "A " codes
prop_parseStatusCodeAdded :: Property
prop_parseStatusCodeAdded = property $ do
    VcsStatus.parseStatusCode "A " === "added"
    VcsStatus.parseStatusCode " A" === "added"
    VcsStatus.parseStatusCode "AA" === "added"

-- | Property: parseStatusCode returns "deleted" for "D " codes
prop_parseStatusCodeDeleted :: Property
prop_parseStatusCodeDeleted = property $ do
    VcsStatus.parseStatusCode "D " === "deleted"
    VcsStatus.parseStatusCode " D" === "deleted"

-- | Property: Any code containing 'M' gives "modified"
prop_parseStatusCodeModified :: Property
prop_parseStatusCodeModified = property $ do
    code <- forAll $ Gen.element ["M ", " M", "MM"]
    VcsStatus.parseStatusCode code === "modified"

-- | Property: Unknown codes default to "modified"
prop_parseStatusCodeUnknown :: Property
prop_parseStatusCodeUnknown = property $ do
    VcsStatus.parseStatusCode "XY" === "modified"
    VcsStatus.parseStatusCode "  " === "modified"

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
            "parseNumstat"
            [ testProperty "parses valid numstat lines" prop_parseNumstatValid
            , testProperty "returns empty for empty input" prop_parseNumstatEmpty
            , testProperty "returns Nothing for binary files" prop_parseNumstatLineBinary
            , testProperty "returns Nothing for malformed lines" prop_parseNumstatLineMalformed
            , testProperty "handles multiple lines" prop_parseNumstatMultipleLines
            ]
        , testGroup
            "parseStatusCode"
            [ testProperty "idempotent known codes" prop_parseStatusCodeIdempotent
            , testProperty "added for ??" prop_parseStatusCodeUntracked
            , testProperty "added for A codes" prop_parseStatusCodeAdded
            , testProperty "deleted for D codes" prop_parseStatusCodeDeleted
            , testProperty "modified for M codes" prop_parseStatusCodeModified
            , testProperty "unknown defaults to modified" prop_parseStatusCodeUnknown
            ]
        , testGroup
            "parseBranchName"
            [ testProperty "empty returns Nothing" prop_parseBranchNameEmpty
            , testProperty "HEAD returns Nothing" prop_parseBranchNameHead
            , testProperty "valid names return Just" prop_parseBranchNameValid
            , testProperty "strips whitespace" prop_parseBranchNameStrips
            , testProperty "whitespace-only returns Nothing" prop_parseBranchNameWhitespaceOnly
            ]
        ]
