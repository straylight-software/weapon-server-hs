{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.DiffProps
Description : Property tests for Vcs.Diff module

Tests the pure parsing functions in the VCS Diff module using
property-based testing with Hedgehog.
-}
module Property.DiffProps where

import Control.Monad (forM_)
import Data.List qualified as List
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Session.Types qualified as ST
import Test.Helpers (listLength)
import Test.Tasty
import Test.Tasty.Hedgehog
import Vcs.Diff qualified as Diff

prop_parseNumstatTotals :: Property
prop_parseNumstatTotals = property $ do
    entries <- forAll $ Gen.list (Range.linear 0 20) genEntry
    let text = T.intercalate "\n" (map toLine entries)
    let summary = Diff.parseNumstat text
    let adds = sumInts (map fst entries)
    let dels = sumInts (map snd entries)
    ST.ssAdditions summary === adds
    ST.ssDeletions summary === dels
    ST.ssFiles summary === Just (listLength entries)
  where
    toLine (a, d) = T.pack (show a) <> "\t" <> T.pack (show d) <> "\tfile.txt"

prop_parseNumstatBinary :: Property
prop_parseNumstatBinary = property $ do
    files <- forAll $ Gen.int (Range.linear 1 10)
    let text = T.intercalate "\n" (replicate files "-\t-\tfile.bin")
    let summary = Diff.parseNumstat text
    ST.ssAdditions summary === 0
    ST.ssDeletions summary === 0
    ST.ssFiles summary === Just files

prop_parseNumstatEmpty :: Property
prop_parseNumstatEmpty = property $ do
    let summary = Diff.parseNumstat ""
    ST.ssAdditions summary === 0
    ST.ssDeletions summary === 0
    ST.ssFiles summary === Just 0

-- | Property: parseNumstatFiles returns correct file entries
prop_parseNumstatFilesEntries :: Property
prop_parseNumstatFilesEntries = property $ do
    entries <- forAll $ Gen.list (Range.linear 0 20) genFileEntry
    let text = T.intercalate "\n" (map toFileLine entries)
    let fileDiffs = Diff.parseNumstatFiles text
    listLength fileDiffs === listLength entries
    -- Check first entry if present
    case (entries, fileDiffs) of
        ((adds, dels, fname) : _, fd : _) -> do
            Diff.fdiAdditions fd === adds
            Diff.fdiDeletions fd === dels
            Diff.fdiFile fd === fname
        _other -> success
  where
    toFileLine (a, d, fname) = T.pack (show a) <> "\t" <> T.pack (show d) <> "\t" <> fname

-- | Property: parseNumstatFiles handles binary files
prop_parseNumstatFilesBinary :: Property
prop_parseNumstatFilesBinary = property $ do
    files <- forAll $ Gen.list (Range.linear 1 5) genFileName
    let text = T.intercalate "\n" (map ("-\t-\t" <>) files)
    let fileDiffs = Diff.parseNumstatFiles text
    listLength fileDiffs === listLength files
    -- Binary files have 0 additions and deletions
    forM_ fileDiffs $ \fd -> do
        Diff.fdiAdditions fd === 0
        Diff.fdiDeletions fd === 0

-- | Property: parseNumstatFiles empty input gives empty list
prop_parseNumstatFilesEmpty :: Property
prop_parseNumstatFilesEmpty = property $ do
    let fileDiffs = Diff.parseNumstatFiles ""
    listLength fileDiffs === 0

-- | Property: parseNumstatFiles preserves file paths
prop_parseNumstatFilesPreservesPaths :: Property
prop_parseNumstatFilesPreservesPaths = property $ do
    entries <- forAll $ Gen.list (Range.linear 1 10) genFileEntry
    let text = T.intercalate "\n" (map toFileLine entries)
    let fileDiffs = Diff.parseNumstatFiles text
    let expectedPaths = map (\(_, _, f) -> f) entries
    let actualPaths = map Diff.fdiFile fileDiffs
    actualPaths === expectedPaths
  where
    toFileLine (a, d, fname) = T.pack (show a) <> "\t" <> T.pack (show d) <> "\t" <> fname

-- ═══════════════════════════════════════════════════════════════════════════
-- readNumstatInt properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: readNumstatInt parses valid integers correctly
prop_readNumstatIntValid :: Property
prop_readNumstatIntValid = property $ do
    n <- forAll $ Gen.int (Range.linear 0 10000)
    Diff.readNumstatInt (T.pack (show n)) === n

-- | Property: readNumstatInt returns 0 for non-numeric input
prop_readNumstatIntNonNumeric :: Property
prop_readNumstatIntNonNumeric = property $ do
    txt <- forAll $ Gen.element ["-", "abc", "", "12a", "a12"]
    Diff.readNumstatInt txt === 0

-- | Property: readNumstatInt handles binary file marker "-"
prop_readNumstatIntBinaryMarker :: Property
prop_readNumstatIntBinaryMarker = property $ do
    Diff.readNumstatInt "-" === 0

-- ═══════════════════════════════════════════════════════════════════════════
-- parseNumstatLine properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: parseNumstatLine parses valid entries correctly
prop_parseNumstatLineValid :: Property
prop_parseNumstatLineValid = property $ do
    adds <- forAll $ Gen.int (Range.linear 0 1000)
    dels <- forAll $ Gen.int (Range.linear 0 1000)
    fname <- forAll genFileName
    let fields = [T.pack (show adds), T.pack (show dels), fname]
    Diff.parseNumstatLine fields === (adds, dels)

-- | Property: parseNumstatLine returns (0, 0) for empty fields
prop_parseNumstatLineEmpty :: Property
prop_parseNumstatLineEmpty = property $ do
    Diff.parseNumstatLine [] === (0, 0)
    Diff.parseNumstatLine ["10"] === (0, 0)

-- | Property: parseNumstatLine ignores extra fields
prop_parseNumstatLineExtraFields :: Property
prop_parseNumstatLineExtraFields = property $ do
    adds <- forAll $ Gen.int (Range.linear 0 1000)
    dels <- forAll $ Gen.int (Range.linear 0 1000)
    let fields = [T.pack (show adds), T.pack (show dels), "file.txt", "extra", "fields"]
    Diff.parseNumstatLine fields === (adds, dels)

-- ═══════════════════════════════════════════════════════════════════════════
-- combineDiffResults properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: combineDiffResults returns Nothing when either input is Nothing
prop_combineDiffResultsNothing :: Property
prop_combineDiffResultsNothing = property $ do
    diffText <- forAll $ Gen.text (Range.linear 1 100) Gen.alphaNum
    numText <- forAll genNumstatText

    -- Nothing diff
    assert $ isNothing (Diff.combineDiffResults Nothing (Just numText))
    -- Nothing numstat
    assert $ isNothing (Diff.combineDiffResults (Just diffText) Nothing)
    -- Both Nothing
    assert $ isNothing (Diff.combineDiffResults Nothing Nothing)

-- | Property: combineDiffResults returns Just when both inputs are Just
prop_combineDiffResultsJust :: Property
prop_combineDiffResultsJust = property $ do
    diffText <- forAll $ Gen.text (Range.linear 1 100) Gen.alphaNum
    numText <- forAll genNumstatText
    let result = Diff.combineDiffResults (Just diffText) (Just numText)
    assert $ isJust result
    case result of
        Just (diffResult, _summary) -> diffResult === diffText
        Nothing -> failure

-- | Property: combineDiffResults summary matches parseNumstat
prop_combineDiffResultsSummaryCorrect :: Property
prop_combineDiffResultsSummaryCorrect = property $ do
    diffText <- forAll $ Gen.text (Range.linear 1 100) Gen.alphaNum
    entries <- forAll $ Gen.list (Range.linear 0 10) genEntry
    let numText = T.intercalate "\n" (map toLine entries)
    let result = Diff.combineDiffResults (Just diffText) (Just numText)
    case result of
        Just (_diff, summary) -> do
            summary === Diff.parseNumstat numText
        Nothing -> failure
  where
    toLine (a, d) = T.pack (show a) <> "\t" <> T.pack (show d) <> "\tfile.txt"

-- ═══════════════════════════════════════════════════════════════════════════
-- Consistency properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: parseNumstat and parseNumstatFiles agree on file count
prop_parseNumstatFilesCountConsistent :: Property
prop_parseNumstatFilesCountConsistent = property $ do
    entries <- forAll $ Gen.list (Range.linear 0 20) genFileEntry
    let text = T.intercalate "\n" (map toFileLine entries)
    let summary = Diff.parseNumstat text
    let fileDiffs = Diff.parseNumstatFiles text
    ST.ssFiles summary === Just (listLength fileDiffs)
  where
    toFileLine (a, d, fname) = T.pack (show a) <> "\t" <> T.pack (show d) <> "\t" <> fname

-- | Property: parseNumstat totals equal sum of parseNumstatFiles entries
prop_parseNumstatTotalsMatchFiles :: Property
prop_parseNumstatTotalsMatchFiles = property $ do
    entries <- forAll $ Gen.list (Range.linear 0 20) genFileEntry
    let text = T.intercalate "\n" (map toFileLine entries)
    let summary = Diff.parseNumstat text
    let fileDiffs = Diff.parseNumstatFiles text
    let totalAdds = sumInts (map Diff.fdiAdditions fileDiffs)
    let totalDels = sumInts (map Diff.fdiDeletions fileDiffs)
    ST.ssAdditions summary === totalAdds
    ST.ssDeletions summary === totalDels
  where
    toFileLine (a, d, fname) = T.pack (show a) <> "\t" <> T.pack (show d) <> "\t" <> fname

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

genFileName :: Gen Text
genFileName = do
    name <- Gen.text (Range.linear 1 20) Gen.alphaNum
    ext <- Gen.text (Range.linear 1 4) Gen.alpha
    pure $ name <> "." <> ext

genFileEntry :: Gen (Int, Int, Text)
genFileEntry = do
    adds <- Gen.int (Range.linear 0 1000)
    dels <- Gen.int (Range.linear 0 1000)
    fname <- genFileName
    pure (adds, dels, fname)

sumInts :: [Int] -> Int
sumInts = List.foldl' (+) 0

genEntry :: Gen (Int, Int)
genEntry = do
    adds <- Gen.int (Range.linear 0 1000)
    dels <- Gen.int (Range.linear 0 1000)
    pure (adds, dels)

-- | Generate valid numstat text
genNumstatText :: Gen Text
genNumstatText = do
    entries <- Gen.list (Range.linear 0 10) genFileEntry
    pure $ T.intercalate "\n" (map toLine entries)
  where
    toLine (a, d, fname) = T.pack (show a) <> "\t" <> T.pack (show d) <> "\t" <> fname

-- ═══════════════════════════════════════════════════════════════════════════
-- Test tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Diff Property Tests"
        [ testGroup
            "parseNumstat"
            [ testProperty "parse numstat totals" prop_parseNumstatTotals
            , testProperty "parse numstat binary" prop_parseNumstatBinary
            , testProperty "parse numstat empty" prop_parseNumstatEmpty
            ]
        , testGroup
            "parseNumstatFiles"
            [ testProperty "parse numstat files entries" prop_parseNumstatFilesEntries
            , testProperty "parse numstat files binary" prop_parseNumstatFilesBinary
            , testProperty "parse numstat files empty" prop_parseNumstatFilesEmpty
            , testProperty "parse numstat files preserves paths" prop_parseNumstatFilesPreservesPaths
            ]
        , testGroup
            "readNumstatInt"
            [ testProperty "parses valid integers" prop_readNumstatIntValid
            , testProperty "returns 0 for non-numeric" prop_readNumstatIntNonNumeric
            , testProperty "handles binary marker" prop_readNumstatIntBinaryMarker
            ]
        , testGroup
            "parseNumstatLine"
            [ testProperty "parses valid entries" prop_parseNumstatLineValid
            , testProperty "handles empty fields" prop_parseNumstatLineEmpty
            , testProperty "ignores extra fields" prop_parseNumstatLineExtraFields
            ]
        , testGroup
            "combineDiffResults"
            [ testProperty "returns Nothing when input is Nothing" prop_combineDiffResultsNothing
            , testProperty "returns Just when both inputs present" prop_combineDiffResultsJust
            , testProperty "summary matches parseNumstat" prop_combineDiffResultsSummaryCorrect
            ]
        , testGroup
            "consistency"
            [ testProperty "file count consistent" prop_parseNumstatFilesCountConsistent
            , testProperty "totals match files" prop_parseNumstatTotalsMatchFiles
            ]
        ]
