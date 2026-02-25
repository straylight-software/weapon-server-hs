{-# LANGUAGE OverloadedStrings #-}

module Property.DiffProps where

import Control.Monad (forM_)
import Data.List qualified as List
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

genFileName :: Gen T.Text
genFileName = do
    name <- Gen.text (Range.linear 1 20) Gen.alphaNum
    ext <- Gen.text (Range.linear 1 4) Gen.alpha
    pure $ name <> "." <> ext

genFileEntry :: Gen (Int, Int, T.Text)
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

tests :: TestTree
tests =
    testGroup
        "Diff Property Tests"
        [ testProperty "parse numstat totals" prop_parseNumstatTotals
        , testProperty "parse numstat binary" prop_parseNumstatBinary
        , testProperty "parse numstat empty" prop_parseNumstatEmpty
        , testProperty "parse numstat files entries" prop_parseNumstatFilesEntries
        , testProperty "parse numstat files binary" prop_parseNumstatFilesBinary
        , testProperty "parse numstat files empty" prop_parseNumstatFilesEmpty
        , testProperty "parse numstat files preserves paths" prop_parseNumstatFilesPreservesPaths
        ]
