{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.VcsInternalProps
Description : Property tests for Vcs.Internal module

Tests the shared internal utilities used by the VCS modules.
-}
module Property.VcsInternalProps where

import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Vcs.Internal qualified as Internal

-- ═══════════════════════════════════════════════════════════════════════════
-- splitNonEmptyLines properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: splitNonEmptyLines returns empty list for empty input
prop_splitNonEmptyLinesEmpty :: Property
prop_splitNonEmptyLinesEmpty = property $ do
    Internal.splitNonEmptyLines "" === []

-- | Property: splitNonEmptyLines filters out empty lines
prop_splitNonEmptyLinesFilters :: Property
prop_splitNonEmptyLinesFilters = property $ do
    -- Generate text with some empty lines
    lines' <- forAll $ Gen.list (Range.linear 1 10) genNonEmptyLine
    let withEmpties = T.intercalate "\n\n" lines' <> "\n\n"
    let result = Internal.splitNonEmptyLines withEmpties
    -- All results should be non-empty
    assert $ not (any T.null result)
    -- Count should match original non-empty lines
    Internal.listLength result === Internal.listLength lines'

-- | Property: splitNonEmptyLines preserves non-empty lines
prop_splitNonEmptyLinesPreserves :: Property
prop_splitNonEmptyLinesPreserves = property $ do
    lines' <- forAll $ Gen.list (Range.linear 1 10) genNonEmptyLine
    let input = T.intercalate "\n" lines'
    let result = Internal.splitNonEmptyLines input
    result === lines'

-- | Property: splitNonEmptyLines handles trailing newline
prop_splitNonEmptyLinesTrailingNewline :: Property
prop_splitNonEmptyLinesTrailingNewline = property $ do
    lines' <- forAll $ Gen.list (Range.linear 1 10) genNonEmptyLine
    let withTrailing = T.intercalate "\n" lines' <> "\n"
    let withoutTrailing = T.intercalate "\n" lines'
    Internal.splitNonEmptyLines withTrailing === Internal.splitNonEmptyLines withoutTrailing

-- ═══════════════════════════════════════════════════════════════════════════
-- splitTabFields properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: splitTabFields splits on tabs
prop_splitTabFieldsBasic :: Property
prop_splitTabFieldsBasic = property $ do
    fields <- forAll $ Gen.list (Range.linear 1 5) genField
    let input = T.intercalate "\t" fields
    Internal.splitTabFields input === fields

-- | Property: splitTabFields returns singleton for no tabs
prop_splitTabFieldsNoTabs :: Property
prop_splitTabFieldsNoTabs = property $ do
    field <- forAll genField
    Internal.splitTabFields field === [field]

-- | Property: splitTabFields handles empty string
prop_splitTabFieldsEmpty :: Property
prop_splitTabFieldsEmpty = property $ do
    Internal.splitTabFields "" === [""]

-- | Property: splitTabFields handles consecutive tabs
prop_splitTabFieldsConsecutive :: Property
prop_splitTabFieldsConsecutive = property $ do
    Internal.splitTabFields "a\t\tb" === ["a", "", "b"]
    Internal.splitTabFields "\t\t" === ["", "", ""]

-- ═══════════════════════════════════════════════════════════════════════════
-- listLength properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: listLength matches reference implementation
prop_listLengthMatchesLength :: Property
prop_listLengthMatchesLength = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 1000))
    -- Use a reference foldl' implementation (safe for finite test lists)
    let referenceLen = foldl' (\acc _ -> acc + 1) (0 :: Int) xs
    Internal.listLength xs === referenceLen

-- | Property: listLength returns 0 for empty list
prop_listLengthEmpty :: Property
prop_listLengthEmpty = property $ do
    Internal.listLength ([] :: [Int]) === 0

-- | Property: listLength is consistent with list construction
prop_listLengthConsistent :: Property
prop_listLengthConsistent = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    let xs = replicate n ()
    Internal.listLength xs === n

-- ═══════════════════════════════════════════════════════════════════════════
-- sumInts properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: sumInts matches a reference foldl' implementation
prop_sumIntsMatchesSum :: Property
prop_sumIntsMatchesSum = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear (-1000) 1000))
    -- Use a reference implementation with foldl' (safe for finite test lists)
    let referenceSum = foldl' (+) 0 xs
    Internal.sumInts xs === referenceSum

-- | Property: sumInts returns 0 for empty list
prop_sumIntsEmpty :: Property
prop_sumIntsEmpty = property $ do
    Internal.sumInts [] === 0

-- | Property: sumInts is commutative with list concatenation
prop_sumIntsCommutative :: Property
prop_sumIntsCommutative = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100))
    ys <- forAll $ Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100))
    Internal.sumInts (xs ++ ys) === Internal.sumInts xs + Internal.sumInts ys

-- | Property: sumInts of singleton is the element
prop_sumIntsSingleton :: Property
prop_sumIntsSingleton = property $ do
    x <- forAll $ Gen.int (Range.linear (-1000) 1000)
    Internal.sumInts [x] === x

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

-- | Generate a non-empty line (no newlines)
genNonEmptyLine :: Gen Text
genNonEmptyLine = Gen.text (Range.linear 1 30) Gen.alphaNum

-- | Generate a field (no tabs or newlines)
genField :: Gen Text
genField = Gen.text (Range.linear 1 20) Gen.alphaNum

-- ═══════════════════════════════════════════════════════════════════════════
-- Test tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "VCS Internal Property Tests"
        [ testGroup
            "splitNonEmptyLines"
            [ testProperty "empty input gives empty list" prop_splitNonEmptyLinesEmpty
            , testProperty "filters out empty lines" prop_splitNonEmptyLinesFilters
            , testProperty "preserves non-empty lines" prop_splitNonEmptyLinesPreserves
            , testProperty "handles trailing newline" prop_splitNonEmptyLinesTrailingNewline
            ]
        , testGroup
            "splitTabFields"
            [ testProperty "splits on tabs" prop_splitTabFieldsBasic
            , testProperty "returns singleton for no tabs" prop_splitTabFieldsNoTabs
            , testProperty "handles empty string" prop_splitTabFieldsEmpty
            , testProperty "handles consecutive tabs" prop_splitTabFieldsConsecutive
            ]
        , testGroup
            "listLength"
            [ testProperty "matches Prelude.length" prop_listLengthMatchesLength
            , testProperty "returns 0 for empty" prop_listLengthEmpty
            , testProperty "consistent with replicate" prop_listLengthConsistent
            ]
        , testGroup
            "sumInts"
            [ testProperty "matches Prelude.sum" prop_sumIntsMatchesSum
            , testProperty "returns 0 for empty" prop_sumIntsEmpty
            , testProperty "commutative with concatenation" prop_sumIntsCommutative
            , testProperty "singleton equals element" prop_sumIntsSingleton
            ]
        ]
