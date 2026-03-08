{-# LANGUAGE OverloadedStrings #-}

-- | Tool.Types property tests
module Property.ToolTypesProps where

import Data.Aeson (decode, encode)
import Data.List qualified as List
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Tool.Types

-- ============================================================================
-- Generators
-- ============================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 0 100) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 100) Gen.alphaNum

genFilePath :: Gen Text
genFilePath = do
    segments <- Gen.list (Range.linear 1 5) (Gen.text (Range.linear 1 20) Gen.alphaNum)
    pure $ case List.unsnoc segments of
        Nothing -> "/" -- Should not happen with Range.linear 1 5
        Just ([], lastSeg) -> "/" <> lastSeg
        Just (initSegs, lastSeg) -> "/" <> mconcat (map (<> "/") initSegs) <> lastSeg

genToolID :: Gen ToolID
genToolID =
    Gen.element
        [ ReadTool
        , WriteTool
        , EditTool
        , BashTool
        , GlobTool
        , GrepTool
        , TodoWriteTool
        , WebFetchTool
        , QuestionTool
        , TaskTool
        ]

genReadInput :: Gen ReadInput
genReadInput =
    ReadInput
        <$> genFilePath
        <*> Gen.maybe (Gen.int (Range.linear 0 1000))
        <*> Gen.maybe (Gen.int (Range.linear 1 1000))

genWriteInput :: Gen WriteInput
genWriteInput =
    WriteInput
        <$> genFilePath
        <*> genText

genEditInput :: Gen EditInput
genEditInput =
    EditInput
        <$> genFilePath
        <*> genNonEmptyText
        <*> genText
        <*> Gen.maybe Gen.bool

genBashInput :: Gen BashInput
genBashInput =
    BashInput
        <$> genNonEmptyText
        <*> genText
        <*> Gen.maybe genFilePath

genGlobInput :: Gen GlobInput
genGlobInput =
    GlobInput
        <$> genNonEmptyText
        <*> Gen.maybe genFilePath

genGrepInput :: Gen GrepInput
genGrepInput =
    GrepInput
        <$> genNonEmptyText
        <*> Gen.maybe genFilePath
        <*> Gen.maybe genText

genToolOutput :: Gen ToolOutput
genToolOutput =
    ToolOutput
        <$> genText
        <*> genText
        <*> Gen.bool
        <*> pure Nothing

-- ============================================================================
-- Properties
-- ============================================================================

prop_toolIDRoundtrip :: Property
prop_toolIDRoundtrip = property $ do
    toolId <- forAll genToolID
    let json = encode toolId
    case decode json of
        Nothing -> failure
        Just toolId' -> toolId === toolId'

prop_readInputRoundtrip :: Property
prop_readInputRoundtrip = property $ do
    input <- forAll genReadInput
    let json = encode input
    case decode json of
        Nothing -> failure
        Just input' -> input === input'

prop_writeInputRoundtrip :: Property
prop_writeInputRoundtrip = property $ do
    input <- forAll genWriteInput
    let json = encode input
    case decode json of
        Nothing -> failure
        Just input' -> input === input'

prop_editInputRoundtrip :: Property
prop_editInputRoundtrip = property $ do
    input <- forAll genEditInput
    let json = encode input
    case decode json of
        Nothing -> failure
        Just input' -> input === input'

prop_bashInputRoundtrip :: Property
prop_bashInputRoundtrip = property $ do
    input <- forAll genBashInput
    let json = encode input
    case decode json of
        Nothing -> failure
        Just input' -> input === input'

prop_globInputRoundtrip :: Property
prop_globInputRoundtrip = property $ do
    input <- forAll genGlobInput
    let json = encode input
    case decode json of
        Nothing -> failure
        Just input' -> input === input'

prop_grepInputRoundtrip :: Property
prop_grepInputRoundtrip = property $ do
    input <- forAll genGrepInput
    let json = encode input
    case decode json of
        Nothing -> failure
        Just input' -> input === input'

prop_toolOutputRoundtrip :: Property
prop_toolOutputRoundtrip = property $ do
    output <- forAll genToolOutput
    let json = encode output
    case decode json of
        Nothing -> failure
        Just output' -> output === output'

-- | Property: toolSuccess creates non-error output
prop_toolSuccessIsNotError :: Property
prop_toolSuccessIsNotError = property $ do
    title <- forAll genText
    output <- forAll genText
    let result = toolSuccess title output
    assert $ not (toIsError result)
    toTitle result === title
    toOutput result === output

-- | Property: toolError creates error output
prop_toolErrorIsError :: Property
prop_toolErrorIsError = property $ do
    title <- forAll genText
    output <- forAll genText
    let result = toolError title output
    assert $ toIsError result
    toTitle result === title
    toOutput result === output

-- | Property: ReadInput offset is non-negative when present
prop_readInputOffsetNonNegative :: Property
prop_readInputOffsetNonNegative = property $ do
    input <- forAll genReadInput
    case riOffset input of
        Just off -> assert $ off >= 0
        Nothing -> success

-- | Property: ReadInput limit is positive when present
prop_readInputLimitPositive :: Property
prop_readInputLimitPositive = property $ do
    input <- forAll genReadInput
    case riLimit input of
        Just lim -> assert $ lim > 0
        Nothing -> success

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Tool.Types Property Tests"
        [ testProperty "ToolID round-trip" prop_toolIDRoundtrip
        , testProperty "ReadInput round-trip" prop_readInputRoundtrip
        , testProperty "WriteInput round-trip" prop_writeInputRoundtrip
        , testProperty "EditInput round-trip" prop_editInputRoundtrip
        , testProperty "BashInput round-trip" prop_bashInputRoundtrip
        , testProperty "GlobInput round-trip" prop_globInputRoundtrip
        , testProperty "GrepInput round-trip" prop_grepInputRoundtrip
        , testProperty "ToolOutput round-trip" prop_toolOutputRoundtrip
        , testProperty "toolSuccess is not error" prop_toolSuccessIsNotError
        , testProperty "toolError is error" prop_toolErrorIsError
        , testProperty "ReadInput offset non-negative" prop_readInputOffsetNonNegative
        , testProperty "ReadInput limit positive" prop_readInputLimitPositive
        ]
