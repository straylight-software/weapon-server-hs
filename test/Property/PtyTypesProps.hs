{-# LANGUAGE OverloadedStrings #-}

-- | Pty.Types property tests
module Property.PtyTypesProps where

import Data.Aeson (decode, encode, object, (.=))
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pty.Types
import Test.Tasty
import Test.Tasty.Hedgehog

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
    pure $ "/" <> mconcat (map (<> "/") (init segments)) <> last segments

genPtyStatus :: Gen PtyStatus
genPtyStatus =
    Gen.choice
        [ pure PtyRunning
        , PtyExited <$> Gen.int (Range.linear 0 255)
        ]

genPtyInfo :: Gen PtyInfo
genPtyInfo =
    PtyInfo
        <$> genNonEmptyText
        <*> genText
        <*> genNonEmptyText
        <*> Gen.list (Range.linear 0 5) genText
        <*> genFilePath
        <*> genPtyStatus
        <*> Gen.int (Range.linear 1 65535)
        <*> Gen.bool

genResizeInput :: Gen ResizeInput
genResizeInput =
    ResizeInput
        <$> Gen.int (Range.linear 1 500)
        <*> Gen.int (Range.linear 1 500)

genUpdatePtyInput :: Gen UpdatePtyInput
genUpdatePtyInput =
    UpdatePtyInput
        <$> Gen.maybe genText
        <*> Gen.maybe genResizeInput

genCreatePtyInput :: Gen CreatePtyInput
genCreatePtyInput =
    CreatePtyInput
        <$> Gen.maybe genNonEmptyText
        <*> Gen.maybe (Gen.list (Range.linear 0 5) genText)
        <*> Gen.maybe genFilePath
        <*> Gen.maybe genText
        <*> Gen.maybe (Gen.list (Range.linear 0 3) ((,) <$> genNonEmptyText <*> genText))
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe (Gen.list (Range.linear 0 2) ((,,) <$> genFilePath <*> genFilePath <*> Gen.bool))
        <*> Gen.maybe genNonEmptyText

-- ============================================================================
-- Properties
-- ============================================================================

prop_ptyStatusRunningJson :: Property
prop_ptyStatusRunningJson = property $ do
    let status = PtyRunning
        json = encode status
    -- Running should encode to "running"
    json === "\"running\""

prop_ptyStatusExitedJson :: Property
prop_ptyStatusExitedJson = property $ do
    exitCode <- forAll $ Gen.int (Range.linear 0 255)
    let status = PtyExited exitCode
        json = encode status
    -- Should contain the exit code - encodes as {"exited": N}
    json === encode (object ["exited" .= exitCode])

prop_ptyInfoRoundtrip :: Property
prop_ptyInfoRoundtrip = property $ do
    info <- forAll genPtyInfo
    let json = encode info
    -- PtyInfo only has ToJSON, not FromJSON, so we just verify encoding works
    assert $ json /= ""

-- | Property: ResizeInput parses from well-formed JSON
prop_resizeInputParsing :: Property
prop_resizeInputParsing = property $ do
    rows <- forAll $ Gen.int (Range.linear 1 500)
    cols <- forAll $ Gen.int (Range.linear 1 500)
    let json = encode $ object ["rows" .= rows, "cols" .= cols]
    case decode json of
        Nothing -> failure
        Just input' -> do
            riRows input' === rows
            riCols input' === cols

-- | Property: UpdatePtyInput parses from well-formed JSON
prop_updatePtyInputParsing :: Property
prop_updatePtyInputParsing = property $ do
    title <- forAll $ Gen.maybe genText
    let json = encode $ object ["title" .= title]
    case decode json of
        Nothing -> failure
        Just (input' :: UpdatePtyInput) -> upiTitle input' === title

-- | Property: CreatePtyInput parses from well-formed JSON
prop_createPtyInputParsing :: Property
prop_createPtyInputParsing = property $ do
    cmd <- forAll $ Gen.maybe genNonEmptyText
    cwd <- forAll $ Gen.maybe genFilePath
    let json = encode $ object ["command" .= cmd, "cwd" .= cwd]
    case decode json of
        Nothing -> failure
        Just (input' :: CreatePtyInput) -> do
            cpiCommand input' === cmd
            cpiCwd input' === cwd

-- | Property: emptyBuffer has zero cursor
prop_emptyBufferZeroCursor :: Property
prop_emptyBufferZeroCursor = property $ do
    pbCursor emptyBuffer === 0
    pbBufferCursor emptyBuffer === 0

-- | Property: emptyBuffer has empty data
prop_emptyBufferEmptyData :: Property
prop_emptyBufferEmptyData = property $ do
    pbData emptyBuffer === mempty

-- | Property: bufferLimit is 2MB
prop_bufferLimitIs2MB :: Property
prop_bufferLimitIs2MB = property $ do
    bufferLimit === 2 * 1024 * 1024

-- | Property: bufferChunk is 64KB
prop_bufferChunkIs64KB :: Property
prop_bufferChunkIs64KB = property $ do
    bufferChunk === 64 * 1024

-- | Property: ResizeInput rows and cols are positive
prop_resizeInputPositive :: Property
prop_resizeInputPositive = property $ do
    input <- forAll genResizeInput
    assert $ riRows input > 0
    assert $ riCols input > 0

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Pty.Types Property Tests"
        [ testProperty "PtyStatus Running JSON" prop_ptyStatusRunningJson
        , testProperty "PtyStatus Exited JSON" prop_ptyStatusExitedJson
        , testProperty "PtyInfo encoding" prop_ptyInfoRoundtrip
        , testProperty "ResizeInput parsing" prop_resizeInputParsing
        , testProperty "UpdatePtyInput parsing" prop_updatePtyInputParsing
        , testProperty "CreatePtyInput parsing" prop_createPtyInputParsing
        , testProperty "emptyBuffer zero cursor" prop_emptyBufferZeroCursor
        , testProperty "emptyBuffer empty data" prop_emptyBufferEmptyData
        , testProperty "bufferLimit is 2MB" prop_bufferLimitIs2MB
        , testProperty "bufferChunk is 64KB" prop_bufferChunkIs64KB
        , testProperty "ResizeInput positive" prop_resizeInputPositive
        ]
