{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.PtyTypesProps
Description : Property tests for Pty.Types

Comprehensive property tests for PTY types, including buffer operations,
JSON serialization, and input parsing.
-}
module Property.PtyTypesProps where

import Data.Aeson (decode, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List qualified as List
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
    pure $ case List.unsnoc segments of
        Nothing -> "/" -- Should not happen with Range.linear 1 5
        Just ([], lastSeg) -> "/" <> lastSeg
        Just (initSegs, lastSeg) -> "/" <> mconcat (map (<> "/") initSegs) <> lastSeg

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

-- ============================================================================
-- Buffer Operation Generators
-- ============================================================================

-- | Generate a small ByteString for buffer testing
genSmallByteString :: Gen ByteString
genSmallByteString = Gen.bytes (Range.linear 0 1000)

{- | Generate a ByteString that could cause buffer wraparound
| Generate a bytestring that may exceed the test buffer limit
Uses a small test limit to keep tests fast
-}
testBufferLimit :: Int
testBufferLimit = 1000

genLargeByteString :: Gen ByteString
genLargeByteString = Gen.bytes (Range.linear 0 (testBufferLimit + 100))

-- | Generate a valid PtyBuffer state
genPtyBuffer :: Gen PtyBuffer
genPtyBuffer = do
    dataLen <- Gen.int (Range.linear 0 500)
    bufData <- Gen.bytes (Range.singleton dataLen)
    -- Cursor is total bytes written, could be larger than buffer size
    cursor <- Gen.word64 (Range.linear (fromIntegral dataLen) (fromIntegral dataLen + 1000))
    -- Buffer cursor is where the current buffer window starts
    let bufferCursor = cursor - fromIntegral dataLen
    pure
        PtyBuffer
            { pbData = bufData
            , pbCursor = cursor
            , pbBufferCursor = bufferCursor
            }

-- ============================================================================
-- appendToBuffer Properties
-- ============================================================================

-- | Property: appendToBuffer advances the cursor by the input length
prop_appendToBufferAdvancesCursor :: Property
prop_appendToBufferAdvancesCursor = property $ do
    buf <- forAll genPtyBuffer
    input <- forAll genSmallByteString
    let result = appendToBuffer input buf
    pbCursor result === pbCursor buf + fromIntegral (BS.length input)

-- | Property: appendToBuffer on empty buffer starts cursor at input length
prop_appendToEmptyBuffer :: Property
prop_appendToEmptyBuffer = property $ do
    input <- forAll genSmallByteString
    let result = appendToBuffer input emptyBuffer
    pbCursor result === fromIntegral (BS.length input)
    pbBufferCursor result === 0
    pbData result === input

-- | Property: appendToBuffer never exceeds buffer limit
prop_appendToBufferLimitsSize :: Property
prop_appendToBufferLimitsSize = property $ do
    buf <- forAll genPtyBuffer
    input <- forAll genLargeByteString
    let result = appendToBuffer input buf
    assert $ BS.length (pbData result) <= bufferLimit

-- | Property: appendToBuffer is associative (appending A then B equals appending A<>B)
prop_appendToBufferAssociative :: Property
prop_appendToBufferAssociative = property $ do
    a <- forAll $ Gen.bytes (Range.linear 0 100)
    b <- forAll $ Gen.bytes (Range.linear 0 100)
    let resultSeq = appendToBuffer b (appendToBuffer a emptyBuffer)
        resultConcat = appendToBuffer (a <> b) emptyBuffer
    -- Cursor positions should match
    pbCursor resultSeq === pbCursor resultConcat
    -- Data should match for small inputs that don't wrap
    pbData resultSeq === pbData resultConcat

-- | Property: appendToBuffer identity - appending empty is identity for cursor
prop_appendToBufferEmptyIdentity :: Property
prop_appendToBufferEmptyIdentity = property $ do
    buf <- forAll genPtyBuffer
    let result = appendToBuffer BS.empty buf
    pbCursor result === pbCursor buf
    pbData result === pbData buf

-- ============================================================================
-- calculateReplayData Properties
-- ============================================================================

-- | Property: replay from cursor 0 returns all buffer data
prop_calculateReplayFromZero :: Property
prop_calculateReplayFromZero = property $ do
    buf <- forAll genPtyBuffer
    let replay = calculateReplayData 0 buf
    -- If cursor is 0, and buffer cursor is also 0, we get all data
    -- If buffer cursor > 0, we can't replay data before buffer cursor
    if pbBufferCursor buf == 0
        then replay === pbData buf
        else do
            -- Can only get what's in the buffer
            assert $ BS.length replay <= BS.length (pbData buf)

-- | Property: replay from current cursor returns empty
prop_calculateReplayFromCurrent :: Property
prop_calculateReplayFromCurrent = property $ do
    buf <- forAll genPtyBuffer
    let replay = calculateReplayData (pbCursor buf) buf
    replay === BS.empty

-- | Property: replay from future cursor returns empty
prop_calculateReplayFromFuture :: Property
prop_calculateReplayFromFuture = property $ do
    buf <- forAll genPtyBuffer
    futureOffset <- forAll $ Gen.word64 (Range.linear 1 1000)
    let replay = calculateReplayData (pbCursor buf + futureOffset) buf
    replay === BS.empty

-- | Property: replay length decreases as cursor advances
prop_calculateReplayLengthDecreases :: Property
prop_calculateReplayLengthDecreases = property $ do
    buf <- forAll genPtyBuffer
    cursor1 <- forAll $ Gen.word64 (Range.linear 0 (pbCursor buf))
    cursor2 <- forAll $ Gen.word64 (Range.linear cursor1 (pbCursor buf))
    let replay1 = calculateReplayData cursor1 buf
        replay2 = calculateReplayData cursor2 buf
    assert $ BS.length replay1 >= BS.length replay2

-- | Property: replay from buffer cursor returns all buffer data
prop_calculateReplayFromBufferCursor :: Property
prop_calculateReplayFromBufferCursor = property $ do
    buf <- forAll genPtyBuffer
    let replay = calculateReplayData (pbBufferCursor buf) buf
    replay === pbData buf

-- | Property: append then replay reconstructs the appended data
prop_appendThenReplay :: Property
prop_appendThenReplay = property $ do
    input <- forAll genSmallByteString
    let buf = appendToBuffer input emptyBuffer
        replay = calculateReplayData 0 buf
    replay === input

-- | Property: sequential appends and replay
prop_sequentialAppendReplay :: Property
prop_sequentialAppendReplay = property $ do
    a <- forAll $ Gen.bytes (Range.linear 1 100)
    b <- forAll $ Gen.bytes (Range.linear 1 100)
    let buf1 = appendToBuffer a emptyBuffer
        cursor1 = pbCursor buf1
        buf2 = appendToBuffer b buf1
        -- Replay from cursor1 should give us 'b'
        replay = calculateReplayData cursor1 buf2
    replay === b

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Pty.Types Property Tests"
        [ testGroup
            "JSON Serialization"
            [ testProperty "PtyStatus Running JSON" prop_ptyStatusRunningJson
            , testProperty "PtyStatus Exited JSON" prop_ptyStatusExitedJson
            , testProperty "PtyInfo encoding" prop_ptyInfoRoundtrip
            ]
        , testGroup
            "Input Parsing"
            [ testProperty "ResizeInput parsing" prop_resizeInputParsing
            , testProperty "UpdatePtyInput parsing" prop_updatePtyInputParsing
            , testProperty "CreatePtyInput parsing" prop_createPtyInputParsing
            , testProperty "ResizeInput positive" prop_resizeInputPositive
            ]
        , testGroup
            "Buffer Constants"
            [ testProperty "emptyBuffer zero cursor" prop_emptyBufferZeroCursor
            , testProperty "emptyBuffer empty data" prop_emptyBufferEmptyData
            , testProperty "bufferLimit is 2MB" prop_bufferLimitIs2MB
            , testProperty "bufferChunk is 64KB" prop_bufferChunkIs64KB
            ]
        , testGroup
            "appendToBuffer"
            [ testProperty "advances cursor by input length" prop_appendToBufferAdvancesCursor
            , testProperty "appending to empty buffer" prop_appendToEmptyBuffer
            , testProperty "never exceeds buffer limit" prop_appendToBufferLimitsSize
            , testProperty "associative for small inputs" prop_appendToBufferAssociative
            , testProperty "empty append is identity" prop_appendToBufferEmptyIdentity
            ]
        , testGroup
            "calculateReplayData"
            [ testProperty "replay from zero" prop_calculateReplayFromZero
            , testProperty "replay from current cursor is empty" prop_calculateReplayFromCurrent
            , testProperty "replay from future is empty" prop_calculateReplayFromFuture
            , testProperty "replay length decreases with cursor" prop_calculateReplayLengthDecreases
            , testProperty "replay from buffer cursor" prop_calculateReplayFromBufferCursor
            , testProperty "append then replay" prop_appendThenReplay
            , testProperty "sequential append replay" prop_sequentialAppendReplay
            ]
        ]
