{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.ApiSessionProps
Description : Property tests for Api.Session types

Property-based tests for Api.Session types including:

* 'FileDiffStatus' - JSON encoding/decoding
* 'FileDiff' - Full diff structure roundtrip
* 'UpdateSessionInput' - Session update input parsing
* 'ForkSessionInput' - Fork input parsing
-}
module Property.ApiSessionProps (
    -- * Test Entry Point
    tests,

    -- * Generators (exported for reuse)
    genFileDiffStatus,
    genFileDiff,
    genUpdateSessionInput,
    genForkSessionInput,
) where

import Api.Session (
    FileDiff (..),
    FileDiffStatus (..),
    ForkSessionInput (..),
    UpdateSessionInput (..),
 )
import Data.Aeson (decode, encode, object, (.=))
import Data.List qualified as List
import Data.Text (Text)
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Session.Types (
    SessionRevert (..),
    SessionShare (..),
    SessionSummary (..),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- ============================================================================
-- Generators
-- ============================================================================

-- | Generate alphanumeric text (0-50 chars)
genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

-- | Generate non-empty alphanumeric text (1-50 chars)
genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 50) Gen.alphaNum

-- | Generate a file path
genFilePath :: Gen Text
genFilePath = do
    segments <- Gen.list (Range.linear 1 5) (Gen.text (Range.linear 1 20) Gen.alphaNum)
    pure $ case List.unsnoc segments of
        Nothing -> "/" -- Should not happen with Range.linear 1 5
        Just ([], lastSeg) -> "/" <> lastSeg
        Just (initSegs, lastSeg) -> "/" <> mconcat (map (<> "/") initSegs) <> lastSeg

-- | Generate a FileDiffStatus
genFileDiffStatus :: Gen FileDiffStatus
genFileDiffStatus = Gen.element [Added, Deleted, Modified]

-- | Generate a FileDiff
genFileDiff :: Gen FileDiff
genFileDiff =
    FileDiff
        <$> genFilePath -- fdFile
        <*> genText -- fdBefore
        <*> genText -- fdAfter
        <*> Gen.int (Range.linear 0 1000) -- fdAdditions
        <*> Gen.int (Range.linear 0 1000) -- fdDeletions
        <*> Gen.maybe genFileDiffStatus -- fdStatus

-- | Generate a SessionSummary
genSessionSummary :: Gen SessionSummary
genSessionSummary =
    SessionSummary
        <$> Gen.int (Range.linear 0 10000) -- ssAdditions
        <*> Gen.int (Range.linear 0 10000) -- ssDeletions
        <*> Gen.maybe (Gen.int (Range.linear 0 1000)) -- ssFiles

-- | Generate a SessionShare
genSessionShare :: Gen SessionShare
genSessionShare = SessionShare <$> genNonEmptyText

-- | Generate a SessionRevert
genSessionRevert :: Gen SessionRevert
genSessionRevert =
    SessionRevert
        <$> genNonEmptyText -- srMessageId
        <*> Gen.maybe genText -- srPartId
        <*> Gen.maybe genText -- srSnapshot
        <*> Gen.maybe genText -- srDiff

-- | Generate an UpdateSessionInput
genUpdateSessionInput :: Gen UpdateSessionInput
genUpdateSessionInput =
    UpdateSessionInput
        <$> Gen.maybe genText -- usiTitle
        <*> Gen.maybe genSessionSummary -- usiSummary
        <*> Gen.maybe genSessionShare -- usiShare
        <*> Gen.maybe genSessionRevert -- usiRevert

-- | Generate a ForkSessionInput
genForkSessionInput :: Gen ForkSessionInput
genForkSessionInput = ForkSessionInput <$> Gen.maybe genNonEmptyText

-- ============================================================================
-- FileDiffStatus Properties
-- ============================================================================

-- | Property: FileDiffStatus JSON round-trip
prop_fileDiffStatusRoundtrip :: Property
prop_fileDiffStatusRoundtrip = property $ do
    status <- forAll genFileDiffStatus
    let json = encode status
    case decode json of
        Nothing -> fail "Failed to decode FileDiffStatus"
        Just status' -> status === status'

-- | Property: Added encodes as "added"
prop_addedEncoding :: Property
prop_addedEncoding = property $ do
    let json = encode Added
    json === "\"added\""

-- | Property: Deleted encodes as "deleted"
prop_deletedEncoding :: Property
prop_deletedEncoding = property $ do
    let json = encode Deleted
    json === "\"deleted\""

-- | Property: Modified encodes as "modified"
prop_modifiedEncoding :: Property
prop_modifiedEncoding = property $ do
    let json = encode Modified
    json === "\"modified\""

-- ============================================================================
-- FileDiff Properties
-- ============================================================================

-- | Property: FileDiff JSON round-trip
prop_fileDiffRoundtrip :: Property
prop_fileDiffRoundtrip = property $ do
    diff <- forAll genFileDiff
    let json = encode diff
    case decode json of
        Nothing -> fail "Failed to decode FileDiff"
        Just diff' -> diff === diff'

-- | Property: FileDiff preserves all fields
prop_fileDiffFieldsPreserved :: Property
prop_fileDiffFieldsPreserved = property $ do
    diff <- forAll genFileDiff
    let json = encode diff
    case decode json of
        Nothing -> fail "Failed to decode FileDiff"
        Just diff' -> do
            fdFile diff' === fdFile diff
            fdBefore diff' === fdBefore diff
            fdAfter diff' === fdAfter diff
            fdAdditions diff' === fdAdditions diff
            fdDeletions diff' === fdDeletions diff
            fdStatus diff' === fdStatus diff

-- | Property: FileDiff additions are non-negative
prop_fileDiffAdditionsNonNegative :: Property
prop_fileDiffAdditionsNonNegative = property $ do
    diff <- forAll genFileDiff
    assert $ fdAdditions diff >= 0

-- | Property: FileDiff deletions are non-negative
prop_fileDiffDeletionsNonNegative :: Property
prop_fileDiffDeletionsNonNegative = property $ do
    diff <- forAll genFileDiff
    assert $ fdDeletions diff >= 0

-- ============================================================================
-- UpdateSessionInput Properties
-- ============================================================================

-- | Property: UpdateSessionInput JSON round-trip
prop_updateSessionInputRoundtrip :: Property
prop_updateSessionInputRoundtrip = property $ do
    input <- forAll genUpdateSessionInput
    let json = encode input
    case decode json of
        Nothing -> fail "Failed to decode UpdateSessionInput"
        Just input' -> input === input'

-- | Property: UpdateSessionInput parses with all optional fields missing
prop_updateSessionInputAllOptional :: Property
prop_updateSessionInputAllOptional = property $ do
    let json = encode $ object []
    case decode json of
        Nothing -> fail "Failed to decode empty UpdateSessionInput"
        Just (input :: UpdateSessionInput) -> do
            usiTitle input === Nothing
            usiSummary input === Nothing
            usiShare input === Nothing
            usiRevert input === Nothing

-- | Property: UpdateSessionInput parses with only title
prop_updateSessionInputTitleOnly :: Property
prop_updateSessionInputTitleOnly = property $ do
    title <- forAll genText
    let json = encode $ object ["title" .= title]
    case decode json of
        Nothing -> fail "Failed to decode UpdateSessionInput with title"
        Just (input :: UpdateSessionInput) -> do
            usiTitle input === Just title
            usiSummary input === Nothing
            usiShare input === Nothing
            usiRevert input === Nothing

-- ============================================================================
-- ForkSessionInput Properties
-- ============================================================================

-- | Property: ForkSessionInput JSON round-trip
prop_forkSessionInputRoundtrip :: Property
prop_forkSessionInputRoundtrip = property $ do
    input <- forAll genForkSessionInput
    let json = encode input
    case decode json of
        Nothing -> fail "Failed to decode ForkSessionInput"
        Just input' -> input === input'

-- | Property: ForkSessionInput parses with no messageID
prop_forkSessionInputNoMessageId :: Property
prop_forkSessionInputNoMessageId = property $ do
    let json = encode $ object []
    case decode json of
        Nothing -> fail "Failed to decode empty ForkSessionInput"
        Just (input :: ForkSessionInput) ->
            fsiMessageId input === Nothing

-- | Property: ForkSessionInput parses with messageID
prop_forkSessionInputWithMessageId :: Property
prop_forkSessionInputWithMessageId = property $ do
    msgId <- forAll genNonEmptyText
    let json = encode $ object ["messageID" .= msgId]
    case decode json of
        Nothing -> fail "Failed to decode ForkSessionInput with messageID"
        Just (input :: ForkSessionInput) ->
            fsiMessageId input === Just msgId

-- ============================================================================
-- Test Tree
-- ============================================================================

-- | All Api.Session property tests
tests :: TestTree
tests =
    testGroup
        "Api.Session Property Tests"
        [ testGroup
            "FileDiffStatus"
            [ testProperty "round-trip" prop_fileDiffStatusRoundtrip
            , testProperty "Added encodes as \"added\"" prop_addedEncoding
            , testProperty "Deleted encodes as \"deleted\"" prop_deletedEncoding
            , testProperty "Modified encodes as \"modified\"" prop_modifiedEncoding
            ]
        , testGroup
            "FileDiff"
            [ testProperty "round-trip" prop_fileDiffRoundtrip
            , testProperty "preserves all fields" prop_fileDiffFieldsPreserved
            , testProperty "additions non-negative" prop_fileDiffAdditionsNonNegative
            , testProperty "deletions non-negative" prop_fileDiffDeletionsNonNegative
            ]
        , testGroup
            "UpdateSessionInput"
            [ testProperty "round-trip" prop_updateSessionInputRoundtrip
            , testProperty "all fields optional" prop_updateSessionInputAllOptional
            , testProperty "title only" prop_updateSessionInputTitleOnly
            ]
        , testGroup
            "ForkSessionInput"
            [ testProperty "round-trip" prop_forkSessionInputRoundtrip
            , testProperty "no messageID" prop_forkSessionInputNoMessageId
            , testProperty "with messageID" prop_forkSessionInputWithMessageId
            ]
        ]
