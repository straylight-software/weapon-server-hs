{-# LANGUAGE OverloadedStrings #-}

-- | Session.Types property tests
module Property.SessionTypesProps where

import Data.Aeson (decode, encode)
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Session.Types
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 50) Gen.alphaNum

genTimestamp :: Gen Double
genTimestamp = Gen.double (Range.linearFrac 0 2000000000)

genSessionTime :: Gen SessionTime
genSessionTime =
    SessionTime
        <$> genTimestamp
        <*> genTimestamp
        <*> Gen.maybe genTimestamp
        <*> Gen.maybe genTimestamp

genSessionSummary :: Gen SessionSummary
genSessionSummary =
    SessionSummary
        <$> Gen.int (Range.linear 0 10000)
        <*> Gen.int (Range.linear 0 10000)
        <*> Gen.maybe (Gen.int (Range.linear 0 1000))

genSessionShare :: Gen SessionShare
genSessionShare =
    SessionShare <$> genNonEmptyText

genSessionRevert :: Gen SessionRevert
genSessionRevert =
    SessionRevert
        <$> genNonEmptyText
        <*> Gen.maybe genText
        <*> Gen.maybe genText
        <*> Gen.maybe genText

genSession :: Gen Session
genSession =
    Session
        <$> genNonEmptyText
        <*> genNonEmptyText
        <*> genNonEmptyText
        <*> genNonEmptyText
        <*> Gen.maybe genText
        <*> genText
        <*> genNonEmptyText
        <*> genSessionTime
        <*> Gen.maybe genSessionSummary
        <*> Gen.maybe genSessionShare
        <*> Gen.maybe genSessionRevert

genCreateSessionInput :: Gen CreateSessionInput
genCreateSessionInput =
    CreateSessionInput
        <$> Gen.maybe genText
        <*> Gen.maybe genText

-- ============================================================================
-- Properties
-- ============================================================================

prop_sessionTimeRoundtrip :: Property
prop_sessionTimeRoundtrip = property $ do
    st <- forAll genSessionTime
    let json = encode st
    case decode json of
        Nothing -> failure
        Just st' -> st === st'

prop_sessionSummaryRoundtrip :: Property
prop_sessionSummaryRoundtrip = property $ do
    ss <- forAll genSessionSummary
    let json = encode ss
    case decode json of
        Nothing -> failure
        Just ss' -> ss === ss'

prop_sessionShareRoundtrip :: Property
prop_sessionShareRoundtrip = property $ do
    share <- forAll genSessionShare
    let json = encode share
    case decode json of
        Nothing -> failure
        Just share' -> share === share'

prop_sessionRevertRoundtrip :: Property
prop_sessionRevertRoundtrip = property $ do
    sr <- forAll genSessionRevert
    let json = encode sr
    case decode json of
        Nothing -> failure
        Just sr' -> sr === sr'

prop_sessionRoundtrip :: Property
prop_sessionRoundtrip = property $ do
    session <- forAll genSession
    let json = encode session
    case decode json of
        Nothing -> failure
        Just session' -> session === session'

prop_createSessionInputRoundtrip :: Property
prop_createSessionInputRoundtrip = property $ do
    csi <- forAll genCreateSessionInput
    let json = encode csi
    case decode json of
        Nothing -> failure
        Just csi' -> csi === csi'

-- | Property: SessionSummary additions are non-negative
prop_sessionSummaryAdditionsNonNegative :: Property
prop_sessionSummaryAdditionsNonNegative = property $ do
    ss <- forAll genSessionSummary
    assert $ ssAdditions ss >= 0

-- | Property: SessionSummary deletions are non-negative
prop_sessionSummaryDeletionsNonNegative :: Property
prop_sessionSummaryDeletionsNonNegative = property $ do
    ss <- forAll genSessionSummary
    assert $ ssDeletions ss >= 0

-- | Property: SessionTime created is non-negative
prop_sessionTimeCreatedNonNegative :: Property
prop_sessionTimeCreatedNonNegative = property $ do
    st <- forAll genSessionTime
    assert $ stCreated st >= 0

-- | Property: SessionTime updated is non-negative
prop_sessionTimeUpdatedNonNegative :: Property
prop_sessionTimeUpdatedNonNegative = property $ do
    st <- forAll genSessionTime
    assert $ stUpdated st >= 0

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Session.Types Property Tests"
        [ testProperty "SessionTime round-trip" prop_sessionTimeRoundtrip
        , testProperty "SessionSummary round-trip" prop_sessionSummaryRoundtrip
        , testProperty "SessionShare round-trip" prop_sessionShareRoundtrip
        , testProperty "SessionRevert round-trip" prop_sessionRevertRoundtrip
        , testProperty "Session round-trip" prop_sessionRoundtrip
        , testProperty "CreateSessionInput round-trip" prop_createSessionInputRoundtrip
        , testProperty "SessionSummary additions non-negative" prop_sessionSummaryAdditionsNonNegative
        , testProperty "SessionSummary deletions non-negative" prop_sessionSummaryDeletionsNonNegative
        , testProperty "SessionTime created non-negative" prop_sessionTimeCreatedNonNegative
        , testProperty "SessionTime updated non-negative" prop_sessionTimeUpdatedNonNegative
        ]
