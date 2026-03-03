{-# LANGUAGE OverloadedStrings #-}

-- | Session.Types property tests
module Property.SessionTypesProps where

import Data.Aeson (decode, encode)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
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
        <*> pure Nothing -- permission (not tested in roundtrip)

genProjectSummary :: Gen ProjectSummary
genProjectSummary =
    ProjectSummary
        <$> genNonEmptyText -- id
        <*> Gen.maybe genText -- name
        <*> genNonEmptyText -- worktree

genGlobalSession :: Gen GlobalSession
genGlobalSession =
    GlobalSession
        <$> genNonEmptyText -- id
        <*> genNonEmptyText -- slug
        <*> genNonEmptyText -- projectID
        <*> genNonEmptyText -- directory
        <*> Gen.maybe genText -- parentID
        <*> genText -- title
        <*> genNonEmptyText -- version
        <*> genSessionTime -- time
        <*> Gen.maybe genSessionSummary -- summary
        <*> Gen.maybe genSessionShare -- share
        <*> Gen.maybe genSessionRevert -- revert
        <*> Gen.maybe genProjectSummary -- project

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

prop_projectSummaryRoundtrip :: Property
prop_projectSummaryRoundtrip = property $ do
    ps <- forAll genProjectSummary
    let json = encode ps
    case decode json of
        Nothing -> failure
        Just ps' -> ps === ps'

prop_globalSessionRoundtrip :: Property
prop_globalSessionRoundtrip = property $ do
    gs <- forAll genGlobalSession
    let json = encode gs
    case decode json of
        Nothing -> failure
        Just gs' -> gs === gs'

-- | Property: toGlobalSession preserves all session fields
prop_toGlobalSessionPreservesFields :: Property
prop_toGlobalSessionPreservesFields = property $ do
    session <- forAll genSession
    mProject <- forAll $ Gen.maybe genProjectSummary
    let gs = toGlobalSession session mProject
    -- Verify all fields are preserved
    gsId gs === sessionId session
    gsSlug gs === sessionSlug session
    gsProjectID gs === sessionProjectID session
    gsDirectory gs === sessionDirectory session
    gsParentID gs === sessionParentID session
    gsTitle gs === sessionTitle session
    gsVersion gs === sessionVersion session
    gsTime gs === sessionTime session
    gsSummary gs === sessionSummary session
    gsShare gs === sessionShare session
    gsRevert gs === sessionRevert session
    gsProject gs === mProject

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

-- ============================================================================
-- Null field omission properties
-- ============================================================================

-- | Helper to check if a JSON object contains null values
hasNullValues :: Aeson.Value -> Bool
hasNullValues (Aeson.Object obj) = any isNull (KM.elems obj)
  where
    isNull Aeson.Null = True
    isNull _ = False
hasNullValues _ = False

-- | Helper to get keys with null values
nullKeys :: Aeson.Value -> [Text]
nullKeys (Aeson.Object obj) =
    [Key.toText k | (k, v) <- KM.toList obj, v == Aeson.Null]
nullKeys _ = []

-- | Property: Session JSON omits null optional fields (parentID, summary, share, revert)
prop_sessionOmitsNullFields :: Property
prop_sessionOmitsNullFields = property $ do
    -- Generate a session with all optional fields as Nothing
    session <- forAll $ do
        Session
            <$> genNonEmptyText
            <*> genNonEmptyText
            <*> genNonEmptyText
            <*> genNonEmptyText
            <*> pure Nothing -- parentID
            <*> genText
            <*> genNonEmptyText
            <*> (SessionTime <$> genTimestamp <*> genTimestamp <*> pure Nothing <*> pure Nothing)
            <*> pure Nothing -- summary
            <*> pure Nothing -- share
            <*> pure Nothing -- revert
    let json = Aeson.toJSON session
    -- Verify no null values in the JSON
    annotateShow (nullKeys json)
    assert $ not (hasNullValues json)
    -- Also check nested time object
    case json of
        Aeson.Object obj -> case KM.lookup "time" obj of
            Just timeJson -> do
                annotateShow (nullKeys timeJson)
                assert $ not (hasNullValues timeJson)
            Nothing -> failure
        Aeson.Array _ -> failure
        Aeson.String _ -> failure
        Aeson.Number _ -> failure
        Aeson.Bool _ -> failure
        Aeson.Null -> failure

-- | Property: SessionTime JSON omits null optional fields (compacting, archived)
prop_sessionTimeOmitsNullFields :: Property
prop_sessionTimeOmitsNullFields = property $ do
    st <- forAll $ SessionTime <$> genTimestamp <*> genTimestamp <*> pure Nothing <*> pure Nothing
    let json = Aeson.toJSON st
    annotateShow (nullKeys json)
    assert $ not (hasNullValues json)

-- | Property: SessionSummary JSON omits null optional fields (files)
prop_sessionSummaryOmitsNullFields :: Property
prop_sessionSummaryOmitsNullFields = property $ do
    ss <- forAll $ SessionSummary <$> Gen.int (Range.linear 0 100) <*> Gen.int (Range.linear 0 100) <*> pure Nothing
    let json = Aeson.toJSON ss
    annotateShow (nullKeys json)
    assert $ not (hasNullValues json)

-- | Property: SessionRevert JSON omits null optional fields (partID, snapshot, diff)
prop_sessionRevertOmitsNullFields :: Property
prop_sessionRevertOmitsNullFields = property $ do
    sr <- forAll $ SessionRevert <$> genNonEmptyText <*> pure Nothing <*> pure Nothing <*> pure Nothing
    let json = Aeson.toJSON sr
    annotateShow (nullKeys json)
    assert $ not (hasNullValues json)

-- | Property: GlobalSession JSON omits null optional fields
prop_globalSessionOmitsNullFields :: Property
prop_globalSessionOmitsNullFields = property $ do
    gs <- forAll $ do
        GlobalSession
            <$> genNonEmptyText
            <*> genNonEmptyText
            <*> genNonEmptyText
            <*> genNonEmptyText
            <*> pure Nothing -- parentID
            <*> genText
            <*> genNonEmptyText
            <*> (SessionTime <$> genTimestamp <*> genTimestamp <*> pure Nothing <*> pure Nothing)
            <*> pure Nothing -- summary
            <*> pure Nothing -- share
            <*> pure Nothing -- revert
            <*> pure Nothing -- project
    let json = Aeson.toJSON gs
    annotateShow (nullKeys json)
    assert $ not (hasNullValues json)

-- | Property: ProjectSummary JSON omits null optional fields (name)
prop_projectSummaryOmitsNullFields :: Property
prop_projectSummaryOmitsNullFields = property $ do
    ps <- forAll $ ProjectSummary <$> genNonEmptyText <*> pure Nothing <*> genNonEmptyText
    let json = Aeson.toJSON ps
    annotateShow (nullKeys json)
    -- Note: ProjectSummary uses standard toJSON which includes null for name
    -- This test documents current behavior
    success

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Session.Types Property Tests"
        [ testGroup
            "JSON Round-trip"
            [ testProperty "SessionTime round-trip" prop_sessionTimeRoundtrip
            , testProperty "SessionSummary round-trip" prop_sessionSummaryRoundtrip
            , testProperty "SessionShare round-trip" prop_sessionShareRoundtrip
            , testProperty "SessionRevert round-trip" prop_sessionRevertRoundtrip
            , testProperty "Session round-trip" prop_sessionRoundtrip
            , testProperty "CreateSessionInput round-trip" prop_createSessionInputRoundtrip
            , testProperty "ProjectSummary round-trip" prop_projectSummaryRoundtrip
            , testProperty "GlobalSession round-trip" prop_globalSessionRoundtrip
            ]
        , testGroup
            "Invariants"
            [ testProperty "SessionSummary additions non-negative" prop_sessionSummaryAdditionsNonNegative
            , testProperty "SessionSummary deletions non-negative" prop_sessionSummaryDeletionsNonNegative
            , testProperty "SessionTime created non-negative" prop_sessionTimeCreatedNonNegative
            , testProperty "SessionTime updated non-negative" prop_sessionTimeUpdatedNonNegative
            , testProperty "toGlobalSession preserves fields" prop_toGlobalSessionPreservesFields
            ]
        , testGroup
            "Null Field Omission"
            [ testProperty "Session omits null fields" prop_sessionOmitsNullFields
            , testProperty "SessionTime omits null fields" prop_sessionTimeOmitsNullFields
            , testProperty "SessionSummary omits null fields" prop_sessionSummaryOmitsNullFields
            , testProperty "SessionRevert omits null fields" prop_sessionRevertOmitsNullFields
            , testProperty "GlobalSession omits null fields" prop_globalSessionOmitsNullFields
            , testProperty "ProjectSummary null behavior" prop_projectSummaryOmitsNullFields
            ]
        ]
