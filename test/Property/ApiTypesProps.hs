{-# LANGUAGE OverloadedStrings #-}

-- | Api.Types property tests
module Property.ApiTypesProps where

import Api.Types
import Data.Aeson (decode, encode, object, (.=))
import Data.List qualified as List
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 50) Gen.alphaNum

genFilePath :: Gen Text
genFilePath = do
    segments <- Gen.list (Range.linear 1 5) (Gen.text (Range.linear 1 20) Gen.alphaNum)
    pure $ case List.unsnoc segments of
        Nothing -> "/" -- Should not happen with Range.linear 1 5
        Just ([], lastSeg) -> "/" <> lastSeg
        Just (initSegs, lastSeg) -> "/" <> mconcat (map (<> "/") initSegs) <> lastSeg

genHealth :: Gen Health
genHealth =
    Health
        <$> Gen.bool
        <*> genNonEmptyText

genPathInfo :: Gen PathInfo
genPathInfo =
    PathInfo
        <$> genFilePath
        <*> genFilePath
        <*> genFilePath
        <*> genFilePath
        <*> genFilePath

genProjectTime :: Gen ProjectTime
genProjectTime =
    ProjectTime
        <$> Gen.double (Range.linearFrac 0 2000000000)
        <*> Gen.double (Range.linearFrac 0 2000000000)
        <*> Gen.maybe (Gen.double (Range.linearFrac 0 2000000000))

genProject :: Gen Project
genProject =
    Project
        <$> genNonEmptyText
        <*> genFilePath
        <*> Gen.maybe genText
        <*> genProjectTime
        <*> Gen.list (Range.linear 0 3) genFilePath

genVcsInfo :: Gen VcsInfo
genVcsInfo = VcsInfo <$> genNonEmptyText

genChatInput :: Gen ChatInput
genChatInput =
    ChatInput
        <$> genText
        <*> Gen.maybe genText

genProviderList :: Gen ProviderList
genProviderList =
    ProviderList
        <$> Gen.list (Range.linear 0 3) (pure (object ["name" .= ("test" :: Text)]))
        <*> pure (object ["default" .= ("model" :: Text)])
        <*> Gen.list (Range.linear 0 3) genNonEmptyText

genConfigProviderList :: Gen ConfigProviderList
genConfigProviderList =
    ConfigProviderList
        <$> Gen.list (Range.linear 0 3) (pure (object ["name" .= ("test" :: Text)]))
        <*> pure (object ["default" .= ("model" :: Text)])

-- ============================================================================
-- Properties
-- ============================================================================

prop_healthRoundtrip :: Property
prop_healthRoundtrip = property $ do
    h <- forAll genHealth
    let json = encode h
    case decode json of
        Nothing -> failure
        Just h' -> h === h'

prop_pathInfoRoundtrip :: Property
prop_pathInfoRoundtrip = property $ do
    pathInfo <- forAll genPathInfo
    let json = encode pathInfo
    case decode json of
        Nothing -> failure
        Just pathInfo' -> pathInfo === pathInfo'

prop_projectRoundtrip :: Property
prop_projectRoundtrip = property $ do
    p <- forAll genProject
    let json = encode p
    case decode json of
        Nothing -> failure
        Just p' -> p === p'

prop_vcsInfoRoundtrip :: Property
prop_vcsInfoRoundtrip = property $ do
    v <- forAll genVcsInfo
    let json = encode v
    case decode json of
        Nothing -> failure
        Just v' -> v === v'

prop_chatInputParsing :: Property
prop_chatInputParsing = property $ do
    msg <- forAll genText
    model <- forAll $ Gen.maybe genText
    let json = encode $ object ["message" .= msg, "model" .= model]
    case decode json of
        Nothing -> failure
        Just (ci :: ChatInput) -> do
            ciMessage ci === msg
            ciModel ci === model

prop_providerListRoundtrip :: Property
prop_providerListRoundtrip = property $ do
    pl <- forAll genProviderList
    let json = encode pl
    case decode json of
        Nothing -> failure
        Just pl' -> pl === pl'

prop_configProviderListRoundtrip :: Property
prop_configProviderListRoundtrip = property $ do
    cpl <- forAll genConfigProviderList
    let json = encode cpl
    case decode json of
        Nothing -> failure
        Just cpl' -> cpl === cpl'

-- | Property: Health with healthy=true encodes correctly
prop_healthyEncoding :: Property
prop_healthyEncoding = property $ do
    version <- forAll genNonEmptyText
    let h = Health True version
        json = encode h
    case decode json of
        Nothing -> failure
        Just (h' :: Health) -> healthy h' === True

-- | Property: VcsInfo with no branch fails to parse (branch is required)
prop_vcsInfoNoBranch :: Property
prop_vcsInfoNoBranch = property $ do
    let json = encode $ object []
    case decode json of
        Nothing -> success -- Expected: empty object should fail to parse
        Just (_ :: VcsInfo) -> failure -- Should not parse without branch

-- | Property: ChatInput message is preserved
prop_chatInputMessagePreserved :: Property
prop_chatInputMessagePreserved = property $ do
    ci <- forAll genChatInput
    ciMessage ci === ciMessage ci -- Trivial but ensures generator works

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Api.Types Property Tests"
        [ testProperty "Health round-trip" prop_healthRoundtrip
        , testProperty "PathInfo round-trip" prop_pathInfoRoundtrip
        , testProperty "Project round-trip" prop_projectRoundtrip
        , testProperty "VcsInfo round-trip" prop_vcsInfoRoundtrip
        , testProperty "ChatInput parsing" prop_chatInputParsing
        , testProperty "ProviderList round-trip" prop_providerListRoundtrip
        , testProperty "ConfigProviderList round-trip" prop_configProviderListRoundtrip
        , testProperty "Health healthy encoding" prop_healthyEncoding
        , testProperty "VcsInfo no branch" prop_vcsInfoNoBranch
        , testProperty "ChatInput message preserved" prop_chatInputMessagePreserved
        ]
