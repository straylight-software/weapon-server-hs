{-# LANGUAGE OverloadedStrings #-}

-- | Provider property tests
module Property.ProviderProps where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Provider.Provider qualified as Provider
import Provider.Types
import Storage.Storage qualified as Storage
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import Test.Tasty
import Test.Tasty.Hedgehog

-- | Property: ModelCost JSON round-trip
prop_modelCostRoundtrip :: Property
prop_modelCostRoundtrip = property $ do
    cost <- forAll genModelCost
    let json = encode cost
    case decode json of
        Nothing -> failure
        Just cost' -> cost === cost'

-- | Property: Model JSON round-trip
prop_modelRoundtrip :: Property
prop_modelRoundtrip = property $ do
    model <- forAll genModel
    let json = encode model
    case decode json of
        Nothing -> failure
        Just model' -> model === model'

-- | Property: ProviderAuth JSON round-trip
prop_providerAuthRoundtrip :: Property
prop_providerAuthRoundtrip = property $ do
    pa <- forAll genProviderAuth
    let json = encode pa
    case decode json of
        Nothing -> failure
        Just pa' -> pa === pa'

-- | Property: ModelInterleaved JSON round-trip
prop_modelInterleavedRoundtrip :: Property
prop_modelInterleavedRoundtrip = property $ do
    interleaved <- forAll genModelInterleaved
    let json = encode interleaved
    case decode json of
        Nothing -> failure
        Just interleaved' -> interleaved === interleaved'

-- | Property: ModelModalities JSON round-trip
prop_modelModalitiesRoundtrip :: Property
prop_modelModalitiesRoundtrip = property $ do
    modalities <- forAll genModelModalities
    let json = encode modalities
    case decode json of
        Nothing -> failure
        Just modalities' -> modalities === modalities'

-- | Property: ModelProvider JSON round-trip
prop_modelProviderRoundtrip :: Property
prop_modelProviderRoundtrip = property $ do
    provider <- forAll genModelProvider
    let json = encode provider
    case decode json of
        Nothing -> failure
        Just provider' -> provider === provider'

-- | Property: ModelLimit JSON round-trip
prop_modelLimitRoundtrip :: Property
prop_modelLimitRoundtrip = property $ do
    limit <- forAll genModelLimit
    let json = encode limit
    case decode json of
        Nothing -> failure
        Just limit' -> limit === limit'

prop_authPersistence :: Property
prop_authPersistence = property $ do
    token <- forAll genNonEmptyText
    result <- evalIO $ do
        tmpDir <- createTempDirectory "/tmp" "provider-auth"
        Storage.withStorage tmpDir $ \storage -> do
            Provider.setAuth storage "openai" token
            auths <- Provider.authStatus storage
            Provider.removeAuth storage "openai"
            authsAfter <- Provider.authStatus storage
            removeDirectoryRecursive tmpDir
            pure (auths, authsAfter)
    let (before, afterAuth) = result
    assert $ any (\a -> paProviderID a == "openai" && paAuthenticated a) before
    assert $ any (\a -> paProviderID a == "openai" && not (paAuthenticated a)) afterAuth

-- | Property: authStatus handles corrupt/invalid JSON files gracefully
prop_authStatusCorruptJson :: Property
prop_authStatusCorruptJson = property $ do
    result <- evalIO $ do
        tmpDir <- createTempDirectory "/tmp" "provider-corrupt"
        Storage.withStorage tmpDir $ \storage -> do
            -- Write invalid JSON to the auth file for openai
            let authDir = tmpDir </> "auth"
            createDirectoryIfMissing True authDir
            BL.writeFile (authDir </> "openai.json") "{ invalid json }"
            -- authStatus should not throw, should return unauthenticated
            auths <- Provider.authStatus storage
            removeDirectoryRecursive tmpDir
            pure auths
    -- Should return results without throwing
    assert $ not (null result)
    -- OpenAI should be marked as not authenticated due to corrupt file
    let openaiAuth = filter (\a -> paProviderID a == "openai") result
    assert $ not (any paAuthenticated openaiAuth)

-- Generators
genText :: Gen Text
genText = Gen.text (Range.linear 0 100) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 100) Gen.alphaNum

genDouble :: Gen Double
genDouble = Gen.double (Range.linearFrac 0 1000)

genModelCost :: Gen ModelCost
genModelCost =
    ModelCost
        <$> genDouble -- mcInput
        <*> genDouble -- mcOutput
        <*> Gen.maybe genDouble -- mcCacheRead
        <*> Gen.maybe genDouble -- mcCacheWrite
        <*> pure Nothing -- mcContextOver200k (don't recurse)

genModelLimit :: Gen ModelLimit
genModelLimit =
    ModelLimit
        <$> Gen.int (Range.linear 1000 200000)
        <*> Gen.maybe (Gen.int (Range.linear 0 100000))
        <*> Gen.int (Range.linear 1000 100000)

genModelInterleaved :: Gen ModelInterleaved
genModelInterleaved =
    Gen.choice
        [ InterleavedBool <$> Gen.bool
        , InterleavedField <$> Gen.element ["reasoning_content", "reasoning_details"]
        ]

genModelModalities :: Gen ModelModalities
genModelModalities =
    ModelModalities
        <$> Gen.list (Range.linear 1 5) (Gen.element ["text", "audio", "image", "video", "pdf"])
        <*> Gen.list (Range.linear 1 5) (Gen.element ["text", "audio", "image", "video", "pdf"])

genModelProvider :: Gen ModelProvider
genModelProvider =
    ModelProvider
        <$> Gen.maybe genNonEmptyText
        <*> Gen.maybe genNonEmptyText

genModel :: Gen Model
genModel =
    Model
        <$> genNonEmptyText -- modelId
        <*> genNonEmptyText -- modelName
        <*> genNonEmptyText -- modelReleaseDate
        <*> Gen.bool -- modelAttachment
        <*> Gen.bool -- modelReasoning
        <*> Gen.bool -- modelTemperature
        <*> Gen.bool -- modelToolCall
        <*> genModelLimit -- modelLimit
        <*> pure Map.empty -- modelOptions
        <*> Gen.maybe genNonEmptyText -- modelFamily
        <*> Gen.maybe genModelInterleaved -- modelInterleaved
        <*> Gen.maybe genModelCost -- modelCost
        <*> Gen.maybe genModelModalities -- modelModalities
        <*> Gen.maybe Gen.bool -- modelExperimental
        <*> Gen.maybe (Gen.element ["alpha", "beta", "deprecated"]) -- modelStatus
        <*> pure Nothing -- modelHeaders
        <*> pure Nothing -- modelProvider
        <*> pure Nothing -- modelVariants

genProviderAuth :: Gen ProviderAuth
genProviderAuth =
    ProviderAuth
        <$> genNonEmptyText
        <*> Gen.bool
        <*> Gen.maybe genNonEmptyText

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Provider Property Tests"
        [ testProperty "ModelCost round-trip" prop_modelCostRoundtrip
        , testProperty "Model round-trip" prop_modelRoundtrip
        , testProperty "ProviderAuth round-trip" prop_providerAuthRoundtrip
        , testProperty "ModelInterleaved round-trip" prop_modelInterleavedRoundtrip
        , testProperty "ModelModalities round-trip" prop_modelModalitiesRoundtrip
        , testProperty "ModelProvider round-trip" prop_modelProviderRoundtrip
        , testProperty "ModelLimit round-trip" prop_modelLimitRoundtrip
        , testProperty "Auth persistence" prop_authPersistence
        , testProperty "Auth status handles corrupt JSON" prop_authStatusCorruptJson
        ]
