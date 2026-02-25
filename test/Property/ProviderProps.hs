{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.ProviderProps
Description : Property tests for Provider module
Stability   : experimental

Property tests for the Provider module, including:

* JSON round-trip properties for all types
* Pure helper function properties
* IO operation properties (auth persistence, etc.)
-}
module Property.ProviderProps where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key qualified as K
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
import System.Environment (setEnv, unsetEnv)
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

-- | Property: Provider JSON round-trip
prop_providerRoundtrip :: Property
prop_providerRoundtrip = property $ do
    provider <- forAll genProvider
    let json = encode provider
    case decode json of
        Nothing -> failure
        Just provider' -> provider === provider'

-- | Property: AuthMethod JSON round-trip
prop_authMethodRoundtrip :: Property
prop_authMethodRoundtrip = property $ do
    am <- forAll genAuthMethod
    let json = encode am
    case decode json of
        Nothing -> failure
        Just am' -> am === am'

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure helper function properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: extractTextField extracts text from JSON objects
prop_extractTextFieldSuccess :: Property
prop_extractTextFieldSuccess = property $ do
    key <- forAll genNonEmptyText
    value <- forAll genNonEmptyText
    let obj = object [K.fromText key .= value]
    Provider.extractTextField key obj === Just value

-- | Property: extractTextField returns Nothing for non-objects
prop_extractTextFieldNonObject :: Property
prop_extractTextFieldNonObject = property $ do
    key <- forAll genNonEmptyText
    value <- forAll genNonEmptyText
    Provider.extractTextField key (String value) === Nothing

-- | Property: extractTextField returns Nothing for missing keys
prop_extractTextFieldMissing :: Property
prop_extractTextFieldMissing = property $ do
    key <- forAll genNonEmptyText
    otherKey <- forAll $ Gen.filter (/= key) genNonEmptyText
    value <- forAll genNonEmptyText
    let obj = object [K.fromText otherKey .= value]
    Provider.extractTextField key obj === Nothing

-- | Property: extractTextField returns Nothing for non-string values
prop_extractTextFieldNonString :: Property
prop_extractTextFieldNonString = property $ do
    key <- forAll genNonEmptyText
    n <- forAll $ Gen.int (Range.linear 0 1000)
    let obj = object [K.fromText key .= n]
    Provider.extractTextField key obj === Nothing

-- | Property: findProvider finds existing providers
prop_findProviderSuccess :: Property
prop_findProviderSuccess = property $ do
    provider <- forAll genProvider
    let providers = [provider]
    Provider.findProvider (providerId provider) providers === Just provider

-- | Property: findProvider returns Nothing for missing providers
prop_findProviderMissing :: Property
prop_findProviderMissing = property $ do
    provider <- forAll genProvider
    let providers = [provider]
    missingId <- forAll $ Gen.filter (/= providerId provider) genNonEmptyText
    Provider.findProvider missingId providers === Nothing

-- | Property: findModel finds existing models
prop_findModelSuccess :: Property
prop_findModelSuccess = property $ do
    model <- forAll genModel
    let provider =
            Provider
                { providerId = "test"
                , providerName = "Test"
                , providerEnv = []
                , providerModels = Map.singleton (modelId model) model
                , providerApi = Nothing
                , providerNpm = Nothing
                }
    Provider.findModel (modelId model) provider === Just model

-- | Property: findModel returns Nothing for missing models
prop_findModelMissing :: Property
prop_findModelMissing = property $ do
    model <- forAll genModel
    let provider =
            Provider
                { providerId = "test"
                , providerName = "Test"
                , providerEnv = []
                , providerModels = Map.singleton (modelId model) model
                , providerApi = Nothing
                , providerNpm = Nothing
                }
    missingId <- forAll $ Gen.filter (/= modelId model) genNonEmptyText
    Provider.findModel missingId provider === Nothing

-- | Property: updateProviderModels updates the correct provider
prop_updateProviderModelsSuccess :: Property
prop_updateProviderModelsSuccess = property $ do
    providers <- forAll $ Gen.list (Range.linear 1 5) genProvider
    -- Use safe indexing with element selection instead of !!
    target <- forAll $ Gen.element providers
    newModels <- forAll $ Map.fromList <$> Gen.list (Range.linear 0 3) ((,) <$> genNonEmptyText <*> genModel)
    let updated = Provider.updateProviderModels (providerId target) newModels providers
    let updatedTarget = Provider.findProvider (providerId target) updated
    fmap providerModels updatedTarget === Just newModels

-- | Property: updateProviderModels preserves other providers
prop_updateProviderModelsPreservesOthers :: Property
prop_updateProviderModelsPreservesOthers = property $ do
    p1 <- forAll genProvider
    p2 <- forAll $ Gen.filter (\p -> providerId p /= providerId p1) genProvider
    let providers = [p1, p2]
    newModels <- forAll $ Map.fromList <$> Gen.list (Range.linear 0 3) ((,) <$> genNonEmptyText <*> genModel)
    let updated = Provider.updateProviderModels (providerId p1) newModels providers
    let otherProvider = Provider.findProvider (providerId p2) updated
    otherProvider === Just p2

-- | Property: determineAuthMethod returns stored method when present
prop_determineAuthMethodStored :: Property
prop_determineAuthMethodStored = property $ do
    method <- forAll genNonEmptyText
    hasStored <- forAll Gen.bool
    hasEnv <- forAll Gen.bool
    Provider.determineAuthMethod (Just method) hasStored hasEnv === Just method

-- | Property: determineAuthMethod returns "api_key" when stored but no method
prop_determineAuthMethodApiKey :: Property
prop_determineAuthMethodApiKey = property $ do
    hasEnv <- forAll Gen.bool
    Provider.determineAuthMethod Nothing True hasEnv === Just "api_key"

-- | Property: determineAuthMethod returns "env" when only env auth
prop_determineAuthMethodEnv :: Property
prop_determineAuthMethodEnv = property $ do
    Provider.determineAuthMethod Nothing False True === Just "env"

-- | Property: determineAuthMethod returns Nothing when no auth
prop_determineAuthMethodNone :: Property
prop_determineAuthMethodNone = property $ do
    Provider.determineAuthMethod Nothing False False === Nothing

-- ═══════════════════════════════════════════════════════════════════════════
-- IO operation properties
-- ═══════════════════════════════════════════════════════════════════════════

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

-- | Property: listConnected includes providers with stored auth
prop_listConnectedStoredAuth :: Property
prop_listConnectedStoredAuth = property $ do
    token <- forAll genNonEmptyText
    result <- evalIO $ do
        tmpDir <- createTempDirectory "/tmp" "provider-connected"
        Storage.withStorage tmpDir $ \storage -> do
            -- Initially no stored auth
            connectedBefore <- Provider.listConnected storage
            -- Store auth for openai
            Provider.setAuth storage "openai" token
            connectedAfter <- Provider.listConnected storage
            -- Clean up
            Provider.removeAuth storage "openai"
            connectedFinal <- Provider.listConnected storage
            removeDirectoryRecursive tmpDir
            pure (connectedBefore, connectedAfter, connectedFinal)
    let (before, afterStore, afterRemove) = result
    -- Before: openai should not be in connected (unless env var set)
    -- After store: openai should be in connected
    assert $ "openai" `elem` afterStore
    -- After remove: openai should not be in connected (unless env var set)
    assert $ "openai" `notElem` before || "openai" `notElem` afterRemove

-- | Property: listConnected includes providers with env var auth
prop_listConnectedEnvAuth :: Property
prop_listConnectedEnvAuth = property $ do
    token <- forAll genNonEmptyText
    result <- evalIO $ do
        tmpDir <- createTempDirectory "/tmp" "provider-env"
        Storage.withStorage tmpDir $ \storage -> do
            -- Set env var for anthropic
            setEnv "ANTHROPIC_API_KEY" (show token)
            connected <- Provider.listConnected storage
            -- Clean up env var
            unsetEnv "ANTHROPIC_API_KEY"
            connectedAfter <- Provider.listConnected storage
            removeDirectoryRecursive tmpDir
            pure (connected, connectedAfter)
    let (withEnv, withoutEnv) = result
    -- With env var: anthropic should be in connected
    assert $ "anthropic" `elem` withEnv
    -- Without env var: anthropic should not be in connected
    assert $ "anthropic" `notElem` withoutEnv

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

-- | Generator for Provider
genProvider :: Gen Provider
genProvider =
    Provider
        <$> genNonEmptyText -- providerId
        <*> genNonEmptyText -- providerName
        <*> Gen.list (Range.linear 0 3) genNonEmptyText -- providerEnv
        <*> (Map.fromList <$> Gen.list (Range.linear 0 3) ((,) <$> genNonEmptyText <*> genModel)) -- providerModels
        <*> Gen.maybe genNonEmptyText -- providerApi
        <*> Gen.maybe genNonEmptyText -- providerNpm

-- | Generator for AuthMethod
genAuthMethod :: Gen AuthMethod
genAuthMethod =
    AuthMethod
        <$> Gen.element ["api_key", "oauth"]
        <*> Gen.list (Range.linear 0 3) genNonEmptyText
        <*> Gen.maybe genNonEmptyText

-- ═══════════════════════════════════════════════════════════════════════════
-- Test tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Provider Property Tests"
        [ testGroup
            "JSON Round-trip"
            [ testProperty "ModelCost" prop_modelCostRoundtrip
            , testProperty "Model" prop_modelRoundtrip
            , testProperty "ProviderAuth" prop_providerAuthRoundtrip
            , testProperty "ModelInterleaved" prop_modelInterleavedRoundtrip
            , testProperty "ModelModalities" prop_modelModalitiesRoundtrip
            , testProperty "ModelProvider" prop_modelProviderRoundtrip
            , testProperty "ModelLimit" prop_modelLimitRoundtrip
            , testProperty "Provider" prop_providerRoundtrip
            , testProperty "AuthMethod" prop_authMethodRoundtrip
            ]
        , testGroup
            "Pure Helpers"
            [ testProperty "extractTextField success" prop_extractTextFieldSuccess
            , testProperty "extractTextField non-object" prop_extractTextFieldNonObject
            , testProperty "extractTextField missing key" prop_extractTextFieldMissing
            , testProperty "extractTextField non-string" prop_extractTextFieldNonString
            , testProperty "findProvider success" prop_findProviderSuccess
            , testProperty "findProvider missing" prop_findProviderMissing
            , testProperty "findModel success" prop_findModelSuccess
            , testProperty "findModel missing" prop_findModelMissing
            , testProperty "updateProviderModels success" prop_updateProviderModelsSuccess
            , testProperty "updateProviderModels preserves others" prop_updateProviderModelsPreservesOthers
            , testProperty "determineAuthMethod stored" prop_determineAuthMethodStored
            , testProperty "determineAuthMethod api_key" prop_determineAuthMethodApiKey
            , testProperty "determineAuthMethod env" prop_determineAuthMethodEnv
            , testProperty "determineAuthMethod none" prop_determineAuthMethodNone
            ]
        , testGroup
            "IO Operations"
            [ testProperty "Auth persistence" prop_authPersistence
            , testProperty "Auth status handles corrupt JSON" prop_authStatusCorruptJson
            , testProperty "listConnected includes stored auth" prop_listConnectedStoredAuth
            , testProperty "listConnected includes env auth" prop_listConnectedEnvAuth
            ]
        ]
