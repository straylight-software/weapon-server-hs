{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Unit.ProviderSpec
Description : Unit tests for Provider module
Stability   : experimental

This module contains unit tests for the 'Provider.Provider' module,
covering the basic functionality of provider management operations.
-}
module Unit.ProviderSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Provider.Provider qualified as Provider
import Provider.Types
import Test.Helpers (listLength)
import Test.Hspec

spec :: Spec
spec = do
    describe "Provider.builtinProviders" $ do
        it "is not empty" $ do
            let providers = Provider.builtinProviders
            null providers `shouldBe` False

        it "has unique IDs" $ do
            let providers = Provider.builtinProviders
            let ids = map providerId providers
            listLength ids `shouldBe` Set.size (Set.fromList ids)

        it "includes expected providers" $ do
            let ids = map providerId Provider.builtinProviders
            ids `shouldContain` ["anthropic"]
            ids `shouldContain` ["openai"]
            ids `shouldContain` ["openrouter"]

        it "has anthropic with models" $ do
            let mProvider = Provider.findProvider "anthropic" Provider.builtinProviders
            case mProvider of
                Nothing -> expectationFailure "Expected to find anthropic provider"
                Just p -> Map.null (providerModels p) `shouldBe` False

        it "has openai with models" $ do
            let mProvider = Provider.findProvider "openai" Provider.builtinProviders
            case mProvider of
                Nothing -> expectationFailure "Expected to find openai provider"
                Just p -> Map.null (providerModels p) `shouldBe` False

        it "has openrouter with empty models (dynamic)" $ do
            let mProvider = Provider.findProvider "openrouter" Provider.builtinProviders
            case mProvider of
                Nothing -> expectationFailure "Expected to find openrouter provider"
                Just p -> Map.null (providerModels p) `shouldBe` True

    describe "Provider.list" $ do
        it "returns all builtin providers" $ do
            listed <- Provider.list
            let builtins = Provider.builtinProviders
            listLength listed `shouldBe` listLength builtins

    describe "Provider.get" $ do
        it "returns correct provider for known IDs" $ do
            let providers = Provider.builtinProviders
            results <- mapM (Provider.get . providerId) providers
            let pairs = zip providers results
            mapM_
                ( \(provider, mResult) -> case mResult of
                    Nothing -> expectationFailure $ "Expected provider " ++ show (providerId provider)
                    Just retrieved -> providerId retrieved `shouldBe` providerId provider
                )
                pairs

        it "returns Nothing for unknown IDs" $ do
            result <- Provider.get "unknown_provider_xyz123"
            result `shouldBe` Nothing

    describe "Provider.getModel" $ do
        it "returns model for known provider and model ID" $ do
            result <- Provider.getModel "anthropic" "claude-sonnet-4-20250514"
            case result of
                Nothing -> expectationFailure "Expected to find claude-sonnet-4-20250514"
                Just model -> modelId model `shouldBe` "claude-sonnet-4-20250514"

        it "returns Nothing for unknown provider" $ do
            result <- Provider.getModel "unknown" "claude-sonnet-4-20250514"
            result `shouldBe` Nothing

        it "returns Nothing for unknown model" $ do
            result <- Provider.getModel "anthropic" "unknown-model"
            result `shouldBe` Nothing

    describe "Provider.findProvider (pure)" $ do
        it "finds existing provider" $ do
            let result = Provider.findProvider "anthropic" Provider.builtinProviders
            case result of
                Nothing -> expectationFailure "Expected to find anthropic"
                Just p -> providerId p `shouldBe` "anthropic"

        it "returns Nothing for missing provider" $ do
            let result = Provider.findProvider "nonexistent" Provider.builtinProviders
            result `shouldBe` Nothing

    describe "Provider.findModel (pure)" $ do
        it "finds existing model" $ do
            let mProvider = Provider.findProvider "anthropic" Provider.builtinProviders
            case mProvider of
                Nothing -> expectationFailure "Expected to find anthropic"
                Just p -> do
                    let mModel = Provider.findModel "claude-sonnet-4-20250514" p
                    case mModel of
                        Nothing -> expectationFailure "Expected to find claude-sonnet-4-20250514"
                        Just m -> modelId m `shouldBe` "claude-sonnet-4-20250514"

        it "returns Nothing for missing model" $ do
            let mProvider = Provider.findProvider "anthropic" Provider.builtinProviders
            case mProvider of
                Nothing -> expectationFailure "Expected to find anthropic"
                Just p -> do
                    let result = Provider.findModel "nonexistent" p
                    result `shouldBe` Nothing

    describe "Provider.updateProviderModels (pure)" $ do
        it "updates correct provider" $ do
            let newModels = Map.singleton "test-model" (defaultModel "test-model" "Test" "2024-01-01" (ModelLimit 1000 Nothing 100))
            let updated = Provider.updateProviderModels "openrouter" newModels Provider.builtinProviders
            let mProvider = Provider.findProvider "openrouter" updated
            case mProvider of
                Nothing -> expectationFailure "Expected to find openrouter"
                Just p -> providerModels p `shouldBe` newModels

        it "preserves other providers" $ do
            let newModels = Map.singleton "test-model" (defaultModel "test-model" "Test" "2024-01-01" (ModelLimit 1000 Nothing 100))
            let updated = Provider.updateProviderModels "openrouter" newModels Provider.builtinProviders
            let mAnthropic = Provider.findProvider "anthropic" updated
            let originalAnthropic = Provider.findProvider "anthropic" Provider.builtinProviders
            mAnthropic `shouldBe` originalAnthropic

    describe "Provider.determineAuthMethod (pure)" $ do
        it "returns stored method when present" $ do
            Provider.determineAuthMethod (Just "oauth") True True `shouldBe` Just "oauth"
            Provider.determineAuthMethod (Just "custom") False False `shouldBe` Just "custom"

        it "returns api_key when stored but no method" $ do
            Provider.determineAuthMethod Nothing True True `shouldBe` Just "api_key"
            Provider.determineAuthMethod Nothing True False `shouldBe` Just "api_key"

        it "returns env when only env auth" $ do
            Provider.determineAuthMethod Nothing False True `shouldBe` Just "env"

        it "returns Nothing when no auth" $ do
            Provider.determineAuthMethod Nothing False False `shouldBe` Nothing

    describe "Model defaults" $ do
        it "defaultModel sets correct required fields" $ do
            let model = defaultModel "test-id" "Test Name" "2024-01-01" (ModelLimit 128000 Nothing 4096)
            modelId model `shouldBe` "test-id"
            modelName model `shouldBe` "Test Name"
            modelReleaseDate model `shouldBe` "2024-01-01"
            mlContext (modelLimit model) `shouldBe` 128000
            mlOutput (modelLimit model) `shouldBe` 4096

        it "defaultModel sets sensible defaults" $ do
            let model = defaultModel "test-id" "Test" "2024-01-01" (ModelLimit 1000 Nothing 100)
            modelAttachment model `shouldBe` True
            modelReasoning model `shouldBe` False
            modelTemperature model `shouldBe` True
            modelToolCall model `shouldBe` True

        it "defaultModel sets optional fields to Nothing" $ do
            let model = defaultModel "test-id" "Test" "2024-01-01" (ModelLimit 1000 Nothing 100)
            modelFamily model `shouldBe` Nothing
            modelCost model `shouldBe` Nothing
            modelModalities model `shouldBe` Nothing
            modelExperimental model `shouldBe` Nothing
            modelStatus model `shouldBe` Nothing
