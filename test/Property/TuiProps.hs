{-# LANGUAGE OverloadedStrings #-}

module Property.TuiProps where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Storage.Storage qualified as Storage
import Test.Fixture (cleanDir, propertyWithTempDir)
import Test.Tasty
import Test.Tasty.Hedgehog
import Tui.Store qualified as TuiStore

prop_appendPrompt :: Property
prop_appendPrompt = propertyWithTempDir $ \tmpDir -> do
    a <- forAll genText
    b <- forAll genText
    result <- evalIO $ do
        cleanDir tmpDir
        Storage.withStorage tmpDir $ \store -> do
            _ <- TuiStore.appendPrompt store a
            TuiStore.appendPrompt store b
    result === (a <> b)

prop_clearPrompt :: Property
prop_clearPrompt = propertyWithTempDir $ \tmpDir -> do
    text <- forAll genText
    result <- evalIO $ do
        cleanDir tmpDir
        Storage.withStorage tmpDir $ \store -> do
            _ <- TuiStore.appendPrompt store text
            TuiStore.clearPrompt store
            TuiStore.getPrompt store
    result === ""

prop_submitPrompt :: Property
prop_submitPrompt = propertyWithTempDir $ \tmpDir -> do
    text <- forAll genText
    (submitted, remaining) <- evalIO $ do
        cleanDir tmpDir
        Storage.withStorage tmpDir $ \store -> do
            _ <- TuiStore.appendPrompt store text
            submitted <- TuiStore.submitPrompt store
            remaining <- TuiStore.getPrompt store
            pure (submitted, remaining)
    submitted === text
    remaining === ""

prop_submitStoresLast :: Property
prop_submitStoresLast = propertyWithTempDir $ \tmpDir -> do
    text <- forAll genText
    result <- evalIO $ do
        cleanDir tmpDir
        Storage.withStorage tmpDir $ \store -> do
            _ <- TuiStore.appendPrompt store text
            _ <- TuiStore.submitPrompt store
            Storage.read store ["tui", "submitted"]
    result === object ["prompt" .= text]

prop_setLastRoundtrip :: Property
prop_setLastRoundtrip = propertyWithTempDir $ \tmpDir -> do
    key <- forAll genText
    val <- forAll genText
    result <- evalIO $ do
        cleanDir tmpDir
        Storage.withStorage tmpDir $ \store -> do
            let payload = object ["key" .= key, "value" .= val]
            TuiStore.setLast store payload
            TuiStore.getLast store
    result === Just (object ["key" .= key, "value" .= val])

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

tests :: TestTree
tests =
    testGroup
        "TUI Property Tests"
        [ testProperty "append prompt" (withTests 1000 prop_appendPrompt)
        , testProperty "clear prompt" (withTests 1000 prop_clearPrompt)
        , testProperty "submit prompt" (withTests 1000 prop_submitPrompt)
        , testProperty "set/get last" (withTests 1000 prop_setLastRoundtrip)
        , testProperty "submit stores last" (withTests 1000 prop_submitStoresLast)
        ]
