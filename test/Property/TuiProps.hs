{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.TuiProps
Description : Property tests for Tui.Store module

Property-based tests for the TUI store, covering both pure helper functions
and IO operations with storage.
-}
module Property.TuiProps (
    -- * Test Entry Point
    tests,

    -- * Generators (exported for reuse)
    genText,
    genJsonValue,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Storage.Storage qualified as Storage
import Test.Fixture (cleanDir, propertyWithTempDir)
import Test.Tasty
import Test.Tasty.Hedgehog
import Tui.Store qualified as TuiStore

--------------------------------------------------------------------------------
-- Pure Function Properties
--------------------------------------------------------------------------------

{- | Property: combinePromptText is associative

(a `combine` b) `combine` c === a `combine` (b `combine` c)
-}
prop_combinePromptText_associative :: Property
prop_combinePromptText_associative = property $ do
    a <- forAll genText
    b <- forAll genText
    c <- forAll genText
    let combine = TuiStore.combinePromptText
    combine (combine a b) c === combine a (combine b c)

{- | Property: combinePromptText with empty is identity

combine "" x === x
combine x "" === x
-}
prop_combinePromptText_identity :: Property
prop_combinePromptText_identity = property $ do
    text <- forAll genText
    TuiStore.combinePromptText "" text === text
    TuiStore.combinePromptText text "" === text

{- | Property: combinePromptText preserves content (concatenation)

combine a b === a <> b
-}
prop_combinePromptText_concatenation :: Property
prop_combinePromptText_concatenation = property $ do
    a <- forAll genText
    b <- forAll genText
    TuiStore.combinePromptText a b === a <> b

-- | Property: extractTextFromValue extracts String correctly
prop_extractTextFromValue_string :: Property
prop_extractTextFromValue_string = property $ do
    text <- forAll genText
    TuiStore.extractTextFromValue (String text) === text

-- | Property: extractTextFromValue returns empty for non-strings
prop_extractTextFromValue_nonString :: Property
prop_extractTextFromValue_nonString = property $ do
    val <- forAll genNonStringValue
    TuiStore.extractTextFromValue val === ""

-- | Property: mkSubmittedPayload creates correct structure
prop_mkSubmittedPayload_structure :: Property
prop_mkSubmittedPayload_structure = property $ do
    text <- forAll genText
    TuiStore.mkSubmittedPayload text === object ["prompt" .= text]

-- | Property: RetryConfig values are preserved
prop_retryConfig_roundtrip :: Property
prop_retryConfig_roundtrip = property $ do
    attempts <- forAll $ Gen.int (Range.linear 0 100)
    delay <- forAll $ Gen.int (Range.linear 0 1000000)
    let cfg = TuiStore.RetryConfig attempts delay
    TuiStore.retryAttempts cfg === attempts
    TuiStore.retryDelayMicros cfg === delay

-- | Property: defaultRetryConfig has expected values
prop_defaultRetryConfig :: Property
prop_defaultRetryConfig = withTests 1 $ property $ do
    let cfg = TuiStore.defaultRetryConfig
    TuiStore.retryAttempts cfg === 3
    TuiStore.retryDelayMicros cfg === 1000

--------------------------------------------------------------------------------
-- IO Operation Properties
--------------------------------------------------------------------------------

-- | Property: appendPrompt concatenates text correctly
prop_appendPrompt :: Property
prop_appendPrompt = propertyWithTempDir $ \tmpDir -> do
    a <- forAll genText
    b <- forAll genText
    result <- evalIO $ do
        cleanDir tmpDir
        Storage.withStorage tmpDir $ \store -> do
            _ <- TuiStore.appendPrompt store a
            TuiStore.appendPrompt store b
    result === TuiStore.combinePromptText a b

-- | Property: clearPrompt resets to empty
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

-- | Property: submitPrompt returns current and clears
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

-- | Property: submitPrompt writes to submitted key
prop_submitStoresLast :: Property
prop_submitStoresLast = propertyWithTempDir $ \tmpDir -> do
    text <- forAll genText
    result <- evalIO $ do
        cleanDir tmpDir
        Storage.withStorage tmpDir $ \store -> do
            _ <- TuiStore.appendPrompt store text
            _ <- TuiStore.submitPrompt store
            Storage.read store TuiStore.submittedKey
    result === TuiStore.mkSubmittedPayload text

-- | Property: setLast/getLast roundtrip
prop_setLastRoundtrip :: Property
prop_setLastRoundtrip = propertyWithTempDir $ \tmpDir -> do
    payload <- forAll genJsonValue
    result <- evalIO $ do
        cleanDir tmpDir
        Storage.withStorage tmpDir $ \store -> do
            TuiStore.setLast store payload
            TuiStore.getLast store
    result === Just payload

-- | Property: getPrompt returns empty for fresh storage
prop_getPrompt_fresh :: Property
prop_getPrompt_fresh = propertyWithTempDir $ \tmpDir -> do
    result <- evalIO $ do
        cleanDir tmpDir
        Storage.withStorage tmpDir TuiStore.getPrompt
    result === ""

-- | Property: getLast returns Nothing for fresh storage
prop_getLast_fresh :: Property
prop_getLast_fresh = propertyWithTempDir $ \tmpDir -> do
    result <- evalIO $ do
        cleanDir tmpDir
        Storage.withStorage tmpDir TuiStore.getLast
    result === Nothing

-- | Property: multiple appends accumulate correctly
prop_appendPrompt_accumulates :: Property
prop_appendPrompt_accumulates = propertyWithTempDir $ \tmpDir -> do
    texts <- forAll $ Gen.list (Range.linear 0 10) genText
    result <- evalIO $ do
        cleanDir tmpDir
        Storage.withStorage tmpDir $ \store -> do
            mapM_ (TuiStore.appendPrompt store) texts
            TuiStore.getPrompt store
    result === mconcat texts

-- | Property: storage keys are non-empty lists
prop_storageKeys_nonEmpty :: Property
prop_storageKeys_nonEmpty = withTests 1 $ property $ do
    assert (not $ null TuiStore.promptKey)
    assert (not $ null TuiStore.lastKey)
    assert (not $ null TuiStore.submittedKey)

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate arbitrary text for prompt testing
genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

-- | Generate a non-string JSON value
genNonStringValue :: Gen Value
genNonStringValue =
    Gen.choice
        [ pure Null
        , Bool <$> Gen.bool
        , Number . fromIntegral <$> Gen.int (Range.linear (-1000) 1000)
        , pure $ Array mempty
        , pure $ object []
        ]

-- | Generate a simple JSON value (for setLast/getLast tests)
genJsonValue :: Gen Value
genJsonValue =
    Gen.choice
        [ String <$> genText
        , Bool <$> Gen.bool
        , Number . fromIntegral <$> Gen.int (Range.linear (-1000) 1000)
        , pure Null
        , do
            key <- genText
            val <- genText
            pure $ object ["key" .= key, "value" .= val]
        ]

--------------------------------------------------------------------------------
-- Test Tree
--------------------------------------------------------------------------------

-- | All TUI property tests
tests :: TestTree
tests =
    testGroup
        "TUI Property Tests"
        [ testGroup
            "Pure Functions"
            [ testProperty "combinePromptText is associative" prop_combinePromptText_associative
            , testProperty "combinePromptText identity" prop_combinePromptText_identity
            , testProperty "combinePromptText concatenates" prop_combinePromptText_concatenation
            , testProperty "extractTextFromValue extracts String" prop_extractTextFromValue_string
            , testProperty "extractTextFromValue returns empty for non-String" prop_extractTextFromValue_nonString
            , testProperty "mkSubmittedPayload creates correct structure" prop_mkSubmittedPayload_structure
            , testProperty "RetryConfig preserves values" prop_retryConfig_roundtrip
            , testProperty "defaultRetryConfig has expected values" prop_defaultRetryConfig
            , testProperty "storage keys are non-empty" prop_storageKeys_nonEmpty
            ]
        , testGroup
            "IO Operations"
            [ testProperty "append prompt" prop_appendPrompt
            , testProperty "clear prompt" prop_clearPrompt
            , testProperty "submit prompt" prop_submitPrompt
            , testProperty "set/get last" prop_setLastRoundtrip
            , testProperty "submit stores submitted" prop_submitStoresLast
            , testProperty "getPrompt returns empty for fresh storage" prop_getPrompt_fresh
            , testProperty "getLast returns Nothing for fresh storage" prop_getLast_fresh
            , testProperty "multiple appends accumulate" prop_appendPrompt_accumulates
            ]
        ]
