{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.RequestProps
Description : Property tests for Request.Store module

This module contains property-based tests for the Request.Store module,
testing both pure functions (for deterministic testing) and IO functions
(for integration testing).
-}
module Property.RequestProps where

import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Request.Store qualified as RequestStore
import Storage.Storage qualified as Storage
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Fixture (propertyWithTempDir)
import Test.Helpers (listLength)
import Test.Tasty
import Test.Tasty.Hedgehog

--------------------------------------------------------------------------------
-- Pure Function Tests (deterministic, fast)
--------------------------------------------------------------------------------

-- | Test formatRequestId always produces "req_" prefix
prop_formatRequestIdPrefix :: Property
prop_formatRequestIdPrefix = property $ do
    n <- forAll $ Gen.word64 Range.linearBounded
    let result = RequestStore.formatRequestId n
    assert $ "req_" `T.isPrefixOf` result

-- | Test formatRequestId produces valid hex suffix
prop_formatRequestIdHex :: Property
prop_formatRequestIdHex = property $ do
    n <- forAll $ Gen.word64 Range.linearBounded
    let result = RequestStore.formatRequestId n
        suffix = T.drop 4 result
    assert $ T.all isHexChar suffix
  where
    isHexChar c = c `elem` ("0123456789abcdef" :: String)

-- | Test formatRequestId with known values
prop_formatRequestIdKnownValues :: Property
prop_formatRequestIdKnownValues = withTests 1 $ property $ do
    RequestStore.formatRequestId 0 === "req_0"
    RequestStore.formatRequestId 255 === "req_ff"
    RequestStore.formatRequestId 0xDEADBEEF === "req_deadbeef"

-- | Test formatRequestId roundtrip with generateId
prop_formatRequestIdMatchesGenerateId :: Property
prop_formatRequestIdMatchesGenerateId = property $ do
    reqId <- evalIO RequestStore.generateId
    -- Verify it matches the expected format
    assert $ "req_" `T.isPrefixOf` reqId
    let suffix = T.drop 4 reqId
    assert $ T.all isHexChar suffix
  where
    isHexChar c = c `elem` ("0123456789abcdef" :: String)

-- | Test buildStorageKey creates correct key structure
prop_buildStorageKeyStructure :: Property
prop_buildStorageKeyStructure = property $ do
    kind <- forAll genText
    reqId <- forAll genText
    let key = RequestStore.buildStorageKey kind reqId
    listLength key === 2
    key === [kind, reqId]

-- | Test buildStorageKey with empty strings
prop_buildStorageKeyEmpty :: Property
prop_buildStorageKeyEmpty = withTests 1 $ property $ do
    RequestStore.buildStorageKey "" "" === ["", ""]
    RequestStore.buildStorageKey "a" "" === ["a", ""]
    RequestStore.buildStorageKey "" "b" === ["", "b"]

-- | Test filterValidValues removes Nothing values
prop_filterValidValuesRemovesNothing :: Property
prop_filterValidValuesRemovesNothing = property $ do
    -- Generate a mix of Just and Nothing values
    values <-
        forAll $
            Gen.list (Range.linear 0 20) $
                Gen.maybe (Gen.int Range.linearBounded)
    let result = RequestStore.filterValidValues values
        expected = catMaybes values
    result === expected

-- | Test filterValidValues preserves order
prop_filterValidValuesPreservesOrder :: Property
prop_filterValidValuesPreservesOrder = property $ do
    -- Generate only Just values
    justs <-
        forAll $
            Gen.list (Range.linear 0 10) $
                Gen.int Range.linearBounded
    let input = map Just justs
        result = RequestStore.filterValidValues input
    result === justs

-- | Test filterValidValues with all Nothing
prop_filterValidValuesAllNothing :: Property
prop_filterValidValuesAllNothing = property $ do
    n <- forAll $ Gen.int (Range.linear 0 10)
    let input = replicate n (Nothing :: Maybe Int)
        result = RequestStore.filterValidValues input
    result === []

-- | Test filterValidValues with all Just
prop_filterValidValuesAllJust :: Property
prop_filterValidValuesAllJust = property $ do
    values <-
        forAll $
            Gen.list (Range.linear 0 10) $
                Gen.int Range.linearBounded
    let input = map Just values
        result = RequestStore.filterValidValues input
    result === values

--------------------------------------------------------------------------------
-- IO Function Tests (integration with storage)
--------------------------------------------------------------------------------

{- | Test that written values can be read back directly (no listing)
This tests the core write/read functionality without filesystem timing issues
-}
prop_writeReadRoundtrip :: Property
prop_writeReadRoundtrip = propertyWithTempDir $ \tmpDir -> do
    kind <- forAll genText
    req <- forAll genText
    value <- forAll genValue
    result <- evalIO $ Storage.withStorage tmpDir $ \store -> do
        RequestStore.writeRequest store kind req value
        Storage.read store [kind, req]
    result === value

-- | Test that readMaybe returns Nothing for non-existent keys
prop_readMaybeNotFound :: Property
prop_readMaybeNotFound = propertyWithTempDir $ \tmpDir -> do
    kind <- forAll genText
    req <- forAll genText
    result <- evalIO $ Storage.withStorage tmpDir $ \store ->
        Storage.readMaybe store [kind, req]
    result === (Nothing :: Maybe Value)

-- | Test that list returns empty for non-existent directory
prop_listEmptyDir :: Property
prop_listEmptyDir = propertyWithTempDir $ \tmpDir -> do
    kind <- forAll genText
    result <- evalIO $ Storage.withStorage tmpDir $ \store ->
        Storage.list store [kind]
    result === []

-- | Test that list returns the correct key after writing
prop_listContainsWrittenKey :: Property
prop_listContainsWrittenKey = propertyWithTempDir $ \tmpDir -> do
    kind <- forAll genText
    req <- forAll genText
    value <- forAll genValue
    keys <- evalIO $ Storage.withStorage tmpDir $ \store -> do
        RequestStore.writeRequest store kind req value
        Storage.list store [kind]
    assert $ [kind, req] `elem` keys

-- | Test that multiple writes create multiple keys
prop_listMultipleKeys :: Property
prop_listMultipleKeys = propertyWithTempDir $ \tmpDir -> do
    kind <- forAll genText
    req1 <- forAll genText
    req2 <- forAll $ Gen.filter (/= req1) genText
    value <- forAll genValue
    keys <- evalIO $ Storage.withStorage tmpDir $ \store -> do
        RequestStore.writeRequest store kind req1 value
        RequestStore.writeRequest store kind req2 value
        Storage.list store [kind]
    listLength keys === 2
    assert $ [kind, req1] `elem` keys
    assert $ [kind, req2] `elem` keys

-- | Test that listRequests returns written values
prop_listRequestsFindsValue :: Property
prop_listRequestsFindsValue = propertyWithTempDir $ \tmpDir -> do
    kind <- forAll genText
    req <- forAll genText
    value <- forAll genValue
    result <- evalIO $ Storage.withStorage tmpDir $ \store -> do
        RequestStore.writeRequest store kind req value
        RequestStore.listRequests store kind
    assert $ value `elem` result

-- | Test that readMaybe handles invalid JSON gracefully
prop_readMaybeSkipsInvalidJson :: Property
prop_readMaybeSkipsInvalidJson = propertyWithTempDir $ \tmpDir -> do
    kind <- forAll genText
    req <- forAll genText
    result <- evalIO $ Storage.withStorage tmpDir $ \store -> do
        let dir = Storage.storageDir store </> T.unpack kind
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> T.unpack req <> ".json") "{"
        Storage.readMaybe store [kind, req]
    result === (Nothing :: Maybe Value)

-- | Test that listRequests skips invalid JSON files
prop_listRequestsSkipsInvalid :: Property
prop_listRequestsSkipsInvalid = propertyWithTempDir $ \tmpDir -> do
    kind <- forAll genText
    validReq <- forAll genText
    invalidReq <- forAll $ Gen.filter (/= validReq) genText
    value <- forAll genValue
    result <- evalIO $ Storage.withStorage tmpDir $ \store -> do
        RequestStore.writeRequest store kind validReq value
        let dir = Storage.storageDir store </> T.unpack kind
        TIO.writeFile (dir </> T.unpack invalidReq <> ".json") "{"
        RequestStore.listRequests store kind
    assert $ value `elem` result
    listLength result === 1

-- | Test generateId always has "req_" prefix
prop_generateIdPrefix :: Property
prop_generateIdPrefix = property $ do
    reqId <- evalIO RequestStore.generateId
    assert $ "req_" `T.isPrefixOf` reqId

-- | Test generateId produces unique values
prop_generateIdUnique :: Property
prop_generateIdUnique = property $ do
    a <- evalIO RequestStore.generateId
    b <- evalIO RequestStore.generateId
    a /== b

-- | Test generateId produces non-empty suffix
prop_generateIdNonEmpty :: Property
prop_generateIdNonEmpty = property $ do
    reqId <- evalIO RequestStore.generateId
    assert $ not (T.null (T.drop 4 reqId))

-- | Test generateId suffix is valid hex
prop_generateIdHex :: Property
prop_generateIdHex = property $ do
    reqId <- evalIO RequestStore.generateId
    let suffix = T.drop 4 reqId
    assert $ T.all isHexChar suffix
  where
    isHexChar c = c `elem` ("0123456789abcdef" :: String)

-- | Test writeRequest uses buildStorageKey correctly
prop_writeRequestUsesCorrectKey :: Property
prop_writeRequestUsesCorrectKey = propertyWithTempDir $ \tmpDir -> do
    kind <- forAll genText
    reqId <- forAll genText
    value <- forAll genValue
    result <- evalIO $ Storage.withStorage tmpDir $ \store -> do
        RequestStore.writeRequest store kind reqId value
        let expectedKey = RequestStore.buildStorageKey kind reqId
        Storage.read store expectedKey
    result === value

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate alphanumeric text (safe for filesystem paths)
genText :: Gen Text
genText = Gen.text (Range.linear 1 10) Gen.alphaNum

-- | Generate a simple JSON object value
genValue :: Gen Value
genValue = do
    text <- genText
    pure $ object ["id" .= text]

--------------------------------------------------------------------------------
-- Test Tree
--------------------------------------------------------------------------------

tests :: TestTree
tests =
    testGroup
        "Request Store Property Tests"
        [ testGroup
            "Pure functions"
            [ testGroup
                "formatRequestId"
                [ testProperty "always has req_ prefix" prop_formatRequestIdPrefix
                , testProperty "suffix is valid hex" prop_formatRequestIdHex
                , testProperty "known values" prop_formatRequestIdKnownValues
                , testProperty "matches generateId format" prop_formatRequestIdMatchesGenerateId
                ]
            , testGroup
                "buildStorageKey"
                [ testProperty "creates correct structure" prop_buildStorageKeyStructure
                , testProperty "handles empty strings" prop_buildStorageKeyEmpty
                ]
            , testGroup
                "filterValidValues"
                [ testProperty "removes Nothing values" prop_filterValidValuesRemovesNothing
                , testProperty "preserves order" prop_filterValidValuesPreservesOrder
                , testProperty "all Nothing returns empty" prop_filterValidValuesAllNothing
                , testProperty "all Just returns all values" prop_filterValidValuesAllJust
                ]
            ]
        , testGroup
            "Storage roundtrip"
            [ testProperty "write then read returns same value" prop_writeReadRoundtrip
            , testProperty "readMaybe returns Nothing for missing" prop_readMaybeNotFound
            , testProperty "readMaybe returns Nothing for invalid JSON" prop_readMaybeSkipsInvalidJson
            , testProperty "writeRequest uses correct key" prop_writeRequestUsesCorrectKey
            ]
        , testGroup
            "Listing"
            [ testProperty "list empty for non-existent directory" prop_listEmptyDir
            , testProperty "list contains written key" prop_listContainsWrittenKey
            , testProperty "list contains multiple keys" prop_listMultipleKeys
            , testProperty "listRequests finds written value" prop_listRequestsFindsValue
            , testProperty "listRequests skips invalid JSON" prop_listRequestsSkipsInvalid
            ]
        , testGroup
            "ID generation"
            [ testProperty "generateId has req_ prefix" prop_generateIdPrefix
            , testProperty "generateId produces unique values" prop_generateIdUnique
            , testProperty "generateId has non-empty suffix" prop_generateIdNonEmpty
            , testProperty "generateId suffix is hex" prop_generateIdHex
            ]
        ]
