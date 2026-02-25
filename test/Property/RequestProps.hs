{-# LANGUAGE OverloadedStrings #-}

module Property.RequestProps where

import Data.Aeson (Value (..), object, (.=))
import Data.List qualified as List
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
import Test.Tasty
import Test.Tasty.Hedgehog

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

genText :: Gen Text
genText = Gen.text (Range.linear 1 10) Gen.alphaNum

genValue :: Gen Value
genValue = do
    text <- genText
    pure $ object ["id" .= text]

listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0

tests :: TestTree
tests =
    testGroup
        "Request Store Property Tests"
        [ testGroup
            "Storage roundtrip"
            [ testProperty "write then read returns same value" prop_writeReadRoundtrip
            , testProperty "readMaybe returns Nothing for missing" prop_readMaybeNotFound
            , testProperty "readMaybe returns Nothing for invalid JSON" prop_readMaybeSkipsInvalidJson
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
