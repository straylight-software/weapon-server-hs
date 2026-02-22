{-# LANGUAGE OverloadedStrings #-}

module Property.RequestProps where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Request.Store qualified as RequestStore
import Storage.Storage qualified as Storage
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import Test.Tasty
import Test.Tasty.Hedgehog

withStore :: (Storage.StorageConfig -> IO a) -> IO a
withStore action = do
    tmpDir <- createTempDirectory "/tmp" "request-test"
    result <- Storage.withStorage tmpDir action
    removeDirectoryRecursive tmpDir
    pure result

prop_requestRoundtrip :: Property
prop_requestRoundtrip = property $ do
    kind <- forAll genText
    req <- forAll genText
    value <- forAll genValue
    result <- evalIO $ withStore $ \store -> do
        RequestStore.writeRequest store kind req value
        RequestStore.listRequests store kind
    assert $ value `elem` result

prop_requestEmptyList :: Property
prop_requestEmptyList = property $ do
    kind <- forAll genText
    result <- evalIO $ withStore $ \store ->
        RequestStore.listRequests store kind
    result === []

prop_requestSkipsInvalidJson :: Property
prop_requestSkipsInvalidJson = property $ do
    kind <- forAll genText
    req <- forAll genText
    value <- forAll genValue
    result <- evalIO $ withStore $ \store -> do
        RequestStore.writeRequest store kind req value
        let badReq = if req == "invalid" then "invalid2" else "invalid"
        let dir = Storage.storageDir store </> T.unpack kind
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> T.unpack badReq <> ".json") "{"
        RequestStore.listRequests store kind
    assert $ value `elem` result
    length result === 1

prop_generateIdPrefix :: Property
prop_generateIdPrefix = property $ do
    reqId <- evalIO RequestStore.generateId
    assert $ "req_" `T.isPrefixOf` reqId

prop_generateIdUnique :: Property
prop_generateIdUnique = property $ do
    a <- evalIO RequestStore.generateId
    b <- evalIO RequestStore.generateId
    a /== b

prop_generateIdNonEmpty :: Property
prop_generateIdNonEmpty = property $ do
    reqId <- evalIO RequestStore.generateId
    assert $ T.length reqId > 4

genText :: Gen Text
genText = Gen.text (Range.linear 1 10) Gen.alphaNum

genValue :: Gen Value
genValue = do
    text <- genText
    pure $ object ["id" .= text]

tests :: TestTree
tests =
    testGroup
        "Request Store Property Tests"
        [ testProperty "request roundtrip" prop_requestRoundtrip
        , testProperty "request list empty" prop_requestEmptyList
        , testProperty "request skips invalid json" prop_requestSkipsInvalidJson
        , testProperty "generate id prefix" prop_generateIdPrefix
        , testProperty "generate id unique" prop_generateIdUnique
        , testProperty "generate id non-empty" prop_generateIdNonEmpty
        ]
