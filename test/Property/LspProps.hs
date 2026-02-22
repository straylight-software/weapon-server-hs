{-# LANGUAGE OverloadedStrings #-}

module Property.LspProps where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Lsp.Store qualified as LspStore
import Storage.Storage qualified as Storage
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Test.Fixture (propertyWithTempDir)
import Test.Tasty
import Test.Tasty.Hedgehog

prop_setGetDiagnostics :: Property
prop_setGetDiagnostics = propertyWithTempDir $ \tmpDir -> do
    values <- forAll $ Gen.list (Range.linear 0 5) genValue
    let storageDir = tmpDir </> ".opencode" </> "storage"
    result <- evalIO $ Storage.withStorage storageDir $ \store -> do
        LspStore.setDiagnostics store values
        LspStore.getDiagnostics store
    result === values

prop_getDiagnosticsFileFallback :: Property
prop_getDiagnosticsFileFallback = propertyWithTempDir $ \tmpDir -> do
    values <- forAll $ Gen.list (Range.linear 1 5) genValue
    let storageDir = tmpDir </> ".opencode" </> "storage"
    result <- evalIO $ Storage.withStorage storageDir $ \store -> do
        -- Canonicalize to resolve symlinks (important in nix sandbox)
        base <- canonicalizePath (takeDirectory (Storage.storageDir store))
        let path = base </> "lsp" </> "diagnostics.json"
        createDirectoryIfMissing True (base </> "lsp")
        Aeson.encodeFile path values
        LspStore.getDiagnostics store
    result === values

genValue :: Gen Value
genValue = do
    line <- Gen.int (Range.linear 1 200)
    pure $ object ["line" .= line]

tests :: TestTree
tests =
    testGroup
        "LSP Property Tests"
        [ testProperty "set/get diagnostics" (withTests 1000 prop_setGetDiagnostics)
        , testProperty "file fallback" (withTests 1000 prop_getDiagnosticsFileFallback)
        ]
