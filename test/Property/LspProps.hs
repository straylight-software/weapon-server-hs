{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.LspProps
Description : Property tests for Lsp.Store module
Stability   : experimental

Property-based tests for the LSP diagnostics storage system.
Tests both the IO operations and pure helper functions.
-}
module Property.LspProps where

import Control.Exception (SomeException, toException)
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

-- * IO Property Tests

{- | Property: setDiagnostics followed by getDiagnostics returns the same values.
This tests the round-trip property of the storage system.
-}
prop_setGetDiagnostics :: Property
prop_setGetDiagnostics = propertyWithTempDir $ \tmpDir -> do
    values <- forAll $ Gen.list (Range.linear 0 5) genDiagnosticValue
    let storageDir = tmpDir </> ".opencode" </> "storage"
    result <- evalIO $ Storage.withStorage storageDir $ \store -> do
        LspStore.setDiagnostics store values
        LspStore.getDiagnostics store
    result === values

{- | Property: getDiagnostics falls back to file-based storage when primary fails.
This tests backward compatibility with legacy file locations.
-}
prop_getDiagnosticsFileFallback :: Property
prop_getDiagnosticsFileFallback = propertyWithTempDir $ \tmpDir -> do
    values <- forAll $ Gen.list (Range.linear 1 5) genDiagnosticValue
    let storageDir = tmpDir </> ".opencode" </> "storage"
    result <- evalIO $ Storage.withStorage storageDir $ \store -> do
        -- Canonicalize to resolve symlinks (important in nix sandbox)
        base <- canonicalizePath (takeDirectory (Storage.storageDir store))
        let path = base </> "lsp" </> "diagnostics.json"
        createDirectoryIfMissing True (base </> "lsp")
        Aeson.encodeFile path values
        LspStore.getDiagnostics store
    result === values

-- | Property: getDiagnostics returns empty list when no diagnostics exist.
prop_getDiagnosticsEmpty :: Property
prop_getDiagnosticsEmpty = propertyWithTempDir $ \tmpDir -> do
    let storageDir = tmpDir </> ".opencode" </> "storage"
    result <- evalIO $ Storage.withStorage storageDir $ \store -> do
        LspStore.getDiagnostics store
    result === []

-- | Property: overwriting diagnostics replaces previous values.
prop_setDiagnosticsOverwrites :: Property
prop_setDiagnosticsOverwrites = propertyWithTempDir $ \tmpDir -> do
    values1 <- forAll $ Gen.list (Range.linear 1 3) genDiagnosticValue
    values2 <- forAll $ Gen.list (Range.linear 1 3) genDiagnosticValue
    let storageDir = tmpDir </> ".opencode" </> "storage"
    result <- evalIO $ Storage.withStorage storageDir $ \store -> do
        LspStore.setDiagnostics store values1
        LspStore.setDiagnostics store values2
        LspStore.getDiagnostics store
    result === values2

-- * Pure Property Tests

-- | Property: extractDiagnosticValues extracts values from Right (Array xs).
prop_extractDiagnosticValuesArray :: Property
prop_extractDiagnosticValuesArray = property $ do
    values <- forAll $ Gen.list (Range.linear 0 10) genDiagnosticValue
    -- Use Aeson to create the Array (which uses Vector internally)
    let arrayValue = Aeson.toJSON values
        input = Right arrayValue :: Either SomeException Value
    LspStore.extractDiagnosticValues input === Just values

-- | Property: extractDiagnosticValues returns Nothing for non-array values.
prop_extractDiagnosticValuesNonArray :: Property
prop_extractDiagnosticValuesNonArray = property $ do
    value <- forAll genNonArrayValue
    let input = Right value :: Either SomeException Value
    LspStore.extractDiagnosticValues input === Nothing

-- | Property: extractDiagnosticValues returns Nothing for Left (errors).
prop_extractDiagnosticValuesError :: Property
prop_extractDiagnosticValuesError = property $ do
    let err = toException (userError "test error")
        input = Left err :: Either SomeException Value
    LspStore.extractDiagnosticValues input === Nothing

-- | Property: diagnosticPaths generates exactly 4 paths.
prop_diagnosticPathsCount :: Property
prop_diagnosticPathsCount = property $ do
    dir <- forAll genFilePath
    -- Check exact count using pattern matching (avoids length on list)
    case LspStore.diagnosticPaths dir of
        [_, _, _, _] -> success
        _other -> failure

-- | Property: diagnosticPaths includes expected path components.
prop_diagnosticPathsContainExpectedPaths :: Property
prop_diagnosticPathsContainExpectedPaths = property $ do
    let dir = "/home/user/.opencode/storage"
        paths = LspStore.diagnosticPaths dir
    -- First path should be dir/lsp/diagnostics.json
    assert $ (dir </> "lsp" </> "diagnostics.json") `elem` paths
    -- Second path should be dir/diagnostics.json
    assert $ (dir </> "diagnostics.json") `elem` paths

-- | Property: diagKey is the expected value.
prop_diagKeyValue :: Property
prop_diagKeyValue = property $ do
    LspStore.diagKey === ["lsp", "diagnostics"]

-- * Generators

-- | Generate a diagnostic value (a JSON object with line number).
genDiagnosticValue :: Gen Value
genDiagnosticValue = do
    line <- Gen.int (Range.linear 1 200)
    col <- Gen.int (Range.linear 1 80)
    severity <- Gen.element ["error", "warning", "info", "hint"]
    pure $
        object
            [ "line" .= line
            , "column" .= col
            , "severity" .= (severity :: String)
            ]

-- | Generate a non-array JSON value.
genNonArrayValue :: Gen Value
genNonArrayValue =
    Gen.choice
        [ pure Null
        , Bool <$> Gen.bool
        , Number . fromIntegral <$> Gen.int (Range.linear 0 100)
        , String <$> Gen.text (Range.linear 0 20) Gen.unicode
        , pure $ object ["key" .= ("value" :: String)]
        ]

-- | Generate a simple file path.
genFilePath :: Gen FilePath
genFilePath = do
    segments <-
        Gen.list (Range.linear 1 5) $
            Gen.string (Range.linear 1 10) Gen.alphaNum
    pure $ "/" <> foldr (</>) "" segments

-- * Test Tree

tests :: TestTree
tests =
    testGroup
        "LSP Property Tests"
        [ testGroup
            "IO Operations"
            [ testProperty "set/get diagnostics round-trip" prop_setGetDiagnostics
            , testProperty "file fallback" prop_getDiagnosticsFileFallback
            , testProperty "empty storage returns empty list" prop_getDiagnosticsEmpty
            , testProperty "set overwrites previous values" prop_setDiagnosticsOverwrites
            ]
        , testGroup
            "Pure Functions"
            [ testProperty "extractDiagnosticValues extracts array values" prop_extractDiagnosticValuesArray
            , testProperty "extractDiagnosticValues returns Nothing for non-arrays" prop_extractDiagnosticValuesNonArray
            , testProperty "extractDiagnosticValues returns Nothing for errors" prop_extractDiagnosticValuesError
            , testProperty "diagnosticPaths generates 4 paths" prop_diagnosticPathsCount
            , testProperty "diagnosticPaths contains expected paths" prop_diagnosticPathsContainExpectedPaths
            , testProperty "diagKey has expected value" prop_diagKeyValue
            ]
        ]
