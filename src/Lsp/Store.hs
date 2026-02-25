{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Lsp.Store
Description : LSP diagnostics persistence layer
Stability   : experimental

This module provides persistence for LSP (Language Server Protocol) diagnostics.
It stores and retrieves diagnostic information using the project's storage system,
with fallback support for reading from legacy file locations.

= Usage

@
import qualified Lsp.Store as LspStore
import qualified Storage.Storage as Storage

main :: IO ()
main = Storage.withStorage ".opencode/storage" $ \store -> do
    -- Store diagnostics
    let diagnostics = [object ["line" .= (1 :: Int), "message" .= "Error"]]
    LspStore.setDiagnostics store diagnostics

    -- Retrieve diagnostics
    retrieved <- LspStore.getDiagnostics store
    print retrieved
@
-}
module Lsp.Store (
    -- * Diagnostics Operations
    getDiagnostics,
    setDiagnostics,

    -- * Pure Helpers (for testing)
    diagKey,
    diagnosticPaths,
    extractDiagnosticValues,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Foldable (toList)
import Data.Text (Text)
import Storage.Storage qualified as Storage
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (takeDirectory, (</>))

{- | The storage key path for LSP diagnostics.
Used as the key prefix when reading/writing to the storage system.
-}
diagKey :: [Text]
diagKey = ["lsp", "diagnostics"]

{- | Retrieve LSP diagnostics from storage.

This function first attempts to read diagnostics from the primary storage
location. If that fails (e.g., file doesn't exist or contains invalid data),
it falls back to searching legacy file locations.

==== __Examples__

>>> storage <- Storage.withStorage ".opencode/storage" pure
>>> diagnostics <- getDiagnostics storage
>>> length diagnostics
0
-}
getDiagnostics :: Storage.StorageConfig -> IO [Value]
getDiagnostics storage = do
    result <- try @SomeException (Storage.read storage diagKey)
    case extractDiagnosticValues result of
        Just values -> pure values
        Nothing -> getDiagnosticsFromFiles storage

{- | Store LSP diagnostics to the storage system.

The diagnostics are serialized as a JSON array and persisted using
the standard storage mechanism.

==== __Examples__

>>> let diags = [object ["line" .= (1 :: Int)]]
>>> storage <- Storage.withStorage ".opencode/storage" pure
>>> setDiagnostics storage diags
-}
setDiagnostics :: Storage.StorageConfig -> [Value] -> IO ()
setDiagnostics storage values =
    Storage.write storage diagKey (Aeson.toJSON values)

{- | Extract diagnostic values from a storage read result.

Returns 'Just' the list of values if the result is a successful read
containing a JSON array. Returns 'Nothing' for any other case
(errors, non-array values, etc.).

This is a pure function that can be tested independently of IO.

==== __Examples__

>>> extractDiagnosticValues (Right (Array (V.fromList [Number 1])))
Just [Number 1.0]

>>> extractDiagnosticValues (Right (Object mempty))
Nothing

>>> extractDiagnosticValues (Left someException)
Nothing
-}
extractDiagnosticValues :: Either SomeException Value -> Maybe [Value]
extractDiagnosticValues (Right (Array xs)) = Just (toList xs)
extractDiagnosticValues _ = Nothing

{- | Read diagnostics from legacy file locations.

Searches multiple potential file paths where diagnostics might be stored
in older versions or alternative configurations. This provides backward
compatibility and works around symlink issues in Nix sandboxes.
-}
getDiagnosticsFromFiles :: Storage.StorageConfig -> IO [Value]
getDiagnosticsFromFiles storage = do
    let rawDir = Storage.storageDir storage
    -- Canonicalize to resolve symlinks (important in nix sandbox)
    dir <- canonicalizePath rawDir
    let paths = diagnosticPaths rawDir <> diagnosticPaths dir
    readFirstValid paths

{- | Generate all potential paths where diagnostics might be stored.

Given a base directory, this function generates a list of possible
file paths to check for diagnostic data. The paths are tried in order.

This is a pure function that can be tested independently of IO.

==== __Examples__

>>> diagnosticPaths "/home/user/.opencode/storage"
["/home/user/.opencode/storage/lsp/diagnostics.json","/home/user/.opencode/storage/diagnostics.json","/home/user/.opencode/lsp/diagnostics.json","/home/user/.opencode/diagnostics.json"]
-}
diagnosticPaths :: FilePath -> [FilePath]
diagnosticPaths dir =
    [ dir </> "lsp" </> "diagnostics.json"
    , dir </> "diagnostics.json"
    , takeDirectory dir </> "lsp" </> "diagnostics.json"
    , takeDirectory dir </> "diagnostics.json"
    ]

{- | Try to read diagnostics from a list of file paths.

Attempts each path in order, returning the first valid JSON array found.
Returns an empty list if no valid file is found.
-}
readFirstValid :: [FilePath] -> IO [Value]
readFirstValid [] = pure []
readFirstValid (path : rest) = do
    exists <- doesFileExist path
    if not exists
        then readFirstValid rest
        else readAndParseFile path rest

{- | Read and parse a single diagnostics file.

If parsing succeeds and yields an array, returns the values.
Otherwise, continues searching the remaining paths.
-}
readAndParseFile :: FilePath -> [FilePath] -> IO [Value]
readAndParseFile path rest = do
    result <- Aeson.eitherDecodeFileStrict path
    case result of
        Right (Array xs) -> pure (toList xs)
        Right _otherValue -> readFirstValid rest
        Left _err -> readFirstValid rest
