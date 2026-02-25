{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Request.Store
Description : Persistent storage for request data

This module provides functions for persisting and retrieving request data
using the underlying 'Storage.Storage' system. Requests are stored as JSON
files organized by kind (category) and request ID.

The module exposes both IO functions for production use and pure functions
for testing, following the pattern established in "Util.Identifier".

== Example Usage

@
import Request.Store qualified as RequestStore
import Storage.Storage qualified as Storage

main :: IO ()
main = Storage.withStorage "\/tmp\/requests" $ \\storage -> do
    reqId <- RequestStore.generateId
    RequestStore.writeRequest storage \"session\" reqId myRequestValue
    requests <- RequestStore.listRequests storage \"session\"
    print requests
@
-}
module Request.Store (
    -- * IO API (production use)
    writeRequest,
    listRequests,
    generateId,

    -- * Pure API (for testing)
    formatRequestId,
    buildStorageKey,
    filterValidValues,

    -- * Types
    RequestId (..),
) where

import Data.Aeson (Value)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Numeric (showHex)
import Storage.Storage qualified as Storage
import System.Random (randomIO)

-- | A newtype wrapper for request identifiers
newtype RequestId = RequestId {unRequestId :: Text}
    deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Pure Functions (for testing)
--------------------------------------------------------------------------------

{- | Format a request ID from a random 64-bit value.

This is the pure core of 'generateId', allowing deterministic testing.

>>> formatRequestId 0xDEADBEEF
"req_deadbeef"

>>> formatRequestId 0
"req_0"
-}
formatRequestId :: Word64 -> Text
formatRequestId n = "req_" <> T.pack (showHex n "")

{- | Build a storage key from kind and request ID.

Storage keys are represented as a list of path segments.

>>> buildStorageKey "session" "req_abc123"
["session", "req_abc123"]
-}
buildStorageKey :: Text -> Text -> [Text]
buildStorageKey kind reqId = [kind, reqId]

{- | Filter out 'Nothing' values from a list, keeping only valid values.

This is used by 'listRequests' to skip over corrupted or unreadable entries.

>>> filterValidValues [Just 1, Nothing, Just 2]
[1, 2]
-}
filterValidValues :: [Maybe a] -> [a]
filterValidValues = catMaybes

--------------------------------------------------------------------------------
-- IO Functions (production use)
--------------------------------------------------------------------------------

{- | Write a request value to storage.

The request is stored at the path @\<kind\>\/\<requestId\>.json@ within
the storage directory.

@
writeRequest storage \"session\" \"req_abc123\" myValue
-- Creates: \<storage-dir\>\/session\/req_abc123.json
@

Note: This function uses non-atomic writes for performance. For crash-safe
writes, consider using 'Storage.writeAtomic' directly.
-}
writeRequest :: Storage.StorageConfig -> Text -> Text -> Value -> IO ()
writeRequest storage kind reqId = Storage.write storage (buildStorageKey kind reqId)

{- | List all request values stored under a given kind.

This function retrieves all requests of a particular kind (category) from
storage. Invalid JSON files are silently skipped.

@
-- Get all session requests
requests <- listRequests storage \"session\"
@

Returns an empty list if the kind directory doesn't exist or contains no
valid JSON files.
-}
listRequests :: Storage.StorageConfig -> Text -> IO [Value]
listRequests storage kind = do
    keys <- Storage.list storage [kind]
    values <- mapM (Storage.readMaybe storage) keys
    pure (filterValidValues values)

{- | Generate a new unique request ID.

Request IDs have the format @req_\<hex\>@ where @\<hex\>@ is a random
64-bit value encoded as hexadecimal.

>>> reqId <- generateId
>>> T.isPrefixOf "req_" reqId
True

Note: Unlike 'Util.Identifier.ascending', these IDs are not lexicographically
sortable by time. Use 'Util.Identifier' for time-ordered IDs.
-}
generateId :: IO Text
generateId = do
    n <- randomIO :: IO Word64
    pure (formatRequestId n)
