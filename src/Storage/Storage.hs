{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Storage.Storage
Description : JSON file-based persistence layer

This module provides a JSON file-based storage system for persisting
application data. Keys are represented as lists of 'Text' segments which
map to filesystem paths with @.json@ extensions.

= Architecture

The storage system uses a hierarchical key structure where each key is
a list of 'Text' segments. For example, the key @[\"session\", \"proj1\", \"sess1\"]@
maps to the file @\<storageDir\>\/session\/proj1\/sess1.json@.

= Write Strategies

Three write strategies are provided with different performance/safety tradeoffs:

  * 'write' - Simple write, creates directories on each call
  * 'writeCached' - Uses a 'DirCache' to avoid redundant directory creation
  * 'writeAtomic' - Uses atomic write (temp file + rename) for crash safety

= Usage Example

@
import Storage.Storage qualified as Storage

main = Storage.withStorage \"\/var\/lib\/myapp\" $ \\cfg -> do
    Storage.write cfg [\"users\", \"alice\"] (User \"Alice\" 30)
    user <- Storage.read cfg [\"users\", \"alice\"]
    print user
@
-}
module Storage.Storage (
    -- * Configuration
    StorageConfig (..),
    withStorage,

    -- * Reading
    read,
    readMaybe,
    readMaybeLogged,

    -- * Writing
    write,
    writeCached,
    writeAtomic,

    -- * Update and Delete
    update,
    remove,

    -- * Listing
    list,

    -- * Error Types
    NotFoundError (..),
    StorageError (..),

    -- * Directory Cache

    {- | A cache for tracking which directories have been created.
    Use with 'writeCached' for better performance when writing many files.
    -}
    DirCache,
    newDirCache,

    -- * Pure Helpers (exported for testing)
    keyPath,
    keyToRelativeParts,
) where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecodeFileStrict, encode, encodeFile, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Katip (LogItem (..), PayloadSelection (..), ToObject (..))
import Log qualified
import System.Directory
import System.FilePath (dropExtension, splitDirectories, takeDirectory, takeExtension, (</>))
import System.IO (hClose, hFlush)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Temp (mkstemp)
import Util.FileSystem (listDirectoryRecursive)
import Prelude hiding (read)

-- ═══════════════════════════════════════════════════════════════════════════
-- Types
-- ═══════════════════════════════════════════════════════════════════════════

{- | Storage configuration holding the base directory path.

Create using 'withStorage' which ensures the directory exists.
-}
newtype StorageConfig = StorageConfig
    { storageDir :: FilePath
    -- ^ The base directory where all storage files are kept
    }
    deriving (Show, Eq)

{- | Error thrown when a requested key does not exist in storage.

Catch this exception to handle missing keys gracefully, or use 'readMaybe'
which returns 'Nothing' instead of throwing.
-}
newtype NotFoundError = NotFoundError
    { notFoundPath :: FilePath
    -- ^ The filesystem path that was not found
    }
    deriving (Show, Eq)

instance Exception NotFoundError

{- | Error thrown when JSON decoding fails.

This indicates the file exists but contains invalid JSON or JSON that
doesn't match the expected type.
-}
data StorageError
    = -- | @StorageDecodeError path message@ - decoding failed at @path@ with @message@
      StorageDecodeError FilePath Text
    deriving (Show, Eq)

instance Exception StorageError

{- | Cache for directories we've already created.

This avoids redundant @mkdir@ and @stat@ calls when writing many files
to the same or nearby directories. Thread-safe via 'IORef' with atomic updates.
-}
newtype DirCache = DirCache (IORef (Set FilePath))

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Helpers
-- ═══════════════════════════════════════════════════════════════════════════

{- | Build the full filesystem path for a storage key.

The key segments are joined with path separators and @.json@ is appended.

@
keyPath (StorageConfig \"\/data\") [\"users\", \"alice\"] == \"\/data\/users\/alice.json\"
@

__Note__: This is a pure function, suitable for unit testing.
-}
keyPath :: StorageConfig -> [Text] -> FilePath
keyPath cfg key = storageDir cfg </> keyToRelativePath key

{- | Convert a key to a relative file path (without base directory).

@
keyToRelativePath [\"session\", \"proj1\", \"sess1\"] == \"session\/proj1\/sess1.json\"
@
-}
keyToRelativePath :: [Text] -> FilePath
keyToRelativePath key = foldr ((</>) . T.unpack) "" key <> ".json"

{- | Convert a file path back to key segments.

This is the inverse of 'keyToRelativePath' (modulo the base directory).

@
keyToRelativeParts \"\/data\/session\" \"\/data\/session\/proj1\/sess1.json\"
  == [\"session\", \"proj1\", \"sess1\"]
@

__Arguments__:

  * @prefix@ - The key prefix that was used for listing
  * @baseDir@ - The directory that was listed
  * @filePath@ - The full path to the JSON file

__Note__: This is a pure function, suitable for unit testing.
-}
keyToRelativeParts :: [Text] -> FilePath -> FilePath -> [Text]
keyToRelativeParts prefix baseDir filePath =
    -- Use foldl' for a strict, finite-safe length computation
    -- FilePaths are always finite strings from the filesystem
    let !baseDirLen = foldl' (\n _ -> n + 1) 0 baseDir
        rel = drop (baseDirLen + 1) filePath
        parts = splitDirectories (dropExtension rel)
     in prefix ++ map T.pack parts

-- ═══════════════════════════════════════════════════════════════════════════
-- Initialization
-- ═══════════════════════════════════════════════════════════════════════════

{- | Initialize storage with a base directory and run an action.

Creates the directory if it doesn't exist, then passes the 'StorageConfig'
to the provided action.

@
withStorage \"\/var\/lib\/myapp\" $ \\cfg -> do
    Storage.write cfg [\"config\"] myConfig
@
-}
withStorage :: FilePath -> (StorageConfig -> IO a) -> IO a
withStorage dir action = do
    createDirectoryIfMissing True dir
    action (StorageConfig dir)

{- | Create a new empty directory cache.

Use with 'writeCached' for better performance when writing many files:

@
cache <- newDirCache
forM_ items $ \\item ->
    writeCached cache cfg (itemKey item) item
@
-}
newDirCache :: IO DirCache
newDirCache = DirCache <$> newIORef Set.empty

-- ═══════════════════════════════════════════════════════════════════════════
-- Internal Helpers
-- ═══════════════════════════════════════════════════════════════════════════

-- | Create directory if needed, using cache to avoid redundant operations.
createDirCached :: DirCache -> FilePath -> IO ()
createDirCached (DirCache ref) dir = do
    known <- readIORef ref
    unless (Set.member dir known) $ do
        createDirectoryIfMissing True dir
        atomicModifyIORef' ref $ \s -> (Set.insert dir s, ())

-- ═══════════════════════════════════════════════════════════════════════════
-- Reading
-- ═══════════════════════════════════════════════════════════════════════════

{- | Read a JSON value from storage.

Throws 'NotFoundError' if the key does not exist.
Throws 'StorageDecodeError' if the JSON is invalid or doesn't match the expected type.

@
user <- Storage.read cfg [\"users\", \"alice\"] :: IO User
@
-}
read :: (FromJSON a) => StorageConfig -> [Text] -> IO a
read cfg key = do
    let target = keyPath cfg key
    result <- eitherDecodeFileStrict target `catch` handleNotFound target
    case result of
        Left err -> throwIO $ StorageDecodeError target (T.pack err)
        Right val -> pure val
  where
    handleNotFound :: FilePath -> IOError -> IO (Either String a)
    handleNotFound target e
        | isDoesNotExistError e = throwIO (NotFoundError target)
        | otherwise = throwIO e

{- | Read a JSON value from storage, returning 'Nothing' if not found or decode fails.

__Warning__: This function silently swallows decode errors, returning 'Nothing'
for both "not found" and "found but corrupted" cases. This makes it impossible
for callers to distinguish between missing data and corrupted data.

Prefer 'readMaybeLogged' which logs decode errors at ERROR level, giving
visibility into data corruption while maintaining the 'Maybe a' return type.

@
mUser <- Storage.readMaybe cfg [\"users\", \"alice\"]
case mUser of
    Nothing -> putStrLn \"User not found or corrupted\"
    Just user -> print user
@
-}
readMaybe :: (FromJSON a) => StorageConfig -> [Text] -> IO (Maybe a)
readMaybe cfg key =
    ( (Just <$> read cfg key)
        `catch` \(NotFoundError _) -> pure Nothing
    )
        `catch` \(StorageDecodeError _path _err) -> pure Nothing

{- | Read a JSON value from storage, returning 'Nothing' if not found.

Unlike 'readMaybe', this function logs decode errors at ERROR level before
returning 'Nothing'. This provides visibility into data corruption while
maintaining backwards compatibility with the 'Maybe a' return type.

__Rationale__: "Not found" is an expected case (the key simply doesn't exist),
but "found but corrupted" indicates a bug - either in serialization, storage,
or external data tampering. These cases should be logged so operators can
investigate and fix the underlying issue.

@
mUser <- Storage.readMaybeLogged logger cfg [\"users\", \"alice\"]
case mUser of
    Nothing -> putStrLn \"User not found\"
    Just user -> print user
@
-}
readMaybeLogged :: (FromJSON a) => Log.Logger -> StorageConfig -> [Text] -> IO (Maybe a)
readMaybeLogged logger cfg key =
    ( (Just <$> read cfg key)
        `catch` \(NotFoundError _) -> pure Nothing
    )
        `catch` \(StorageDecodeError path err) -> do
            -- Log decode errors at ERROR level. This indicates corrupted data which
            -- should never happen in normal operation. Logging gives visibility so
            -- operators can investigate the root cause (bad serialization, disk
            -- corruption, manual file edits, etc).
            Log.logError logger "Storage decode error: corrupted data found" $
                DecodeErrorPayload path err
            pure Nothing

-- | Structured payload for decode error logging
data DecodeErrorPayload = DecodeErrorPayload
    { _depPath :: FilePath
    , _depError :: Text
    }

instance ToObject DecodeErrorPayload where
    toObject (DecodeErrorPayload path err) =
        case object ["path" .= path, "error" .= err] of
            Object o -> o
            _ -> mempty

instance LogItem DecodeErrorPayload where
    payloadKeys _ _ = AllKeys

-- ═══════════════════════════════════════════════════════════════════════════
-- Writing
-- ═══════════════════════════════════════════════════════════════════════════

{- | Write a JSON value to storage.

Uses 'encodeFile' for direct encoding to file (avoids intermediate lazy ByteString).
Creates parent directories if they don't exist.

For crash-safe atomic writes, use 'writeAtomic' instead.
For better performance when doing many writes, use 'writeCached' with a 'DirCache'.

@
Storage.write cfg [\"users\", \"alice\"] (User \"Alice\" 30)
@
-}
write :: (ToJSON a) => StorageConfig -> [Text] -> a -> IO ()
write cfg key content = do
    let target = keyPath cfg key
        dir = takeDirectory target
    createDirectoryIfMissing True dir
    encodeFile target content

{- | Write a JSON value to storage with directory caching.

Avoids redundant 'createDirectoryIfMissing' calls when writing to the same
or nearby directories repeatedly. Use this for batch operations.

@
cache <- newDirCache
forM_ users $ \\user ->
    writeCached cache cfg [\"users\", userName user] user
@
-}
writeCached :: (ToJSON a) => DirCache -> StorageConfig -> [Text] -> a -> IO ()
writeCached dirCache cfg key content = do
    let target = keyPath cfg key
        dir = takeDirectory target
    createDirCached dirCache dir
    encodeFile target content

{- | Write a JSON value to storage using atomic write (temp file + rename).

This ensures the file is never partially written - it will either have
the complete old content or the complete new content. Use this when
data integrity is critical.

Has more overhead than 'write' due to the temp file creation.

@
Storage.writeAtomic cfg [\"critical\", \"data\"] importantData
@
-}
writeAtomic :: (ToJSON a) => StorageConfig -> [Text] -> a -> IO ()
writeAtomic cfg key content = do
    let target = keyPath cfg key
        dir = takeDirectory target
        encoded = encode content
    createDirectoryIfMissing True dir
    -- Atomic write: create temp file, write, flush, close, then rename
    (tmpPath, h) <- mkstemp (dir </> ".tmp.XXXXXX")
    BL.hPut h encoded
    hFlush h
    hClose h
    -- Rename is atomic on POSIX - the target file will either have
    -- the old content or the new content, never partial
    renamePath tmpPath target

-- ═══════════════════════════════════════════════════════════════════════════
-- Update and Delete
-- ═══════════════════════════════════════════════════════════════════════════

{- | Update a JSON value in storage by applying a function.

Reads the current value, applies the function, and writes the result.
Returns the updated value.

Throws 'NotFoundError' if the key does not exist.

@
updatedUser <- Storage.update cfg [\"users\", \"alice\"] $ \\user ->
    user { userAge = userAge user + 1 }
@
-}
update :: (FromJSON a, ToJSON a) => StorageConfig -> [Text] -> (a -> a) -> IO a
update cfg key fn = do
    val <- read cfg key
    let updated = fn val
    write cfg key updated
    pure updated

{- | Remove a value from storage.

Does nothing if the key doesn't exist (idempotent).

@
Storage.remove cfg [\"users\", \"alice\"]
@
-}
remove :: StorageConfig -> [Text] -> IO ()
remove cfg key = do
    let target = keyPath cfg key
    removeFile target `catch` \e ->
        unless (isDoesNotExistError e) $ throwIO e

-- ═══════════════════════════════════════════════════════════════════════════
-- Listing
-- ═══════════════════════════════════════════════════════════════════════════

{- | List all keys with a given prefix.

Returns all keys that start with the given prefix. The prefix segments
are included in the returned keys.

Returns an empty list if the directory doesn't exist.

@
-- List all sessions for a project
sessions <- Storage.list cfg [\"session\", \"myproject\"]
-- sessions might be [[\"session\", \"myproject\", \"sess1\"], [\"session\", \"myproject\", \"sess2\"]]
@
-}
list :: StorageConfig -> [Text] -> IO [[Text]]
list cfg prefix = do
    let dir = prefixToDirectory cfg prefix
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else do
            files <- listDirectoryRecursive dir
            let jsonFiles = filterJsonFiles files
            pure $ map (keyToRelativeParts prefix dir) jsonFiles

{- | Convert a key prefix to a directory path.

This is used internally by 'list' to find the directory to search.
-}
prefixToDirectory :: StorageConfig -> [Text] -> FilePath
prefixToDirectory cfg prefix = storageDir cfg </> foldr ((</>) . T.unpack) "" prefix

-- | Filter a list of file paths to only include .json files.
filterJsonFiles :: [FilePath] -> [FilePath]
filterJsonFiles = filter (\f -> takeExtension f == ".json")
