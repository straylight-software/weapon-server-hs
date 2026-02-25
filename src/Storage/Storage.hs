{-# LANGUAGE OverloadedStrings #-}

{- | Storage module - JSON file-based persistence
Mirrors the TypeScript Storage namespace
-}
module Storage.Storage (
    read,
    readMaybe,
    write,
    writeCached,
    writeAtomic,
    update,
    remove,
    list,
    NotFoundError (..),
    StorageError (..),
    withStorage,
    StorageConfig (..),

    -- * Directory cache for performance
    DirCache,
    newDirCache,
) where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encode, encodeFile)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory
import System.FilePath (dropExtension, splitDirectories, takeDirectory, takeExtension, (</>))
import System.IO (hClose, hFlush)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Temp (mkstemp)
import Util.FileSystem (listDirectoryRecursive)
import Prelude hiding (read)

-- | Cache for directories we've already created (avoids redundant mkdir/stat calls)
newtype DirCache = DirCache (IORef (Set FilePath))

-- | Create a new empty directory cache
newDirCache :: IO DirCache
newDirCache = DirCache <$> newIORef Set.empty

-- | Create directory if needed, using cache to avoid redundant operations
createDirCached :: DirCache -> FilePath -> IO ()
createDirCached (DirCache ref) dir = do
    known <- readIORef ref
    unless (Set.member dir known) $ do
        createDirectoryIfMissing True dir
        atomicModifyIORef' ref $ \s -> (Set.insert dir s, ())

-- | Storage configuration
newtype StorageConfig = StorageConfig
    { storageDir :: FilePath
    }
    deriving (Show, Eq)

-- | Not found error
newtype NotFoundError = NotFoundError {notFoundPath :: FilePath}
    deriving (Show, Eq)

instance Exception NotFoundError

-- | Storage error
data StorageError
    = StorageDecodeError FilePath Text
    deriving (Show, Eq)

instance Exception StorageError

-- | Initialize storage with a base directory
withStorage :: FilePath -> (StorageConfig -> IO a) -> IO a
withStorage dir action = do
    createDirectoryIfMissing True dir
    action (StorageConfig dir)

-- | Build the full path for a key
keyPath :: StorageConfig -> [Text] -> FilePath
keyPath cfg key = storageDir cfg </> foldr ((</>) . T.unpack) "" key <> ".json"

-- | Read a JSON value from storage
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

-- | Read a JSON value from storage, returning Nothing if not found
readMaybe :: (FromJSON a) => StorageConfig -> [Text] -> IO (Maybe a)
readMaybe cfg key =
    ( (Just <$> read cfg key)
        `catch` \(NotFoundError _) -> pure Nothing
    )
        `catch` \(StorageDecodeError _path _err) -> pure Nothing

{- | Write a JSON value to storage
Uses encodeFile for direct encoding to file (avoids intermediate lazy ByteString).
For crash-safe atomic writes, use writeAtomic instead.

This version creates directories on every call. For better performance
when doing many writes, use writeCached with a DirCache.
-}
write :: (ToJSON a) => StorageConfig -> [Text] -> a -> IO ()
write cfg key content = do
    let target = keyPath cfg key
        dir = takeDirectory target
    createDirectoryIfMissing True dir
    encodeFile target content

{- | Write a JSON value to storage with directory caching
Avoids redundant createDirectoryIfMissing calls when writing to the same
or nearby directories repeatedly. Uses encodeFile for efficiency.
-}
writeCached :: (ToJSON a) => DirCache -> StorageConfig -> [Text] -> a -> IO ()
writeCached dirCache cfg key content = do
    let target = keyPath cfg key
        dir = takeDirectory target
    createDirCached dirCache dir
    encodeFile target content

{- | Write a JSON value to storage using atomic write (temp file + rename)
This ensures the file is never partially written, but has more overhead.
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

-- | Update a JSON value in storage
update :: (FromJSON a, ToJSON a) => StorageConfig -> [Text] -> (a -> a) -> IO a
update cfg key fn = do
    val <- read cfg key
    let updated = fn val
    write cfg key updated
    pure updated

-- | Remove a value from storage
remove :: StorageConfig -> [Text] -> IO ()
remove cfg key = do
    let target = keyPath cfg key
    removeFile target `catch` \e ->
        unless (isDoesNotExistError e) $ throwIO e

-- | List all keys with a given prefix
list :: StorageConfig -> [Text] -> IO [[Text]]
list cfg prefix = do
    let dir = storageDir cfg </> foldr ((</>) . T.unpack) "" prefix
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else do
            files <- listDirectoryRecursive dir
            let jsonFiles = filter (\f -> takeExtension f == (".json" :: String)) files
            pure $ map (toKey prefix dir) jsonFiles
  where
    toKey pfx base file =
        let rel = drop (listLength base + 1) file
            parts = splitDirectories (dropExtension rel)
         in pfx ++ map T.pack parts

listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0
