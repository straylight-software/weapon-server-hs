{-# LANGUAGE OverloadedStrings #-}

{- | Storage module - JSON file-based persistence
Mirrors the TypeScript Storage namespace
-}
module Storage.Storage (
    read,
    readMaybe,
    write,
    writeAtomic,
    update,
    remove,
    list,
    NotFoundError (..),
    StorageError (..),
    withStorage,
    StorageConfig (..),
) where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encode)
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory
import System.FilePath (dropExtension, splitDirectories, takeDirectory, takeExtension, (</>))
import System.IO (hClose, hFlush)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Temp (mkstemp)
import Util.FileSystem (listDirectoryRecursive)
import Prelude hiding (read)

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
Uses direct write for performance. For crash-safe atomic writes,
use writeAtomic instead.
-}
write :: (ToJSON a) => StorageConfig -> [Text] -> a -> IO ()
write cfg key content = do
    let target = keyPath cfg key
        dir = takeDirectory target
        encoded = encode content
    createDirectoryIfMissing True dir
    BL.writeFile target encoded

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
