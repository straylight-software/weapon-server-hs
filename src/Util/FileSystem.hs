{-# LANGUAGE OverloadedStrings #-}

-- | Shared filesystem utilities
module Util.FileSystem (
    listDirectoryRecursive,
) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

-- | Recursively list all files in a directory
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
    contents <- listDirectory dir
    paths <- forM contents $ \name -> do
        let path = dir </> name
        isDir <- doesDirectoryExist path
        if isDir
            then listDirectoryRecursive path
            else pure [path]
    pure (concat paths)
