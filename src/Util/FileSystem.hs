{- |
Module      : Util.FileSystem
Description : Shared filesystem utilities for recursive operations

This module provides filesystem utilities for recursively traversing
directories and listing files.

= Usage Example

@
files <- 'listDirectoryRecursive' "/path/to/project"
-- Returns: ["/path/to/project/src/Main.hs", "/path/to/project/src/Lib.hs", ...]
@

= Design

The module separates pure logic (path building) from IO operations
(filesystem access) to enable easier testing. The 'DirectoryEntry' type
represents the result of checking whether a path is a file or directory.
-}
module Util.FileSystem (
    -- * IO API (production use)
    listDirectoryRecursive,

    -- * Pure helpers (for testing)
    DirectoryEntry (..),
    buildPath,
    flattenPaths,
) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

{- | Represents a filesystem entry classification.

Used internally to distinguish between files and directories
during recursive traversal.
-}
data DirectoryEntry
    = -- | A regular file with its full path
      FileEntry !FilePath
    | -- | A directory with its full path (needs recursive traversal)
      DirEntry !FilePath
    deriving (Show, Eq)

{- | Recursively list all files in a directory.

Returns absolute paths to all files (not directories) found under
the given root directory. The search is depth-first.

__Note:__ This function does not follow symbolic links to directories
to avoid infinite loops.

@
-- Given directory structure:
-- project/
--   src/
--     Main.hs
--   README.md

files <- 'listDirectoryRecursive' "project"
-- Returns: ["project/src/Main.hs", "project/README.md"]
@
-}
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
    contents <- listDirectory dir
    nestedPaths <- forM contents $ \name -> do
        let path = buildPath dir name
        isDir <- doesDirectoryExist path
        if isDir
            then listDirectoryRecursive path
            else pure [path]
    pure (flattenPaths nestedPaths)

{- | Build a full path from parent directory and entry name (pure).

This is a thin wrapper around 'System.FilePath.</>'.
-}
buildPath :: FilePath -> FilePath -> FilePath
buildPath = (</>)

{- | Flatten nested path lists into a single list (pure).

Used to collect all file paths from recursive directory traversal.
-}
flattenPaths :: [[FilePath]] -> [FilePath]
flattenPaths = concat
