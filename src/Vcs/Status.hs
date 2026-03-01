{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Vcs.Status
Description : Git status parsing and querying

This module provides functionality for querying git repository status,
including file status parsing from porcelain output and branch detection.

== Usage

@
-- Load status from a repository (includes line counts)
status <- loadStatus exeCache "/path/to/repo"

-- Get the current branch
branch <- loadBranch exeCache "/path/to/repo"
@
-}
module Vcs.Status (
    -- * Types
    FileStatus (..),

    -- * Pure parsing functions
    parseNumstat,
    parseNumstatLine,
    parseStatusCode,
    parseBranchName,

    -- * IO operations
    loadBranch,
    loadStatus,
) where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)
import Util.ExeCache (ExeCache)
import Util.Git (runGit, withGit)
import Vcs.Internal (splitNonEmptyLines)

-- | Represents the status of a single file in a git repository.
data FileStatus = FileStatus
    { fsPath :: Text
    -- ^ The file path (for renames, this is the destination path)
    , fsAdded :: Int
    -- ^ Number of lines added
    , fsRemoved :: Int
    -- ^ Number of lines removed
    , fsStatus :: Text
    -- ^ Human-readable status: "modified", "added", "deleted"
    }
    deriving (Eq, Show)

instance ToJSON FileStatus where
    toJSON s =
        object
            [ "path" .= fsPath s
            , "added" .= fsAdded s
            , "removed" .= fsRemoved s
            , "status" .= fsStatus s
            ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure parsing functions
-- ═══════════════════════════════════════════════════════════════════════════

{- | Parse git diff --numstat output into a map of path -> (added, removed).

The numstat format is: @ADDED\tREMOVED\tPATH@
where ADDED and REMOVED are integers or "-" for binary files.

==== __Examples__

>>> parseNumstat "10\t5\tfile.txt"
fromList [("file.txt",(10,5))]
-}
parseNumstat :: Text -> Map Text (Int, Int)
parseNumstat = Map.fromList . mapMaybe parseNumstatLine . splitNonEmptyLines

{- | Parse a single line of numstat output.

Returns Nothing for binary files (which show as "-\t-\tpath").
-}
parseNumstatLine :: Text -> Maybe (Text, (Int, Int))
parseNumstatLine line =
    case T.splitOn "\t" line of
        [addedT, removedT, path] -> do
            added <- readMaybe (T.unpack addedT)
            removed <- readMaybe (T.unpack removedT)
            pure (path, (added, removed))
        -- Expected format is exactly 3 tab-separated fields
        [] -> Nothing
        [_oneField] -> Nothing
        [_field1, _field2] -> Nothing
        _fourOrMoreFields -> Nothing

{- | Convert a two-character git status code to a normalized status.

The OpenAPI schema only allows: "added", "deleted", "modified".
-}
parseStatusCode :: Text -> Text
parseStatusCode code
    | code == "??" = "added" -- untracked -> added
    | "A" `T.isInfixOf` code = "added"
    | "D" `T.isInfixOf` code = "deleted"
    | otherwise = "modified" -- M, R, C, U all become modified

-- ═══════════════════════════════════════════════════════════════════════════
-- IO operations
-- ═══════════════════════════════════════════════════════════════════════════

{- | Load the git status for a repository.

Uses a combination of git commands to get file status with line counts:

* @git diff --numstat HEAD@ - for modified files with line counts
* @git ls-files --others --exclude-standard@ - for untracked (added) files
* @git diff --name-only --diff-filter=D HEAD@ - for deleted files

This matches the TypeScript implementation and the OpenAPI File schema.

==== __Example__

@
status <- loadStatus exeCache "/path/to/repo"
for_ status $ \\file ->
    putStrLn $ fsPath file <> ": " <> fsStatus file
@
-}
loadStatus :: ExeCache -> FilePath -> IO [FileStatus]
loadStatus exeCache root = withGit exeCache [] $ do
    -- Get modified files with line counts
    numstatOut <- runGit exeCache root ["-c", "core.quotepath=false", "diff", "--numstat", "HEAD"]
    let numstatMap = maybe Map.empty parseNumstat numstatOut

    -- Get untracked files
    untrackedOut <- runGit exeCache root ["-c", "core.quotepath=false", "ls-files", "--others", "--exclude-standard"]
    let untrackedFiles = maybe [] splitNonEmptyLines untrackedOut

    -- Get deleted files
    deletedOut <- runGit exeCache root ["-c", "core.quotepath=false", "diff", "--name-only", "--diff-filter=D", "HEAD"]
    let deletedFiles = maybe [] splitNonEmptyLines deletedOut

    -- Build FileStatus list
    let modifiedStatuses =
            [ FileStatus path added removed "modified"
            | (path, (added, removed)) <- Map.toList numstatMap
            , path `notElem` deletedFiles -- exclude deleted files from modified
            ]
        -- For untracked files, we don't have line counts (would need to read each file)
        -- TypeScript counts lines by reading the file, but for now we use 0
        untrackedStatuses =
            [ FileStatus path 0 0 "added"
            | path <- untrackedFiles
            ]
        deletedStatuses =
            [ FileStatus path 0 (maybe 0 snd (Map.lookup path numstatMap)) "deleted"
            | path <- deletedFiles
            ]

    pure $ modifiedStatuses ++ untrackedStatuses ++ deletedStatuses

{- | Load the current branch name for a repository.

Attempts to get the branch via @git symbolic-ref@, falling back to
@git rev-parse@ if that fails. Returns 'Nothing' if:

* Git is not available
* The repository is in detached HEAD state
* The branch name cannot be determined

==== __Example__

@
mbranch <- loadBranch exeCache "/path/to/repo"
case mbranch of
    Just branch -> putStrLn $ "On branch: " <> branch
    Nothing -> putStrLn "Detached HEAD or not a git repo"
@
-}
loadBranch :: ExeCache -> FilePath -> IO (Maybe Text)
loadBranch exeCache root = withGit exeCache Nothing $ do
    symbolicRef <- runGit exeCache root ["symbolic-ref", "--short", "HEAD"]
    revParse <- runGit exeCache root ["rev-parse", "--abbrev-ref", "HEAD"]
    pure $ parseBranchName =<< (symbolicRef <|> revParse)

{- | Parse a branch name, returning 'Nothing' for empty or HEAD.

This handles the case where rev-parse returns "HEAD" for detached state.
-}
parseBranchName :: Text -> Maybe Text
parseBranchName name =
    let stripped = T.strip name
     in if stripped == "" || stripped == "HEAD"
            then Nothing
            else Just stripped
