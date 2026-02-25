{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Vcs.Status
Description : Git status parsing and querying

This module provides functionality for querying git repository status,
including file status parsing from porcelain output and branch detection.

== Usage

@
-- Parse porcelain output directly
let files = parsePorcelain "?? newfile.txt\nM  modified.txt"

-- Load status from a repository
status <- loadStatus exeCache "/path/to/repo"

-- Get the current branch
branch <- loadBranch exeCache "/path/to/repo"
@
-}
module Vcs.Status (
    -- * Types
    FileStatus (..),

    -- * Pure parsing functions
    parsePorcelain,
    parseLine,
    parseStatusCode,
    extractFinalPath,
    parseBranchName,

    -- * IO operations
    loadBranch,
    loadStatus,
) where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Util.ExeCache (ExeCache)
import Util.Git (runGit, withGit)
import Vcs.Internal (splitNonEmptyLines)

-- | Represents the status of a single file in a git repository.
data FileStatus = FileStatus
    { fsPath :: Text
    -- ^ The file path (for renames, this is the destination path)
    , fsStatus :: Text
    -- ^ Human-readable status: "untracked", "modified", "added", etc.
    }
    deriving (Eq, Show)

instance ToJSON FileStatus where
    toJSON s =
        object
            [ "path" .= fsPath s
            , "status" .= fsStatus s
            ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure parsing functions
-- ═══════════════════════════════════════════════════════════════════════════

{- | Parse git status porcelain (v1) output into a list of 'FileStatus'.

The porcelain format has two status characters followed by a space and the path:

@
XY PATH
XY ORIG_PATH -> PATH  (for renames/copies)
@

Where X is the index status and Y is the worktree status.

==== __Examples__

>>> parsePorcelain "?? newfile.txt"
[FileStatus {fsPath = "newfile.txt", fsStatus = "untracked"}]

>>> parsePorcelain "M  modified.txt\nA  added.txt"
[FileStatus {fsPath = "modified.txt", fsStatus = "modified"}, FileStatus {fsPath = "added.txt", fsStatus = "added"}]
-}
parsePorcelain :: Text -> [FileStatus]
parsePorcelain = map parseLine . splitNonEmptyLines

{- | Parse a single line of git status porcelain output.

Internal helper that splits the line into status code and path,
then converts both to their final representations.
-}
parseLine :: Text -> FileStatus
parseLine line =
    let (code, rest) = T.splitAt 2 line
        pathRaw = T.dropWhile (== ' ') rest
        path = extractFinalPath pathRaw
     in FileStatus path (parseStatusCode code)

{- | Convert a two-character git status code to a human-readable status.

The status codes follow git's porcelain format:

* @??@ - Untracked file
* @U@  - Unmerged (conflict)
* @A@  - Added to index
* @D@  - Deleted
* @R@  - Renamed
* @C@  - Copied
* @M@  - Modified

==== __Examples__

>>> parseStatusCode "??"
"untracked"

>>> parseStatusCode "M "
"modified"

>>> parseStatusCode " M"
"modified"
-}
parseStatusCode :: Text -> Text
parseStatusCode code
    | code == "??" = "untracked"
    | "U" `T.isInfixOf` code = "unmerged"
    | "A" `T.isInfixOf` code = "added"
    | "D" `T.isInfixOf` code = "deleted"
    | "R" `T.isInfixOf` code = "renamed"
    | "C" `T.isInfixOf` code = "copied"
    | "M" `T.isInfixOf` code = "modified"
    | otherwise = "unknown"

{- | Extract the final path from a porcelain path field.

For renames and copies, git outputs "old_path -> new_path".
This function extracts just the new path.

==== __Examples__

>>> extractFinalPath "file.txt"
"file.txt"

>>> extractFinalPath "old.txt -> new.txt"
"new.txt"
-}
extractFinalPath :: Text -> Text
extractFinalPath raw =
    case List.unsnoc (T.splitOn " -> " raw) of
        Nothing -> raw
        Just (_prefix, suffix) -> suffix

-- ═══════════════════════════════════════════════════════════════════════════
-- IO operations
-- ═══════════════════════════════════════════════════════════════════════════

{- | Load the git status for a repository.

Runs @git status --porcelain@ and parses the output.
Returns an empty list if git is not available.

==== __Example__

@
status <- loadStatus exeCache "/path/to/repo"
for_ status $ \\file ->
    putStrLn $ fsPath file <> ": " <> fsStatus file
@
-}
loadStatus :: ExeCache -> FilePath -> IO [FileStatus]
loadStatus exeCache root = withGit exeCache [] $ do
    mout <- runGit exeCache root ["status", "--porcelain"]
    pure $ maybe [] parsePorcelain mout

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
