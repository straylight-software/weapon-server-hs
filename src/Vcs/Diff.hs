{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Vcs.Diff
Description : Git diff parsing and statistics

This module provides functionality for parsing git diff output,
computing diff statistics (additions, deletions, file counts),
and loading diffs from repositories.

== Usage

@
-- Parse numstat output for summary statistics
let summary = parseNumstat "10\t5\tfile.hs\n3\t1\tanother.hs"

-- Parse numstat for per-file statistics
let fileDiffs = parseNumstatFiles "10\t5\tfile.hs"

-- Load diff from repository
(diffText, summary) <- loadDiff exeCache "/path/to/repo"
@
-}
module Vcs.Diff (
    -- * Types
    FileDiffInternal (..),
    VcsError (..),

    -- * Pure parsing functions
    parseNumstat,
    parseNumstatFiles,
    parseNumstatLine,
    readNumstatInt,

    -- * IO operations
    loadDiff,
    loadFileDiffs,

    -- * Git availability checking
    requireGit,

    -- * Pure helpers (for testing)
    combineDiffResults,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Session.Types qualified as ST
import Text.Read (readMaybe)
import Util.ExeCache (ExeCache, findExecutableCached)
import Util.Git (runGit)
import Vcs.Internal (listLength, splitNonEmptyLines, splitTabFields, sumInts)

-- ═══════════════════════════════════════════════════════════════════════════
-- Types
-- ═══════════════════════════════════════════════════════════════════════════

{- | Error thrown when git is not available on the system.

This error is thrown by IO operations that require git when the
executable cannot be found in PATH.
-}
data VcsError = GitNotFound
    deriving (Eq)

instance Show VcsError where
    show GitNotFound = "Required tool 'git' not found in PATH. Install git: https://git-scm.com/"

instance Exception VcsError

{- | Internal representation of diff statistics for a single file.

This is used internally by the session system to track per-file
changes. The 'Internal' suffix indicates this is not part of the
public API contract.
-}
data FileDiffInternal = FileDiffInternal
    { fdiFile :: Text
    -- ^ The file path
    , fdiAdditions :: Int
    -- ^ Number of lines added
    , fdiDeletions :: Int
    -- ^ Number of lines deleted
    }
    deriving (Show, Eq)

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure parsing functions
-- ═══════════════════════════════════════════════════════════════════════════

{- | Parse text as an integer, defaulting to 0 for non-numeric values.

This handles the "-" output that git uses for binary files.

==== __Examples__

>>> readNumstatInt "42"
42

>>> readNumstatInt "-"
0

>>> readNumstatInt ""
0
-}
readNumstatInt :: Text -> Int
readNumstatInt txt
    | T.all isDigit txt = fromMaybe 0 $ readMaybe (T.unpack txt)
    | otherwise = 0

{- | Parse git diff --numstat output into a 'SessionSummary'.

The numstat format is:

@
ADDITIONS\tDELETIONS\tFILENAME
@

Binary files show "-\t-\tfilename" and contribute 0 to the counts.

==== __Examples__

>>> parseNumstat "10\t5\tfile.hs\n3\t1\tanother.hs"
SessionSummary {ssAdditions = 13, ssDeletions = 6, ssFiles = Just 2}

>>> parseNumstat ""
SessionSummary {ssAdditions = 0, ssDeletions = 0, ssFiles = Just 0}
-}
parseNumstat :: Text -> ST.SessionSummary
parseNumstat input =
    let stats = parseAllLines input
        (totalAdds, totalDels) = sumStats stats
        fileCount = listLength stats
     in ST.SessionSummary totalAdds totalDels (Just fileCount)

{- | Parse a single numstat line into (additions, deletions).

Returns (0, 0) for malformed lines or binary files.
-}
parseNumstatLine :: [Text] -> (Int, Int)
parseNumstatLine fields = case fields of
    (addTxt : delTxt : _) -> (readNumstatInt addTxt, readNumstatInt delTxt)
    _otherFields -> (0, 0)

{- | Parse numstat output into individual file diff entries.

==== __Examples__

>>> parseNumstatFiles "10\t5\tfile.hs"
[FileDiffInternal {fdiFile = "file.hs", fdiAdditions = 10, fdiDeletions = 5}]

>>> parseNumstatFiles ""
[]
-}
parseNumstatFiles :: Text -> [FileDiffInternal]
parseNumstatFiles input =
    map parseFileDiffLine (splitLines input)

-- ═══════════════════════════════════════════════════════════════════════════
-- Internal helpers (pure)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Split input into non-empty lines and tab-separated fields.
splitLines :: Text -> [[Text]]
splitLines = map splitTabFields . splitNonEmptyLines

-- | Parse all lines and extract (additions, deletions) pairs.
parseAllLines :: Text -> [(Int, Int)]
parseAllLines = map parseNumstatLine . splitLines

-- | Sum up all addition/deletion pairs.
sumStats :: [(Int, Int)] -> (Int, Int)
sumStats stats = (sumInts (map fst stats), sumInts (map snd stats))

-- | Parse a single line's fields into a FileDiffInternal.
parseFileDiffLine :: [Text] -> FileDiffInternal
parseFileDiffLine fields = case fields of
    (addTxt : delTxt : file : _) ->
        FileDiffInternal file (readNumstatInt addTxt) (readNumstatInt delTxt)
    _otherFields -> FileDiffInternal "" 0 0

-- ═══════════════════════════════════════════════════════════════════════════
-- IO operations
-- ═══════════════════════════════════════════════════════════════════════════

{- | Check that git is available, throwing 'GitNotFound' if not.

This is factored out to avoid duplicating the git check in
multiple IO functions.
-}
requireGit :: ExeCache -> IO ()
requireGit exeCache = do
    hasGit <- isJust <$> findExecutableCached exeCache "git"
    unless hasGit $ throwIO GitNotFound

{- | Load the full diff and summary statistics for a repository.

Runs @git diff --no-color@ and @git diff --numstat@ and combines
the results.

Returns 'Nothing' if the git commands fail (e.g., not a git repository).

Throws 'GitNotFound' if git is not available.

==== __Example__

@
result <- loadDiff exeCache "/path/to/repo"
case result of
    Just (diffText, summary) -> do
        putStrLn $ "Added: " <> show (ssAdditions summary)
        putStrLn diffText
    Nothing -> putStrLn "No diff available"
@
-}
loadDiff :: ExeCache -> FilePath -> IO (Maybe (Text, ST.SessionSummary))
loadDiff exeCache root = do
    requireGit exeCache
    mDiff <- runGit exeCache root ["diff", "--no-color"]
    mNum <- runGit exeCache root ["diff", "--numstat"]
    pure $ combineDiffResults mDiff mNum

{- | Combine diff and numstat outputs into a result.

Pure helper for loadDiff to enable easier testing.
-}
combineDiffResults :: Maybe Text -> Maybe Text -> Maybe (Text, ST.SessionSummary)
combineDiffResults mDiff mNum =
    case (mDiff, mNum) of
        (Just diffOut, Just numOut) -> Just (diffOut, parseNumstat numOut)
        _otherOutputs -> Nothing

{- | Load per-file diff statistics for a repository.

Runs @git diff --numstat@ and parses the output into individual
file entries.

Returns an empty list if the git command fails.

Throws 'GitNotFound' if git is not available.

==== __Example__

@
fileDiffs <- loadFileDiffs exeCache "/path/to/repo"
for_ fileDiffs $ \\fd ->
    putStrLn $ fdiFile fd <> ": +" <> show (fdiAdditions fd) <> "/-" <> show (fdiDeletions fd)
@
-}
loadFileDiffs :: ExeCache -> FilePath -> IO [FileDiffInternal]
loadFileDiffs exeCache root = do
    requireGit exeCache
    mNum <- runGit exeCache root ["diff", "--numstat"]
    pure $ maybe [] parseNumstatFiles mNum
