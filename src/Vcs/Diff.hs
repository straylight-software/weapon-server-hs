{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Vcs.Diff
Description : Git diff parsing and statistics

This module provides functionality for parsing git diff output,
computing diff statistics (additions, deletions, file counts),
and loading diffs from repositories.

It supports two levels of detail:

* __Summary / numstat parsing__ via @git diff --numstat@
* __Per-file patch parsing__ via full unified diff output from @git diff@

The unified diff parser stores the raw per-file patch in 'fdiPatch' and
also includes optional 'fdiBefore' and 'fdiAfter' fields so the type is
ready for future expansion to full before/after file contents.

== Usage

@
-- Parse numstat output for summary statistics
let summary = parseNumstat "10\t5\tfile.hs\n3\t1\tanother.hs"

-- Parse numstat for per-file statistics
let fileDiffs = parseNumstatFiles "10\t5\tfile.hs"

-- Load diff from repository
(diffText, summary) <- loadDiff exeCache "/path/to/repo"

-- Load per-file unified diffs
fileDiffs' <- loadFileDiffs exeCache "/path/to/repo"
@
-}
module Vcs.Diff (
    -- * Types
    FileDiffInternal (..),
    FileDiffStatus (..),
    VcsError (..),

    -- * Pure parsing functions
    parseNumstat,
    parseNumstatFiles,
    parseNumstatLine,
    parseUnifiedDiffFiles,
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
import Data.List (find)
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

{- | High-level status for a file in a diff.

This is derived from unified diff metadata where available, and falls back
to a best-effort inference for numstat-only parsing.
-}
data FileDiffStatus
    = FileAdded
    | FileDeleted
    | FileModified
    | FileRenamed
    deriving (Show, Eq)

{- | Internal representation of diff information for a single file.

This type now supports both lightweight numstat parsing and richer
unified diff parsing.

The 'fdiBefore' and 'fdiAfter' fields are optional and are not populated
by the current loaders yet. They are included so the type is ready for a
future implementation that hydrates full before/after file contents
separately from the patch text.

The 'Internal' suffix indicates this is not part of the public API
contract.
-}
data FileDiffInternal = FileDiffInternal
    { fdiFile :: Text
    -- ^ The file path
    , fdiAdditions :: Int
    -- ^ Number of lines added
    , fdiDeletions :: Int
    -- ^ Number of lines deleted
    , fdiStatus :: FileDiffStatus
    -- ^ File-level diff status
    , fdiPatch :: Text
    -- ^ Raw unified diff block for this file, if available
    , fdiBefore :: Maybe Text
    -- ^ Optional full file contents before the change (reserved for future use)
    , fdiAfter :: Maybe Text
    -- ^ Optional full file contents after the change (reserved for future use)
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

Since numstat output does not contain patch bodies or exact status
metadata, this function performs a best-effort status inference and
leaves 'fdiPatch', 'fdiBefore', and 'fdiAfter' empty.

==== __Examples__

>>> parseNumstatFiles "10\t5\tfile.hs"
[FileDiffInternal {fdiFile = "file.hs", fdiAdditions = 10, fdiDeletions = 5, fdiStatus = FileModified, fdiPatch = "", fdiBefore = Nothing, fdiAfter = Nothing}]

>>> parseNumstatFiles ""
[]
-}
parseNumstatFiles :: Text -> [FileDiffInternal]
parseNumstatFiles input =
    map parseFileDiffLine (splitLines input)

{- | Parse unified diff output into individual file diff entries.

This expects standard @git diff@ output with blocks beginning with:

@
diff --git a/path b/path
@

Each block is parsed into a 'FileDiffInternal' containing:

* file path
* additions/deletions
* status
* raw per-file patch text

The optional 'fdiBefore' and 'fdiAfter' fields are currently left as
'Nothing'. They are reserved for a future loader that reads full file
contents for both sides of the diff.

==== __Example__

@
let diffs = parseUnifiedDiffFiles diffText
for_ diffs $ \fd ->
    putStrLn $ T.unpack (fdiFile fd) <> ": " <> show (fdiStatus fd)
@
-}
parseUnifiedDiffFiles :: Text -> [FileDiffInternal]
parseUnifiedDiffFiles input =
    map parseUnifiedDiffBlock (splitDiffBlocks input)

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

-- | Parse a single line's fields into a 'FileDiffInternal'.
parseFileDiffLine :: [Text] -> FileDiffInternal
parseFileDiffLine fields = case fields of
    (addTxt : delTxt : file : _) ->
        let adds = readNumstatInt addTxt
            dels = readNumstatInt delTxt
         in FileDiffInternal
                { fdiFile = file
                , fdiAdditions = adds
                , fdiDeletions = dels
                , fdiStatus = inferNumstatStatus adds dels
                , fdiPatch = ""
                , fdiBefore = Nothing
                , fdiAfter = Nothing
                }
    _otherFields ->
        FileDiffInternal
            { fdiFile = ""
            , fdiAdditions = 0
            , fdiDeletions = 0
            , fdiStatus = FileModified
            , fdiPatch = ""
            , fdiBefore = Nothing
            , fdiAfter = Nothing
            }

-- | Best-effort status inference for numstat-only output.
inferNumstatStatus :: Int -> Int -> FileDiffStatus
inferNumstatStatus adds dels
    | adds > 0 && dels == 0 = FileAdded
    | adds == 0 && dels > 0 = FileDeleted
    | otherwise = FileModified

-- | Split unified diff text into individual @diff --git@ blocks.
splitDiffBlocks :: Text -> [Text]
splitDiffBlocks input =
    case T.splitOn "diff --git " input of
        [] -> []
        (_prefix : rest) ->
            map ("diff --git " <>) $
                filter (not . T.null) rest

-- | Parse one per-file unified diff block.
parseUnifiedDiffBlock :: Text -> FileDiffInternal
parseUnifiedDiffBlock block =
    let ls = T.lines block
        file = parseDiffFile ls
        additions = countAddedLines ls
        deletions = countDeletedLines ls
        status = parseDiffStatus ls
     in FileDiffInternal
            { fdiFile = file
            , fdiAdditions = additions
            , fdiDeletions = deletions
            , fdiStatus = status
            , fdiPatch = block
            , fdiBefore = Nothing
            , fdiAfter = Nothing
            }

{- | Extract the file path for a unified diff block.

Prefers the @+++@ path for normal or added files, falls back to the
@---@ path for deleted files, and finally the @diff --git@ header.
-}
parseDiffFile :: [Text] -> Text
parseDiffFile ls =
    let newPath = cleanDiffPath =<< firstPathAfter "+++ " ls
        oldPath = cleanDiffPath =<< firstPathAfter "--- " ls
     in case (newPath, oldPath) of
            (Just p, _) -> p
            (Nothing, Just p) -> p
            _ -> parseFromDiffGitHeader ls

-- | Find the first line with a given prefix and return its suffix.
firstPathAfter :: Text -> [Text] -> Maybe Text
firstPathAfter prefix ls =
    T.drop (T.length prefix) <$> find (T.isPrefixOf prefix) ls

-- | Normalize diff header paths such as @a/foo@ or @b/foo@.
cleanDiffPath :: Text -> Maybe Text
cleanDiffPath p
    | p == "/dev/null" = Nothing
    | "a/" `T.isPrefixOf` p = Just (T.drop 2 p)
    | "b/" `T.isPrefixOf` p = Just (T.drop 2 p)
    | otherwise = Just p

-- | Fallback path extraction from the @diff --git@ header.
parseFromDiffGitHeader :: [Text] -> Text
parseFromDiffGitHeader ls = case ls of
    (hdr : _) ->
        case T.words hdr of
            ("diff" : "--git" : _oldPath : newPath : _) ->
                fromMaybe "" (cleanDiffPath newPath)
            _otherWords -> ""
    [] -> ""

-- | Count added lines in a unified diff block, excluding header lines.
countAddedLines :: [Text] -> Int
countAddedLines =
    length . filter isAddedLine

-- | Count deleted lines in a unified diff block, excluding header lines.
countDeletedLines :: [Text] -> Int
countDeletedLines =
    length . filter isDeletedLine

-- | Whether a line is an added content line in a hunk.
isAddedLine :: Text -> Bool
isAddedLine line =
    T.isPrefixOf "+" line && not (T.isPrefixOf "+++" line)

-- | Whether a line is a deleted content line in a hunk.
isDeletedLine :: Text -> Bool
isDeletedLine line =
    T.isPrefixOf "-" line && not (T.isPrefixOf "---" line)

-- | Determine file status from unified diff metadata.
parseDiffStatus :: [Text] -> FileDiffStatus
parseDiffStatus ls
    | any (T.isPrefixOf "new file mode ") ls = FileAdded
    | any (T.isPrefixOf "deleted file mode ") ls = FileDeleted
    | any (T.isPrefixOf "rename from ") ls || any (T.isPrefixOf "rename to ") ls = FileRenamed
    | otherwise = FileModified

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

Pure helper for 'loadDiff' to enable easier testing.
-}
combineDiffResults :: Maybe Text -> Maybe Text -> Maybe (Text, ST.SessionSummary)
combineDiffResults mDiff mNum =
    case (mDiff, mNum) of
        (Just diffOut, Just numOut) -> Just (diffOut, parseNumstat numOut)
        _otherOutputs -> Nothing

{- | Load per-file diffs for a repository.

Runs @git diff --no-color --no-ext-diff --find-renames@ and parses
the output into individual file entries.

This returns richer file-level information than @git diff --numstat@,
including:

* file path
* additions/deletions
* status
* raw unified patch block

The optional 'fdiBefore' and 'fdiAfter' fields are currently left as
'Nothing'. This function is structured so that full file-content hydration
can be added later without changing the exported type.

Returns an empty list if the git command fails.

Throws 'GitNotFound' if git is not available.

==== __Example__

@
fileDiffs <- loadFileDiffs exeCache "/path/to/repo"
for_ fileDiffs $ \fd ->
    putStrLn $
        T.unpack (fdiFile fd)
            <> ": "
            <> show (fdiStatus fd)
            <> " +"
            <> show (fdiAdditions fd)
            <> "/-"
            <> show (fdiDeletions fd)
@
-}
loadFileDiffs :: ExeCache -> FilePath -> IO [FileDiffInternal]
loadFileDiffs exeCache root = do
    requireGit exeCache
    mPatch <-
        runGit
            exeCache
            root
            [ "diff"
            , "--no-color"
            , "--no-ext-diff"
            , "--find-renames"
            ]
    pure $ maybe [] parseUnifiedDiffFiles mPatch
