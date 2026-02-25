{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Find.Search
Description : File and text search functionality using external tools

This module provides search functionality for finding files and text content
within a codebase. It wraps external CLI tools:

* @ripgrep@ (@rg@) for fast text/regex searching
* @fd@ for fast file finding

The module separates pure transformation logic from IO operations to
facilitate testing.
-}
module Find.Search (
    -- * Text Search
    findText,
    findSymbol,

    -- * File Search
    findFile,
    findFileWithOptions,
    FindFileOptions (..),
    defaultFindFileOptions,

    -- * Pure Transformations
    rgMatchesToJson,
    fdResultsToJson,
    buildFdTypeArgs,
    applyResultLimit,

    -- * Errors
    SearchError (..),
) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Find.Parse

{- | Error thrown when a required CLI tool is not found in PATH.

This error indicates that the user needs to install a dependency
before the search functionality can be used.
-}
data SearchError = MissingExecutable
    { seName :: !String
    -- ^ Name of the missing executable (e.g., "rg", "fd")
    , seDescription :: !String
    -- ^ Human-readable description with installation instructions
    }
    deriving (Eq)

instance Show SearchError where
    show (MissingExecutable name desc) =
        "Required tool '" <> name <> "' not found in PATH. " <> desc

instance Exception SearchError

{- | Options for configuring file search behavior.

These options control what types of filesystem entries are returned
and how many results to include.
-}
data FindFileOptions = FindFileOptions
    { ffoIncludeDirs :: !Bool
    -- ^ Include directories in results (default: 'False')
    , ffoFileType :: !(Maybe Text)
    -- ^ Filter by type: @"file"@, @"directory"@, or 'Nothing' for both
    , ffoLimit :: !(Maybe Int)
    -- ^ Maximum number of results to return ('Nothing' for unlimited)
    }
    deriving (Eq, Show)

{- | Default search options: files only, no limit.

>>> defaultFindFileOptions
FindFileOptions {ffoIncludeDirs = False, ffoFileType = Nothing, ffoLimit = Nothing}
-}
defaultFindFileOptions :: FindFileOptions
defaultFindFileOptions = FindFileOptions False Nothing Nothing

--------------------------------------------------------------------------------
-- Text Search (IO)
--------------------------------------------------------------------------------

{- | Search for text content within files under the given root directory.

Uses @ripgrep@ to perform fast regex-capable text search.

==== Parameters

* @root@ - The root directory to search within
* @query@ - The search pattern (supports regex)

==== Returns

A list of JSON objects, each containing:

* @path@ - File path where match was found
* @line@ - Line number of the match
* @text@ - The matching line content

==== Errors

Throws 'SearchError' if @rg@ is not found in PATH.
-}
findText :: FilePath -> Text -> IO [Value]
findText = runRg

{- | Search for symbol definitions within files.

Currently uses the same implementation as 'findText', but could be
extended to use language-aware symbol search in the future.

==== Parameters

* @root@ - The root directory to search within
* @query@ - The symbol pattern to search for

==== Errors

Throws 'SearchError' if @rg@ is not found in PATH.
-}
findSymbol :: FilePath -> Text -> IO [Value]
findSymbol = runRg

--------------------------------------------------------------------------------
-- File Search (IO)
--------------------------------------------------------------------------------

{- | Find files matching a glob pattern.

Uses default options (files only, no limit).
For more control, use 'findFileWithOptions'.

==== Parameters

* @root@ - The root directory to search within
* @pattern@ - Glob pattern to match (e.g., @"*.hs"@, @"src/**/*.json"@)

==== Returns

A list of JSON objects, each containing:

* @path@ - Full path to the matching file

==== Errors

Throws 'SearchError' if @fd@ is not found in PATH.
-}
findFile :: FilePath -> Text -> IO [Value]
findFile root pat = findFileWithOptions root pat defaultFindFileOptions

{- | Find files matching a glob pattern with configurable options.

==== Parameters

* @root@ - The root directory to search within
* @pattern@ - Glob pattern to match
* @opts@ - Search options (see 'FindFileOptions')

==== Returns

A list of JSON objects, each containing:

* @path@ - Full path to the matching entry

==== Errors

Throws 'SearchError' if @fd@ is not found in PATH.
-}
findFileWithOptions :: FilePath -> Text -> FindFileOptions -> IO [Value]
findFileWithOptions root pat opts = do
    requireExecutable "fd" "Install fd-find: https://github.com/sharkdp/fd"
    let args = buildFdArgs opts pat root
    output <- runProcess "fd" args
    pure $ processFdOutput opts output

--------------------------------------------------------------------------------
-- Pure Transformation Functions
--------------------------------------------------------------------------------

{- | Convert ripgrep matches to JSON values.

Transforms parsed ripgrep output into JSON objects suitable for API responses.

==== Example

>>> rgMatchesToJson [("src/Main.hs", 42, "main = ...")]
[{"path":"src/Main.hs","line":42,"text":"main = ..."}]
-}
rgMatchesToJson :: [(Text, Int, Text)] -> [Value]
rgMatchesToJson = map toRgValue
  where
    toRgValue (path, lineNum, text) =
        object ["path" .= path, "line" .= lineNum, "text" .= text]

{- | Convert fd results to JSON values.

Transforms parsed fd output into JSON objects suitable for API responses.

==== Example

>>> fdResultsToJson ["src/Main.hs", "src/Lib.hs"]
[{"path":"src/Main.hs"},{"path":"src/Lib.hs"}]
-}
fdResultsToJson :: [Text] -> [Value]
fdResultsToJson = map toFdValue
  where
    toFdValue path = object ["path" .= path]

{- | Build fd type arguments based on options.

Determines the @--type@ flags to pass to fd based on the search options.

==== Examples

>>> buildFdTypeArgs (FindFileOptions False (Just "file") Nothing)
["--type", "f"]

>>> buildFdTypeArgs (FindFileOptions True Nothing Nothing)
[]

>>> buildFdTypeArgs (FindFileOptions False Nothing Nothing)
["--type", "f"]
-}
buildFdTypeArgs :: FindFileOptions -> [String]
buildFdTypeArgs opts =
    case ffoFileType opts of
        Just "file" -> ["--type", "f"]
        Just "directory" -> ["--type", "d"]
        Just _unknownType -> ["--type", "f"] -- default to files for unknown types
        Nothing ->
            if ffoIncludeDirs opts
                then []
                else ["--type", "f"]

{- | Apply an optional limit to a list of results.

==== Examples

>>> applyResultLimit (Just 2) ["a", "b", "c"]
["a", "b"]

>>> applyResultLimit Nothing ["a", "b", "c"]
["a", "b", "c"]
-}
applyResultLimit :: Maybe Int -> [a] -> [a]
applyResultLimit Nothing xs = xs
applyResultLimit (Just n) xs = take n xs

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Require an executable to be present, throwing 'SearchError' if not found.
requireExecutable :: String -> String -> IO ()
requireExecutable name installInstructions = do
    exe <- findExecutable name
    case exe of
        Nothing -> throwIO $ MissingExecutable name installInstructions
        Just _path -> pure ()

-- | Build the full argument list for fd.
buildFdArgs :: FindFileOptions -> Text -> FilePath -> [String]
buildFdArgs opts pat root =
    buildFdTypeArgs opts ++ ["--glob", T.unpack pat, root]

-- | Run a process and return its stdout on success, empty string on failure.
runProcess :: String -> [String] -> IO String
runProcess cmd args = do
    (code, out, _err) <- readProcessWithExitCode cmd args ""
    pure $ case code of
        ExitSuccess -> out
        ExitFailure _exitCode -> ""

-- | Process fd output into JSON values with limit applied.
processFdOutput :: FindFileOptions -> String -> [Value]
processFdOutput opts output =
    let paths = mapMaybe parseFdLine (T.lines (T.pack output))
        results = fdResultsToJson paths
     in applyResultLimit (ffoLimit opts) results

-- | Run ripgrep and return results as JSON.
runRg :: FilePath -> Text -> IO [Value]
runRg root query = do
    requireExecutable "rg" "Install ripgrep: https://github.com/BurntSushi/ripgrep"
    let args = ["--line-number", "--no-heading", "--color", "never", T.unpack query, root]
    output <- runProcess "rg" args
    pure $ processRgOutput output

-- | Process ripgrep output into JSON values.
processRgOutput :: String -> [Value]
processRgOutput output =
    let matches = mapMaybe parseRgLine (T.lines (T.pack output))
     in rgMatchesToJson matches
