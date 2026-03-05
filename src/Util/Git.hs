{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Util.Git
Description : Shared Git utilities for running git commands

This module provides utilities for running git commands with proper
executable caching and error handling.

= Usage Example

@
cache <- 'Util.ExeCache.newExeCache'

-- Run a git command
mBranch <- 'runGit' cache "/path/to/repo" ["branch", "--show-current"]

-- Conditionally run an action if git is available
result <- 'withGit' cache "no git" $ do
    runGit cache "/path/to/repo" ["status"]
@

= Design

The module uses 'Util.ExeCache.ExeCache' to avoid repeated PATH lookups for
the git executable. Pure functions are exposed for testing the result parsing
logic without IO.
-}
module Util.Git (
    -- * IO API (production use)
    withGit,
    runGit,

    -- * Pure helpers (for testing)
    parseGitResult,
    buildGitArgs,
) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Util.ExeCache (ExeCache, findExecutableCached)

{- | Run an action if git is available, otherwise return a fallback.

This is useful for gracefully degrading when git is not installed.

@
status <- 'withGit' cache "unknown" $ do
    mStatus <- 'runGit' cache root ["status", "--porcelain"]
    pure $ fromMaybe "error" mStatus
@
-}
withGit :: ExeCache -> a -> IO a -> IO a
withGit exeCache fallback action = do
    hasGit <- isJust <$> findExecutableCached exeCache "git"
    if hasGit then action else pure fallback

{- | Run a git command in a directory, returning stdout on success.

The command is run with @-C \<root\>@ to set the working directory.
Returns 'Nothing' if:

* Git is not installed
* The command exits with a non-zero status

Returns @'Just' output@ on success, where @output@ is the stdout.
-}
runGit :: ExeCache -> FilePath -> [String] -> IO (Maybe Text)
runGit exeCache root args = do
    mGitPath <- findExecutableCached exeCache "git"
    case mGitPath of
        Nothing -> pure Nothing
        Just gitPath -> do
            let fullArgs = buildGitArgs root args
            (code, out, _err) <- readProcessWithExitCode gitPath fullArgs ""
            pure $ parseGitResult code out

{- | Parse git command result (pure, for testing).

Returns 'Just' the output text on success, 'Nothing' on failure.
-}
parseGitResult :: ExitCode -> String -> Maybe Text
parseGitResult ExitSuccess out = Just (T.pack out)
parseGitResult (ExitFailure _) _ = Nothing

{- | Build git command arguments with -C flag (pure, for testing).

Prepends @["-C", root]@ to the provided arguments to run git
in the specified directory.
-}
buildGitArgs :: FilePath -> [String] -> [String]
buildGitArgs root args = ["-C", root] ++ args
