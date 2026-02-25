{-# LANGUAGE OverloadedStrings #-}

-- | Shared Git utilities
module Util.Git (
    withGit,
    runGit,
) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Util.ExeCache (ExeCache, findExecutableCached)

-- | Run an action if git is available, otherwise return a fallback
withGit :: ExeCache -> a -> IO a -> IO a
withGit exeCache fallback action = do
    hasGit <- isJust <$> findExecutableCached exeCache "git"
    if hasGit then action else pure fallback

-- | Run a git command in a directory, returning stdout on success
runGit :: ExeCache -> FilePath -> [String] -> IO (Maybe Text)
runGit exeCache root args = do
    mGitPath <- findExecutableCached exeCache "git"
    case mGitPath of
        Nothing -> pure Nothing
        Just gitPath -> do
            (code, out, _) <- readProcessWithExitCode gitPath (["-C", root] ++ args) ""
            pure $ case code of
                ExitSuccess -> Just (T.pack out)
                ExitFailure _exitCode -> Nothing
