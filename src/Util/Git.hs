{-# LANGUAGE OverloadedStrings #-}

-- | Shared Git utilities
module Util.Git (
    withGit,
    runGit,
) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Run an action if git is available, otherwise return a fallback
withGit :: a -> IO a -> IO a
withGit fallback action = do
    hasGit <- isJust <$> findExecutable "git"
    if hasGit then action else pure fallback

-- | Run a git command in a directory, returning stdout on success
runGit :: FilePath -> [String] -> IO (Maybe Text)
runGit root args = do
    (code, out, _) <- readProcessWithExitCode "git" (["-C", root] ++ args) ""
    pure $ case code of
        ExitSuccess -> Just (T.pack out)
        ExitFailure _exitCode -> Nothing
