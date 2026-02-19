{-# LANGUAGE OverloadedStrings #-}

module Vcs.Diff (
    parseNumstat,
    loadDiff,
    loadFileDiffs,
    FileDiffInternal (..),
    VcsError (..),
) where

import Control.Exception (Exception, throwIO)
import Data.Char (isDigit)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Session.Types qualified as ST
import System.Directory (findExecutable)
import Util.Git (runGit)

-- | Error when git is not available
data VcsError = GitNotFound
    deriving (Eq)

instance Show VcsError where
    show GitNotFound = "Required tool 'git' not found in PATH. Install git: https://git-scm.com/"

instance Exception VcsError

parseNumstat :: Text -> ST.SessionSummary
parseNumstat input =
    let entries = filter (not . T.null) (T.lines input)
        parts = map (T.splitOn "\t") entries
        stats = map toStat parts
        adds = sum (map fst stats)
        dels = sum (map snd stats)
        count = length stats
     in ST.SessionSummary adds dels (Just count)
  where
    toStat fields = case fields of
        (addTxt : delTxt : _) -> (readInt addTxt, readInt delTxt)
        _ -> (0, 0)
    readInt txt =
        if T.all isDigit txt
            then case reads (T.unpack txt) of
                [(n, "")] -> n
                _ -> 0
            else 0

loadDiff :: FilePath -> IO (Maybe (Text, ST.SessionSummary))
loadDiff root = do
    hasGit <- isJust <$> findExecutable "git"
    if not hasGit
        then throwIO GitNotFound
        else do
            mDiff <- runGit root ["diff", "--no-color"]
            mNum <- runGit root ["diff", "--numstat"]
            case (mDiff, mNum) of
                (Just diffOut, Just numOut) -> pure $ Just (diffOut, parseNumstat numOut)
                _ -> pure Nothing

-- | Internal file diff representation
data FileDiffInternal = FileDiffInternal
    { fdiFile :: Text
    , fdiAdditions :: Int
    , fdiDeletions :: Int
    }
    deriving (Show)

-- | Parse numstat into individual file diffs
parseNumstatFiles :: Text -> [FileDiffInternal]
parseNumstatFiles input =
    let entries = filter (not . T.null) (T.lines input)
        parts = map (T.splitOn "\t") entries
     in map toFileDiff parts
  where
    toFileDiff fields = case fields of
        (addTxt : delTxt : file : _) ->
            FileDiffInternal file (readInt addTxt) (readInt delTxt)
        _ -> FileDiffInternal "" 0 0
    readInt txt =
        if T.all isDigit txt
            then case reads (T.unpack txt) of
                [(n, "")] -> n
                _ -> 0
            else 0

-- | Load file-level diffs for the session
loadFileDiffs :: FilePath -> IO [FileDiffInternal]
loadFileDiffs root = do
    hasGit <- isJust <$> findExecutable "git"
    if not hasGit
        then throwIO GitNotFound
        else do
            mNum <- runGit root ["diff", "--numstat"]
            case mNum of
                Just numOut -> pure $ parseNumstatFiles numOut
                Nothing -> pure []
