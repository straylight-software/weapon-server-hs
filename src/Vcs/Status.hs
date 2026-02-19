{-# LANGUAGE OverloadedStrings #-}

module Vcs.Status (
    FileStatus (..),
    parsePorcelain,
    loadBranch,
    loadStatus,
) where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Util.Git (runGit, withGit)

data FileStatus = FileStatus
    { fsPath :: Text
    , fsStatus :: Text
    }
    deriving (Eq, Show)

instance ToJSON FileStatus where
    toJSON s =
        object
            [ "path" .= fsPath s
            , "status" .= fsStatus s
            ]

parsePorcelain :: Text -> [FileStatus]
parsePorcelain input =
    map toStatus $ filter (not . T.null) (T.lines input)
  where
    toStatus line =
        let (code, rest) = T.splitAt 2 line
            pathRaw = T.dropWhile (== ' ') rest
            path = parsePath pathRaw
         in FileStatus path (codeStatus code)

    parsePath raw =
        case T.splitOn " -> " raw of
            [] -> raw
            parts -> last parts

    codeStatus code
        | code == "??" = "untracked"
        | "U" `T.isInfixOf` code = "unmerged"
        | "A" `T.isInfixOf` code = "added"
        | "D" `T.isInfixOf` code = "deleted"
        | "R" `T.isInfixOf` code = "renamed"
        | "C" `T.isInfixOf` code = "copied"
        | "M" `T.isInfixOf` code = "modified"
        | otherwise = "unknown"

loadStatus :: FilePath -> IO [FileStatus]
loadStatus root = withGit [] $ do
    mout <- runGit root ["status", "--porcelain"]
    pure $ maybe [] parsePorcelain mout

loadBranch :: FilePath -> IO (Maybe Text)
loadBranch root = withGit Nothing $ do
    symbolicRef <- runGit root ["symbolic-ref", "--short", "HEAD"]
    revParse <- runGit root ["rev-parse", "--abbrev-ref", "HEAD"]
    let result = (symbolicRef <|> revParse) >>= parseBranchName
    pure result
  where
    parseBranchName name =
        let stripped = T.strip name
         in if stripped == "" || stripped == "HEAD"
                then Nothing
                else Just stripped
