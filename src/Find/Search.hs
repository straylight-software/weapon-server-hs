{-# LANGUAGE OverloadedStrings #-}

module Find.Search (
    findText,
    findFile,
    findFileWithOptions,
    findSymbol,
    SearchError (..),
    FindFileOptions (..),
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

-- | Error when a required CLI tool is missing
data SearchError = MissingExecutable
    { seName :: String
    , seDescription :: String
    }
    deriving (Eq)

instance Show SearchError where
    show (MissingExecutable name desc) =
        "Required tool '" <> name <> "' not found in PATH. " <> desc

instance Exception SearchError

findText :: FilePath -> Text -> IO [Value]
findText root query = do
    runRg root query

findSymbol :: FilePath -> Text -> IO [Value]
findSymbol root query = do
    runRg root query

-- | Options for findFile search
data FindFileOptions = FindFileOptions
    { ffoIncludeDirs :: Bool
    -- ^ Include directories in results
    , ffoFileType :: Maybe Text
    -- ^ Filter by type: "file", "directory", or Nothing for both
    , ffoLimit :: Maybe Int
    -- ^ Maximum number of results
    }

-- | Default options (files only, no limit)
defaultFindFileOptions :: FindFileOptions
defaultFindFileOptions = FindFileOptions False Nothing Nothing

findFile :: FilePath -> Text -> IO [Value]
findFile root pat = findFileWithOptions root pat defaultFindFileOptions

findFileWithOptions :: FilePath -> Text -> FindFileOptions -> IO [Value]
findFileWithOptions root pat opts = do
    exe <- findExecutable "fd"
    case exe of
        Nothing -> throwIO $ MissingExecutable "fd" "Install fd-find: https://github.com/sharkdp/fd"
        Just _fdExe -> do
            let typeArgs = case ffoFileType opts of
                    Just "file" -> ["--type", "f"]
                    Just "directory" -> ["--type", "d"]
                    Nothing -> if ffoIncludeDirs opts then [] else ["--type", "f"]
                    Just _otherType -> ["--type", "f"] -- default to files for unknown types
            (code, out, _) <- readProcessWithExitCode "fd" (typeArgs ++ ["--glob", T.unpack pat, root]) ""
            case code of
                ExitSuccess -> do
                    let results = map toValue $ mapMaybe parseFdLine (T.lines (T.pack out))
                    pure $ case ffoLimit opts of
                        Just n -> take n results
                        Nothing -> results
                ExitFailure _exitCode -> pure []
  where
    toValue path = object ["path" .= path]

runRg :: FilePath -> Text -> IO [Value]
runRg root query = do
    exe <- findExecutable "rg"
    case exe of
        Nothing -> throwIO $ MissingExecutable "rg" "Install ripgrep: https://github.com/BurntSushi/ripgrep"
        Just _rgExe -> do
            (code, out, _) <- readProcessWithExitCode "rg" ["--line-number", "--no-heading", "--color", "never", T.unpack query, root] ""
            case code of
                ExitSuccess -> pure $ map toValue $ mapMaybe parseRgLine (T.lines (T.pack out))
                ExitFailure _exitCode -> pure []
  where
    toValue (path, lineNum, text) = object ["path" .= path, "line" .= lineNum, "text" .= text]
