{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tool execution
module Tool.Exec (
    execute,
    executeToolUse,
)
where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, Value, eitherDecode, encode)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Types (ToolResult (..), ToolUse (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.Process (CreateProcess (..), StdStream (..), proc, readCreateProcessWithExitCode)
import Tool.Types

-- | Execute a tool from a ToolUse block
executeToolUse :: ToolContext -> ToolUse -> IO ToolResult
executeToolUse ctx ToolUse{..} = do
    result <- execute ctx tuName tuInput
    pure
        ToolResult
            { trToolUseId = tuId
            , trContent = getOutput result
            , trIsError = getIsError result
            }

-- | Extract output text from ToolOutput
getOutput :: ToolOutput -> Text
getOutput = toOutput

-- | Check if tool output is an error
getIsError :: ToolOutput -> Bool
getIsError = toIsError

-- | Execute a tool by name with JSON input
execute :: ToolContext -> Text -> Value -> IO ToolOutput
execute ctx name input = case name of
    "read" -> parseAndRun ctx input execRead
    "write" -> parseAndRun ctx input execWrite
    "edit" -> parseAndRun ctx input execEdit
    "bash" -> parseAndRun ctx input execBash
    "glob" -> parseAndRun ctx input execGlob
    "grep" -> parseAndRun ctx input execGrep
    _ -> pure $ toolError "Error" ("Unknown tool: " <> name)

-- | Parse input and run executor
parseAndRun :: (FromJSON a) => ToolContext -> Value -> (ToolContext -> a -> IO ToolOutput) -> IO ToolOutput
parseAndRun ctx input exec = case eitherDecode (encode input) of
    Left err -> pure $ toolError "Parse Error" (T.pack err)
    Right parsed -> exec ctx parsed

-- | Read file or directory
execRead :: ToolContext -> ReadInput -> IO ToolOutput
execRead ctx ReadInput{..} = do
    let path = T.unpack (resolvePath ctx riFilePath)
    let offset = maybe 1 id riOffset
    let limit = maybe 2000 id riLimit

    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if isFile
        then do
            result <- try @SomeException $ TIO.readFile path
            case result of
                Left e -> pure $ toolError "Read Error" (T.pack $ show e)
                Right content -> do
                    let ls = T.lines content
                    let numbered = zipWith (\n l -> T.pack (show (n :: Int)) <> ": " <> l) [1 ..] ls
                    let sliced = take limit $ drop (offset - 1) numbered
                    pure $ toolSuccess ("Read " <> riFilePath) (T.unlines sliced)
        else
            if isDir
                then do
                    result <- try @SomeException $ listDirectory path
                    case result of
                        Left e -> pure $ toolError "Read Error" (T.pack $ show e)
                        Right entries -> pure $ toolSuccess ("List " <> riFilePath) (T.unlines $ map T.pack entries)
                else
                    pure $ toolError "Read Error" ("Path does not exist: " <> riFilePath)

-- | Write file
execWrite :: ToolContext -> WriteInput -> IO ToolOutput
execWrite ctx WriteInput{..} = do
    let path = T.unpack (resolvePath ctx wiFilePath)
    result <- try @SomeException $ do
        createDirectoryIfMissing True (takeDirectory path)
        TIO.writeFile path wiContent
    case result of
        Left e -> pure $ toolError "Write Error" (T.pack $ show e)
        Right () -> pure $ toolSuccess ("Wrote " <> wiFilePath) ("Successfully wrote " <> T.pack (show (T.length wiContent)) <> " characters")

-- | Edit file
execEdit :: ToolContext -> EditInput -> IO ToolOutput
execEdit ctx EditInput{..} = do
    let path = T.unpack (resolvePath ctx eiFilePath)
    let replaceAll = fromMaybe False eiReplaceAll

    result <- try @SomeException $ TIO.readFile path
    case result of
        Left e -> pure $ toolError "Edit Error" (T.pack $ show e)
        Right content -> do
            let count = length $ T.breakOnAll eiOldString content
            if count == 0
                then pure $ toolError "Edit Error" "oldString not found in content"
                else if count > 1 && not replaceAll
                    then pure $ toolError "Edit Error" ("Found " <> T.pack (show count) <> " matches for oldString. Provide more surrounding lines to identify the correct match or use replaceAll.")
                    else do
                        let newContent =
                                if replaceAll
                                    then T.replace eiOldString eiNewString content
                                    else replaceFirst eiOldString eiNewString content
                        writeResult <- try @SomeException $ TIO.writeFile path newContent
                        case writeResult of
                            Left e -> pure $ toolError "Edit Error" (T.pack $ show e)
                            Right () -> pure $ toolSuccess ("Edited " <> eiFilePath) ("Replaced " <> T.pack (show (if replaceAll then count else 1)) <> " occurrence(s)")

-- | Replace first occurrence
replaceFirst :: Text -> Text -> Text -> Text
replaceFirst old new txt = case T.breakOn old txt of
    (before, after)
        | T.null after -> txt
        | otherwise -> before <> new <> T.drop (T.length old) after

-- | Execute bash command
execBash :: ToolContext -> BashInput -> IO ToolOutput
execBash ctx BashInput{..} = do
    let workdir = maybe (tcWorkdir ctx) T.unpack biWorkdir
    let timeoutMs = maybe 120000 id biTimeout
    let timeoutS = max 1 ((timeoutMs + 999) `div` 1000)

    let cmd =
            (proc "timeout" [show timeoutS, "bash", "-c", T.unpack biCommand])
                { cwd = Just workdir
                , std_in = NoStream
                }

    result <- try @SomeException $ readCreateProcessWithExitCode cmd ""
    case result of
        Left e -> pure $ toolError "Bash Error" (T.pack $ show e)
        Right (exitCode, stdout, stderr) -> do
            let output = T.pack stdout <> (if null stderr then "" else "\n[stderr]\n" <> T.pack stderr)
            let output' =
                    if T.null output
                        then "Command completed successfully with no output."
                        else output
            if exitCode /= ExitSuccess
                then pure $ toolError "Command failed" output
                else pure $ toolSuccess biDescription output'

-- | Glob file search
execGlob :: ToolContext -> GlobInput -> IO ToolOutput
execGlob ctx GlobInput{..} = do
    let searchPath = maybe "." T.unpack giPath
    let cwdDir = tcWorkdir ctx
    let cmd =
            (proc "fd" ["--type", "f", "--glob", T.unpack giPattern, searchPath])
                { cwd = Just cwdDir
                , std_in = NoStream
                }

    result <- try @SomeException $ readCreateProcessWithExitCode cmd ""
    case result of
        Left e -> pure $ toolError "Glob Error" (T.pack $ show e)
        Right (exitCode, stdout, stderr) -> do
            let outputLines = take 100 (T.lines (T.pack stdout))
            let output = T.unlines outputLines
            case exitCode of
                ExitSuccess -> pure $ toolSuccess ("Glob " <> giPattern) output
                ExitFailure _ ->
                    pure $
                        toolError
                            "Glob Error"
                            (if null stderr then T.pack stdout else T.pack stderr)

-- | Grep content search
execGrep :: ToolContext -> GrepInput -> IO ToolOutput
execGrep ctx GrepInput{..} = do
    let searchPath = maybe "." T.unpack grPath
    let cwdDir = tcWorkdir ctx
    let baseArgs = ["--line-number", "--no-heading", T.unpack grPattern]
    let includeArgs = maybe [] (\p -> ["--glob", T.unpack p]) grInclude
    let cmd =
            (proc "rg" (baseArgs <> includeArgs <> [searchPath]))
                { cwd = Just cwdDir
                , std_in = NoStream
                }

    result <- try @SomeException $ readCreateProcessWithExitCode cmd ""
    case result of
        Left e -> pure $ toolError "Grep Error" (T.pack $ show e)
        Right (exitCode, stdout, stderr) -> do
            let outputLines = take 100 (T.lines (T.pack stdout))
            let output = T.unlines outputLines
            case exitCode of
                ExitSuccess -> pure $ toolSuccess ("Grep " <> grPattern) output
                ExitFailure 1 | null stdout -> pure $ toolSuccess ("Grep " <> grPattern) ""
                ExitFailure _ ->
                    pure $
                        toolError
                            "Grep Error"
                            (if null stderr then T.pack stdout else T.pack stderr)

-- | Resolve path relative to workdir if not absolute
resolvePath :: ToolContext -> Text -> Text
resolvePath ctx p
    | "/" `T.isPrefixOf` p = p
    | otherwise = T.pack (tcWorkdir ctx) <> "/" <> p
