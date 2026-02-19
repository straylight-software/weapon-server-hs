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
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Types (ToolResult (..), ToolUse (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.Process (readProcessWithExitCode)
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
    let replaceAll = maybe False id eiReplaceAll

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
    let timeoutS = timeoutMs `div` 1000

    -- Use timeout command for safety
    let cmdLine = "cd " <> show workdir <> " && " <> T.unpack biCommand
    let cmd = "timeout " <> show timeoutS <> " bash -c " <> show cmdLine

    result <- try @SomeException $ readProcessWithExitCode "bash" ["-c", cmd] ""
    case result of
        Left e -> pure $ toolError "Bash Error" (T.pack $ show e)
        Right (exitCode, stdout, stderr) -> do
            let output = T.pack stdout <> (if null stderr then "" else "\n[stderr]\n" <> T.pack stderr)
            if exitCode /= ExitSuccess
                then pure $ toolError "Command failed" output
                else pure $ toolSuccess biDescription output

-- | Glob file search
execGlob :: ToolContext -> GlobInput -> IO ToolOutput
execGlob ctx GlobInput{..} = do
    let searchPath = maybe (tcWorkdir ctx) T.unpack giPath
    -- Use fd for fast globbing
    let cmd = "fd --type f --glob " <> show (T.unpack giPattern) <> " " <> searchPath <> " | head -100"

    result <- try @SomeException $ readProcessWithExitCode "bash" ["-c", cmd] ""
    case result of
        Left e -> pure $ toolError "Glob Error" (T.pack $ show e)
        Right (_, stdout, _) -> pure $ toolSuccess ("Glob " <> giPattern) (T.pack stdout)

-- | Grep content search
execGrep :: ToolContext -> GrepInput -> IO ToolOutput
execGrep ctx GrepInput{..} = do
    let searchPath = maybe (tcWorkdir ctx) T.unpack grPath
    let includeArg = maybe "" (\p -> " --glob " <> show (T.unpack p)) grInclude
    -- Use ripgrep for fast searching
    let cmd = "rg --line-number --no-heading " <> show (T.unpack grPattern) <> includeArg <> " " <> searchPath <> " | head -100"

    result <- try @SomeException $ readProcessWithExitCode "bash" ["-c", cmd] ""
    case result of
        Left e -> pure $ toolError "Grep Error" (T.pack $ show e)
        Right (_, stdout, _) -> pure $ toolSuccess ("Grep " <> grPattern) (T.pack stdout)

-- | Resolve path relative to workdir if not absolute
resolvePath :: ToolContext -> Text -> Text
resolvePath ctx p
    | "/" `T.isPrefixOf` p = p
    | otherwise = T.pack (tcWorkdir ctx) <> "/" <> p
