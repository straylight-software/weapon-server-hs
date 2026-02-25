{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tool execution
module Tool.Exec (
    execute,
    executeToolUse,
    executeToolUseStreaming,
    executeStreaming,

    -- * Streaming process execution
    runProcessStreaming,

    -- * Pure output processing (for testing)
    processBashOutput,
    processGlobOutput,
    processGrepOutput,
)
where

import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (IOException, SomeException, try)
import Data.Aeson (FromJSON, Value, eitherDecode, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import GHC.IO.Handle (Handle, hClose)
import LLM.Types (ToolResult (..), ToolUse (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Text.Printf (printf)
import Tool.Types

-- | Execute a tool from a ToolUse block
executeToolUse :: ToolContext -> ToolUse -> IO ToolResult
executeToolUse ctx tu = executeToolUseStreaming ctx tu noStreaming

-- | Execute a tool from a ToolUse block with streaming callback
executeToolUseStreaming :: ToolContext -> ToolUse -> StreamingCallback -> IO ToolResult
executeToolUseStreaming ctx ToolUse{..} callback = do
    result <- executeStreaming ctx tuName tuInput callback
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
execute ctx name input = executeStreaming ctx name input noStreaming

-- | Execute a tool by name with JSON input and streaming callback
executeStreaming :: ToolContext -> Text -> Value -> StreamingCallback -> IO ToolOutput
executeStreaming ctx name input callback = case name of
    "read" -> parseAndRun ctx input execRead -- read doesn't need streaming
    "write" -> parseAndRun ctx input execWrite -- write doesn't need streaming
    "edit" -> parseAndRun ctx input execEdit -- edit doesn't need streaming
    "bash" -> parseAndRunStreaming ctx input callback execBashStreaming
    "glob" -> parseAndRunStreaming ctx input callback execGlobStreaming
    "grep" -> parseAndRunStreaming ctx input callback execGrepStreaming
    _otherTool -> pure $ toolError "Error" ("Unknown tool: " <> name)

-- | Parse input and run executor
parseAndRun :: (FromJSON a) => ToolContext -> Value -> (ToolContext -> a -> IO ToolOutput) -> IO ToolOutput
parseAndRun ctx input exec = case eitherDecode (encode input) of
    Left err -> pure $ toolError "Parse Error" (T.pack err)
    Right parsed -> exec ctx parsed

-- | Parse input and run streaming executor
parseAndRunStreaming :: (FromJSON a) => ToolContext -> Value -> StreamingCallback -> (ToolContext -> a -> StreamingCallback -> IO ToolOutput) -> IO ToolOutput
parseAndRunStreaming ctx input callback exec = case eitherDecode (encode input) of
    Left err -> pure $ toolError "Parse Error" (T.pack err)
    Right parsed -> exec ctx parsed callback

-- | Read file or directory
execRead :: ToolContext -> ReadInput -> IO ToolOutput
execRead ctx ReadInput{..} = do
    let path = T.unpack (resolvePath ctx riFilePath)
    let offset = fromMaybe 1 riOffset
    let limit = fromMaybe 2000 riLimit

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
        Right () -> pure $ toolSuccess ("Wrote " <> wiFilePath) ("Successfully wrote " <> T.pack (show (textLength wiContent)) <> " characters")

-- | Edit file
execEdit :: ToolContext -> EditInput -> IO ToolOutput
execEdit ctx EditInput{..} = do
    let path = T.unpack (resolvePath ctx eiFilePath)
    let replaceAll = fromMaybe False eiReplaceAll

    result <- try @SomeException $ TIO.readFile path
    case result of
        Left e -> pure $ toolError "Edit Error" (T.pack $ show e)
        Right content -> do
            let count = listLength $ T.breakOnAll eiOldString content
            if count == 0
                then pure $ toolError "Edit Error" "oldString not found in content"
                else
                    if count > 1 && not replaceAll
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
        | otherwise -> before <> new <> T.drop (textLength old) after

listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0

textLength :: Text -> Int
textLength = T.foldl' (\acc _ -> acc + 1) 0

-- | Resolve path relative to workdir if not absolute
resolvePath :: ToolContext -> Text -> Text
resolvePath ctx p
    | "/" `T.isPrefixOf` p = p
    | otherwise = T.pack (tcWorkdir ctx) <> "/" <> p

-- ═══════════════════════════════════════════════════════════════════════════
-- Streaming Process Execution
-- ═══════════════════════════════════════════════════════════════════════════

{- | Run a process and stream its output incrementally.
Calls the callback with accumulated output after each chunk is read.
Returns the exit code and final accumulated output.

IMPORTANT: We must read from stdout/stderr concurrently with the process running,
otherwise the process may block if pipe buffers fill up. We use async threads
to read handles and wait for them concurrently with the process.
-}
runProcessStreaming ::
    CreateProcess ->
    StreamingCallback ->
    IO (ExitCode, Text, Text)
runProcessStreaming cp callback = do
    let cp' =
            cp
                { std_out = CreatePipe
                , std_err = CreatePipe
                , std_in = NoStream
                }
    result <- try @SomeException $ createProcess cp'
    case result of
        Left e -> pure (ExitFailure 1, "", T.pack (show e))
        Right (_, mStdout, mStderr, ph) -> do
            case (mStdout, mStderr) of
                (Just hOut, Just hErr) -> do
                    -- Set binary mode to avoid encoding issues
                    hSetBinaryMode hOut True
                    hSetBinaryMode hErr True

                    -- Refs to accumulate output
                    stdoutRef <- newIORef ""
                    stderrRef <- newIORef ""

                    -- Use withAsync to ensure proper thread cleanup and avoid deadlocks.
                    -- We must read stdout/stderr while the process runs to prevent pipe buffer deadlock.
                    withAsync (readHandleStreaming hOut stdoutRef stderrRef callback) $ \stdoutAsync ->
                        withAsync (readHandleToRef hErr stderrRef) $ \stderrAsync -> do
                            -- Wait for readers to complete (they complete when handles reach EOF)
                            -- This happens when the process closes its stdout/stderr (i.e., exits)
                            wait stdoutAsync
                            wait stderrAsync

                            -- Now wait for process to fully terminate and get exit code
                            exitCode <- waitForProcess ph

                            -- Get final output
                            finalStdout <- readIORef stdoutRef
                            finalStderr <- readIORef stderrRef

                            pure (exitCode, finalStdout, finalStderr)
                _missingHandles -> do
                    exitCode <- waitForProcess ph
                    pure (exitCode, "", "Failed to create process pipes")

{- | Read from a handle and stream accumulated output via callback
Uses a Seq of chunks for O(1) snoc (append at end)
For streaming, only sends the last portion to avoid sending huge payloads
-}
readHandleStreaming :: Handle -> IORef Text -> IORef Text -> StreamingCallback -> IO ()
readHandleStreaming h stdoutRef stderrRef callback = do
    -- Use a Seq of chunks for efficient append (O(1) snoc)
    chunksRef <- newIORef (Seq.empty :: Seq Text)
    loop chunksRef
    -- At the end, concatenate all chunks and store in stdoutRef
    chunks <- readIORef chunksRef
    let finalOutput = T.concat (toList chunks)
    atomicModifyIORef' stdoutRef (const (finalOutput, ()))
  where
    -- Maximum characters to send in streaming callback (last N chars)
    maxStreamingChars :: Int
    maxStreamingChars = 50000 -- ~50KB limit for streaming updates
    loop chunksRef = do
        result <- try @IOException $ BS.hGetSome h 4096
        case result of
            Left _e -> hClose h -- IO error, close handle
            Right chunk
                | BS.null chunk -> hClose h -- EOF
                | otherwise -> do
                    -- Decode chunk (lenient to handle partial UTF-8)
                    let txt = decodeUtf8Lenient chunk
                    -- Append chunk to Seq (O(1) snoc)
                    atomicModifyIORef' chunksRef (\cs -> (cs |> txt, ()))
                    -- Get current chunks for streaming preview
                    currentChunks <- readIORef chunksRef
                    -- Build output for streaming (may be truncated)
                    let fullOutput = T.concat (toList currentChunks)
                    let streamingOutput =
                            if T.compareLength fullOutput maxStreamingChars == GT
                                then "...(truncated)...\n" <> T.takeEnd maxStreamingChars fullOutput
                                else fullOutput
                    currentStderr <- readIORef stderrRef
                    let combined =
                            streamingOutput
                                <> (if T.null currentStderr then "" else "\n[stderr]\n" <> currentStderr)
                    callback combined
                    loop chunksRef

-- | Read from handle into ref (no streaming callback, for stderr)
readHandleToRef :: Handle -> IORef Text -> IO ()
readHandleToRef h ref = loop
  where
    loop = do
        result <- try @IOException $ BS.hGetSome h 4096
        case result of
            Left _e -> hClose h
            Right chunk
                | BS.null chunk -> hClose h
                | otherwise -> do
                    let txt = decodeUtf8Lenient chunk
                    atomicModifyIORef' ref (\acc -> (acc <> txt, ()))
                    loop

-- | Decode ByteString to Text, replacing invalid UTF-8 sequences
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With (\_ _ -> Just '\xFFFD')

-- ═══════════════════════════════════════════════════════════════════════════
-- Streaming Tool Implementations
-- ═══════════════════════════════════════════════════════════════════════════

-- | Execute bash command with streaming output
execBashStreaming :: ToolContext -> BashInput -> StreamingCallback -> IO ToolOutput
execBashStreaming ctx BashInput{..} callback = do
    let workdir = maybe (tcWorkdir ctx) T.unpack biWorkdir
    let timeoutMs = fromMaybe 120000 biTimeout
    let timeoutS = max 0.001 (fromIntegral timeoutMs / 1000 :: Double)
    let timeoutArg = printf "%.3fs" timeoutS

    let cmd =
            (proc "timeout" [timeoutArg, "bash", "-c", T.unpack biCommand])
                { cwd = Just workdir
                }

    (exitCode, stdout, stderr) <- runProcessStreaming cmd callback
    pure $ processBashOutput biDescription exitCode stdout stderr

-- | Pure output processing for bash tool results
processBashOutput :: Text -> ExitCode -> Text -> Text -> ToolOutput
processBashOutput description exitCode stdout stderr =
    let output = stdout <> (if T.null stderr then "" else "\n[stderr]\n" <> stderr)
        output' =
            if T.null output
                then "Command completed successfully with no output."
                else output
     in if exitCode /= ExitSuccess
            then toolError "Command failed" output
            else toolSuccess description output'

-- | Glob file search with streaming output
execGlobStreaming :: ToolContext -> GlobInput -> StreamingCallback -> IO ToolOutput
execGlobStreaming ctx GlobInput{..} callback = do
    let searchPath = maybe "." T.unpack giPath
    let cwdDir = tcWorkdir ctx
    let cmd =
            (proc "fd" ["--type", "f", "--glob", T.unpack giPattern, searchPath])
                { cwd = Just cwdDir
                }

    (exitCode, stdout, stderr) <- runProcessStreaming cmd callback
    pure $ processGlobOutput giPattern exitCode stdout stderr

-- | Pure output processing for glob tool results
processGlobOutput :: Text -> ExitCode -> Text -> Text -> ToolOutput
processGlobOutput pat exitCode stdout stderr =
    let outputLines = take 100 (T.lines stdout)
        output = T.unlines outputLines
     in case exitCode of
            ExitSuccess -> toolSuccess ("Glob " <> pat) output
            ExitFailure _exitCode ->
                toolError
                    "Glob Error"
                    (if T.null stderr then stdout else stderr)

-- | Grep content search with streaming output
execGrepStreaming :: ToolContext -> GrepInput -> StreamingCallback -> IO ToolOutput
execGrepStreaming ctx GrepInput{..} callback = do
    let searchPath = maybe "." T.unpack grPath
    let cwdDir = tcWorkdir ctx
    let baseArgs = ["--line-number", "--no-heading", T.unpack grPattern]
    let includeArgs = maybe [] (\p -> ["--glob", T.unpack p]) grInclude
    let cmd =
            (proc "rg" (baseArgs <> includeArgs <> [searchPath]))
                { cwd = Just cwdDir
                }

    (exitCode, stdout, stderr) <- runProcessStreaming cmd callback
    pure $ processGrepOutput grPattern exitCode stdout stderr

-- | Pure output processing for grep tool results
processGrepOutput :: Text -> ExitCode -> Text -> Text -> ToolOutput
processGrepOutput pat exitCode stdout stderr =
    let outputLines = take 100 (T.lines stdout)
        output = T.unlines outputLines
     in case exitCode of
            ExitSuccess -> toolSuccess ("Grep " <> pat) output
            ExitFailure 1 | T.null stdout -> toolSuccess ("Grep " <> pat) ""
            ExitFailure _exitCode ->
                toolError
                    "Grep Error"
                    (if T.null stderr then stdout else stderr)
