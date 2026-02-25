{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Tool.Exec
Description : Tool execution engine

This module implements the tool execution engine that runs file and shell
operations requested by AI agents. It supports both synchronous and streaming
execution modes.

== Architecture

The execution flow is:

1. Parse JSON input into typed input structure
2. Execute the tool operation (IO)
3. Process output into 'ToolOutput' (pure)

The pure output processing functions are exported for testing.

== Streaming Support

For long-running operations (bash, glob, grep), output is streamed back
incrementally via a 'StreamingCallback'. This allows the UI to show
progress in real-time.

== Error Handling

All exceptions are caught and converted to error 'ToolOutput' values.
This ensures tools never throw exceptions to callers.
-}
module Tool.Exec (
    -- * High-level Execution
    -- $execution
    execute,
    executeToolUse,
    executeToolUseStreaming,
    executeStreaming,

    -- * Streaming Process Execution
    -- $streaming
    runProcessStreaming,

    -- * Pure Output Processing
    -- $pureprocessing
    processBashOutput,
    processGlobOutput,
    processGrepOutput,

    -- * Pure Text Utilities
    -- $textutils
    replaceFirst,
    formatLinesWithNumbers,
    truncateLines,
    resolvePath,
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

{- $execution
High-level tool execution functions that route tool invocations to the
appropriate handlers.
-}

{- | Execute a tool from a 'ToolUse' block without streaming.

This is the simplest entry point for tool execution. It dispatches to
the appropriate tool handler based on the tool name.
-}
executeToolUse :: ToolContext -> ToolUse -> IO ToolResult
executeToolUse ctx tu = executeToolUseStreaming ctx tu noStreaming

{- | Execute a tool from a 'ToolUse' block with streaming callback.

The callback is invoked with accumulated output during long-running
operations (bash, glob, grep).
-}
executeToolUseStreaming :: ToolContext -> ToolUse -> StreamingCallback -> IO ToolResult
executeToolUseStreaming ctx ToolUse{..} callback = do
    result <- executeStreaming ctx tuName tuInput callback
    pure
        ToolResult
            { trToolUseId = tuId
            , trContent = toOutput result
            , trIsError = toIsError result
            }

{- | Execute a tool by name with JSON input.

This is a convenience wrapper around 'executeStreaming' with no streaming.
-}
execute :: ToolContext -> Text -> Value -> IO ToolOutput
execute ctx name input = executeStreaming ctx name input noStreaming

{- | Execute a tool by name with JSON input and streaming callback.

Supported tools: @read@, @write@, @edit@, @bash@, @glob@, @grep@.
Returns an error 'ToolOutput' for unknown tool names.
-}
executeStreaming :: ToolContext -> Text -> Value -> StreamingCallback -> IO ToolOutput
executeStreaming ctx name input callback = case name of
    "read" -> parseAndRun ctx input execRead
    "write" -> parseAndRun ctx input execWrite
    "edit" -> parseAndRun ctx input execEdit
    "bash" -> parseAndRunStreaming ctx input callback execBashStreaming
    "glob" -> parseAndRunStreaming ctx input callback execGlobStreaming
    "grep" -> parseAndRunStreaming ctx input callback execGrepStreaming
    _otherTool -> pure $ toolError "Error" ("Unknown tool: " <> name)

{- | Parse JSON input and run a non-streaming executor.

If parsing fails, returns a "Parse Error" 'ToolOutput'.
-}
parseAndRun ::
    (FromJSON a) =>
    ToolContext ->
    Value ->
    (ToolContext -> a -> IO ToolOutput) ->
    IO ToolOutput
parseAndRun ctx input exec = case eitherDecode (encode input) of
    Left err -> pure $ toolError "Parse Error" (T.pack err)
    Right parsed -> exec ctx parsed

{- | Parse JSON input and run a streaming executor.

If parsing fails, returns a "Parse Error" 'ToolOutput'.
-}
parseAndRunStreaming ::
    (FromJSON a) =>
    ToolContext ->
    Value ->
    StreamingCallback ->
    (ToolContext -> a -> StreamingCallback -> IO ToolOutput) ->
    IO ToolOutput
parseAndRunStreaming ctx input callback exec = case eitherDecode (encode input) of
    Left err -> pure $ toolError "Parse Error" (T.pack err)
    Right parsed -> exec ctx parsed callback

{- | Execute the read tool.

Reads file contents with line numbers or lists directory entries.
-}
execRead :: ToolContext -> ReadInput -> IO ToolOutput
execRead ctx ReadInput{..} = do
    let path = T.unpack (resolvePath ctx riFilePath)
    let offset = fromMaybe 1 riOffset
    let limit = fromMaybe 2000 riLimit

    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    case (isFile, isDir) of
        (True, _) -> readFileWithLines path riFilePath offset limit
        (_, True) -> readDirectory path riFilePath
        (False, False) -> pure $ toolError "Read Error" ("Path does not exist: " <> riFilePath)

-- | Read a file and format with line numbers.
readFileWithLines :: FilePath -> Text -> Int -> Int -> IO ToolOutput
readFileWithLines path displayPath offset limit = do
    result <- try @SomeException $ TIO.readFile path
    case result of
        Left e -> pure $ toolError "Read Error" (T.pack $ show e)
        Right content -> pure $ processReadOutput displayPath offset limit content

{- | Pure processing of file read output.

Adds line numbers and slices to the requested range.
-}
processReadOutput :: Text -> Int -> Int -> Text -> ToolOutput
processReadOutput displayPath offset limit content =
    let allLines = T.lines content
        numbered = formatLinesWithNumbers allLines
        sliced = truncateLines limit $ drop (offset - 1) numbered
     in toolSuccess ("Read " <> displayPath) (T.unlines sliced)

-- | Read a directory and list its entries.
readDirectory :: FilePath -> Text -> IO ToolOutput
readDirectory path displayPath = do
    result <- try @SomeException $ listDirectory path
    case result of
        Left e -> pure $ toolError "Read Error" (T.pack $ show e)
        Right entries -> pure $ toolSuccess ("List " <> displayPath) (T.unlines $ map T.pack entries)

{- | Execute the write tool.

Creates parent directories if needed, then writes the content.
-}
execWrite :: ToolContext -> WriteInput -> IO ToolOutput
execWrite ctx WriteInput{..} = do
    let path = T.unpack (resolvePath ctx wiFilePath)
    result <- try @SomeException $ do
        createDirectoryIfMissing True (takeDirectory path)
        TIO.writeFile path wiContent
    case result of
        Left e -> pure $ toolError "Write Error" (T.pack $ show e)
        Right () -> pure $ processWriteOutput wiFilePath wiContent

-- | Pure processing of write output.
processWriteOutput :: Text -> Text -> ToolOutput
processWriteOutput displayPath content =
    let charCount = textLength content
     in toolSuccess ("Wrote " <> displayPath) ("Successfully wrote " <> T.pack (show charCount) <> " characters")

{- | Execute the edit tool.

Reads the file, applies the replacement, and writes back.
-}
execEdit :: ToolContext -> EditInput -> IO ToolOutput
execEdit ctx EditInput{..} = do
    let path = T.unpack (resolvePath ctx eiFilePath)
    let doReplaceAll = fromMaybe False eiReplaceAll

    result <- try @SomeException $ TIO.readFile path
    case result of
        Left e -> pure $ toolError "Edit Error" (T.pack $ show e)
        Right content ->
            case validateEdit eiOldString content doReplaceAll of
                Left err -> pure $ toolError "Edit Error" err
                Right count -> do
                    let newContent = applyEdit eiOldString eiNewString content doReplaceAll
                    writeResult <- try @SomeException $ TIO.writeFile path newContent
                    case writeResult of
                        Left e -> pure $ toolError "Edit Error" (T.pack $ show e)
                        Right () -> pure $ processEditOutput eiFilePath count doReplaceAll

{- | Validate that an edit can be performed.

Returns 'Left' with error message if invalid, 'Right' with match count if valid.
-}
validateEdit :: Text -> Text -> Bool -> Either Text Int
validateEdit oldString content doReplaceAll =
    let count = listLength $ T.breakOnAll oldString content
     in if count == 0
            then Left "oldString not found in content"
            else
                if count > 1 && not doReplaceAll
                    then
                        Left $
                            "Found "
                                <> T.pack (show count)
                                <> " matches for oldString. Provide more surrounding lines to identify the correct match or use replaceAll."
                    else Right count

{- | Apply an edit to content (pure).

If @replaceAll@ is 'True', replaces all occurrences; otherwise only the first.
-}
applyEdit :: Text -> Text -> Text -> Bool -> Text
applyEdit oldString newString content doReplaceAll
    | doReplaceAll = T.replace oldString newString content
    | otherwise = replaceFirst oldString newString content

-- | Pure processing of edit output.
processEditOutput :: Text -> Int -> Bool -> ToolOutput
processEditOutput displayPath count doReplaceAll =
    let replacedCount = if doReplaceAll then count else 1
     in toolSuccess ("Edited " <> displayPath) ("Replaced " <> T.pack (show replacedCount) <> " occurrence(s)")

{- $textutils
Pure text manipulation utilities used by the tool executors.
These are exported for testing.
-}

{- | Replace the first occurrence of a substring.

If the old string is not found, returns the original text unchanged.

==== __Examples__

>>> replaceFirst "foo" "bar" "foo foo foo"
"bar foo foo"

>>> replaceFirst "missing" "new" "original text"
"original text"
-}
replaceFirst :: Text -> Text -> Text -> Text
replaceFirst old new txt = case T.breakOn old txt of
    (before, after)
        | T.null after -> txt
        | otherwise -> before <> new <> T.drop (textLength old) after

{- | Format lines with line numbers (1-indexed).

Each line is prefixed with its line number followed by ": ".

==== __Examples__

>>> formatLinesWithNumbers ["a", "b", "c"]
["1: a", "2: b", "3: c"]
-}
formatLinesWithNumbers :: [Text] -> [Text]
formatLinesWithNumbers = zipWith formatLine [1 ..]
  where
    formatLine :: Int -> Text -> Text
    formatLine n l = T.pack (show n) <> ": " <> l

{- | Truncate a list of lines to at most @n@ lines.

This is used to prevent extremely long outputs from overwhelming the UI.
-}
truncateLines :: Int -> [Text] -> [Text]
truncateLines = take

-- | O(n) strict list length that avoids space leaks.
listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0

-- | O(n) strict text length.
textLength :: Text -> Int
textLength = T.foldl' (\acc _ -> acc + 1) 0

{- | Resolve a path relative to the working directory.

Absolute paths (starting with "/") are returned unchanged.
Relative paths are prefixed with the working directory.

==== __Examples__

>>> let ctx = ToolContext "s" "m" "/home/user/project"
>>> resolvePath ctx "/etc/passwd"
"/etc/passwd"

>>> resolvePath ctx "src/main.hs"
"/home/user/project/src/main.hs"
-}
resolvePath :: ToolContext -> Text -> Text
resolvePath ctx p
    | "/" `T.isPrefixOf` p = p
    | otherwise = T.pack (tcWorkdir ctx) <> "/" <> p

-- ═══════════════════════════════════════════════════════════════════════════
-- Streaming Process Execution
-- ═══════════════════════════════════════════════════════════════════════════

{- $streaming
Functions for running external processes with real-time output streaming.

The key challenge is avoiding pipe buffer deadlocks: if the process fills
up the OS pipe buffers before we read from them, it will block forever.
We solve this by reading stdout\/stderr concurrently with async threads.
-}

{- | Run a process and stream its output incrementally.

Calls the callback with accumulated output after each chunk is read.
Returns @(exitCode, stdout, stderr)@.

__Important__: We read stdout\/stderr concurrently to prevent pipe buffer
deadlocks. The process may block if we don't drain its output pipes fast enough.
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

{- | Maximum characters to send in streaming callback (last N chars).

This limits the payload size sent to the UI for very long outputs.
-}
maxStreamingChars :: Int
maxStreamingChars = 50000 -- ~50KB limit for streaming updates

{- | Read from a handle and stream accumulated output via callback.

Uses a 'Seq' of chunks for O(1) append operations.
For streaming, only sends the last portion to avoid huge payloads.
-}
readHandleStreaming :: Handle -> IORef Text -> IORef Text -> StreamingCallback -> IO ()
readHandleStreaming h stdoutRef stderrRef callback = do
    chunksRef <- newIORef (Seq.empty :: Seq Text)
    readHandleLoop h chunksRef (Just (stderrRef, callback))
    finalizeChunks chunksRef stdoutRef

-- | Finalize accumulated chunks into a single Text in the ref.
finalizeChunks :: IORef (Seq Text) -> IORef Text -> IO ()
finalizeChunks chunksRef outputRef = do
    chunks <- readIORef chunksRef
    let finalOutput = T.concat (toList chunks)
    atomicModifyIORef' outputRef (const (finalOutput, ()))

{- | Core loop for reading from a handle.

If @streamingInfo@ is 'Just', sends streaming updates with truncation.
If 'Nothing', just accumulates output silently (used for stderr).
-}
readHandleLoop ::
    Handle ->
    IORef (Seq Text) ->
    Maybe (IORef Text, StreamingCallback) ->
    IO ()
readHandleLoop h chunksRef streamingInfo = loop
  where
    loop = do
        result <- try @IOException $ BS.hGetSome h 4096
        case result of
            Left _e -> hClose h -- IO error, close handle
            Right chunk
                | BS.null chunk -> hClose h -- EOF
                | otherwise -> do
                    let txt = decodeUtf8Lenient chunk
                    atomicModifyIORef' chunksRef (\cs -> (cs |> txt, ()))
                    case streamingInfo of
                        Nothing -> pure ()
                        Just (stderrRef, callback) -> do
                            currentChunks <- readIORef chunksRef
                            let fullOutput = T.concat (toList currentChunks)
                            let streamingOutput = truncateForStreaming maxStreamingChars fullOutput
                            currentStderr <- readIORef stderrRef
                            let combined = combineOutputStreams streamingOutput currentStderr
                            callback combined
                    loop

-- | Truncate text for streaming if it exceeds the limit.
truncateForStreaming :: Int -> Text -> Text
truncateForStreaming limit txt
    | T.compareLength txt limit == GT =
        "...(truncated)...\n" <> T.takeEnd limit txt
    | otherwise = txt

-- | Combine stdout and stderr for streaming display.
combineOutputStreams :: Text -> Text -> Text
combineOutputStreams stdout stderr
    | T.null stderr = stdout
    | otherwise = stdout <> "\n[stderr]\n" <> stderr

-- | Prefer stderr for error messages, fall back to stdout if stderr is empty.
preferStderr :: Text -> Text -> Text
preferStderr stderr stdout
    | T.null stderr = stdout
    | otherwise = stderr

{- | Read from handle into ref (no streaming callback, for stderr).

This is a simplified version of 'readHandleStreaming' that just accumulates
output without sending streaming updates.
-}
readHandleToRef :: Handle -> IORef Text -> IO ()
readHandleToRef h ref = do
    chunksRef <- newIORef (Seq.empty :: Seq Text)
    readHandleLoop h chunksRef Nothing
    finalizeChunks chunksRef ref

-- | Decode ByteString to Text, replacing invalid UTF-8 sequences
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With (\_ _ -> Just '\xFFFD')

-- ═══════════════════════════════════════════════════════════════════════════
-- Streaming Tool Implementations
-- ═══════════════════════════════════════════════════════════════════════════

{- | Execute bash command with streaming output.

Wraps the command with @timeout@ to enforce the specified timeout.
-}
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

{- $pureprocessing
Pure functions for processing tool output. These are exported for testing
so that output formatting logic can be verified without running actual processes.
-}

{- | Process bash command output into a 'ToolOutput' (pure).

Combines stdout and stderr, handles empty output case, and determines
success\/failure based on exit code.
-}
processBashOutput :: Text -> ExitCode -> Text -> Text -> ToolOutput
processBashOutput description exitCode stdout stderr =
    let output = combineOutputStreams stdout stderr
        output' =
            if T.null output
                then "Command completed successfully with no output."
                else output
     in if exitCode /= ExitSuccess
            then toolError "Command failed" output
            else toolSuccess description output'

{- | Execute glob file search with streaming output.

Uses @fd@ for fast file searching with glob patterns.
-}
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

{- | Process glob (fd) output into a 'ToolOutput' (pure).

Truncates output to 100 lines to prevent overwhelming the UI.
-}
processGlobOutput :: Text -> ExitCode -> Text -> Text -> ToolOutput
processGlobOutput pat exitCode stdout stderr =
    let output = T.unlines $ truncateLines 100 $ T.lines stdout
     in case exitCode of
            ExitSuccess -> toolSuccess ("Glob " <> pat) output
            ExitFailure _ -> toolError "Glob Error" (preferStderr stderr stdout)

{- | Execute grep content search with streaming output.

Uses @ripgrep@ (rg) for fast regex searching.
-}
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

{- | Process grep (ripgrep) output into a 'ToolOutput' (pure).

Truncates output to 100 lines. Exit code 1 with no output is treated
as "no matches found" (success), not an error.
-}
processGrepOutput :: Text -> ExitCode -> Text -> Text -> ToolOutput
processGrepOutput pat exitCode stdout stderr =
    let output = T.unlines $ truncateLines 100 $ T.lines stdout
     in case exitCode of
            ExitSuccess -> toolSuccess ("Grep " <> pat) output
            ExitFailure 1 | T.null stdout -> toolSuccess ("Grep " <> pat) ""
            ExitFailure _ -> toolError "Grep Error" (preferStderr stderr stdout)
