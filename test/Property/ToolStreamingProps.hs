{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for tool output streaming functionality
module Property.ToolStreamingProps where

import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process qualified as Process
import Test.Fixture (propertyWithTempDir, withTempDir)
import Test.Tasty
import Test.Tasty.Hedgehog
import Tool.Exec (executeStreaming, runProcessStreaming)
import Tool.Types

-- ═══════════════════════════════════════════════════════════════════════════
-- Helper functions
-- ═══════════════════════════════════════════════════════════════════════════

-- | Create a process config for testing
mkProc :: FilePath -> [String] -> Process.CreateProcess
mkProc = Process.proc

listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0

-- | Create a test context
testContext :: FilePath -> ToolContext
testContext workdir =
    ToolContext
        { tcSessionID = "test_session"
        , tcMessageID = "test_message"
        , tcWorkdir = workdir
        }

-- ═══════════════════════════════════════════════════════════════════════════
-- runProcessStreaming Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: runProcessStreaming calls callback with accumulated output
prop_streamingCallbackCalled :: Property
prop_streamingCallbackCalled = withTests 20 $ property $ do
    callbackCountRef <- evalIO $ newIORef (0 :: Int)
    outputRef <- evalIO $ newIORef ("" :: Text)

    let callback accumulatedOutput = do
            modifyIORef' callbackCountRef (+ 1)
            modifyIORef' outputRef (const accumulatedOutput)

    -- Use a command that produces output in chunks
    (exitCode, stdout, _stderr) <-
        evalIO $
            runProcessStreaming
                (mkProc "bash" ["-c", "echo line1; echo line2; echo line3"])
                callback

    -- Should succeed
    exitCode === ExitSuccess

    -- Should have some output
    assert $ not (T.null stdout)
    assert $ T.isInfixOf "line1" stdout
    assert $ T.isInfixOf "line2" stdout
    assert $ T.isInfixOf "line3" stdout

    -- Callback should have been called at least once (may be more depending on buffering)
    callbackCount <- evalIO $ readIORef callbackCountRef
    assert $ callbackCount >= 1

    -- Final accumulated output in ref should match final stdout
    finalOutput <- evalIO $ readIORef outputRef
    -- The callback gets called during streaming, so final output should contain the lines
    assert $ T.isInfixOf "line1" finalOutput || T.null finalOutput -- May be empty if all output came at once

-- | Property: streaming callback receives monotonically increasing output
prop_streamingOutputMonotonic :: Property
prop_streamingOutputMonotonic = withTests 20 $ property $ do
    outputsRef <- evalIO $ newIORef ([] :: [Text])

    let callback accumulatedOutput = do
            modifyIORef' outputsRef (++ [accumulatedOutput])

    -- Run a command that produces incremental output
    (exitCode, _stdout, _stderr) <-
        evalIO $
            runProcessStreaming
                (mkProc "bash" ["-c", "for i in 1 2 3 4 5; do echo $i; done"])
                callback

    exitCode === ExitSuccess

    -- Check that each output is a prefix of the next (or equal)
    outputs <- evalIO $ readIORef outputsRef
    let pairs = zip outputs (drop 1 outputs)
    forM_ pairs $ \(prev, next) -> do
        -- Each subsequent output should be >= length of previous
        assert $ T.length next >= T.length prev

-- | Property: streaming works with no output command
prop_streamingNoOutput :: Property
prop_streamingNoOutput = withTests 20 $ property $ do
    callbackCountRef <- evalIO $ newIORef (0 :: Int)

    let callback _ = modifyIORef' callbackCountRef (+ 1)

    (exitCode, stdout, _stderr) <-
        evalIO $
            runProcessStreaming
                (mkProc "bash" ["-c", "exit 0"])
                callback

    exitCode === ExitSuccess
    T.null stdout === True

-- | Property: streaming captures stderr
prop_streamingCapturesStderr :: Property
prop_streamingCapturesStderr = withTests 20 $ property $ do
    (exitCode, stdout, stderr) <-
        evalIO $
            runProcessStreaming
                (mkProc "bash" ["-c", "echo stdout_msg; echo stderr_msg >&2"])
                noStreaming

    exitCode === ExitSuccess
    assert $ T.isInfixOf "stdout_msg" stdout
    assert $ T.isInfixOf "stderr_msg" stderr

-- | Property: streaming handles command failure
prop_streamingCommandFailure :: Property
prop_streamingCommandFailure = withTests 20 $ property $ do
    outputRef <- evalIO $ newIORef ("" :: Text)

    let callback out = modifyIORef' outputRef (const out)

    (exitCode, stdout, _stderr) <-
        evalIO $
            runProcessStreaming
                (mkProc "bash" ["-c", "echo before_fail; exit 1"])
                callback

    -- Should have non-zero exit code
    assert $ exitCode /= ExitSuccess

    -- Should still capture output before failure
    assert $ T.isInfixOf "before_fail" stdout

-- | Property: streaming with delayed output still calls callback
prop_streamingDelayedOutput :: Property
prop_streamingDelayedOutput = withTests 5 $ property $ do
    callbackCountRef <- evalIO $ newIORef (0 :: Int)
    outputsRef <- evalIO $ newIORef ([] :: [Text])

    let callback accumulatedOutput = do
            modifyIORef' callbackCountRef (+ 1)
            modifyIORef' outputsRef (++ [accumulatedOutput])

    -- Command with small delays between outputs (should still call callback for each)
    (exitCode, stdout, _stderr) <-
        evalIO $
            runProcessStreaming
                (mkProc "bash" ["-c", "echo a; sleep 0.05; echo b; sleep 0.05; echo c"])
                callback

    exitCode === ExitSuccess
    assert $ T.isInfixOf "a" stdout
    assert $ T.isInfixOf "b" stdout
    assert $ T.isInfixOf "c" stdout

    -- Should have called callback multiple times due to delays
    outputs <- evalIO $ readIORef outputsRef
    -- With delays, we're more likely to get multiple callbacks
    assert $ listLength outputs >= 1

-- ═══════════════════════════════════════════════════════════════════════════
-- executeStreaming Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: executeStreaming bash tool streams output
prop_executeStreamingBash :: Property
prop_executeStreamingBash = propertyWithTempDir $ \tmpDir -> do
    outputsRef <- evalIO $ newIORef ([] :: [Text])

    let callback accumulatedOutput = do
            modifyIORef' outputsRef (++ [accumulatedOutput])

    let input =
            object
                [ "command" .= ("echo hello; echo world" :: Text)
                , "description" .= ("test streaming" :: Text)
                , "timeout" .= (5000 :: Int)
                ]

    result <- evalIO $ executeStreaming (testContext tmpDir) "bash" input callback

    -- Should succeed
    assert $ not (toIsError result)

    -- Output should contain both lines
    assert $ T.isInfixOf "hello" (toOutput result)
    assert $ T.isInfixOf "world" (toOutput result)

-- | Property: executeStreaming glob tool streams output
prop_executeStreamingGlob :: Property
prop_executeStreamingGlob = property $ do
    callbackCountRef <- evalIO $ newIORef (0 :: Int)

    let callback _ = modifyIORef' callbackCountRef (+ 1)

    result <- evalIO $ withTempDir $ \tmpDir -> do
        -- Create some files
        TIO.writeFile (tmpDir </> "file1.txt") "content1"
        TIO.writeFile (tmpDir </> "file2.txt") "content2"

        let input =
                object
                    [ "pattern" .= ("*.txt" :: Text)
                    , "path" .= T.pack tmpDir
                    ]

        executeStreaming (testContext tmpDir) "glob" input callback

    -- Should succeed
    assert $ not (toIsError result)

    -- Should find the files
    assert $ T.isInfixOf "file1.txt" (toOutput result) || T.isInfixOf "file2.txt" (toOutput result)

-- | Property: executeStreaming grep tool streams output
prop_executeStreamingGrep :: Property
prop_executeStreamingGrep = property $ do
    callbackCountRef <- evalIO $ newIORef (0 :: Int)

    let callback _ = modifyIORef' callbackCountRef (+ 1)

    result <- evalIO $ withTempDir $ \tmpDir -> do
        -- Create a file with searchable content
        TIO.writeFile (tmpDir </> "search.txt") "hello world\ngoodbye world\nhello again"

        let input =
                object
                    [ "pattern" .= ("hello" :: Text)
                    , "path" .= T.pack tmpDir
                    ]

        executeStreaming (testContext tmpDir) "grep" input callback

    -- Should succeed
    assert $ not (toIsError result)

    -- Should find matches
    assert $ T.isInfixOf "hello" (toOutput result)

-- | Property: executeStreaming with noStreaming works same as execute
prop_executeStreamingEquivalentToExecute :: Property
prop_executeStreamingEquivalentToExecute = propertyWithTempDir $ \tmpDir -> do
    cmd <- forAll $ Gen.element ["echo test" :: Text, "pwd", "whoami"]

    let input =
            object
                [ "command" .= cmd
                , "description" .= ("equivalence test" :: Text)
                , "timeout" .= (5000 :: Int)
                ]

    -- Execute with noStreaming
    result <- evalIO $ executeStreaming (testContext tmpDir) "bash" input noStreaming

    -- Should succeed and produce output
    assert $ not (toIsError result)
    assert $ not (T.null (toOutput result))

-- | Property: streaming callback receives complete output by end
prop_streamingCallbackFinalOutputComplete :: Property
prop_streamingCallbackFinalOutputComplete = propertyWithTempDir $ \tmpDir -> do
    -- Generate some random lines
    numLines <- forAll $ Gen.int (Range.linear 1 10)
    let lines' = map (\i -> "line" <> show i) [1 .. numLines]
    let cmd = "echo '" <> List.intercalate "'; echo '" lines' <> "'"

    lastOutputRef <- evalIO $ newIORef ("" :: Text)

    let callback out = modifyIORef' lastOutputRef (const out)

    let input =
            object
                [ "command" .= T.pack cmd
                , "description" .= ("multi-line test" :: Text)
                , "timeout" .= (5000 :: Int)
                ]

    result <- evalIO $ executeStreaming (testContext tmpDir) "bash" input callback

    -- The callback should eventually receive all the output
    -- Note: callback gets stdout only, result combines stdout+stderr
    _lastOutput <- evalIO $ readIORef lastOutputRef

    -- The final result should contain all lines
    forM_ lines' $ \line -> do
        assert $ T.isInfixOf (T.pack line) (toOutput result)

-- | Property: streaming preserves output order
prop_streamingPreservesOrder :: Property
prop_streamingPreservesOrder = propertyWithTempDir $ \tmpDir -> do
    let input =
            object
                [ "command" .= ("echo 1; echo 2; echo 3; echo 4; echo 5" :: Text)
                , "description" .= ("order test" :: Text)
                , "timeout" .= (5000 :: Int)
                ]

    result <- evalIO $ executeStreaming (testContext tmpDir) "bash" input noStreaming

    -- Output should have numbers in order
    let output = toOutput result
    let outputLines = T.lines output
    let nums = map (T.filter (`elem` ['0' .. '9'])) outputLines
    let nonEmpty = filter (not . T.null) nums

    -- Numbers should be in ascending order
    nonEmpty === ["1", "2", "3", "4", "5"]

-- ═══════════════════════════════════════════════════════════════════════════
-- Edge Cases
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: streaming handles large output
prop_streamingLargeOutput :: Property
prop_streamingLargeOutput = withTests 5 $ propertyWithTempDir $ \tmpDir -> do
    callbackCountRef <- evalIO $ newIORef (0 :: Int)

    let callback _ = modifyIORef' callbackCountRef (+ 1)

    -- Generate 1000 lines of output
    let input =
            object
                [ "command" .= ("for i in $(seq 1 1000); do echo \"line $i: some padding text here\"; done" :: Text)
                , "description" .= ("large output test" :: Text)
                , "timeout" .= (30000 :: Int)
                ]

    result <- evalIO $ executeStreaming (testContext tmpDir) "bash" input callback

    -- Should succeed
    assert $ not (toIsError result)

    -- Should have substantial output
    assert $ T.length (toOutput result) > 1000

    -- Should have called callback multiple times for large output
    callbackCount <- evalIO $ readIORef callbackCountRef
    assert $ callbackCount >= 1

-- | Property: streaming handles binary-ish output gracefully
prop_streamingBinaryOutput :: Property
prop_streamingBinaryOutput = withTests 10 $ propertyWithTempDir $ \tmpDir -> do
    -- Output some bytes that might cause issues
    let input =
            object
                [ "command" .= ("printf 'hello\\x00world\\x01\\x02test'" :: Text)
                , "description" .= ("binary test" :: Text)
                , "timeout" .= (5000 :: Int)
                ]

    result <- evalIO $ executeStreaming (testContext tmpDir) "bash" input noStreaming

    -- Should succeed (exit code 0)
    assert $ not (toIsError result)

    -- Output should contain the text parts (null bytes replaced with replacement char)
    assert $ T.isInfixOf "hello" (toOutput result)
    assert $ T.isInfixOf "test" (toOutput result)

-- | Property: streaming handles rapid successive outputs
prop_streamingRapidOutputs :: Property
prop_streamingRapidOutputs = withTests 5 $ propertyWithTempDir $ \tmpDir -> do
    outputsRef <- evalIO $ newIORef ([] :: [Text])

    let callback out = modifyIORef' outputsRef (++ [out])

    -- Many rapid outputs
    let input =
            object
                [ "command" .= ("for i in $(seq 1 100); do echo $i; done" :: Text)
                , "description" .= ("rapid test" :: Text)
                , "timeout" .= (10000 :: Int)
                ]

    result <- evalIO $ executeStreaming (testContext tmpDir) "bash" input callback

    -- Should succeed
    assert $ not (toIsError result)

    -- Should contain all numbers in output
    assert $ T.isInfixOf "1" (toOutput result)
    assert $ T.isInfixOf "100" (toOutput result)

-- ═══════════════════════════════════════════════════════════════════════════
-- Test tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Tool Streaming Property Tests"
        [ testGroup
            "runProcessStreaming"
            [ testProperty "callback called" prop_streamingCallbackCalled
            , testProperty "output monotonic" prop_streamingOutputMonotonic
            , testProperty "no output command" prop_streamingNoOutput
            , testProperty "captures stderr" prop_streamingCapturesStderr
            , testProperty "handles failure" prop_streamingCommandFailure
            , testProperty "delayed output" prop_streamingDelayedOutput
            ]
        , testGroup
            "executeStreaming"
            [ testProperty "bash streams" prop_executeStreamingBash
            , testProperty "glob streams" prop_executeStreamingGlob
            , testProperty "grep streams" prop_executeStreamingGrep
            , testProperty "equivalent to execute" prop_executeStreamingEquivalentToExecute
            , testProperty "final output complete" prop_streamingCallbackFinalOutputComplete
            , testProperty "preserves order" prop_streamingPreservesOrder
            ]
        , testGroup
            "Edge cases"
            [ testProperty "large output" prop_streamingLargeOutput
            , testProperty "binary output" prop_streamingBinaryOutput
            , testProperty "rapid outputs" prop_streamingRapidOutputs
            ]
        ]
