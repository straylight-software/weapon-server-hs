{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for runProcessStreaming subprocess handling
module Unit.ToolStreamingSpec where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Process qualified as Process
import Test.Hspec
import Tool.Exec (runProcessStreaming)
import Tool.Types (noStreaming)

spec :: Spec
spec = do
    describe "runProcessStreaming" $ do
        it "captures stdout from a simple command" $ do
            (exitCode, stdout, _stderr) <-
                runProcessStreaming
                    (Process.proc "printf" ["%s\n%s\n%s\n", "line1", "line2", "line3"])
                    noStreaming
            exitCode `shouldBe` ExitSuccess
            T.isInfixOf "line1" stdout `shouldBe` True
            T.isInfixOf "line2" stdout `shouldBe` True
            T.isInfixOf "line3" stdout `shouldBe` True

        it "invokes the streaming callback at least once" $ do
            callbackCountRef <- newIORef (0 :: Int)
            let callback _ = modifyIORef' callbackCountRef (+ 1)

            (exitCode, _stdout, _stderr) <-
                runProcessStreaming
                    (Process.proc "printf" ["%s\n%s\n%s\n", "a", "b", "c"])
                    callback
            exitCode `shouldBe` ExitSuccess
            callbackCount <- readIORef callbackCountRef
            callbackCount `shouldSatisfy` (>= 1)

        it "streaming callback receives monotonically growing output" $ do
            outputsRef <- newIORef ([] :: [Text])
            let callback accumulatedOutput =
                    modifyIORef' outputsRef (++ [accumulatedOutput])

            (exitCode, _stdout, _stderr) <-
                runProcessStreaming
                    (Process.proc "printf" ["%s\n%s\n%s\n%s\n%s\n", "1", "2", "3", "4", "5"])
                    callback
            exitCode `shouldBe` ExitSuccess

            outputs <- readIORef outputsRef
            let pairs = zip outputs (drop 1 outputs)
            mapM_ (\(prev, next) -> prev `T.isPrefixOf` next `shouldBe` True) pairs

        it "handles commands with no output" $ do
            (exitCode, stdout, _stderr) <-
                runProcessStreaming
                    (Process.proc "true" [])
                    noStreaming
            exitCode `shouldBe` ExitSuccess
            T.null stdout `shouldBe` True

        it "captures stderr separately from stdout" $ do
            (exitCode, stdout, stderr) <-
                runProcessStreaming
                    (Process.proc "/bin/sh" ["-c", "printf 'stdout_msg\\n'; printf 'stderr_msg\\n' >&2"])
                    noStreaming
            exitCode `shouldBe` ExitSuccess
            T.isInfixOf "stdout_msg" stdout `shouldBe` True
            T.isInfixOf "stderr_msg" stderr `shouldBe` True

        it "returns non-zero exit code for failing commands" $ do
            (exitCode, _stdout, _stderr) <-
                runProcessStreaming
                    (Process.proc "false" [])
                    noStreaming
            exitCode `shouldSatisfy` (/= ExitSuccess)

        it "preserves output order" $ do
            (exitCode, stdout, _stderr) <-
                runProcessStreaming
                    (Process.proc "printf" ["%s\n%s\n%s\n%s\n%s\n", "1", "2", "3", "4", "5"])
                    noStreaming
            exitCode `shouldBe` ExitSuccess

            let outputLines = T.lines stdout
            let nums = map (T.filter (`elem` ['0' .. '9'])) outputLines
            let nonEmpty = filter (not . T.null) nums
            nonEmpty `shouldBe` ["1", "2", "3", "4", "5"]
