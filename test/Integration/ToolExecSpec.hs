{-# LANGUAGE OverloadedStrings #-}

{- | Integration tests for Tool.Exec

These tests verify that the tool execution works correctly with real
file system operations and subprocesses.
-}
module Integration.ToolExecSpec (spec) where

import Data.Aeson (object, (.=))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Tool.Exec (execute, executeStreaming)
import Tool.Types

-- | Create a test context with the given working directory
testContext :: FilePath -> ToolContext
testContext workdir =
    ToolContext
        { tcSessionID = "test_session"
        , tcMessageID = "test_message"
        , tcWorkdir = workdir
        }

spec :: Spec
spec = describe "Tool.Exec Integration" $ do
    describe "read tool" $ do
        it "reads file contents with line numbers" $ do
            withSystemTempDirectory "tool-test" $ \tmpDir -> do
                let path = tmpDir </> "test.txt"
                TIO.writeFile path "line1\nline2\nline3"
                let input = object ["filePath" .= path]
                result <- execute (testContext tmpDir) "read" input
                toIsError result `shouldBe` False
                T.isInfixOf "1: line1" (toOutput result) `shouldBe` True
                T.isInfixOf "2: line2" (toOutput result) `shouldBe` True

        it "supports offset and limit" $ do
            withSystemTempDirectory "tool-test" $ \tmpDir -> do
                let path = tmpDir </> "test.txt"
                TIO.writeFile path (T.unlines [T.pack $ "line" <> show n | n <- [1 .. 10 :: Int]])
                let input = object ["filePath" .= path, "offset" .= (3 :: Int), "limit" .= (2 :: Int)]
                result <- execute (testContext tmpDir) "read" input
                toIsError result `shouldBe` False
                T.isInfixOf "3: line3" (toOutput result) `shouldBe` True
                T.isInfixOf "4: line4" (toOutput result) `shouldBe` True
                T.isInfixOf "5: line5" (toOutput result) `shouldBe` False

        it "returns error for nonexistent file" $ do
            withSystemTempDirectory "tool-test" $ \tmpDir -> do
                let input = object ["filePath" .= (tmpDir </> "nonexistent.txt")]
                result <- execute (testContext tmpDir) "read" input
                toIsError result `shouldBe` True

    describe "write tool" $ do
        it "creates a new file" $ do
            withSystemTempDirectory "tool-test" $ \tmpDir -> do
                let path = tmpDir </> "new.txt"
                let input = object ["filePath" .= path, "content" .= ("hello world" :: Text)]
                result <- execute (testContext tmpDir) "write" input
                toIsError result `shouldBe` False
                exists <- doesFileExist path
                exists `shouldBe` True
                content <- TIO.readFile path
                content `shouldBe` "hello world"

        it "creates parent directories" $ do
            withSystemTempDirectory "tool-test" $ \tmpDir -> do
                let path = tmpDir </> "a" </> "b" </> "c" </> "new.txt"
                let input = object ["filePath" .= path, "content" .= ("nested" :: Text)]
                result <- execute (testContext tmpDir) "write" input
                toIsError result `shouldBe` False
                exists <- doesFileExist path
                exists `shouldBe` True

    describe "edit tool" $ do
        it "replaces unique occurrence" $ do
            withSystemTempDirectory "tool-test" $ \tmpDir -> do
                let path = tmpDir </> "edit.txt"
                TIO.writeFile path "foo bar baz"
                let input =
                        object
                            [ "filePath" .= path
                            , "oldString" .= ("foo" :: Text)
                            , "newString" .= ("XXX" :: Text)
                            ]
                result <- execute (testContext tmpDir) "edit" input
                toIsError result `shouldBe` False
                content <- TIO.readFile path
                content `shouldBe` "XXX bar baz"

        it "replaces all occurrences with replaceAll" $ do
            withSystemTempDirectory "tool-test" $ \tmpDir -> do
                let path = tmpDir </> "edit.txt"
                TIO.writeFile path "foo bar foo baz foo"
                let input =
                        object
                            [ "filePath" .= path
                            , "oldString" .= ("foo" :: Text)
                            , "newString" .= ("XXX" :: Text)
                            , "replaceAll" .= True
                            ]
                result <- execute (testContext tmpDir) "edit" input
                toIsError result `shouldBe` False
                content <- TIO.readFile path
                content `shouldBe` "XXX bar XXX baz XXX"

        it "errors on multiple matches without replaceAll" $ do
            withSystemTempDirectory "tool-test" $ \tmpDir -> do
                let path = tmpDir </> "edit.txt"
                TIO.writeFile path "dup dup"
                let input =
                        object
                            [ "filePath" .= path
                            , "oldString" .= ("dup" :: Text)
                            , "newString" .= ("new" :: Text)
                            ]
                result <- execute (testContext tmpDir) "edit" input
                toIsError result `shouldBe` True

    describe "bash tool with streaming" $ do
        it "invokes streaming callback" $ do
            withSystemTempDirectory "tool-test" $ \tmpDir -> do
                callsRef <- newIORef ([] :: [Text])
                let callback txt = atomicModifyIORef' callsRef (\xs -> (xs ++ [txt], ()))
                let input =
                        object
                            [ "command" .= ("echo 'hello'" :: Text)
                            , "description" .= ("test echo" :: Text)
                            ]
                result <- executeStreaming (testContext tmpDir) "bash" input callback
                toIsError result `shouldBe` False
                T.isInfixOf "hello" (toOutput result) `shouldBe` True
                calls <- readIORef callsRef
                -- Should have received at least one streaming callback
                case calls of
                    [] -> expectationFailure "Expected at least one streaming callback, got none"
                    (_atLeastOne : _rest) -> pure ()
