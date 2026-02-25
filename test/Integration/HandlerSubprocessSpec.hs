{-# LANGUAGE OverloadedStrings #-}

{- | Integration tests for handlers that spawn subprocesses.
These tests must run sequentially as they create real processes.
-}
module Integration.HandlerSubprocessSpec (spec) where

import Api
import Bus.Bus qualified as Bus
import Config.Dhall qualified as Dhall
import Control.Concurrent.STM
import Control.Exception (bracket, catch)
import Control.Monad (void)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Find.Search (SearchError)
import Formatter.Status qualified as Formatter
import Handlers
import Katip (Severity (ErrorS))
import Log qualified
import Pty.Pty qualified as Pty

import Servant.Server (ServerError)
import State
import Storage.Storage qualified as Storage
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Signals qualified as Sig
import System.Process (readProcessWithExitCode)
import Test.Fixture (withTempDir)
import Test.Helpers (hasKey, lookupArray, lookupText, runHandlerIO, waitVar)
import Test.Hspec
import Util.Identifier qualified as Identifier

-- | Result type for PTY lifecycle test to avoid large tuples
data PtyLifecycleResult = PtyLifecycleResult
    { _plrCreated :: !(Either ServerError Value)
    , _plrListed :: !(Maybe (Either ServerError [Value]))
    , _plrFetched :: !(Maybe (Either ServerError Value))
    , _plrDeleted :: !(Maybe (Either ServerError Bool))
    , _plrFetched2 :: !(Maybe (Either ServerError Value))
    }

-- | Run an IO action with signals ignored
withIgnoreSignals :: IO a -> IO a
withIgnoreSignals action =
    bracket
        ( do
            oldTerm <- Sig.installHandler Sig.sigTERM Sig.Ignore Nothing
            oldHup <- Sig.installHandler Sig.sigHUP Sig.Ignore Nothing
            pure (oldTerm, oldHup)
        )
        ( \(oldTerm, oldHup) -> do
            _ <- Sig.installHandler Sig.sigTERM oldTerm Nothing
            _ <- Sig.installHandler Sig.sigHUP oldHup Nothing
            pure ()
        )
        (const action)

-- | Create test state with caches
withState :: Dhall.DhallCache -> Formatter.ExeCache -> (AppState -> IO a) -> IO a
withState dhallCache exeCache action =
    withTempDir $ \dir ->
        Log.withLoggerLevel "test" ErrorS $ \lg ->
            Storage.withStorage dir $ \store -> do
                bus <- Bus.newBus
                chan <- newBroadcastTChanIO
                _ <- Bus.subscribeAll bus $ \event ->
                    atomically $ writeTChan chan (toJSON event)
                pty <- Pty.newManager dir
                queue <- newTQueueIO
                activeAgents <- newTVarIO Map.empty
                idGen <- Identifier.newIdGenState
                dirCache <- Storage.newDirCache
                let st =
                        AppState
                            { stBus = bus
                            , stStorage = store
                            , stProjectID = "test"
                            , stDirectory = T.pack dir
                            , stVersion = "test"
                            , stEventChan = chan
                            , stPtyManager = pty
                            , stProxy = Nothing
                            , stLogger = lg
                            , stPromptAsyncQueue = queue
                            , stHomeDir = Just dir
                            , stActiveAgents = activeAgents
                            , stIdGen = idGen
                            , stDhallCache = dhallCache
                            , stExeCache = exeCache
                            , stDirCache = dirCache
                            }
                action st

matchFind :: Text -> Text -> Value -> Bool
matchFind token path val =
    case lookupText "path" val of
        Just p -> token `T.isInfixOf` p || p == path
        Nothing -> False

hasSuffix :: Text -> Value -> Bool
hasSuffix suffix val =
    case lookupText "path" val of
        Just p -> suffix `T.isSuffixOf` p
        Nothing -> False

spec :: Dhall.DhallCache -> Formatter.ExeCache -> Spec
spec dhallCache exeCache = do
    describe "Subprocess Handler Integration Tests" $ do
        it "session command handler runs echo and captures output" $ do
            result <- withIgnoreSignals $ withState dhallCache exeCache $ \st -> do
                var <- newEmptyTMVarIO
                _ <- Bus.subscribe (stBus st) "command.executed" $ \event ->
                    atomically $ void $ tryPutTMVar var event
                let input =
                        object
                            [ "command" .= ("echo hello_test" :: Text)
                            , "description" .= ("test" :: Text)
                            , "timeout" .= (2000 :: Int)
                            ]
                res <- runHandlerIO (sessionCommandHandler st "session" input)
                evt <- waitVar 1000000 var
                pure (res, evt)
            case result of
                (Left err, _) -> expectationFailure $ "Handler failed: " ++ show err
                (Right val, evt) -> do
                    hasKey "info" val `shouldBe` True
                    hasKey "parts" val `shouldBe` True
                    case lookupArray "parts" val of
                        Nothing -> expectationFailure "No parts array"
                        Just parts -> case parts of
                            [] -> expectationFailure "Empty parts"
                            (p : _) -> case lookupText "text" p of
                                Nothing -> expectationFailure "No text in part"
                                Just out -> T.isInfixOf "hello_test" out `shouldBe` True
                    isJust evt `shouldBe` True

        it "vcs handler returns branch info when git is available" $ do
            result <- withIgnoreSignals $ withState dhallCache exeCache $ \st -> do
                exe <- findExecutable "git"
                case exe of
                    Nothing -> do
                        res <- runHandlerIO (vcsHandler st)
                        pure (res, Nothing)
                    Just _ -> do
                        let root = T.unpack (stDirectory st)
                        -- Initialize git repo
                        (code, _, _) <- readProcessWithExitCode "git" ["-C", root, "init", "-b", "main"] ""
                        case code of
                            ExitSuccess -> pure ()
                            ExitFailure _ -> do
                                _ <- readProcessWithExitCode "git" ["-C", root, "init"] ""
                                pure ()
                        -- Configure git user (required for some git operations)
                        _ <- readProcessWithExitCode "git" ["-C", root, "config", "user.email", "test@test.com"] ""
                        _ <- readProcessWithExitCode "git" ["-C", root, "config", "user.name", "Test"] ""
                        res <- runHandlerIO (vcsHandler st)
                        pure (res, Just ())
            case result of
                (Left err, _) -> expectationFailure $ "Handler failed: " ++ show err
                (Right info, Nothing) -> branch info `shouldBe` Nothing
                (Right info, Just _) -> isJust (branch info) `shouldBe` True

        it "find handler finds content in files" $ do
            result <- withIgnoreSignals $ withState dhallCache exeCache $ \st -> do
                let root = T.unpack (stDirectory st)
                let path = root </> "find_test.txt"
                TIO.writeFile path "find unique_token_xyz"
                ( do
                        vals <- runHandlerIO (findHandler st (Just "unique_token_xyz") Nothing Nothing)
                        pure (T.pack path, vals, Nothing)
                    )
                    `catch` \(e :: SearchError) -> pure (T.pack path, Right [], Just e)
            case result of
                (_, Left err, _) -> expectationFailure $ "Handler failed: " ++ show err
                (_, Right _, Just e) -> expectationFailure $ "Search error: " ++ show e
                (path, Right vals, Nothing) -> do
                    let matches = filter (matchFind "unique_token_xyz" path) vals
                    null matches `shouldBe` False

        it "find file handler finds files by pattern" $ do
            result <- withIgnoreSignals $ withState dhallCache exeCache $ \st -> do
                let root = T.unpack (stDirectory st)
                let path = root </> "test_findfile.txt"
                TIO.writeFile path "file content"
                ( do
                        vals <- runHandlerIO (findFileHandler st (Just "*.txt") Nothing Nothing Nothing Nothing)
                        pure (T.pack path, vals, Nothing)
                    )
                    `catch` \(e :: SearchError) -> pure (T.pack path, Right [], Just e)
            case result of
                (_, Left err, _) -> expectationFailure $ "Handler failed: " ++ show err
                (_, Right _, Just e) -> expectationFailure $ "Search error: " ++ show e
                (path, Right vals, Nothing) -> do
                    let matches = filter (\v -> lookupText "path" v == Just path || hasSuffix ".txt" v) vals
                    null matches `shouldBe` False

        it "pty handler lifecycle creates, lists, updates, and deletes" $ do
            result <- withIgnoreSignals $ withState dhallCache exeCache $ \st -> do
                let input = object ["command" .= ("sleep" :: Text), "args" .= (["infinity"] :: [Text]), "sandbox" .= False]
                created <- runHandlerIO (ptyCreateHandler st input)
                case created of
                    Left err -> pure $ PtyLifecycleResult (Left err) Nothing Nothing Nothing Nothing
                    Right val -> case lookupText "id" val of
                        Nothing -> pure $ PtyLifecycleResult (Right val) Nothing Nothing Nothing Nothing
                        Just ptyId -> do
                            listed <- runHandlerIO (ptyListHandler st)
                            fetched <- runHandlerIO (ptyGetHandler st ptyId)
                            deleted <- runHandlerIO (ptyDeleteHandler st ptyId)
                            fetched2 <- runHandlerIO (ptyGetHandler st ptyId)
                            pure $ PtyLifecycleResult (Right val) (Just listed) (Just fetched) (Just deleted) (Just fetched2)
            case result of
                PtyLifecycleResult (Left err) _ _ _ _ -> expectationFailure $ "Create failed: " ++ show err
                PtyLifecycleResult (Right val) Nothing _ _ _ -> isJust (lookupText "error" val) `shouldBe` True
                PtyLifecycleResult _ (Just (Left err)) _ _ _ -> expectationFailure $ "List failed: " ++ show err
                PtyLifecycleResult _ _ (Just (Left err)) _ _ -> expectationFailure $ "Fetch failed: " ++ show err
                PtyLifecycleResult _ _ _ (Just (Left err)) _ -> expectationFailure $ "Delete failed: " ++ show err
                PtyLifecycleResult _ (Just (Right listed)) (Just (Right fetched)) (Just (Right deleted)) (Just fetched2) -> do
                    null listed `shouldBe` False
                    isJust (lookupText "id" fetched) `shouldBe` True
                    deleted `shouldBe` True
                    case fetched2 of
                        Left _ -> pure () -- Expected - PTY was deleted
                        Right val -> isJust (lookupText "error" val) `shouldBe` True
                -- Remaining patterns: partial completion of lifecycle (should not occur given code flow)
                PtyLifecycleResult (Right _) (Just (Right _)) Nothing Nothing _ -> expectationFailure "Impossible: fetched Nothing"
                PtyLifecycleResult (Right _) (Just (Right _)) Nothing (Just (Right _)) _ -> expectationFailure "Impossible: fetched Nothing with deleted Right"
                PtyLifecycleResult (Right _) (Just (Right _)) (Just (Right _)) Nothing _ -> expectationFailure "Impossible: deleted Nothing"
                PtyLifecycleResult (Right _) (Just (Right _)) (Just (Right _)) (Just (Right _)) Nothing -> expectationFailure "Impossible: fetched2 Nothing"

        it "session shell handler creates PTY with sleep command" $ do
            result <- withIgnoreSignals $ withState dhallCache exeCache $ \st -> do
                var <- newEmptyTMVarIO
                _ <- Bus.subscribe (stBus st) "pty.created" $ \event ->
                    atomically $ void $ tryPutTMVar var event
                let input =
                        object
                            [ "command" .= ("sleep" :: Text)
                            , "args" .= (["infinity"] :: [Text])
                            , "sandbox" .= False
                            ]
                res <- runHandlerIO (sessionShellHandler st "session" input)
                evt <- waitVar 100000 var
                case res of
                    Left err -> pure (Left err, evt)
                    Right val -> do
                        case lookupText "id" val of
                            Nothing -> pure (Right val, evt)
                            Just pid -> do
                                _ <- Pty.remove (stPtyManager st) pid
                                pure (Right val, evt)
            case result of
                (Left err, _) -> expectationFailure $ "Handler failed: " ++ show err
                (Right val, evt) -> do
                    case lookupText "id" val of
                        Nothing -> expectationFailure "No PTY id returned"
                        Just _ -> isJust evt `shouldBe` True
