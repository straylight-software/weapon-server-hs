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
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.List qualified as List
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
import Test.Helpers (hasKey, lookupArray, lookupText, runHandlerIO, valueToText, waitForCount, waitVar)
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

-- | Match a find.text result which has path: {text: "..."} and lines: {text: "..."}
matchFind :: Text -> Text -> Value -> Bool
matchFind token path val =
    case lookupNestedText "path" "text" val of
        Just p -> token `T.isInfixOf` p || p == path
        Nothing -> False

-- | Extract nested text value like path.text from ripgrep JSON output
lookupNestedText :: Text -> Text -> Value -> Maybe Text
lookupNestedText outerKey innerKey val =
    case lookupObject outerKey val of
        Just inner -> lookupText innerKey inner
        Nothing -> Nothing

-- | Extract an object value from a JSON object by key
lookupObject :: Text -> Value -> Maybe Value
lookupObject key (Object obj) = KM.lookup (K.fromText key) obj
lookupObject _ _ = Nothing

{- | Check if a JSON string value ends with a suffix.
find.files returns array of strings, not objects.
-}
hasSuffix :: Text -> Value -> Bool
hasSuffix suffix val =
    case valueToText val of
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
                        SessionCommandInput
                            { sciCommand = "echo"
                            , sciArguments = "hello_test"
                            , sciMessageID = Nothing
                            , sciAgent = Nothing
                            , sciModel = Nothing
                            , sciVariant = Nothing
                            , sciParts = Nothing
                            }
                res <- runHandlerIO (sessionCommandHandler st "session" Nothing input)
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

        it "vcs handler returns branch info in git repo, 404 otherwise" $ do
            result <- withIgnoreSignals $ withState dhallCache exeCache $ \st -> do
                exe <- findExecutable "git"
                case exe of
                    Nothing -> do
                        -- No git available, should return 404
                        res <- runHandlerIO (vcsHandler st)
                        pure (res, False)
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
                        pure (res, True)
            case result of
                -- When git is not available or not in repo, expect 404
                (Left _err, False) -> pure ()
                -- When in git repo, expect branch info
                (Left err, True) -> expectationFailure $ "Handler failed in git repo: " ++ show err
                (Right info, True) -> T.null (branch info) `shouldBe` False
                (Right _info, False) -> expectationFailure "Expected 404 when not in git repo"

        it "find handler finds content in files" $ do
            result <- withIgnoreSignals $ withState dhallCache exeCache $ \st -> do
                let root = T.unpack (stDirectory st)
                let path = root </> "find_test.txt"
                TIO.writeFile path "find unique_token_xyz"
                ( do
                        vals <- runHandlerIO (findHandler st (Just "unique_token_xyz") Nothing)
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
                        vals <- runHandlerIO (findFileHandler st (Just "*.txt") Nothing Nothing Nothing)
                        pure (T.pack path, vals, Nothing)
                    )
                    `catch` \(e :: SearchError) -> pure (T.pack path, Right [], Just e)
            case result of
                (_, Left err, _) -> expectationFailure $ "Handler failed: " ++ show err
                (_, Right _, Just e) -> expectationFailure $ "Search error: " ++ show e
                (path, Right vals, Nothing) -> do
                    -- find.files returns array of strings, not objects
                    let matches = filter (\v -> valueToText v == Just path || hasSuffix ".txt" v) vals
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
                        SessionShellInput
                            { ssiAgent = "test"
                            , ssiCommand = "sleep 1"
                            , ssiModel = Nothing
                            }
                res <- runHandlerIO (sessionShellHandler st "session" Nothing input)
                evt <- waitVar 100000 var
                -- Clean up any PTY created - extract id from event properties
                case evt of
                    Nothing -> pure ()
                    Just busEvt -> do
                        let props = Bus.beProperties busEvt
                        case lookupObject "info" props >>= lookupText "id" of
                            Nothing -> pure ()
                            Just pid -> void $ Pty.remove (stPtyManager st) pid
                pure (res, evt)
            case result of
                (Left err, _) -> expectationFailure $ "Handler failed: " ++ show err
                (Right msgInfo, evt) -> do
                    -- Verify we got a valid AssistantMessageInfo with an ID
                    T.null (amiId msgInfo) `shouldBe` False
                    isJust evt `shouldBe` True

        it "messages remain ordered after session cancel" $ do
            result <- withIgnoreSignals $ withState dhallCache exeCache $ \st -> do
                let sessionId = "session_cancel_order_test"

                -- Track session.status events to know when background work is done
                statusCountVar <- newTVarIO (0 :: Int)
                unsubStatus <- Bus.subscribe (stBus st) "session.status" $ \_event ->
                    atomically $ modifyTVar' statusCountVar (+ 1)

                -- 1. Send first message
                let parts1 = [object ["type" .= ("text" :: Text), "text" .= ("first message" :: Text)]]
                let input1 = CreateMessageInput Nothing parts1 Nothing Nothing
                _ <- runHandlerIO (sessionMessageCreateHandler st sessionId input1)
                -- Wait for first message cycle to complete (busy + idle = 2)
                _ <- waitForCount 2000000 statusCountVar 2

                -- 2. Abort/cancel the session (publishes session.error)
                abortVar <- newEmptyTMVarIO
                _ <- Bus.subscribe (stBus st) "session.error" $ \event ->
                    atomically $ void $ tryPutTMVar abortVar event
                _ <- runHandlerIO (sessionAbortHandler st sessionId Nothing)
                _ <- waitVar 2000000 abortVar

                -- 3. Send second message
                let parts2 = [object ["type" .= ("text" :: Text), "text" .= ("second message" :: Text)]]
                let input2 = CreateMessageInput Nothing parts2 Nothing Nothing
                _ <- runHandlerIO (sessionMessageCreateHandler st sessionId input2)
                -- Wait for second message cycle to complete (should be 4 total now)
                _ <- waitForCount 2000000 statusCountVar 4
                unsubStatus

                -- 4. Get all messages
                runHandlerIO (sessionMessageListHandler st sessionId Nothing)

            case result of
                Left err -> expectationFailure $ "Handler failed: " ++ show err
                Right msgs -> do
                    -- Extract roles
                    let roles = map (messageInfoRole . msgInfo) msgs
                    let userIndices = List.elemIndices "user" roles
                    let assistantIndices = List.elemIndices "assistant" roles

                    -- Key property: Each user message should come before its corresponding assistant
                    -- Verify strict ordering: user1 < assistant1 < user2 < assistant2
                    -- Pattern matching ensures we have at least 2 of each
                    case (userIndices, assistantIndices) of
                        (u1 : u2 : _moreUsers, a1 : a2 : _moreAssistants) -> do
                            u1 `shouldSatisfy` (< a1)
                            a1 `shouldSatisfy` (< u2)
                            u2 `shouldSatisfy` (< a2)
                        ([], _anyAssistants) -> expectationFailure "Expected at least 2 user messages, got 0"
                        ([_oneUser], _anyAssistants) -> expectationFailure "Expected at least 2 user messages, got 1"
                        (_anyUsers, []) -> expectationFailure "Expected at least 2 assistant messages, got 0"
                        (_anyUsers, [_oneAssistant]) -> expectationFailure "Expected at least 2 assistant messages, got 1"
