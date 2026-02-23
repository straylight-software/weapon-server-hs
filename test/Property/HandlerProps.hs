{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Property.HandlerProps where

import Api
import Bus.Bus qualified as Bus
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket, catch)
import Control.Monad (void)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Find.Search (SearchError)
import Global.Event (globalEventHandler)
import Handlers
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Katip (Severity (ErrorS))
import Log qualified
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai (defaultRequest, responseToStream)
import Network.Wai.Internal (ResponseReceived (..))
import Prompt.Async qualified as PromptAsync
import Pty.Connect (ptyConnectHandler)
import Pty.Pty qualified as Pty
import Servant (Tagged (..))
import Servant.Server (Handler, ServerError, runHandler)
import State
import Storage.Storage qualified as Storage
import System.Directory (createDirectory, findExecutable)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Signals qualified as Sig
import System.Process (readProcessWithExitCode)
import Test.Fixture (withTempDir)
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Runners (NumThreads (..))
import Tui.Store qualified as TuiStore
import Util.StorageKeys (todoKey)
import Vcs.Diff (VcsError)

data ProjectHandlersResult = ProjectHandlersResult
    { phrListed :: !(Either ServerError [Project])
    , phrCurrent :: !(Either ServerError Project)
    , phrFetched :: !(Either ServerError Project)
    , phrDir :: !Text
    }

data PtyLifecycleDetails = PtyLifecycleDetails
    { pldId :: !Text
    , pldListed :: !(Either ServerError [Value])
    , pldFetched :: !(Either ServerError Value)
    , pldUpdated :: !(Either ServerError Value)
    , pldDeleted :: !(Either ServerError Bool)
    , pldFetched2 :: !(Either ServerError Value)
    }

data PtyLifecycleResult = PtyLifecycleResult
    { plCreated :: !(Either ServerError Value)
    , plDetails :: !(Maybe PtyLifecycleDetails)
    }

data TuiHandlersResult = TuiHandlersResult
    { thrAppended :: !(Either ServerError Bool)
    , thrPrompt :: !Text
    , thrSubmitted :: !(Either ServerError Bool)
    , thrCleared :: !(Either ServerError Bool)
    , thrOpenHelp :: !(Either ServerError Bool)
    , thrOpenSessions :: !(Either ServerError Bool)
    , thrOpenThemes :: !(Either ServerError Bool)
    , thrOpenModels :: !(Either ServerError Bool)
    , thrExec :: !(Either ServerError Bool)
    , thrToast :: !(Either ServerError Bool)
    , thrPublish :: !(Either ServerError Bool)
    , thrSelect :: !(Either ServerError Bool)
    , thrControlNext :: !(Either ServerError Bool)
    , thrControlResponse :: !(Either ServerError Bool)
    , thrLast :: !(Maybe Value)
    }

data ExperimentalWorktreeResult = ExperimentalWorktreeResult
    { ewrGet :: !(Either ServerError [Text])
    , ewrSet :: !(Either ServerError Value)
    , ewrReset :: !(Either ServerError Bool)
    , ewrRoot :: !Text
    }

data SessionLifecycleResult = SessionLifecycleResult
    { slrCreated :: !(Either ServerError Session)
    , slrListed :: !(Either ServerError [Session])
    , slrFetched :: !(Either ServerError Session)
    , slrUpdated :: !(Either ServerError Session)
    , slrDeleted :: !(Either ServerError Bool)
    , slrFetched2 :: !(Either ServerError Session)
    }

{- | Create a temporary directory for testing
Uses /dev/shm for faster in-memory operations
-}
withTmp :: (FilePath -> IO a) -> IO a
withTmp = withTempDir

{- | Run an IO action with SIGTERM and SIGHUP ignored to prevent signal
propagation from child processes (PTYs) terminating during tests.
This is needed because PTY processes can send signals when they exit.
-}
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

withState :: (AppState -> IO a) -> IO a
withState action =
    withTmp $ \dir ->
        Log.withLoggerLevel "test" ErrorS $ \lg ->
            Storage.withStorage dir $ \store -> do
                bus <- Bus.newBus
                chan <- newBroadcastTChanIO
                _ <- Bus.subscribeAll bus $ \event ->
                    atomically $ writeTChan chan (toJSON event)
                pty <- Pty.newManager dir
                queue <- newTQueueIO
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
                            }
                action st

setVar :: String -> Maybe String -> IO ()
setVar key val = case val of
    Nothing -> unsetEnv key
    Just v -> setEnv key v

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv key val action =
    bracket (lookupEnv key) (setVar key) (\_previous -> setVar key val >> action)

runHandlerIO :: Handler a -> IO (Either ServerError a)
runHandlerIO = runHandler

waitVar :: Int -> TMVar a -> IO (Maybe a)
waitVar delay var = do
    gate <- registerDelay delay
    atomically $
        (Just <$> takeTMVar var)
            `orElse` do
                done <- readTVar gate
                if done then pure Nothing else retry

lookupText :: Text -> Value -> Maybe Text
lookupText key (Object obj) = case KM.lookup (K.fromText key) obj of
    Just (String t) -> Just t
    _otherResult -> Nothing
lookupText _ _ = Nothing

lookupBool :: Text -> Value -> Maybe Bool
lookupBool key (Object obj) = case KM.lookup (K.fromText key) obj of
    Just (Bool b) -> Just b
    _otherResult -> Nothing
lookupBool _ _ = Nothing

lookupArray :: Text -> Value -> Maybe [Value]
lookupArray key (Object obj) = case KM.lookup (K.fromText key) obj of
    Just (Array arr) -> Just (toList arr)
    _otherResult -> Nothing
  where
    toList = foldr (:) []
lookupArray _ _ = Nothing

hasKey :: Text -> Value -> Bool
hasKey key (Object obj) = KM.member (K.fromText key) obj
hasKey _ _ = False

isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False

matchFind :: Text -> Text -> Value -> Bool
matchFind token path val =
    case (lookupText "text" val, lookupText "path" val) of
        (Just txt, Just pth) -> token `T.isInfixOf` txt && pth == path
        _otherResult -> False

hasSuffix :: Text -> Value -> Bool
hasSuffix suffix val = case lookupText "path" val of
    Just pth -> suffix `T.isSuffixOf` pth
    Nothing -> False

genName :: Gen Text
genName = Gen.text (Range.linear 1 12) Gen.alphaNum

genText :: Gen Text
genText = Gen.text (Range.linear 1 64) Gen.alphaNum

prop_healthHandler :: Property
prop_healthHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> do
        res <- runHandlerIO (healthHandler st)
        pure (res, stVersion st)
    case result of
        (Left _err, _) -> failure
        (Right health, ver) -> do
            healthy health === True
            version health === ver

prop_pathHandler :: Property
prop_pathHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> do
        res <- runHandlerIO (pathHandler st)
        pure (res, stDirectory st)
    case result of
        (Left _err, _) -> failure
        (Right info, dir) -> do
            let PathInfo{worktree = wt, state = stPath} = info
            wt === dir
            assert $ T.isSuffixOf ".opencode/state" stPath

prop_globalConfigHandler :: Property
prop_globalConfigHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> runHandlerIO (globalConfigHandler st)
    case result of
        Left _err -> failure
        Right val -> assert $ isObject val

prop_projectHandlers :: Property
prop_projectHandlers = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> do
        listed <- runHandlerIO (projectListHandler st)
        current <- runHandlerIO (projectCurrentHandler st Nothing)
        fetched <- case current of
            Left _err -> pure (Left _err)
            Right cur -> runHandlerIO (projectGetHandler st (Api.id cur))
        pure
            ProjectHandlersResult
                { phrListed = listed
                , phrCurrent = current
                , phrFetched = fetched
                , phrDir = stDirectory st
                }
    case result of
        ProjectHandlersResult{phrListed = Left _err} -> failure
        ProjectHandlersResult{phrCurrent = Left _err} -> failure
        ProjectHandlersResult{phrFetched = Left _err} -> failure
        ProjectHandlersResult
            { phrListed = Right listed
            , phrCurrent = Right current
            , phrFetched = Right fetched
            , phrDir = dir
            } -> do
                let projWork p = case p of Project{worktree = wt} -> wt
                assert $ any (\p -> projWork p == dir) listed
                projWork current === dir
                projWork fetched === dir

prop_projectUpdateHandler :: Property
prop_projectUpdateHandler = withTests 20 $ property $ do
    newName <- forAll genText
    result <- evalIO $ withState $ \st -> do
        current <- runHandlerIO (projectCurrentHandler st Nothing)
        case current of
            Left _err -> pure (Left _err)
            Right cur -> do
                let pid = Api.id cur
                let input = object ["name" .= newName]
                updated <- runHandlerIO (projectUpdateHandler st pid input)
                current2 <- runHandlerIO (projectCurrentHandler st Nothing)
                listed <- runHandlerIO (projectListHandler st)
                pure (Right (updated, current2, listed))
    case result of
        Left _err -> failure
        Right (updated, current2, listed) -> do
            case updated of
                Left _err -> failure
                Right proj -> name proj === Just newName
            case current2 of
                Left _err -> failure
                Right proj -> name proj === Just newName
            case listed of
                Left _err -> failure
                Right projs -> assert $ any (\p -> name p == Just newName) projs

prop_providerListHandler :: Property
prop_providerListHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> runHandlerIO (providerListHandler st Nothing)
    case result of
        Left _err -> failure
        Right pl -> assert $ isObject (cplDefault pl)

prop_providerAuthHandler :: Property
prop_providerAuthHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> runHandlerIO (providerAuthHandler st)
    case result of
        Left _err -> failure
        Right val -> assert $ isObject val

prop_providerHandler :: Property
prop_providerHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> runHandlerIO (providerHandler st Nothing)
    case result of
        Left _err -> failure
        Right _result -> success

prop_providerOauthHandlers :: Property
prop_providerOauthHandlers = withTests 10 $ property $ do
    pid <- forAll genName
    token <- forAll genName
    result <- evalIO $ withState $ \st -> do
        let input = object ["redirect" .= ("http://localhost" :: Text), "scopes" .= ["read" :: Text]]
        auth <- runHandlerIO (providerOauthAuthorizeHandler st pid input)
        callback <- case auth of
            Left _err -> pure (Left _err)
            Right val -> do
                case lookupText "state" val of
                    Nothing -> pure (Right False)
                    Just state -> do
                        let payload = object ["state" .= state, "token" .= token]
                        runHandlerIO (providerOauthCallbackHandler st pid Nothing payload)
        pure (auth, callback)
    case result of
        (Left _err, _) -> failure
        (_, Left _err) -> failure
        (Right auth, Right callbackResult) -> do
            lookupText "providerID" auth === Just pid
            assert $ isJust (lookupText "state" auth)
            -- Now callback returns Bool
            callbackResult === True

prop_configHandler :: Property
prop_configHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> runHandlerIO (configHandler st)
    case result of
        Left _err -> failure
        Right val -> assert $ isObject val

prop_commandHandler :: Property
prop_commandHandler = withTests 20 $ property $ do
    result <- evalIO $ runHandlerIO commandHandler
    case result of
        Left _err -> failure
        Right defs -> do
            let names = mapMaybe (lookupText "name") defs
            assert $ "bash" `elem` names

prop_agentHandler :: Property
prop_agentHandler = withTests 20 $ property $ do
    result <- evalIO $ runHandlerIO agentHandler
    case result of
        Left _err -> failure
        Right _result -> success

prop_sessionStatusHandler :: Property
prop_sessionStatusHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> runHandlerIO (sessionStatusHandler st Nothing)
    case result of
        Left _err -> failure
        Right val -> do
            -- Returns an empty map (object {}) when no sessions are active
            -- The map has type Map<SessionID, SessionStatus>
            assert $ isObject val

prop_sessionLifecycleHandler :: Property
prop_sessionLifecycleHandler = withTests 20 $ property $ do
    title <- forAll genText
    next <- forAll genText
    result <- evalIO $ withState $ \st -> do
        let input = CreateSessionInput (Just title) Nothing
        created <- runHandlerIO (sessionCreateHandler st Nothing input)
        listed <- runHandlerIO (sessionListHandler st Nothing Nothing Nothing Nothing Nothing)
        fetched <- case created of
            Left _err -> pure (Left _err)
            Right ses -> runHandlerIO (sessionGetHandler st (sessionId ses))
        updated <- case created of
            Left _err -> pure (Left _err)
            Right ses -> runHandlerIO (sessionUpdateHandler st (sessionId ses) (UpdateSessionInput (Just next) Nothing Nothing Nothing))
        deleted <- case created of
            Left _err -> pure (Left _err)
            Right ses -> runHandlerIO (sessionDeleteHandler st (sessionId ses))
        fetched2 <- case created of
            Left _err -> pure (Left _err)
            Right ses -> runHandlerIO (sessionGetHandler st (sessionId ses))
        pure
            SessionLifecycleResult
                { slrCreated = created
                , slrListed = listed
                , slrFetched = fetched
                , slrUpdated = updated
                , slrDeleted = deleted
                , slrFetched2 = fetched2
                }
    case result of
        SessionLifecycleResult{slrCreated = Left _err} -> failure
        SessionLifecycleResult{slrListed = Left _err} -> failure
        SessionLifecycleResult{slrFetched = Left _err} -> failure
        SessionLifecycleResult{slrUpdated = Left _err} -> failure
        SessionLifecycleResult{slrDeleted = Left _err} -> failure
        SessionLifecycleResult
            { slrCreated = Right created
            , slrListed = Right listed
            , slrFetched = Right fetched
            , slrUpdated = Right updated
            , slrDeleted = Right deleted
            , slrFetched2 = Left _err
            } -> do
                assert $ any (\s -> sessionId s == sessionId created) listed
                sessionId fetched === sessionId created
                sessionTitle updated === next
                deleted === True
        _otherResult -> failure

prop_sessionChildrenHandler :: Property
prop_sessionChildrenHandler = withTests 20 $ property $ do
    parentTitle <- forAll genText
    childTitle <- forAll genText
    result <- evalIO $ withState $ \st -> do
        parent <- runHandlerIO (sessionCreateHandler st Nothing (CreateSessionInput (Just parentTitle) Nothing))
        child <- case parent of
            Left _err -> pure (Left _err)
            Right ses -> runHandlerIO (sessionCreateHandler st Nothing (CreateSessionInput (Just childTitle) (Just (sessionId ses))))
        kids <- case parent of
            Left _err -> pure (Left _err)
            Right ses -> runHandlerIO (sessionChildrenHandler st (sessionId ses) Nothing)
        pure (parent, child, kids)
    case result of
        (Left _err, _, _) -> failure
        (_, Left _err, _) -> failure
        (_, _, Left _err) -> failure
        (Right parent, Right child, Right kids) ->
            assert $ any (\s -> sessionId s == sessionId child && sessionParentID s == Just (sessionId parent)) kids

prop_sessionTodoHandler :: Property
prop_sessionTodoHandler = withTests 20 $ property $ do
    sid <- forAll genName
    item <- forAll genText
    result <- evalIO $ withState $ \st -> do
        let todos = [object ["text" .= item]]
        Storage.write (stStorage st) (todoKey sid) todos
        runHandlerIO (sessionTodoHandler st sid)
    case result of
        Left _err -> failure
        Right todos -> todos === [object ["text" .= item]]

prop_sessionInitHandler :: Property
prop_sessionInitHandler = withTests 20 $ property $ do
    sid <- forAll genName
    result <- evalIO $ withState $ \st -> do
        var <- newEmptyTMVarIO
        _ <- Bus.subscribe (stBus st) "session.initialized" $ \event ->
            atomically $ void $ tryPutTMVar var event
        res <- runHandlerIO (sessionInitHandler st sid)
        evt <- waitVar 1000000 var
        pure (res, evt)
    case result of
        (Left _err, _) -> failure
        (Right val, evt) -> do
            -- Handler now returns Bool (true on success)
            val === True
            assert $ isJust evt

prop_sessionForkHandler :: Property
prop_sessionForkHandler = withTests 20 $ property $ do
    title <- forAll genText
    result <- evalIO $ withState $ \st -> do
        parent <- runHandlerIO (sessionCreateHandler st Nothing (CreateSessionInput (Just title) Nothing))
        forked <- case parent of
            Left _err -> pure (Left _err)
            Right ses -> runHandlerIO (sessionForkHandler st (sessionId ses) (ForkSessionInput Nothing))
        pure (parent, forked)
    case result of
        (Left _err, _) -> failure
        (_, Left _err) -> failure
        (Right parent, Right forked) -> do
            sessionParentID forked === Just (sessionId parent)
            assert $ "Fork of" `T.isPrefixOf` sessionTitle forked

prop_sessionAbortHandler :: Property
prop_sessionAbortHandler = withTests 20 $ property $ do
    sid <- forAll genName
    result <- evalIO $ withState $ \st -> do
        var <- newEmptyTMVarIO
        _ <- Bus.subscribe (stBus st) "session.error" $ \event ->
            atomically $ void $ tryPutTMVar var event
        res <- runHandlerIO (sessionAbortHandler st sid Nothing)
        evt <- waitVar 1000000 var
        pure (res, evt)
    case result of
        (Left _err, _) -> failure
        (Right True, evt) -> do
            assert $ isJust evt
        _otherResult -> failure

prop_sessionShareHandlers :: Property
prop_sessionShareHandlers = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> do
        created <- runHandlerIO (sessionCreateHandler st Nothing (CreateSessionInput (Just "share") Nothing))
        shared <- case created of
            Left _err -> pure (Left _err)
            Right ses -> runHandlerIO (sessionShareCreateHandler st (sessionId ses))
        deleted <- case created of
            Left _err -> pure (Left _err)
            Right ses -> runHandlerIO (sessionShareDeleteHandler st (sessionId ses))
        pure (created, shared, deleted)
    case result of
        (Left _err, _, _) -> failure
        (_, Left _err, _) -> failure
        (_, _, Left _err) -> failure
        (Right created, Right shared, Right deleted) -> do
            -- After sharing, the session should have a share URL containing the session ID
            case sessionShare shared of
                Nothing -> failure
                Just share -> assert $ sessionId created `T.isInfixOf` shareUrl share
            -- After unsharing, the session should have no share
            sessionShare deleted === Nothing

prop_sessionDiffHandler :: Property
prop_sessionDiffHandler = withTests 20 $ property $ do
    sid <- forAll genName
    result <- evalIO $ withState $ \st ->
        ( do
            res <- runHandlerIO (sessionDiffHandler st sid Nothing)
            pure (res, Nothing :: Maybe VcsError)
        )
            `catch` \(e :: VcsError) -> pure (Right [], Just e)
    case result of
        (_, Just e) -> do
            annotate $ show e
            failure
        (Left _err, Nothing) -> failure
        (Right diffs, Nothing) -> do
            -- Handler now returns [FileDiff] - check it's a valid list
            -- (can be empty if no changes in working directory)
            assert $ all validFileDiff diffs
  where
    validFileDiff fd = not (T.null (fdFile fd))

prop_sessionSummarizeHandler :: Property
prop_sessionSummarizeHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st ->
        ( do
            created <- runHandlerIO (sessionCreateHandler st Nothing (CreateSessionInput (Just "sum") Nothing))
            summarized <- case created of
                Left _err -> pure (Left _err)
                Right ses -> runHandlerIO (sessionSummarizeHandler st (sessionId ses))
            pure (summarized, Nothing)
        )
            `catch` \(e :: VcsError) -> pure (Right True, Just e)
    case result of
        (_, Just e) -> do
            annotate $ show e
            failure
        (Left _err, Nothing) -> failure
        (Right ok, Nothing) -> ok === True

prop_sessionRevertHandlers :: Property
prop_sessionRevertHandlers = withTests 20 $ property $ do
    mid <- forAll genName
    result <- evalIO $ withState $ \st -> do
        created <- runHandlerIO (sessionCreateHandler st Nothing (CreateSessionInput (Just "rev") Nothing))
        reverted <- case created of
            Left _err -> pure (Left _err)
            Right ses -> runHandlerIO (sessionRevertHandler st (sessionId ses) (SessionRevert mid Nothing Nothing Nothing))
        unreverted <- case created of
            Left _err -> pure (Left _err)
            Right ses -> runHandlerIO (sessionUnrevertHandler st (sessionId ses))
        pure (reverted, unreverted)
    case result of
        (Left _err, _) -> failure
        (_, Left _err) -> failure
        (Right reverted, Right unreverted) -> do
            -- After revert, the session should have a revert with the message ID
            case sessionRevert reverted of
                Nothing -> failure
                Just rev -> revertMessageID rev === mid
            -- After unrevert, the session should have no revert
            sessionRevert unreverted === Nothing

prop_sessionPermissionHandler :: Property
prop_sessionPermissionHandler = withTests 20 $ property $ do
    sid <- forAll genName
    pid <- forAll genName
    result <- evalIO $ withState $ \st -> do
        var <- newEmptyTMVarIO
        _ <- Bus.subscribe (stBus st) "permission.replied" $ \event ->
            atomically $ void $ tryPutTMVar var event
        let input = object ["ok" .= True]
        res <- runHandlerIO (sessionPermissionHandler st sid pid Nothing input)
        evt <- waitVar 1000000 var
        pure (res, evt)
    case result of
        (Left _err, _) -> failure
        (Right val, evt) -> do
            val === True
            assert $ isJust evt

prop_sessionMessageHandlers :: Property
prop_sessionMessageHandlers = withTests 20 $ property $ do
    msg <- forAll genText
    result <- evalIO $ withState $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            let parts = [object ["id" .= ("part_1" :: Text), "type" .= ("text" :: Text), "text" .= msg]]
            let input = CreateMessageInput Nothing parts Nothing Nothing
            created <- runHandlerIO (sessionMessageCreateHandler st "session" input)
            -- Longer delay to let the forked LLM thread complete (it writes error when no API key)
            threadDelay 100000 -- 100ms
            listed <- runHandlerIO (sessionMessageListHandler st "session" Nothing)
            -- Get the user message ID from the listed messages (first user message)
            let userMsgs = case listed of
                    Right msgs -> filter (\m -> messageInfoRole (msgInfo m) == ("user" :: Text)) msgs
                    Left _ -> []
            let userMsgId = case userMsgs of
                    (m : _) -> messageInfoId (msgInfo m)
                    [] -> "not_found"
            fetched <- runHandlerIO (sessionMessageGetHandler st "session" userMsgId)
            pure (created, listed, fetched)
    case result of
        (Left _err, _, _) -> failure
        (_, Left _err, _) -> failure
        (_, _, Left _err) -> failure
        (Right _created, Right listed, Right fetched) -> do
            assert $ listLength listed >= 2
            -- Verify fetched message is a user message
            messageInfoRole (msgInfo fetched) === "user"
            let assistants = filter (\m -> messageInfoRole (msgInfo m) == ("assistant" :: Text)) listed
            assert $ not (all (null . msgParts) assistants)

{- | Property: Message creation publishes message.updated events to the bus.
This is critical for SSE event delivery - the TUI relies on these events
to know when messages are created/updated. Without them, Ctrl+C won't work
because the TUI doesn't know the assistant is responding.
-}
prop_sessionMessageCreatePublishesEvents :: Property
prop_sessionMessageCreatePublishesEvents = withTests 20 $ property $ do
    msg <- forAll genText
    result <- evalIO $ withState $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            -- Collect all message.updated events
            eventsVar <- newTVarIO ([] :: [Bus.BusEvent])
            unsubscribe <- Bus.subscribe (stBus st) "message.updated" $ \event ->
                atomically $ modifyTVar' eventsVar (event :)

            -- Also collect message.part.updated events
            partsVar <- newTVarIO ([] :: [Bus.BusEvent])
            unsubscribeParts <- Bus.subscribe (stBus st) "message.part.updated" $ \event ->
                atomically $ modifyTVar' partsVar (event :)

            let parts = [object ["id" .= ("part_1" :: Text), "type" .= ("text" :: Text), "text" .= msg]]
            let input = CreateMessageInput (Just "msg_test") parts Nothing Nothing
            _ <- runHandlerIO (sessionMessageCreateHandler st "session_test" input)

            -- Wait for events to be delivered
            threadDelay 50000 -- 50ms
            unsubscribe
            unsubscribeParts

            msgEvents <- readTVarIO eventsVar
            partEvents <- readTVarIO partsVar
            pure (msgEvents, partEvents)
    case result of
        (msgEvents, partEvents) -> do
            -- Should have at least 2 message.updated events (user + assistant)
            annotate $ "message.updated events: " ++ show (listLength msgEvents)
            assert $ listLength msgEvents >= 2
            -- Should have at least 1 message.part.updated event (user's text part)
            annotate $ "message.part.updated events: " ++ show (listLength partEvents)
            assert $ listLength partEvents >= 1

{- | Property: Message events are forwarded from bus to eventChan (SSE delivery path).
This verifies the State.hs subscription that forwards bus events to eventChan,
which is what SSE handlers read from.
-}
prop_messageEventsForwardedToEventChan :: Property
prop_messageEventsForwardedToEventChan = withTests 20 $ property $ do
    msg <- forAll genText
    result <- evalIO $ withState $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            -- Create a reader from eventChan (like SSE handler does)
            reader <- atomically $ dupTChan (stEventChan st)
            eventsVar <- newTVarIO ([] :: [Value])

            -- Start collecting events in background
            collectTid <- forkIO $ do
                let collectLoop = do
                        val <- atomically $ readTChan reader
                        atomically $ modifyTVar' eventsVar (val :)
                        collectLoop
                collectLoop

            -- Create a message
            let parts = [object ["id" .= ("part_1" :: Text), "type" .= ("text" :: Text), "text" .= msg]]
            let input = CreateMessageInput (Just "msg_sse") parts Nothing Nothing
            _ <- runHandlerIO (sessionMessageCreateHandler st "session_sse" input)

            -- Wait for events to propagate
            threadDelay 100000 -- 100ms
            killThread collectTid

            events <- readTVarIO eventsVar
            let messageEvents = filter isMessageEvent events
            pure messageEvents
    case result of
        events -> do
            annotate $ "Events received via eventChan: " ++ show (listLength events)
            -- Should receive message.updated events via eventChan
            assert $ listLength events >= 2
  where
    isMessageEvent val = case val of
        Object obj -> case KM.lookup "type" obj of
            Just (String t) -> "message" `T.isPrefixOf` t
            Just (Object _) -> False
            Just (Array _) -> False
            Just (Number _) -> False
            Just (Bool _) -> False
            Just Null -> False
            Nothing -> False
        Array _ -> False
        String _ -> False
        Number _ -> False
        Bool _ -> False
        Null -> False

{- | Property: Message creation publishes session.status events.
This is critical for TUI Ctrl+C support - the TUI needs to know when
a session is busy (processing) vs idle (done).
-}
prop_sessionStatusEventsPublished :: Property
prop_sessionStatusEventsPublished = withTests 20 $ property $ do
    msg <- forAll genText
    result <- evalIO $ withState $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            -- Collect session.status events
            statusEvents <- newTVarIO ([] :: [Bus.BusEvent])
            unsubscribe <- Bus.subscribe (stBus st) "session.status" $ \event ->
                atomically $ modifyTVar' statusEvents (event :)

            let parts = [object ["id" .= ("part_1" :: Text), "type" .= ("text" :: Text), "text" .= msg]]
            let input = CreateMessageInput (Just "msg_status") parts Nothing Nothing
            _ <- runHandlerIO (sessionMessageCreateHandler st "session_status_test" input)

            -- Wait for events
            threadDelay 100000 -- 100ms
            unsubscribe

            readTVarIO statusEvents
    case result of
        events -> do
            -- Should have at least 2 session.status events (busy at start, idle at end)
            annotate $ "session.status events: " ++ show (listLength events)
            assert $ listLength events >= 2

prop_sessionMessagePartHandlers :: Property
prop_sessionMessagePartHandlers = withTests 20 $ property $ do
    txt <- forAll genText
    next <- forAll genText
    result <- evalIO $ withState $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            let pid = "part_1" :: Text
            let parts = [object ["id" .= pid, "type" .= ("text" :: Text), "text" .= txt]]
            let input = CreateMessageInput Nothing parts Nothing Nothing
            _ <- runHandlerIO (sessionMessageCreateHandler st "session" input)
            -- Wait a bit for message storage
            threadDelay 50000
            -- Get the user message ID from listed messages (create returns assistant message)
            listed <- runHandlerIO (sessionMessageListHandler st "session" Nothing)
            let userMsgId = case listed of
                    Right msgs ->
                        case filter (\m -> messageInfoRole (msgInfo m) == ("user" :: Text)) msgs of
                            (m : _) -> messageInfoId (msgInfo m)
                            [] -> "not_found"
                    Left _ -> "not_found"
            let update =
                    object
                        [ "id" .= pid
                        , "sessionID" .= ("session" :: Text)
                        , "messageID" .= userMsgId
                        , "type" .= ("text" :: Text)
                        , "text" .= next
                        ]
            updated <- runHandlerIO (sessionMessagePartUpdateHandler st "session" userMsgId pid update)
            deleted <- runHandlerIO (sessionMessagePartDeleteHandler st "session" userMsgId pid)
            pure (listed, updated, deleted)
    case result of
        (Left _err, _, _) -> failure
        (_, Left _err, _) -> failure
        (_, _, Left _err) -> failure
        (Right _listed, Right updated, Right deleted) -> do
            lookupText "text" updated === Just next
            deleted === True

prop_lspHandler :: Property
prop_lspHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> runHandlerIO (lspHandler st)
    case result of
        Left _err -> failure
        Right _result -> success

prop_permissionHandlers :: Property
prop_permissionHandlers = withTests 20 $ property $ do
    rid <- forAll genName
    result <- evalIO $ withState $ \st -> do
        let payload = object ["ok" .= True]
        _ <- runHandlerIO (permissionReplyHandler st rid Nothing payload)
        runHandlerIO (permissionHandler st Nothing)
    case result of
        Left _err -> failure
        Right vals -> do
            let hits = filter (\v -> lookupText "requestID" v == Just rid) vals
            assert $ not (null hits)

prop_questionHandlers :: Property
prop_questionHandlers = withTests 20 $ property $ do
    rid <- forAll genName
    result <- evalIO $ withState $ \st -> do
        let payload = object ["ok" .= True]
        _ <- runHandlerIO (questionReplyHandler st rid Nothing payload)
        _ <- runHandlerIO (questionRejectHandler st (rid <> "_r") Nothing payload)
        runHandlerIO (questionHandler st Nothing)
    case result of
        Left _err -> failure
        Right vals -> do
            let hits = filter (\v -> lookupText "requestID" v == Just rid) vals
            assert $ not (null hits)

prop_fileStatusHandler :: Property
prop_fileStatusHandler = withTests 20 $ property $ do
    name <- forAll genName
    result <- evalIO $ withState $ \st -> do
        let root = T.unpack (stDirectory st)
        let path = root </> T.unpack name
        TIO.writeFile path "ok"
        runHandlerIO (fileStatusHandler st (Just (stDirectory st)) (Just name))
    case result of
        Left _err -> failure
        Right vals -> do
            let matches = filter (\v -> lookupText "path" v == Just name) vals
            assert $ not (null matches)

prop_tuiHandlers :: Property
prop_tuiHandlers = withTests 20 $ property $ do
    txt <- forAll genText
    result <- evalIO $ withState $ \st -> do
        let input = object ["text" .= txt]
        appended <- runHandlerIO (tuiAppendPromptHandler st Nothing input)
        prompt <- TuiStore.getPrompt (stStorage st)
        submitted <- runHandlerIO (tuiSubmitPromptHandler st Nothing)
        cleared <- runHandlerIO (tuiClearPromptHandler st Nothing)
        openHelp <- runHandlerIO (tuiOpenHandler st "open-help" Nothing)
        openSessions <- runHandlerIO (tuiOpenHandler st "open-sessions" Nothing)
        openThemes <- runHandlerIO (tuiOpenHandler st "open-themes" Nothing)
        openModels <- runHandlerIO (tuiOpenHandler st "open-models" Nothing)
        exec <- runHandlerIO (tuiExecuteCommandHandler st Nothing (object ["cmd" .= ("ok" :: Text)]))
        toast <- runHandlerIO (tuiShowToastHandler st Nothing (object ["msg" .= ("ok" :: Text)]))
        publish <- runHandlerIO (tuiPublishHandler st Nothing (object ["payload" .= ("ok" :: Text)]))
        select <- runHandlerIO (tuiSelectSessionHandler st Nothing (object ["sessionID" .= ("ok" :: Text)]))
        controlNext <- runHandlerIO (tuiControlHandler st "next" Nothing (object ["ok" .= True]))
        controlResponse <- runHandlerIO (tuiControlHandler st "response" Nothing (object ["ok" .= True]))
        lastVal <- TuiStore.getLast (stStorage st)
        pure
            TuiHandlersResult
                { thrAppended = appended
                , thrPrompt = prompt
                , thrSubmitted = submitted
                , thrCleared = cleared
                , thrOpenHelp = openHelp
                , thrOpenSessions = openSessions
                , thrOpenThemes = openThemes
                , thrOpenModels = openModels
                , thrExec = exec
                , thrToast = toast
                , thrPublish = publish
                , thrSelect = select
                , thrControlNext = controlNext
                , thrControlResponse = controlResponse
                , thrLast = lastVal
                }
    case result of
        TuiHandlersResult
            { thrAppended = Right True
            , thrSubmitted = Right True
            , thrCleared = Right True
            , thrOpenHelp = Right True
            , thrOpenSessions = Right True
            , thrOpenThemes = Right True
            , thrOpenModels = Right True
            , thrExec = Right True
            , thrToast = Right True
            , thrPublish = Right True
            , thrSelect = Right True
            , thrControlNext = Right True
            , thrControlResponse = Right True
            , thrLast = lastVal
            } -> do
                assert $ isJust lastVal
        _otherResult -> failure

prop_skillHandler :: Property
prop_skillHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> runHandlerIO (skillHandler st Nothing)
    case result of
        Left _err -> failure
        Right _result -> success

prop_formatterHandler :: Property
prop_formatterHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> runHandlerIO (formatterHandler st Nothing)
    case result of
        Left _err -> failure
        Right _result -> success

prop_experimentalWorktreeHandlers :: Property
prop_experimentalWorktreeHandlers = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> do
        get1 <- runHandlerIO (experimentalWorktreeGetHandler st Nothing)
        let input = object ["root" .= ("test" :: Text), "ready" .= True]
        set1 <- runHandlerIO (experimentalWorktreePostHandler st input)
        reset1 <- runHandlerIO (experimentalWorktreeResetHandler st Nothing)
        pure
            ExperimentalWorktreeResult
                { ewrGet = get1
                , ewrSet = set1
                , ewrReset = reset1
                , ewrRoot = stDirectory st
                }
    case result of
        ExperimentalWorktreeResult{ewrGet = Left _err} -> failure
        ExperimentalWorktreeResult{ewrSet = Left _err} -> failure
        ExperimentalWorktreeResult{ewrReset = Left _err} -> failure
        ExperimentalWorktreeResult{ewrGet = Right get1, ewrSet = Right set1, ewrReset = Right True} -> do
            get1 === ([] :: [Text])
            set1 === object ["root" .= ("test" :: Text), "ready" .= True]
        _otherResult -> failure

prop_fileListHandler :: Property
prop_fileListHandler = withTests 20 $ property $ do
    name <- forAll genName
    dir <- forAll (Gen.filter (/= name) genName)
    result <- evalIO $ withTmp $ \root -> do
        createDirectory (root </> T.unpack dir)
        TIO.writeFile (root </> T.unpack name) "ok"
        runHandlerIO (fileListHandler (Just (T.pack root)) ".")
    case result of
        Left _err -> failure
        Right nodes -> do
            assert $ any (\node -> fnName node == name) nodes
            assert $ any (\node -> fnName node == dir && fnType node == FileTypeDirectory) nodes

prop_fileReadHandler :: Property
prop_fileReadHandler = withTests 20 $ property $ do
    name <- forAll genName
    content <- forAll genText
    result <- evalIO $ withTmp $ \root -> do
        TIO.writeFile (root </> T.unpack name) content
        runHandlerIO (fileReadHandler (Just (T.pack root)) name)
    case result of
        Left _err -> failure
        Right file -> do
            fcType file === ContentTypeText
            fcContent file === content

prop_fileReadHandlerBinary :: Property
prop_fileReadHandlerBinary = withTests 20 $ property $ do
    name <- forAll genName
    let bytes = BS.pack [0, 1, 2, 255]
    result <- evalIO $ withTmp $ \root -> do
        BS.writeFile (root </> T.unpack name) bytes
        runHandlerIO (fileReadHandler (Just (T.pack root)) name)
    case result of
        Left _err -> failure
        Right file -> do
            fcType file === ContentTypeBinary
            let encoded = TE.decodeUtf8 (B64.encode bytes)
            fcContent file === encoded

prop_chatHandlerAnthropicMissing :: Property
prop_chatHandlerAnthropicMissing = withTests 20 $ property $ do
    msg <- forAll genText
    result <- evalIO $ withState $ \st ->
        withEnv "ANTHROPIC_API_KEY" Nothing $
            runHandlerIO (chatHandler st (ChatInput msg Nothing))
    case result of
        Left _err -> failure
        Right val -> lookupText "error" val === Just "No Anthropic API key configured. Set ANTHROPIC_API_KEY or add via provider auth."

prop_chatHandlerOpenRouterMissing :: Property
prop_chatHandlerOpenRouterMissing = withTests 20 $ property $ do
    msg <- forAll genText
    result <- evalIO $ withState $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $
            runHandlerIO (chatHandler st (ChatInput msg (Just "openrouter/test-model")))
    case result of
        Left _err -> failure
        Right val -> lookupText "error" val === Just "No OpenRouter API key configured. Set OPENROUTER_API_KEY or add via provider auth."

prop_sessionCommandHandler :: Property
prop_sessionCommandHandler = withTests 20 $ property $ do
    txt <- forAll genName
    result <- evalIO $ withIgnoreSignals $ withState $ \st -> do
        var <- newEmptyTMVarIO
        _ <- Bus.subscribe (stBus st) "command.executed" $ \event ->
            atomically $ void $ tryPutTMVar var event
        let input =
                object
                    [ "command" .= ("echo " <> txt)
                    , "description" .= ("test" :: Text)
                    , "timeout" .= (2000 :: Int)
                    ]
        res <- runHandlerIO (sessionCommandHandler st "session" input)
        evt <- waitVar 1000000 var
        pure (res, evt)
    case result of
        (Left _err, _) -> failure
        (Right val, evt) -> do
            -- Response structure is { "info": AssistantMessage, "parts": [Part] }
            assert $ hasKey "info" val
            assert $ hasKey "parts" val
            -- Check output is in parts array - first part should have "text" field
            case lookupArray "parts" val of
                Nothing -> failure
                Just parts -> case parts of
                    [] -> failure
                    (p : _) -> case lookupText "text" p of
                        Nothing -> failure
                        Just out -> assert $ txt `T.isInfixOf` out
            assert $ isJust evt

prop_sessionShellHandler :: Property
prop_sessionShellHandler = withTests 10 $ property $ do
    result <- evalIO $ withIgnoreSignals $ withState $ \st -> do
        var <- newEmptyTMVarIO
        _ <- Bus.subscribe (stBus st) "pty.created" $ \event ->
            atomically $ void $ tryPutTMVar var event
        -- Use 'sleep' instead of '/bin/sh' to avoid signal propagation issues
        -- when shells set up their own signal handlers
        let input =
                object
                    [ "command" .= ("sleep" :: Text)
                    , "args" .= (["infinity"] :: [Text])
                    , "sandbox" .= False
                    ]
        res <- runHandlerIO (sessionShellHandler st "session" input)
        -- Short wait - event should arrive immediately since publish is synchronous
        evt <- waitVar 100000 var
        case res of
            Left _err -> pure (res, evt)
            Right val -> do
                case lookupText "id" val of
                    Nothing -> pure (res, evt)
                    Just pid -> do
                        _ <- Pty.remove (stPtyManager st) pid
                        pure (res, evt)
    case result of
        (Left _err, _) -> failure
        (Right val, evt) -> do
            let pid = lookupText "id" val
            -- PTY creation must succeed - require pid to be present
            case pid of
                Nothing -> failure
                Just _event -> assert $ isJust evt

prop_promptAsyncIndex :: Property
prop_promptAsyncIndex = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> do
        let parts = [object ["type" .= ("text" :: Text), "text" .= ("hi" :: Text)]]
        let input = CreateMessageInput Nothing parts Nothing Nothing
        res <- runHandlerIO (sessionPromptAsyncHandler st "session" input)
        case res of
            Left _err -> pure (res, False)
            Right val -> do
                let reqId = lookupText "requestID" val
                ids <- Storage.read (stStorage st) (PromptAsync.promptAsyncIndexKey "session") :: IO [Text]
                pure (res, maybe False (`elem` ids) reqId)
    case result of
        (Left _err, _) -> failure
        (Right _result, ok) -> assert ok

prop_ptyHandlersLifecycle :: Property
prop_ptyHandlersLifecycle = withTests 20 $ property $ do
    result <- evalIO $ withIgnoreSignals $ withState $ \st -> do
        -- Use 'sleep' instead of '/bin/sh' to avoid signal propagation issues
        let input = object ["command" .= ("sleep" :: Text), "args" .= (["infinity"] :: [Text]), "sandbox" .= False]
        created <- runHandlerIO (ptyCreateHandler st input)
        case created of
            Left _err -> pure (PtyLifecycleResult created Nothing)
            Right val -> do
                let pid = lookupText "id" val
                case pid of
                    Nothing -> pure (PtyLifecycleResult created Nothing)
                    Just ptyId -> do
                        listed <- runHandlerIO (ptyListHandler st)
                        fetched <- runHandlerIO (ptyGetHandler st ptyId)
                        updated <- runHandlerIO (ptyUpdateHandler st ptyId (object ["title" .= ("Test" :: Text)]))
                        deleted <- runHandlerIO (ptyDeleteHandler st ptyId)
                        fetched2 <- runHandlerIO (ptyGetHandler st ptyId)
                        pure $
                            PtyLifecycleResult
                                created
                                ( Just
                                    PtyLifecycleDetails
                                        { pldId = ptyId
                                        , pldListed = listed
                                        , pldFetched = fetched
                                        , pldUpdated = updated
                                        , pldDeleted = deleted
                                        , pldFetched2 = fetched2
                                        }
                                )
    case result of
        PtyLifecycleResult{plCreated = Left _err} -> failure
        PtyLifecycleResult{plCreated = Right val, plDetails = Nothing} -> do
            assert $ isJust (lookupText "error" val)
        PtyLifecycleResult{plCreated = Right _result, plDetails = Just details} -> do
            case pldListed details of
                Left _err -> failure
                Right xs -> assert $ any (\v -> lookupText "id" v == Just (pldId details)) xs
            case pldFetched details of
                Left _err -> failure
                Right val -> lookupText "id" val === Just (pldId details)
            case pldUpdated details of
                Left _err -> failure
                Right val -> lookupText "title" val === Just "Test"
            case pldDeleted details of
                Left _err -> failure
                Right ok -> ok === True
            case pldFetched2 details of
                Left _err -> failure
                Right val -> assert $ isJust (lookupText "error" val)

prop_ptyHandlersUnsandboxedChanges :: Property
prop_ptyHandlersUnsandboxedChanges = withTests 20 $ property $ do
    result <- evalIO $ withIgnoreSignals $ withState $ \st -> do
        -- Use 'sleep' instead of '/bin/sh' to avoid signal propagation issues
        let input = object ["command" .= ("sleep" :: Text), "args" .= (["infinity"] :: [Text]), "sandbox" .= False]
        created <- runHandlerIO (ptyCreateHandler st input)
        case created of
            Left _err -> pure (created, Nothing, Nothing)
            Right val -> do
                let pid = lookupText "id" val
                case pid of
                    Nothing -> pure (created, Nothing, Nothing)
                    Just ptyId -> do
                        changes <- runHandlerIO (ptyChangesHandler st ptyId)
                        commit <- runHandlerIO (ptyCommitHandler st ptyId)
                        _ <- runHandlerIO (ptyDeleteHandler st ptyId)
                        pure (created, Just changes, Just commit)
    case result of
        (Left _err, _, _) -> failure
        (Right val, Nothing, _) -> do
            assert $ isJust (lookupText "error" val)
        (Right _result, Just changes, Just commit) -> do
            case changes of
                Left _err -> failure
                Right val -> assert $ isJust (lookupText "error" val)
            case commit of
                Left _err -> failure
                Right val -> assert $ isJust (lookupText "error" val)
        _otherResult -> failure

prop_ptyConnectHandler :: Property
prop_ptyConnectHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> do
        var <- newEmptyMVar
        let Tagged app = ptyConnectHandler st "missing"
        _ <- app defaultRequest $ \res -> do
            putMVar var res
            pure ResponseReceived
        res <- takeMVar var
        let (status, _headers, withBody) = responseToStream res
        box <- newEmptyMVar
        let send chunk = void $ tryPutMVar box chunk
        let flush = pure ()
        chunk <- withBody $ \body -> do
            tid <- forkIO $ body send flush
            part <- takeMVar box
            killThread tid
            pure part
        pure (status, chunk)
    case result of
        (status, chunk) -> do
            status === status400
            let text = TE.decodeUtf8 (LBS.toStrict (toLazyByteString chunk))
            assert $ "PTY not found" `T.isInfixOf` text

prop_findHandler :: Property
prop_findHandler = withTests 20 $ property $ do
    token <- forAll genName
    result <- evalIO $ withIgnoreSignals $ withState $ \st -> do
        let root = T.unpack (stDirectory st)
        let path = root </> "find.txt"
        TIO.writeFile path ("find " <> token)
        ( do
                vals <- runHandlerIO (findHandler st (Just token) Nothing Nothing)
                pure (T.pack path, vals, Nothing)
            )
            `catch` \(e :: SearchError) -> pure (T.pack path, Right [], Just e)
    case result of
        (_, Left _err, _) -> failure
        (_, Right _result, Just e) -> do
            annotate $ show e
            failure
        (path, Right vals, Nothing) -> do
            let matches = filter (matchFind token path) vals
            assert $ not (null matches)

prop_findFileHandler :: Property
prop_findFileHandler = withTests 20 $ property $ do
    name <- forAll genName
    result <- evalIO $ withIgnoreSignals $ withState $ \st -> do
        let root = T.unpack (stDirectory st)
        let path = root </> T.unpack name <> ".txt"
        TIO.writeFile path "file"
        ( do
                vals <- runHandlerIO (findFileHandler st (Just "*.txt") Nothing Nothing Nothing Nothing)
                pure (T.pack path, vals, Nothing)
            )
            `catch` \(e :: SearchError) -> pure (T.pack path, Right [], Just e)
    case result of
        (_, Left _err, _) -> failure
        (_, Right _result, Just e) -> do
            annotate $ show e
            failure
        (path, Right vals, Nothing) -> do
            let matches = filter (\v -> lookupText "path" v == Just path || hasSuffix ".txt" v) vals
            assert $ not (null matches)

prop_findSymbolHandler :: Property
prop_findSymbolHandler = withTests 20 $ property $ do
    token <- forAll genName
    result <- evalIO $ withIgnoreSignals $ withState $ \st -> do
        let root = T.unpack (stDirectory st)
        let path = root </> "symbol.txt"
        TIO.writeFile path ("symbol " <> token)
        ( do
                vals <- runHandlerIO (findSymbolHandler st (Just token) Nothing)
                pure (T.pack path, vals, Nothing)
            )
            `catch` \(e :: SearchError) -> pure (T.pack path, Right [], Just e)
    case result of
        (_, Left _err, _) -> failure
        (_, Right _result, Just e) -> do
            annotate $ show e
            failure
        (path, Right vals, Nothing) -> do
            let matches = filter (matchFind token path) vals
            assert $ not (null matches)

prop_vcsHandler :: Property
prop_vcsHandler = withTests 20 $ property $ do
    result <- evalIO $ withIgnoreSignals $ withState $ \st -> do
        exe <- findExecutable "git"
        case exe of
            Nothing -> do
                res <- runHandlerIO (vcsHandler st)
                pure (res, Nothing)
            Just _existingPid -> do
                let root = T.unpack (stDirectory st)
                (code, _, _) <- readProcessWithExitCode "git" ["-C", root, "init", "-b", "main"] ""
                case code of
                    ExitSuccess -> pure ()
                    ExitFailure _exitCode -> do
                        _ <- readProcessWithExitCode "git" ["-C", root, "init"] ""
                        pure ()
                res <- runHandlerIO (vcsHandler st)
                pure (res, Just ())
    case result of
        (Left _err, _) -> failure
        (Right info, Nothing) -> branch info === Nothing
        (Right info, Just _existingBranch) -> assert $ isJust (branch info)

prop_instanceDisposeHandler :: Property
prop_instanceDisposeHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> do
        var <- newEmptyTMVarIO
        _ <- Bus.subscribe (stBus st) "server.instance.disposed" $ \event ->
            atomically $ void $ tryPutTMVar var event
        res <- runHandlerIO (instanceDisposeHandler st)
        evt <- waitVar 1000000 var
        pure (res, evt)
    case result of
        (Left _err, _) -> failure
        (Right val, evt) -> do
            val === True
            assert $ isJust evt

prop_logHandler :: Property
prop_logHandler = withTests 20 $ property $ do
    msg <- forAll genText
    result <- evalIO $ withState $ \st ->
        runHandlerIO (logHandler st Nothing (object ["msg" .= msg]))
    case result of
        Left _err -> failure
        Right True -> success
        _otherResult -> failure

prop_globalEventHandler :: Property
prop_globalEventHandler = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> do
        var <- newEmptyMVar
        let Tagged app = globalEventHandler st
        _ <- app defaultRequest $ \res -> do
            putMVar var res
            pure ResponseReceived
        res <- takeMVar var
        let (status, headers, withBody) = responseToStream res
        queue <- newTQueueIO
        tid <- forkIO $ withBody $ \body -> do
            let record chunk = atomically $ writeTQueue queue chunk
            body record (pure ())
        first <- readSseEvent queue
        killThread tid
        pure (status, headers, first)
    case result of
        (status, headers, event) -> do
            status === status200
            let ctype = lookup hContentType headers
            ctype === Just "text/event-stream"
            assert $ "server.connected" `T.isInfixOf` event

prop_globalEventHandlerBusEvent :: Property
prop_globalEventHandlerBusEvent = withTests 20 $ property $ do
    result <- evalIO $ withState $ \st -> do
        var <- newEmptyMVar
        let Tagged app = globalEventHandler st
        _ <- app defaultRequest $ \res -> do
            putMVar var res
            pure ResponseReceived
        res <- takeMVar var
        let (status, _, withBody) = responseToStream res
        queue <- newTQueueIO
        tid <- forkIO $ withBody $ \body -> do
            let record chunk = atomically $ writeTQueue queue chunk
            body record (pure ())
        _ <- readSseEvent queue
        Bus.publish (stBus st) "test.event" (object ["ok" .= True])
        event <- readSseEvent queue
        killThread tid
        pure (status, event)
    case result of
        (status, event) -> do
            status === status200
            assert $ "\"type\":\"test.event\"" `T.isInfixOf` event

readSseEvent :: TQueue Builder -> IO Text
readSseEvent queue = go ""
  where
    go acc = do
        chunk <- atomically $ readTQueue queue
        let textChunk = TE.decodeUtf8 (LBS.toStrict (toLazyByteString chunk))
        let merged = acc <> textChunk
        if "\n\n" `T.isInfixOf` merged
            then pure merged
            else go merged

prop_experimentalToolIdsHandler :: Property
prop_experimentalToolIdsHandler = withTests 20 $ property $ do
    result <- evalIO $ runHandlerIO experimentalToolIdsHandler
    case result of
        Left _err -> failure
        Right ids -> assert $ "bash" `elem` ids

prop_experimentalToolHandler :: Property
prop_experimentalToolHandler = withTests 20 $ property $ do
    name <- forAll genName
    result <- evalIO $ withState $ \st -> do
        let input = object ["name" .= name, "payload" .= ("ok" :: Text)]
        res <- runHandlerIO (experimentalToolHandler st input)
        stored <- Storage.read (stStorage st) ["experimental-tool", name] :: IO Value
        pure (res, stored)
    case result of
        (Left _err, _) -> failure
        (Right val, stored) -> val === stored

prop_authCreateHandler :: Property
prop_authCreateHandler = withTests 20 $ property $ do
    pid <- forAll genName
    token <- forAll genName
    result <- evalIO $ withState $ \st -> do
        let input = object ["token" .= token]
        res <- runHandlerIO (authCreateHandler st pid input)
        stored <- Storage.read (stStorage st) ["auth", pid] :: IO Value
        pure (res, stored)
    case result of
        (Left _err, _) -> failure
        (Right val, stored) -> do
            val === True
            lookupText "token" stored === Just token

prop_authUpdateHandler :: Property
prop_authUpdateHandler = withTests 20 $ property $ do
    pid <- forAll genName
    tok1 <- forAll genName
    tok2 <- forAll genName
    result <- evalIO $ withState $ \st -> do
        let inputA = object ["token" .= tok1]
        let inputB = object ["token" .= tok2]
        _ <- runHandlerIO (authCreateHandler st pid inputA)
        res <- runHandlerIO (authUpdateHandler st pid inputB)
        stored <- Storage.read (stStorage st) ["auth", pid] :: IO Value
        pure (res, stored)
    case result of
        (Left _err, _) -> failure
        (Right val, stored) -> do
            val === True
            lookupText "token" stored === Just tok2

prop_authDeleteHandler :: Property
prop_authDeleteHandler = withTests 20 $ property $ do
    pid <- forAll genName
    token <- forAll genName
    result <- evalIO $ withState $ \st -> do
        let input = object ["token" .= token]
        _ <- runHandlerIO (authCreateHandler st pid input)
        res <- runHandlerIO (authDeleteHandler st pid)
        keys <- Storage.list (stStorage st) ["auth"]
        let removed = not $ any (\key -> key == ["auth", pid]) keys
        pure (res, removed)
    case result of
        (Left _err, _) -> failure
        (Right val, removed) -> do
            val === True -- delete always returns true
            assert removed

listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0

tests :: TestTree
tests =
    testGroup
        "Handler Property Tests"
        [ testProperty "health handler" prop_healthHandler
        , testProperty "path handler" prop_pathHandler
        , testProperty "global config handler" prop_globalConfigHandler
        , testProperty "project handlers" prop_projectHandlers
        , testProperty "project update handler" prop_projectUpdateHandler
        , testProperty "provider list handler" prop_providerListHandler
        , testProperty "provider auth handler" prop_providerAuthHandler
        , testProperty "provider handler" prop_providerHandler
        , testProperty "provider oauth handlers" prop_providerOauthHandlers
        , testProperty "config handler" prop_configHandler
        , testProperty "command handler" prop_commandHandler
        , testProperty "agent handler" prop_agentHandler
        , testProperty "session status handler" prop_sessionStatusHandler
        , testProperty "session lifecycle handler" prop_sessionLifecycleHandler
        , testProperty "session children handler" prop_sessionChildrenHandler
        , testProperty "session todo handler" prop_sessionTodoHandler
        , testProperty "session init handler" prop_sessionInitHandler
        , testProperty "session fork handler" prop_sessionForkHandler
        , testProperty "session abort handler" prop_sessionAbortHandler
        , testProperty "session share handlers" prop_sessionShareHandlers
        , testProperty "session diff handler" prop_sessionDiffHandler
        , testProperty "session summarize handler" prop_sessionSummarizeHandler
        , testProperty "session revert handlers" prop_sessionRevertHandlers
        , testProperty "session permission handler" prop_sessionPermissionHandler
        , testProperty "session message handlers" prop_sessionMessageHandlers
        , testProperty "session message create publishes events" prop_sessionMessageCreatePublishesEvents
        , testProperty "message events forwarded to eventChan" prop_messageEventsForwardedToEventChan
        , testProperty "session.status events published" prop_sessionStatusEventsPublished
        , testProperty "session message part handlers" prop_sessionMessagePartHandlers
        , testProperty "lsp handler" prop_lspHandler
        , testProperty "permission handlers" prop_permissionHandlers
        , testProperty "question handlers" prop_questionHandlers
        , testProperty "file status handler" prop_fileStatusHandler
        , testProperty "tui handlers" prop_tuiHandlers
        , testProperty "skill handler" prop_skillHandler
        , testProperty "formatter handler" prop_formatterHandler
        , testProperty "experimental worktree handlers" prop_experimentalWorktreeHandlers
        , testProperty "file list handler" prop_fileListHandler
        , testProperty "file read handler" prop_fileReadHandler
        , testProperty "file read handler binary" prop_fileReadHandlerBinary
        , testProperty "chat handler anthropic missing" prop_chatHandlerAnthropicMissing
        , testProperty "chat handler openrouter missing" prop_chatHandlerOpenRouterMissing
        , testProperty "prompt async index" prop_promptAsyncIndex
        , testProperty "pty connect handler" prop_ptyConnectHandler
        , testProperty "instance dispose handler" prop_instanceDisposeHandler
        , testProperty "log handler" prop_logHandler
        , testProperty "global event handler" prop_globalEventHandler
        , testProperty "global event handler bus event" prop_globalEventHandlerBusEvent
        , testProperty "experimental tool ids handler" prop_experimentalToolIdsHandler
        , testProperty "experimental tool handler" prop_experimentalToolHandler
        , testProperty "auth create handler" prop_authCreateHandler
        , testProperty "auth update handler" prop_authUpdateHandler
        , testProperty "auth delete handler" prop_authDeleteHandler
        , -- Tests that spawn subprocesses must run with limited parallelism to avoid
          -- signal propagation issues. Use localOption to limit to 1 thread for this group.
          localOption (NumThreads 1) $
            sequentialTestGroup
                "Subprocess Tests"
                AllFinish
                [ testProperty "session command handler" prop_sessionCommandHandler
                , testProperty "session shell handler" prop_sessionShellHandler
                , testProperty "pty handler lifecycle" prop_ptyHandlersLifecycle
                , testProperty "pty handler unsandboxed changes" prop_ptyHandlersUnsandboxedChanges
                , testProperty "find handler" prop_findHandler
                , testProperty "find file handler" prop_findFileHandler
                , testProperty "find symbol handler" prop_findSymbolHandler
                , testProperty "vcs handler" prop_vcsHandler
                ]
        ]
