{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Property.HandlerProps (tests) where

import Api
import Bus.Bus qualified as Bus
import Config.Dhall qualified as Dhall
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (forM_, void)
import Data.Aeson (Value (..), object, toJSON, (.=))

import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO

import Formatter.Status qualified as Formatter
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

import Servant (NoContent (..), Tagged (..))
import Servant.Server (ServerError (..))
import State
import Storage.Storage qualified as Storage
import System.Directory (createDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)

import System.FilePath ((</>))

import Test.Fixture (withTempDir)
import Test.Helpers (genName, genText, isObject, listLength, lookupText, runHandlerIO, waitForCount, waitForLength, waitVar)
import Test.Tasty
import Test.Tasty.Hedgehog

import Tui.Store qualified as TuiStore
import Util.Identifier qualified as Identifier
import Util.StorageKeys (todoKey)
import Vcs.Diff (VcsError)

data ProjectHandlersResult = ProjectHandlersResult
    { phrListed :: !(Either ServerError [Project])
    , phrCurrent :: !(Either ServerError Project)
    , phrFetched :: !(Either ServerError Project)
    , phrDir :: !Text
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

-- | Type alias for property constructors that need DhallCache and ExeCache
type CachedProperty = Dhall.DhallCache -> Formatter.ExeCache -> Property

-- | Create test state with provided DhallCache and ExeCache
withStateWith :: Dhall.DhallCache -> Formatter.ExeCache -> (AppState -> IO a) -> IO a
withStateWith dhallCache exeCache action =
    withTmp $ \dir ->
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

setVar :: String -> Maybe String -> IO ()
setVar key val = case val of
    Nothing -> unsetEnv key
    Just v -> setEnv key v

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv key val action =
    bracket (lookupEnv key) (setVar key) (\_previous -> setVar key val >> action)

prop_healthHandler :: CachedProperty
prop_healthHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
        res <- runHandlerIO (healthHandler st)
        pure (res, stVersion st)
    case result of
        (Left _err, _) -> failure
        (Right health, ver) -> do
            healthy health === True
            version health === ver

prop_pathHandler :: CachedProperty
prop_pathHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
        res <- runHandlerIO (pathHandler st)
        pure (res, stDirectory st)
    case result of
        (Left _err, _) -> failure
        (Right info, dir) -> do
            let PathInfo{worktree = wt, state = stPath} = info
            wt === dir
            assert $ T.isSuffixOf ".opencode/state" stPath

prop_globalConfigHandler :: CachedProperty
prop_globalConfigHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> runHandlerIO (globalConfigHandler st)
    case result of
        Left _err -> failure
        Right val -> assert $ isObject val

prop_projectHandlers :: CachedProperty
prop_projectHandlers dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_projectUpdateHandler :: CachedProperty
prop_projectUpdateHandler dhallCache exeCache = property $ do
    newName <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_providerListHandler :: CachedProperty
prop_providerListHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> runHandlerIO (providerListHandler st Nothing)
    case result of
        Left _err -> failure
        Right pl -> assert $ isObject (cplDefault pl)

prop_providerAuthHandler :: CachedProperty
prop_providerAuthHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> runHandlerIO (providerAuthHandler st)
    case result of
        Left _err -> failure
        Right val -> assert $ isObject val

prop_providerHandler :: CachedProperty
prop_providerHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> runHandlerIO (providerHandler st Nothing)
    case result of
        Left _err -> failure
        Right providerList -> do
            -- Should return a ProviderList with default provider info
            assert $ isObject (plDefault providerList)

prop_providerOauthHandlers :: CachedProperty
prop_providerOauthHandlers dhallCache exeCache = property $ do
    pid <- forAll genName
    code <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
        let input = object ["redirect" .= ("http://localhost" :: Text), "scopes" .= ["read" :: Text]]
        auth <- runHandlerIO (providerOauthAuthorizeHandler st pid input)
        callback <- case auth of
            Left _err -> pure (Left _err)
            Right _val -> do
                -- Callback expects: { method: number, code?: string }
                let payload = object ["method" .= (0 :: Int), "code" .= code]
                runHandlerIO (providerOauthCallbackHandler st pid Nothing payload)
        pure (auth, callback)
    case result of
        (Left _err, _) -> failure
        (_, Left _err) -> failure
        (Right auth, Right callbackResult) -> do
            lookupText "providerID" auth === Just pid
            assert $ isJust (lookupText "state" auth)
            -- Callback returns Bool
            callbackResult === True

prop_configHandler :: CachedProperty
prop_configHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> runHandlerIO (configHandler st)
    case result of
        Left _err -> failure
        Right val -> assert $ isObject val

prop_commandHandler :: CachedProperty
prop_commandHandler _dhallCache _exeCache = property $ do
    result <- evalIO $ runHandlerIO commandHandler
    case result of
        Left _err -> failure
        Right defs -> do
            let names = mapMaybe (lookupText "name") defs
            assert $ "bash" `elem` names

prop_agentHandler :: CachedProperty
prop_agentHandler _dhallCache _exeCache = property $ do
    result <- evalIO $ runHandlerIO agentHandler
    case result of
        Left _err -> failure
        Right agents -> do
            -- Should return a list of agent definitions
            assert $ not (null agents)

prop_sessionStatusHandler :: CachedProperty
prop_sessionStatusHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> runHandlerIO (sessionStatusHandler st Nothing)
    case result of
        Left _err -> failure
        Right val -> do
            -- Returns an empty map (object {}) when no sessions are active
            -- The map has type Map<SessionID, SessionStatus>
            assert $ isObject val

prop_sessionLifecycleHandler :: CachedProperty
prop_sessionLifecycleHandler dhallCache exeCache = property $ do
    title <- forAll genText
    next <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_sessionChildrenHandler :: CachedProperty
prop_sessionChildrenHandler dhallCache exeCache = property $ do
    parentTitle <- forAll genText
    childTitle <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_sessionTodoHandler :: CachedProperty
prop_sessionTodoHandler dhallCache exeCache = property $ do
    sid <- forAll genName
    item <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
        let todos = [object ["text" .= item]]
        Storage.write (stStorage st) (todoKey sid) todos
        runHandlerIO (sessionTodoHandler st sid)
    case result of
        Left _err -> failure
        Right todos -> todos === [object ["text" .= item]]

prop_sessionInitHandler :: CachedProperty
prop_sessionInitHandler dhallCache exeCache = property $ do
    sid <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_sessionForkHandler :: CachedProperty
prop_sessionForkHandler dhallCache exeCache = property $ do
    title <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_sessionAbortHandler :: CachedProperty
prop_sessionAbortHandler dhallCache exeCache = property $ do
    sid <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
        var <- newEmptyTMVarIO
        _ <- Bus.subscribe (stBus st) "session.error" $ \event ->
            atomically $ void $ tryPutTMVar var event
        res <- runHandlerIO (sessionAbortHandler st sid Nothing)
        evt <- waitVar 1000000 var
        pure (res, evt)
    case result of
        (Left _err, _) -> failure
        -- Handler returns wasRunning (True if agent was killed, False if no agent running)
        -- Either way, the session.error event should be published
        (Right _wasRunning, evt) -> do
            assert $ isJust evt

prop_sessionShareHandlers :: CachedProperty
prop_sessionShareHandlers dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_sessionDiffHandler :: CachedProperty
prop_sessionDiffHandler dhallCache exeCache = property $ do
    sid <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
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

prop_sessionSummarizeHandler :: CachedProperty
prop_sessionSummarizeHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
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

prop_sessionRevertHandlers :: CachedProperty
prop_sessionRevertHandlers dhallCache exeCache = property $ do
    mid <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_sessionPermissionHandler :: CachedProperty
prop_sessionPermissionHandler dhallCache exeCache = property $ do
    sid <- forAll genName
    pid <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_sessionMessageHandlers :: CachedProperty
prop_sessionMessageHandlers dhallCache exeCache = property $ do
    msg <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            -- Track session.status events to know when background work is done
            statusVar <- newTVarIO (0 :: Int)
            unsubStatus <- Bus.subscribe (stBus st) "session.status" $ \_event ->
                atomically $ modifyTVar' statusVar (+ 1)
            let parts = [object ["id" .= ("part_1" :: Text), "type" .= ("text" :: Text), "text" .= msg]]
            let input = CreateMessageInput Nothing parts Nothing Nothing
            created <- runHandlerIO (sessionMessageCreateHandler st "session" input)
            -- Wait for at least 2 session.status events (busy + idle)
            _ <- waitForCount 2000000 statusVar 2
            unsubStatus
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
prop_sessionMessageCreatePublishesEvents :: CachedProperty
prop_sessionMessageCreatePublishesEvents dhallCache exeCache = property $ do
    msg <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            -- Read events from eventChan directly (single-hop, same as SSE delivery)
            reader <- atomically $ dupTChan (stEventChan st)

            let parts = [object ["id" .= ("part_1" :: Text), "type" .= ("text" :: Text), "text" .= msg]]
            let input = CreateMessageInput (Just "msg_test") parts Nothing Nothing
            _ <- runHandlerIO (sessionMessageCreateHandler st "session_test" input)

            -- Read events until we see 2 session.status events (busy + idle)
            let readUntilDone statusSeen acc = do
                    gate <- registerDelay 2000000
                    mval <-
                        atomically $
                            (Just <$> readTChan reader)
                                `orElse` do
                                    done <- readTVar gate
                                    if done then pure Nothing else retry
                    case mval of
                        Nothing -> pure acc
                        Just val ->
                            let newSeen = if isSessionStatus val then statusSeen + 1 else statusSeen
                             in if newSeen >= (2 :: Int)
                                    then pure (val : acc)
                                    else readUntilDone newSeen (val : acc)
            events <- readUntilDone (0 :: Int) []

            let msgEvents = filter isMessageUpdated events
            let partEvents = filter isPartUpdated events
            pure (msgEvents, partEvents)
    case result of
        (msgEvents, partEvents) -> do
            -- Should have at least 2 message.updated events (user + assistant)
            annotate $ "message.updated events: " ++ show (listLength msgEvents)
            assert $ listLength msgEvents >= 2
            -- Should have at least 1 message.part.updated event (user's text part)
            annotate $ "message.part.updated events: " ++ show (listLength partEvents)
            assert $ listLength partEvents >= 1
  where
    isSessionStatus val = case val of
        Object obj -> case KM.lookup "type" obj of
            Just (String "session.status") -> True
            _other -> False
        _other -> False
    isMessageUpdated val = case val of
        Object obj -> case KM.lookup "type" obj of
            Just (String "message.updated") -> True
            _other -> False
        _other -> False
    isPartUpdated val = case val of
        Object obj -> case KM.lookup "type" obj of
            Just (String "message.part.updated") -> True
            _other -> False
        _other -> False

{- | Property: Messages are returned in correct order (user before assistant).
The TUI uses binary search to insert messages by ID, which assumes messages
are sorted by ID in ascending order. When a user sends a message, the user
message must appear before the assistant response in the list.
-}
prop_sessionMessagesOrderedCorrectly :: CachedProperty
prop_sessionMessagesOrderedCorrectly dhallCache exeCache = property $ do
    msg <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            -- Track session.status events to know when background work is done
            statusVar <- newTVarIO (0 :: Int)
            unsubStatus <- Bus.subscribe (stBus st) "session.status" $ \_event ->
                atomically $ modifyTVar' statusVar (+ 1)
            let parts = [object ["type" .= ("text" :: Text), "text" .= msg]]
            let input = CreateMessageInput Nothing parts Nothing Nothing
            _ <- runHandlerIO (sessionMessageCreateHandler st "session_order_test" input)
            -- Wait for background work to complete (busy + idle)
            _ <- waitForCount 2000000 statusVar 2
            unsubStatus
            runHandlerIO (sessionMessageListHandler st "session_order_test" Nothing)
    case result of
        Left _err -> failure
        Right msgs -> do
            -- Should have at least 2 messages (user + assistant)
            annotate $ "Number of messages: " ++ show (listLength msgs)
            assert $ listLength msgs >= 2

            -- Verify user message comes before assistant message
            let roles = map (messageInfoRole . msgInfo) msgs
            annotate $ "Message roles: " ++ show roles

            -- Find first user and first assistant
            let firstUserIdx = List.elemIndex "user" roles
            let firstAssistantIdx = List.elemIndex "assistant" roles

            case (firstUserIdx, firstAssistantIdx) of
                (Just uIdx, Just aIdx) -> do
                    annotate $ "User index: " ++ show uIdx ++ ", Assistant index: " ++ show aIdx
                    -- User message must come before assistant message
                    assert $ uIdx < aIdx
                (Nothing, _) -> failure
                (_, Nothing) -> failure

{- | Property: After cancelling a message and sending a new one, messages remain correctly ordered.
When a user:
1. Sends a message (user1 + assistant1)
2. Cancels the assistant response
3. Sends another message (user2 + assistant2)

The messages should be ordered: user1 < assistant1 < user2 < assistant2
This tests that cancellation doesn't break the timestamp-based ordering.
-}
prop_messagesOrderedAfterCancel :: CachedProperty
prop_messagesOrderedAfterCancel dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
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
        Left _err -> failure
        Right msgs -> do
            -- Should have 4 messages: user1, assistant1, user2, assistant2
            annotate $ "Number of messages: " ++ show (listLength msgs)
            assert $ listLength msgs >= 4

            -- Extract roles, IDs, and timestamps for debugging
            let roles = map (messageInfoRole . msgInfo) msgs
            let ids = map (messageInfoId . msgInfo) msgs
            let times = map (messageInfoCreatedTime . msgInfo) msgs
            annotate $ "Message roles in order: " ++ show roles
            annotate $ "Message IDs in order: " ++ show ids
            annotate $ "Message created times: " ++ show times

            -- Find all user and assistant indices
            let userIndices = List.elemIndices "user" roles
            let assistantIndices = List.elemIndices "assistant" roles

            annotate $ "User indices: " ++ show userIndices
            annotate $ "Assistant indices: " ++ show assistantIndices

            -- Should have at least 2 users and 2 assistants
            assert $ listLength userIndices >= 2
            assert $ listLength assistantIndices >= 2

            -- Key property: Each user message should come before its corresponding assistant
            -- user1 < assistant1, user2 < assistant2
            -- More specifically: roles should follow pattern [user, assistant, user, assistant, ...]
            let pairs = zip userIndices assistantIndices
            forM_ pairs $ \(uIdx, aIdx) -> do
                annotate $ "Checking user@" ++ show uIdx ++ " < assistant@" ++ show aIdx
                assert $ uIdx < aIdx

            -- Also verify strict ordering: user1 < assistant1 < user2 < assistant2
            case (userIndices, assistantIndices) of
                (u1 : u2 : _restU, a1 : a2 : _restA) -> do
                    annotate $ "Strict order check: u1=" ++ show u1 ++ " a1=" ++ show a1 ++ " u2=" ++ show u2 ++ " a2=" ++ show a2
                    assert $ u1 < a1
                    assert $ a1 < u2
                    assert $ u2 < a2
                ([], _anyA) -> failure
                ([_singleU], _anyA) -> failure
                (_anyU, []) -> failure
                (_anyU, [_singleA]) -> failure

{- | Property: When user and assistant messages have the same timestamp,
user message should come before assistant message (role priority sorting).
This is the core property that ensures correct message ordering.
-}
prop_sameTimestampUserBeforeAssistant :: CachedProperty
prop_sameTimestampUserBeforeAssistant dhallCache exeCache = property $ do
    -- Generate unique session ID to avoid cleanup conflicts
    sessionSuffix <- forAll $ Gen.text (Range.linear 8 12) Gen.alphaNum
    let sessionId = "session_same_time_" <> sessionSuffix

    -- Send a message (creates user + assistant with same timestamp)
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            -- Track session.status events
            statusVar <- newTVarIO (0 :: Int)
            unsubStatus <- Bus.subscribe (stBus st) "session.status" $ \_event ->
                atomically $ modifyTVar' statusVar (+ 1)
            let parts = [object ["type" .= ("text" :: Text), "text" .= ("test" :: Text)]]
            let input = CreateMessageInput Nothing parts Nothing Nothing
            _ <- runHandlerIO (sessionMessageCreateHandler st sessionId input)
            -- Wait for background work to complete (busy + idle)
            _ <- waitForCount 2000000 statusVar 2
            unsubStatus
            -- Get messages
            runHandlerIO (sessionMessageListHandler st sessionId Nothing)

    case result of
        Left _err -> failure
        Right msgs -> do
            annotate $ "Number of messages: " ++ show (listLength msgs)
            assert $ listLength msgs >= 2

            -- Get the first user and first assistant
            let roles = map (messageInfoRole . msgInfo) msgs
            let times = map (messageInfoCreatedTime . msgInfo) msgs
            let ids = map (messageInfoId . msgInfo) msgs

            annotate $ "Roles: " ++ show roles
            annotate $ "Times: " ++ show times
            annotate $ "IDs: " ++ show ids

            -- First two messages should be user then assistant
            case roles of
                ("user" : "assistant" : _rest) -> do
                    -- Verify they have the same timestamp
                    case (times, ids) of
                        (t1 : t2 : _restTimes, id1 : id2 : _restIds) -> do
                            annotate $ "User time: " ++ show t1 ++ ", Assistant time: " ++ show t2
                            annotate $ "User ID: " ++ T.unpack id1 ++ ", Assistant ID: " ++ T.unpack id2
                            annotate $ "User ID < Assistant ID: " ++ show (id1 < id2)
                            -- They should have the same created time
                            t1 === t2
                            -- And user ID should be less than assistant ID
                            assert $ id1 < id2
                        ([], _anyIds) -> failure
                        ([_singleTime], _anyIds) -> failure
                        (_anyTimes, []) -> failure
                        (_anyTimes, [_singleId]) -> failure
                [] -> do
                    annotate $ "Expected [user, assistant, ...] but got: " ++ show roles
                    failure
                [_single] -> do
                    annotate $ "Expected [user, assistant, ...] but got: " ++ show roles
                    failure
                (_other : _rest) -> do
                    annotate $ "Expected [user, assistant, ...] but got: " ++ show roles
                    failure

{- | Property: Message events are forwarded from bus to eventChan (SSE delivery path).
This verifies the State.hs subscription that forwards bus events to eventChan,
which is what SSE handlers read from.
-}
prop_messageEventsForwardedToEventChan :: CachedProperty
prop_messageEventsForwardedToEventChan dhallCache exeCache = property $ do
    msg <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            -- Create a reader from eventChan BEFORE creating message
            reader <- atomically $ dupTChan (stEventChan st)

            -- Create a message
            let parts = [object ["id" .= ("part_1" :: Text), "type" .= ("text" :: Text), "text" .= msg]]
            let input = CreateMessageInput (Just "msg_sse") parts Nothing Nothing
            _ <- runHandlerIO (sessionMessageCreateHandler st "session_sse" input)

            -- Read events from the TChan until we see 2 session.status events
            -- (busy + idle means background work is complete). Single-hop:
            -- Bus → subscribeAll in withStateWith → eventChan → reader.
            let readUntilDone statusSeen acc = do
                    gate <- registerDelay 2000000
                    mval <-
                        atomically $
                            (Just <$> readTChan reader)
                                `orElse` do
                                    done <- readTVar gate
                                    if done then pure Nothing else retry
                    case mval of
                        Nothing -> pure acc -- timeout
                        Just val ->
                            let newSeen = if isSessionStatus val then statusSeen + 1 else statusSeen
                             in if newSeen >= (2 :: Int)
                                    then pure (val : acc) -- done
                                    else readUntilDone newSeen (val : acc)
            events <- readUntilDone (0 :: Int) []
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
            _other -> False
        _other -> False
    isSessionStatus val = case val of
        Object obj -> case KM.lookup "type" obj of
            Just (String "session.status") -> True
            _other -> False
        _other -> False

{- | Property: Message creation publishes session.status events.
This is critical for TUI Ctrl+C support - the TUI needs to know when
a session is busy (processing) vs idle (done).
-}
prop_sessionStatusEventsPublished :: CachedProperty
prop_sessionStatusEventsPublished dhallCache exeCache = property $ do
    msg <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            -- Collect session.status events
            statusEvents <- newTVarIO ([] :: [Bus.BusEvent])
            unsubscribe <- Bus.subscribe (stBus st) "session.status" $ \event ->
                atomically $ modifyTVar' statusEvents (event :)

            let parts = [object ["id" .= ("part_1" :: Text), "type" .= ("text" :: Text), "text" .= msg]]
            let input = CreateMessageInput (Just "msg_status") parts Nothing Nothing
            _ <- runHandlerIO (sessionMessageCreateHandler st "session_status_test" input)

            -- Wait for at least 2 status events (busy + idle) via the same var
            _ <- waitForLength 2000000 statusEvents 2
            unsubscribe

            readTVarIO statusEvents
    case result of
        events -> do
            -- Should have at least 2 session.status events (busy at start, idle at end)
            annotate $ "session.status events: " ++ show (listLength events)
            assert $ listLength events >= 2

prop_sessionMessagePartHandlers :: CachedProperty
prop_sessionMessagePartHandlers dhallCache exeCache = property $ do
    txt <- forAll genText
    next <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $ do
            -- Track session.status events to know when background work is done
            statusVar <- newTVarIO (0 :: Int)
            unsubStatus <- Bus.subscribe (stBus st) "session.status" $ \_event ->
                atomically $ modifyTVar' statusVar (+ 1)
            let pid = "part_1" :: Text
            let parts = [object ["id" .= pid, "type" .= ("text" :: Text), "text" .= txt]]
            let input = CreateMessageInput Nothing parts Nothing Nothing
            _ <- runHandlerIO (sessionMessageCreateHandler st "session" input)
            -- Wait for background work to complete (busy + idle)
            _ <- waitForCount 2000000 statusVar 2
            unsubStatus
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

prop_lspHandler :: CachedProperty
prop_lspHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> runHandlerIO (lspHandler st)
    case result of
        Left _err -> failure
        Right lspInfo -> do
            -- Returns LSP server info (list of configured LSP servers)
            -- Empty list is valid when no LSP servers are configured
            assert $ listLength lspInfo >= 0

prop_permissionHandlers :: CachedProperty
prop_permissionHandlers dhallCache exeCache = property $ do
    rid <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
        let payload = object ["ok" .= True]
        _ <- runHandlerIO (permissionReplyHandler st rid Nothing payload)
        runHandlerIO (permissionHandler st Nothing)
    case result of
        Left _err -> failure
        Right vals -> do
            let hits = filter (\v -> lookupText "requestID" v == Just rid) vals
            assert $ not (null hits)

prop_questionHandlers :: CachedProperty
prop_questionHandlers dhallCache exeCache = property $ do
    rid <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
        let payload = object ["ok" .= True]
        _ <- runHandlerIO (questionReplyHandler st rid Nothing payload)
        _ <- runHandlerIO (questionRejectHandler st (rid <> "_r") Nothing payload)
        runHandlerIO (questionHandler st Nothing)
    case result of
        Left _err -> failure
        Right vals -> do
            let hits = filter (\v -> lookupText "requestID" v == Just rid) vals
            assert $ not (null hits)

prop_fileStatusHandler :: CachedProperty
prop_fileStatusHandler dhallCache exeCache = property $ do
    name <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
        let root = T.unpack (stDirectory st)
        let path = root </> T.unpack name
        TIO.writeFile path "ok"
        runHandlerIO (fileStatusHandler st (Just (stDirectory st)) (Just name))
    case result of
        Left _err -> failure
        Right vals -> do
            let matches = filter (\v -> lookupText "path" v == Just name) vals
            assert $ not (null matches)

prop_tuiHandlers :: CachedProperty
prop_tuiHandlers dhallCache exeCache = property $ do
    txt <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_skillHandler :: CachedProperty
prop_skillHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> runHandlerIO (skillHandler st Nothing)
    case result of
        Left _err -> failure
        Right skills -> do
            -- Returns a list of skills (may be empty if none configured)
            assert $ listLength skills >= 0

prop_formatterHandler :: CachedProperty
prop_formatterHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> runHandlerIO (formatterHandler st Nothing)
    case result of
        Left _err -> failure
        Right formatters -> do
            -- Returns a list of formatters (at least base formatters should exist)
            assert $ not (null formatters)

prop_experimentalWorktreeHandlers :: CachedProperty
prop_experimentalWorktreeHandlers dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_fileListHandler :: CachedProperty
prop_fileListHandler _dhallCache _exeCache = property $ do
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

prop_fileReadHandler :: CachedProperty
prop_fileReadHandler _dhallCache _exeCache = property $ do
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

prop_fileReadHandlerBinary :: CachedProperty
prop_fileReadHandlerBinary _dhallCache _exeCache = property $ do
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

-- | Property: fileReadHandler returns 400 for empty path
prop_fileReadHandlerEmptyPath :: CachedProperty
prop_fileReadHandlerEmptyPath _dhallCache _exeCache = property $ do
    result <- evalIO $ withTmp $ \root ->
        runHandlerIO (fileReadHandler (Just (T.pack root)) "")
    case result of
        Left err -> errHTTPCode err === 400
        Right _ -> failure

-- | Property: fileReadHandler returns 400 for directory path
prop_fileReadHandlerDirectoryPath :: CachedProperty
prop_fileReadHandlerDirectoryPath _dhallCache _exeCache = property $ do
    dirName <- forAll genName
    result <- evalIO $ withTmp $ \root -> do
        createDirectory (root </> T.unpack dirName)
        runHandlerIO (fileReadHandler (Just (T.pack root)) dirName)
    case result of
        Left err -> errHTTPCode err === 400
        Right _ -> failure

-- | Property: fileReadHandler returns 404 for non-existent file
prop_fileReadHandlerNotFound :: CachedProperty
prop_fileReadHandlerNotFound _dhallCache _exeCache = property $ do
    name <- forAll genName
    result <- evalIO $ withTmp $ \root ->
        runHandlerIO (fileReadHandler (Just (T.pack root)) name)
    case result of
        Left err -> errHTTPCode err === 404
        Right _ -> failure

prop_chatHandlerAnthropicMissing :: CachedProperty
prop_chatHandlerAnthropicMissing dhallCache exeCache = property $ do
    msg <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
        withEnv "ANTHROPIC_API_KEY" Nothing $
            runHandlerIO (chatHandler st (ChatInput msg Nothing))
    case result of
        Left _err -> failure
        Right val -> lookupText "error" val === Just "No Anthropic API key configured. Set ANTHROPIC_API_KEY or add via provider auth."

prop_chatHandlerOpenRouterMissing :: CachedProperty
prop_chatHandlerOpenRouterMissing dhallCache exeCache = property $ do
    msg <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
        withEnv "OPENROUTER_API_KEY" Nothing $
            runHandlerIO (chatHandler st (ChatInput msg (Just "openrouter/test-model")))
    case result of
        Left _err -> failure
        Right val -> lookupText "error" val === Just "No OpenRouter API key configured. Set OPENROUTER_API_KEY or add via provider auth."

prop_promptAsyncIndex :: CachedProperty
prop_promptAsyncIndex dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
        let parts = [object ["type" .= ("text" :: Text), "text" .= ("hi" :: Text)]]
        let input = CreateMessageInput Nothing parts Nothing Nothing
        -- Get index before (may not exist, so default to empty list)
        idsBefore <-
            Control.Exception.catch
                (Storage.read (stStorage st) (PromptAsync.promptAsyncIndexKey "session") :: IO [Text])
                (\(_ :: SomeException) -> pure [])
        res <- runHandlerIO (sessionPromptAsyncHandler st "session" input)
        case res of
            Left _err -> pure (res, False)
            Right NoContent -> do
                -- Get index after - should have one more entry
                idsAfter <-
                    Control.Exception.catch
                        (Storage.read (stStorage st) (PromptAsync.promptAsyncIndexKey "session") :: IO [Text])
                        (\(_ :: SomeException) -> pure [])
                -- Check that exactly one new ID was added
                let newIds = filter (`notElem` idsBefore) idsAfter
                pure (res, newIds /= [])
    case result of
        (Left _err, _) -> failure
        (Right _result, ok) -> assert ok

prop_ptyConnectHandler :: CachedProperty
prop_ptyConnectHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_instanceDisposeHandler :: CachedProperty
prop_instanceDisposeHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_logHandler :: CachedProperty
prop_logHandler dhallCache exeCache = property $ do
    msg <- forAll genText
    result <- evalIO $ withStateWith dhallCache exeCache $ \st ->
        runHandlerIO (logHandler st Nothing (object ["msg" .= msg]))
    case result of
        Left _err -> failure
        Right True -> success
        _otherResult -> failure

prop_globalEventHandler :: CachedProperty
prop_globalEventHandler dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_globalEventHandlerBusEvent :: CachedProperty
prop_globalEventHandlerBusEvent dhallCache exeCache = property $ do
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_experimentalToolIdsHandler :: CachedProperty
prop_experimentalToolIdsHandler _dhallCache _exeCache = property $ do
    result <- evalIO $ runHandlerIO experimentalToolIdsHandler
    case result of
        Left _err -> failure
        Right ids -> assert $ "bash" `elem` ids

prop_experimentalToolHandler :: CachedProperty
prop_experimentalToolHandler dhallCache exeCache = property $ do
    name <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
        let input = object ["name" .= name, "payload" .= ("ok" :: Text)]
        res <- runHandlerIO (experimentalToolHandler st input)
        stored <- Storage.read (stStorage st) ["experimental-tool", name] :: IO Value
        pure (res, stored)
    case result of
        (Left _err, _) -> failure
        (Right val, stored) -> val === stored

prop_authCreateHandler :: CachedProperty
prop_authCreateHandler dhallCache exeCache = property $ do
    pid <- forAll genName
    token <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
        let input = object ["token" .= token]
        res <- runHandlerIO (authCreateHandler st pid input)
        stored <- Storage.read (stStorage st) ["auth", pid] :: IO Value
        pure (res, stored)
    case result of
        (Left _err, _) -> failure
        (Right val, stored) -> do
            val === True
            lookupText "token" stored === Just token

prop_authUpdateHandler :: CachedProperty
prop_authUpdateHandler dhallCache exeCache = property $ do
    pid <- forAll genName
    tok1 <- forAll genName
    tok2 <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

prop_authDeleteHandler :: CachedProperty
prop_authDeleteHandler dhallCache exeCache = property $ do
    pid <- forAll genName
    token <- forAll genName
    result <- evalIO $ withStateWith dhallCache exeCache $ \st -> do
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

-- | Create tests with provided DhallCache
tests :: Dhall.DhallCache -> Formatter.ExeCache -> TestTree
tests dhallCache exeCache =
    testGroup
        "Handler Property Tests"
        [ testProperty "health handler" (prop_healthHandler dhallCache exeCache)
        , testProperty "path handler" (prop_pathHandler dhallCache exeCache)
        , testProperty "global config handler" (prop_globalConfigHandler dhallCache exeCache)
        , testProperty "project handlers" (prop_projectHandlers dhallCache exeCache)
        , testProperty "project update handler" (prop_projectUpdateHandler dhallCache exeCache)
        , testProperty "provider list handler" (prop_providerListHandler dhallCache exeCache)
        , testProperty "provider auth handler" (prop_providerAuthHandler dhallCache exeCache)
        , testProperty "provider handler" (prop_providerHandler dhallCache exeCache)
        , testProperty "provider oauth handlers" (prop_providerOauthHandlers dhallCache exeCache)
        , testProperty "config handler" (prop_configHandler dhallCache exeCache)
        , testProperty "command handler" (prop_commandHandler dhallCache exeCache)
        , testProperty "agent handler" (prop_agentHandler dhallCache exeCache)
        , testProperty "session status handler" (prop_sessionStatusHandler dhallCache exeCache)
        , testProperty "session lifecycle handler" (prop_sessionLifecycleHandler dhallCache exeCache)
        , testProperty "session children handler" (prop_sessionChildrenHandler dhallCache exeCache)
        , testProperty "session todo handler" (prop_sessionTodoHandler dhallCache exeCache)
        , testProperty "session init handler" (prop_sessionInitHandler dhallCache exeCache)
        , testProperty "session fork handler" (prop_sessionForkHandler dhallCache exeCache)
        , testProperty "session abort handler" (prop_sessionAbortHandler dhallCache exeCache)
        , testProperty "session share handlers" (prop_sessionShareHandlers dhallCache exeCache)
        , testProperty "session diff handler" (prop_sessionDiffHandler dhallCache exeCache)
        , testProperty "session summarize handler" (prop_sessionSummarizeHandler dhallCache exeCache)
        , testProperty "session revert handlers" (prop_sessionRevertHandlers dhallCache exeCache)
        , testProperty "session permission handler" (prop_sessionPermissionHandler dhallCache exeCache)
        , testProperty "session message handlers" (prop_sessionMessageHandlers dhallCache exeCache)
        , testProperty "session message create publishes events" (prop_sessionMessageCreatePublishesEvents dhallCache exeCache)
        , testProperty "session messages ordered correctly" (prop_sessionMessagesOrderedCorrectly dhallCache exeCache)
        , testProperty "messages ordered after cancel" (prop_messagesOrderedAfterCancel dhallCache exeCache)
        , testProperty "same timestamp user before assistant" (prop_sameTimestampUserBeforeAssistant dhallCache exeCache)
        , testProperty "message events forwarded to eventChan" (prop_messageEventsForwardedToEventChan dhallCache exeCache)
        , testProperty "session.status events published" (prop_sessionStatusEventsPublished dhallCache exeCache)
        , testProperty "session message part handlers" (prop_sessionMessagePartHandlers dhallCache exeCache)
        , testProperty "lsp handler" (prop_lspHandler dhallCache exeCache)
        , testProperty "permission handlers" (prop_permissionHandlers dhallCache exeCache)
        , testProperty "question handlers" (prop_questionHandlers dhallCache exeCache)
        , testProperty "file status handler" (prop_fileStatusHandler dhallCache exeCache)
        , testProperty "tui handlers" (prop_tuiHandlers dhallCache exeCache)
        , testProperty "skill handler" (prop_skillHandler dhallCache exeCache)
        , testProperty "formatter handler" (prop_formatterHandler dhallCache exeCache)
        , testProperty "experimental worktree handlers" (prop_experimentalWorktreeHandlers dhallCache exeCache)
        , testProperty "file list handler" (prop_fileListHandler dhallCache exeCache)
        , testProperty "file read handler" (prop_fileReadHandler dhallCache exeCache)
        , testProperty "file read handler binary" (prop_fileReadHandlerBinary dhallCache exeCache)
        , testProperty "file read handler empty path" (prop_fileReadHandlerEmptyPath dhallCache exeCache)
        , testProperty "file read handler directory path" (prop_fileReadHandlerDirectoryPath dhallCache exeCache)
        , testProperty "file read handler not found" (prop_fileReadHandlerNotFound dhallCache exeCache)
        , testProperty "chat handler anthropic missing" (prop_chatHandlerAnthropicMissing dhallCache exeCache)
        , testProperty "chat handler openrouter missing" (prop_chatHandlerOpenRouterMissing dhallCache exeCache)
        , testProperty "prompt async index" (prop_promptAsyncIndex dhallCache exeCache)
        , testProperty "pty connect handler" (prop_ptyConnectHandler dhallCache exeCache)
        , testProperty "instance dispose handler" (prop_instanceDisposeHandler dhallCache exeCache)
        , testProperty "log handler" (prop_logHandler dhallCache exeCache)
        , testProperty "global event handler" (prop_globalEventHandler dhallCache exeCache)
        , testProperty "global event handler bus event" (prop_globalEventHandlerBusEvent dhallCache exeCache)
        , testProperty "experimental tool ids handler" (prop_experimentalToolIdsHandler dhallCache exeCache)
        , testProperty "experimental tool handler" (prop_experimentalToolHandler dhallCache exeCache)
        , testProperty "auth create handler" (prop_authCreateHandler dhallCache exeCache)
        , testProperty "auth update handler" (prop_authUpdateHandler dhallCache exeCache)
        , testProperty "auth delete handler" (prop_authDeleteHandler dhallCache exeCache)
        ]
