{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (
    -- * Server
    server,

    -- * Core Handlers
    healthHandler,
    pathHandler,
    globalConfigHandler,
    globalDisposeHandler,
    instanceDisposeHandler,
    logHandler,
    eventHandler,

    -- * Project Handlers
    projectListHandler,
    projectCurrentHandler,
    projectGetHandler,
    projectUpdateHandler,

    -- * Provider Handlers
    providerListHandler,
    providerAuthHandler,
    providerHandler,
    providerOauthAuthorizeHandler,
    providerOauthCallbackHandler,
    authCreateHandler,
    authUpdateHandler,
    authDeleteHandler,

    -- * Config Handlers
    configHandler,
    commandHandler,
    agentHandler,

    -- * Session Handlers
    sessionStatusHandler,
    sessionListHandler,
    sessionCreateHandler,
    sessionGetHandler,
    sessionDeleteHandler,
    sessionUpdateHandler,
    sessionChildrenHandler,
    sessionTodoHandler,
    sessionInitHandler,
    sessionForkHandler,
    sessionAbortHandler,
    sessionShareCreateHandler,
    sessionShareDeleteHandler,
    sessionDiffHandler,
    sessionSummarizeHandler,
    sessionCommandHandler,
    sessionShellHandler,
    sessionRevertHandler,
    sessionUnrevertHandler,
    sessionPermissionHandler,

    -- * Message Handlers
    sessionMessageListHandler,
    sessionMessageCreateHandler,
    sessionMessageGetHandler,
    sessionMessagePartDeleteHandler,
    sessionMessagePartUpdateHandler,
    sessionPromptAsyncHandler,
    startPromptAsyncWorker,

    -- * Infrastructure Handlers
    lspHandler,
    vcsHandler,
    permissionHandler,
    permissionReplyHandler,
    questionHandler,
    questionReplyHandler,
    questionRejectHandler,

    -- * Find Handlers
    findHandler,
    findFileHandler,
    findSymbolHandler,
    findMatches,

    -- * File Handlers
    fileListHandler,
    fileReadHandler,
    fileStatusHandler,

    -- * PTY Handlers
    ptyListHandler,
    ptyCreateHandler,
    ptyGetHandler,
    ptyUpdateHandler,
    ptyDeleteHandler,
    ptyCommitHandler,
    ptyChangesHandler,

    -- * TUI Handlers
    tuiAppendPromptHandler,
    tuiOpenHandler,
    tuiSubmitPromptHandler,
    tuiClearPromptHandler,
    tuiExecuteCommandHandler,
    tuiShowToastHandler,
    tuiPublishHandler,
    tuiSelectSessionHandler,
    tuiControlHandler,

    -- * Skill/Formatter Handlers
    skillHandler,
    formatterHandler,

    -- * Experimental Handlers
    experimentalToolIdsHandler,
    experimentalToolHandler,
    experimentalToolListHandler,
    experimentalWorktreeGetHandler,
    experimentalWorktreePostHandler,
    experimentalWorktreeResetHandler,
    experimentalWorktreeDeleteHandler,
    experimentalSessionListHandler,

    -- * Chat Handlers
    chatHandler,

    -- * Session Helpers
    sessionContext,
)
where

import Agent.Agent qualified as Agent
import Agent.Types qualified as AT
import Api
import Bus.Bus qualified as Bus
import Config.Config qualified as Config
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM
import Control.Exception (AsyncException (ThreadKilled), SomeException, catch, fromException)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64

import Data.Foldable (for_)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Experimental.Worktree qualified as Worktree
import Find.Search qualified as FindSearch
import Formatter.Status qualified as Formatter
import Global.Event qualified as Event
import Health.Build qualified as HealthBuild
import Katip qualified
import LLM.Anthropic qualified as Anthropic
import LLM.OpenRouter qualified as OpenRouter
import LLM.Types qualified as LLMTypes
import Log qualified
import Lsp.Store qualified as LspStore
import Message.Parts qualified as Parts
import Message.Todo qualified as Todo

import Path.Build qualified as PathBuild
import Project.Build qualified as ProjectBuild
import Project.Discovery qualified as ProjectDiscovery
import Prompt.Async qualified as PromptAsync
import Provider.OAuth qualified as OAuth
import Provider.Provider qualified as Provider
import Provider.Types qualified as PT
import Proxy.Proxy qualified as Proxy
import Pty.Connect qualified as PtyConnect
import Pty.Parse qualified as PtyParse
import Pty.Pty qualified as Pty
import Pty.Types qualified as PtyT
import Request.Store qualified as RequestStore
import Servant
import Session.Session qualified as Sess
import Session.Types (GlobalSession)
import Skill.Skill qualified as Skill
import State
import Storage.Storage qualified as Storage
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getHomeDirectory, listDirectory, makeAbsolute)

import System.FilePath ((</>))

import Tool.Defs qualified as Tool
import Tool.Exec qualified as ToolExec
import Tool.Types qualified as ToolT
import Tui.Store qualified as TuiStore
import Util.Identifier qualified as Identifier
import Util.StorageKeys (projectKey)
import Vcs.Diff qualified as Diff
import Vcs.Status qualified as VcsStatus

-- Helper to resolve paths
resolvePath :: Maybe Text -> Text -> IO FilePath
resolvePath mDir path = do
    base <- case mDir of
        Just d -> pure (unpack d)
        Nothing -> getCurrentDirectory
    makeAbsolute (base </> unpack path)

findMatches :: Text -> Text -> Maybe Text -> IO [Value]
findMatches _ _ _ = pure []

-- | Get session context from app state
sessionContext :: AppState -> Sess.SessionContext
sessionContext st =
    Sess.SessionContext
        { Sess.scStorage = stStorage st
        , Sess.scBus = stBus st
        , Sess.scProjectID = stProjectID st
        , Sess.scDirectory = stDirectory st
        , Sess.scVersion = stVersion st
        , Sess.scIdGen = stIdGen st
        }

{- | Helper for updating a session and returning the API representation
Returns 404 if session not found
-}
withSessionUpdate :: AppState -> Text -> (Session -> Session) -> Handler Session
withSessionUpdate st sid f = do
    let ctx = sessionContext st
    msession <- liftIO $ Sess.update ctx sid f
    case msession of
        Nothing -> throwError err404
        Just session -> return session

-- * Global Handlers

healthHandler :: AppState -> Handler Health
healthHandler st = return $ HealthBuild.buildHealth (stVersion st)

pathHandler :: AppState -> Handler PathInfo
pathHandler st = liftIO $ do
    cwd <- getCurrentDirectory
    home <- getHomeDirectory
    cfg <- Config.globalConfigPath
    let workdir = unpack (stDirectory st)
    let stateDir = workdir </> ".opencode" </> "state"
    return $
        PathBuild.buildPath
            (pack home)
            (pack stateDir)
            (pack cfg)
            (pack workdir)
            (pack cwd)

globalConfigHandler :: AppState -> Handler Value
globalConfigHandler st = liftIO $ do
    path <- getGlobalConfigPath st
    cfg <- Config.loadFile (stDhallCache st) path
    return $ Data.Aeson.toJSON $ fromMaybe Config.defaultConfig cfg

-- | Get global config path, using stHomeDir override if set
getGlobalConfigPath :: AppState -> IO FilePath
getGlobalConfigPath st = case stHomeDir st of
    Just home -> pure $ home </> ".config" </> "weapon" </> "weapon.dhall"
    Nothing -> Config.globalConfigPath

-- * Project Handlers

projectListHandler :: AppState -> Handler [Project]
projectListHandler st = liftIO $ do
    projects <- ProjectDiscovery.discoverProjects (unpack (stDirectory st))
    -- Merge with any stored overrides
    mapM (mergeProjectOverrides st) projects

projectCurrentHandler :: AppState -> Maybe Text -> Handler Project
projectCurrentHandler st mDir = liftIO $ do
    let dir = maybe (unpack (stDirectory st)) unpack mDir
    full <- makeAbsolute dir
    let proj = ProjectBuild.projectFromDir full
    mergeProjectOverrides st proj

projectGetHandler :: AppState -> Text -> Handler Project
projectGetHandler st pid = do
    let current = ProjectBuild.projectFromDir (unpack (stDirectory st))
    if Api.id current == pid
        then liftIO $ mergeProjectOverrides st current
        else throwError err404

-- | Merge stored project overrides with computed project
mergeProjectOverrides :: AppState -> Project -> IO Project
mergeProjectOverrides st proj = do
    mStored <- Storage.readMaybe (stStorage st) (projectKey (Api.id proj))
    case mStored of
        Nothing -> return proj
        Just stored -> return proj{Api.name = Api.name stored}

-- | Update project properties (PATCH /project/{projectID})
projectUpdateHandler :: AppState -> Text -> Value -> Handler Project
projectUpdateHandler st pid input = do
    let current = ProjectBuild.projectFromDir (unpack (stDirectory st))
    if Api.id current /= pid
        then throwError err404
        else liftIO $ do
            -- Extract name from input
            let newName = extractText input "name"
            -- Merge with current project - update name if provided
            let updated = case newName of
                    Just n -> current{Api.name = Just n}
                    Nothing -> current
            -- Store project metadata for persistence
            Storage.writeCached (stDirCache st) (stStorage st) (projectKey pid) updated
            -- Publish update event
            Bus.publish (stBus st) "project.updated" (object ["info" .= updated])
            return updated

-- * Provider/Config Handlers

providerListHandler :: AppState -> Maybe Text -> Handler ConfigProviderList
providerListHandler st _mDir = liftIO $ do
    providers <- Provider.listWithModels (stStorage st)
    -- For config.providers, we need to add "source" field per the component schema
    let providerJson = map toConfigProvider providers

    -- Default model selection as a map of providerID -> modelID
    let defaultModel = case providers of
            (p : _) -> case Map.elems (PT.providerModels p) of
                (m : _) -> object [K.fromText (PT.providerId p) .= PT.modelId m]
                [] -> object []
            [] -> object []

    return $ ConfigProviderList providerJson defaultModel
  where
    -- Convert provider to config.providers format (adds required "source" and "options" fields)
    toConfigProvider p =
        object
            [ "id" .= PT.providerId p
            , "name" .= PT.providerName p
            , "source" .= ("env" :: Text) -- Default source for builtin providers
            , "env" .= PT.providerEnv p
            , "options" .= object []
            , "models" .= PT.providerModels p
            ]

providerAuthHandler :: AppState -> Handler Value
providerAuthHandler _st = liftIO $ do
    providers <- Provider.list
    -- Auth methods are derived from env vars - all use API key auth
    let entries = map (\p -> (PT.providerId p, [authMethodForProvider p])) providers
    return $ object (map (\(pid, methods) -> K.fromText pid .= methods) entries)
  where
    authMethodForProvider p =
        object
            [ "type" .= ("api" :: Text)
            , "label" .= ("API key" :: Text)
            , "envVars" .= PT.providerEnv p
            ]

providerHandler :: AppState -> Maybe Text -> Handler ProviderList
providerHandler st _mDir = liftIO $ do
    -- Fetch providers with dynamically loaded models (e.g., from OpenRouter API)
    providers <- Provider.listWithModels (stStorage st)
    let providerJson = map Data.Aeson.toJSON providers

    -- Get connected providers (those with stored auth)
    connectedIds <- Provider.listConnected (stStorage st)

    -- Default model selection as a map of providerID -> modelID
    let defaultModel = case providers of
            (p : _) -> case Map.elems (PT.providerModels p) of
                (m : _) -> object [K.fromText (PT.providerId p) .= PT.modelId m]
                [] -> object []
            [] -> object []

    return $ ProviderList providerJson defaultModel connectedIds

providerOauthAuthorizeHandler :: AppState -> Text -> Value -> Handler Value
providerOauthAuthorizeHandler st pid input = liftIO $ do
    state <- OAuth.generateState
    let redirect = case extractText input "redirectURI" of
            Just r -> Just r
            Nothing -> case extractText input "redirect" of
                Just r -> Just r
                Nothing -> extractText input "redirect_uri"
    let scopes = extractTextList input "scopes"
    let url = OAuth.buildAuthorizeUrl pid state redirect scopes
    let payload = object ["providerID" .= pid, "state" .= state, "url" .= url]
    Storage.writeCached (stDirCache st) (stStorage st) ["auth", "oauth", pid] (object ["state" .= state, "redirect" .= redirect])
    return payload

providerOauthCallbackHandler :: AppState -> Text -> Maybe Text -> Value -> Handler Bool
providerOauthCallbackHandler _st _pid _mDir input = do
    -- Input expects: { method: number, code?: string }
    -- method is required, code is optional
    case extractInt input "method" of
        Nothing -> throwError $ err400{errBody = "{\"error\":\"method is required\"}"}
        Just _method -> do
            -- In a real implementation, we would:
            -- 1. Look up pending OAuth state by providerID
            -- 2. Based on method, either use the code or auto-authenticate
            -- 3. Store the resulting auth tokens
            -- For the mock, we just accept and return success
            let mCode = extractText input "code"
            case mCode of
                Just _code -> do
                    -- Code-based OAuth flow - would exchange code for tokens
                    return True
                Nothing -> do
                    -- Auto OAuth flow - would use stored credentials
                    return True

authCreateHandler :: AppState -> Text -> Value -> Handler Bool
authCreateHandler st pid input = liftIO $ do
    case extractToken input of
        Nothing -> return False
        Just token -> do
            Provider.setAuth (stStorage st) pid token
            return True

authUpdateHandler :: AppState -> Text -> Value -> Handler Bool
authUpdateHandler = authCreateHandler

authDeleteHandler :: AppState -> Text -> Handler Bool
authDeleteHandler st pid = liftIO $ do
    Provider.removeAuth (stStorage st) pid
    return True

extractToken :: Value -> Maybe Text
extractToken (Object obj) = case KM.lookup "token" obj of
    Just (String t) -> Just t
    Just (Object _) -> extractFromApiKey
    Just (Array _) -> extractFromApiKey
    Just (Number _) -> extractFromApiKey
    Just (Bool _) -> extractFromApiKey
    Just Null -> extractFromApiKey
    Nothing -> extractFromApiKey
  where
    extractFromApiKey = case KM.lookup "apiKey" obj of
        Just (String t) -> Just t
        Just (Object _) -> Nothing
        Just (Array _) -> Nothing
        Just (Number _) -> Nothing
        Just (Bool _) -> Nothing
        Just Null -> Nothing
        Nothing -> Nothing
extractToken (Array _) = Nothing
extractToken (String _) = Nothing
extractToken (Number _) = Nothing
extractToken (Bool _) = Nothing
extractToken Null = Nothing

extractText :: Value -> Text -> Maybe Text
extractText (Object obj) key = case KM.lookup (K.fromText key) obj of
    Just (String t) -> Just t
    Just (Object _) -> Nothing
    Just (Array _) -> Nothing
    Just (Number _) -> Nothing
    Just (Bool _) -> Nothing
    Just Null -> Nothing
    Nothing -> Nothing
extractText (Array _) _ = Nothing
extractText (String _) _ = Nothing
extractText (Number _) _ = Nothing
extractText (Bool _) _ = Nothing
extractText Null _ = Nothing

extractInt :: Value -> Text -> Maybe Int
extractInt (Object obj) key = case KM.lookup (K.fromText key) obj of
    Just (Number n) -> Just (round n)
    _other -> Nothing
extractInt _other _ = Nothing

extractTextList :: Value -> Text -> [Text]
extractTextList (Object obj) key = case KM.lookup (K.fromText key) obj of
    Just (Array xs) -> foldr collect [] xs
    Just (Object _) -> []
    Just (String _) -> []
    Just (Number _) -> []
    Just (Bool _) -> []
    Just Null -> []
    Nothing -> []
  where
    collect (String t) acc = t : acc
    collect (Object _) acc = acc
    collect (Array _) acc = acc
    collect (Number _) acc = acc
    collect (Bool _) acc = acc
    collect Null acc = acc
extractTextList (Array _) _ = []
extractTextList (String _) _ = []
extractTextList (Number _) _ = []
extractTextList (Bool _) _ = []
extractTextList Null _ = []

-- | Create a JSON error response object
errorResponse :: Text -> Value
errorResponse msg = object ["error" .= msg]

configHandler :: AppState -> Handler Value
configHandler st = liftIO $ do
    cfg <- Config.load (stDhallCache st) (unpack (stDirectory st))
    return $ Data.Aeson.toJSON cfg

commandHandler :: Handler [Value]
commandHandler = return Tool.toolDefinitions

agentHandler :: Handler [Value]
agentHandler = liftIO $ do
    agents <- Agent.list
    -- Filter out hidden agents
    let visible = filter (not . fromMaybe False . AT.agentHidden) agents
    return $ map Data.Aeson.toJSON visible

-- * Session Handlers

sessionStatusHandler :: AppState -> Maybe Text -> Handler Value
sessionStatusHandler _st _mDir = liftIO $ do
    -- Return empty map since we don't track per-session status yet
    -- The spec expects Map<SessionID, SessionStatus>
    -- An empty object {} is a valid empty map
    return $ Object mempty

sessionListHandler :: AppState -> Maybe Text -> Maybe Bool -> Maybe Double -> Maybe Double -> Maybe Text -> Handler [Session]
sessionListHandler st mDir mRoots mLimit mStart mSearch = liftIO $ do
    let ctx = sessionContext st
    Sess.list ctx mDir mRoots (round <$> mLimit) mStart mSearch

sessionCreateHandler :: AppState -> Maybe Text -> CreateSessionInput -> Handler Session
sessionCreateHandler st _mDir input = liftIO $ do
    let ctx = sessionContext st
    Sess.create ctx input

sessionGetHandler :: AppState -> Text -> Handler Session
sessionGetHandler st sid = do
    let ctx = sessionContext st
    msession <- liftIO $ Sess.get ctx sid
    case msession of
        Nothing -> throwError err404
        Just session -> return session

sessionDeleteHandler :: AppState -> Text -> Handler Bool
sessionDeleteHandler st sid = liftIO $ do
    let ctx = sessionContext st
    Sess.delete ctx sid

sessionUpdateHandler :: AppState -> Text -> UpdateSessionInput -> Handler Session
sessionUpdateHandler st sid input = withSessionUpdate st sid (applyUpdate input)
  where
    applyUpdate usi s =
        let title = case usiTitle usi of
                Just t -> t
                Nothing -> sessionTitle s
            summary = case usiSummary usi of
                Just v -> Just v
                Nothing -> sessionSummary s
            share = case usiShare usi of
                Just v -> Just v
                Nothing -> sessionShare s
            revert = case usiRevert usi of
                Just v -> Just v
                Nothing -> sessionRevert s
         in s
                { sessionTitle = title
                , sessionSummary = summary
                , sessionShare = share
                , sessionRevert = revert
                }

sessionChildrenHandler :: AppState -> Text -> Maybe Text -> Handler [Session]
sessionChildrenHandler st sid _mDir = liftIO $ do
    let ctx = sessionContext st
    sessions <- Sess.list ctx Nothing Nothing Nothing Nothing Nothing
    let children = filter (\s -> sessionParentID s == Just sid) sessions
    return children

sessionTodoHandler :: AppState -> Text -> Handler [Value]
sessionTodoHandler st sid = liftIO $ do
    let key = ["todo", sid]
    result <- Storage.readMaybe (stStorage st) key
    pure $ fromMaybe [] result

sessionInitHandler :: AppState -> Text -> Handler Bool
sessionInitHandler st sid = liftIO $ do
    Bus.publish (stBus st) "session.initialized" (object ["sessionID" .= sid])
    return True

sessionForkHandler :: AppState -> Text -> ForkSessionInput -> Handler Session
sessionForkHandler st sid _forkInput = liftIO $ do
    let ctx = sessionContext st
    parent <- Sess.get ctx sid
    let title = case parent of
            Just p -> Just ("Fork of " <> sessionTitle p)
            Nothing -> Just "Forked session"
    Sess.create
        ctx
        CreateSessionInput
            { csiTitle = title
            , csiParentID = Just sid
            }

sessionAbortHandler :: AppState -> Text -> Maybe Text -> Handler Bool
sessionAbortHandler st sid _mDir = liftIO $ do
    -- Actually kill the running agent thread if any
    wasRunning <- State.abortAgent st sid
    -- Publish abort event for TUI notification
    Bus.publish (stBus st) "session.error" (object ["sessionID" .= sid, "aborted" .= True])
    return wasRunning

sessionShareCreateHandler :: AppState -> Text -> Handler Session
sessionShareCreateHandler st sid =
    withSessionUpdate st sid $ \s ->
        let url = "https://share.opencode.ai/session/" <> sid
         in s{sessionShare = Just (SessionShare url)}

sessionShareDeleteHandler :: AppState -> Text -> Handler Session
sessionShareDeleteHandler st sid =
    withSessionUpdate st sid $ \s -> s{sessionShare = Nothing}

sessionDiffHandler :: AppState -> Text -> Maybe Text -> Handler [FileDiff]
sessionDiffHandler st _sid _mMessageID = liftIO $ do
    -- Load file-level diffs for the session
    fileDiffs <- Diff.loadFileDiffs (stExeCache st) (unpack (stDirectory st))
    return $ map toApiFileDiff fileDiffs
  where
    toApiFileDiff fd =
        FileDiff
            { fdFile = Diff.fdiFile fd
            , fdBefore = "" -- Would need full file content diff to populate
            , fdAfter = "" -- Would need full file content diff to populate
            , fdAdditions = Diff.fdiAdditions fd
            , fdDeletions = Diff.fdiDeletions fd
            , fdStatus = Just $ inferStatus fd
            }
    inferStatus fd
        | Diff.fdiAdditions fd > 0 && Diff.fdiDeletions fd == 0 = Added
        | Diff.fdiAdditions fd == 0 && Diff.fdiDeletions fd > 0 = Deleted
        | otherwise = Modified

sessionSummarizeHandler :: AppState -> Text -> Handler Bool
sessionSummarizeHandler st sid = do
    summary <- liftIO $ loadSummary (stExeCache st) (unpack (stDirectory st))
    _ <- withSessionUpdate st sid $ \s -> s{sessionSummary = Just summary}
    return True

loadSummary :: Formatter.ExeCache -> FilePath -> IO SessionSummary
loadSummary exeCache root = do
    mresult <- Diff.loadDiff exeCache root
    case mresult of
        Nothing -> pure (SessionSummary 0 0 (Just 0))
        Just (_, summary) -> pure summary

sessionCommandHandler :: AppState -> Text -> Value -> Handler Value
sessionCommandHandler st sid input = liftIO $ do
    let ctx =
            ToolT.ToolContext
                { ToolT.tcSessionID = sid
                , ToolT.tcMessageID = "command"
                , ToolT.tcWorkdir = unpack (stDirectory st)
                }
    now <- getCurrentTime
    let timestamp = realToFrac (utcTimeToPOSIXSeconds now) :: Double
    output <- ToolExec.execute ctx "bash" input
    let isError = ToolT.toIsError output
        outputText = ToolT.toOutput output
        -- Build AssistantMessage (info)
        info =
            object
                [ "id" .= ("msg_cmd_" <> sid)
                , "sessionID" .= sid
                , "role" .= ("assistant" :: Text)
                , "time" .= object ["created" .= timestamp, "completed" .= timestamp]
                , "error" .= if isError then Just (object ["type" .= ("error" :: Text), "message" .= outputText]) else Nothing
                ]
        -- Build parts array with a text part containing the output
        parts =
            [ object
                [ "id" .= ("part_cmd_" <> sid)
                , "type" .= ("text" :: Text)
                , "text" .= outputText
                ]
            ]
        response = object ["info" .= info, "parts" .= parts]
    Bus.publish (stBus st) "command.executed" response
    return response

sessionShellHandler :: AppState -> Text -> Value -> Handler Value
sessionShellHandler st sid input = liftIO $ do
    let ptyInput =
            (PtyParse.parseInput input)
                { PtyT.cpiSessionId = Just sid
                }
    result <- Pty.create (stPtyManager st) ptyInput
    case result of
        Left err -> return $ errorResponse err
        Right info -> do
            Bus.publish (stBus st) "pty.created" (object ["info" .= info, "sessionID" .= sid])
            return $ Data.Aeson.toJSON info

sessionRevertHandler :: AppState -> Text -> SessionRevert -> Handler Session
sessionRevertHandler st sid input =
    withSessionUpdate st sid $ \s -> s{sessionRevert = Just input}

sessionUnrevertHandler :: AppState -> Text -> Handler Session
sessionUnrevertHandler st sid =
    withSessionUpdate st sid $ \s -> s{sessionRevert = Nothing}

sessionPermissionHandler :: AppState -> Text -> Text -> Maybe Text -> Value -> Handler Bool
sessionPermissionHandler st sid pid _mDir input = liftIO $ do
    Bus.publish (stBus st) "permission.replied" (object ["sessionID" .= sid, "permissionID" .= pid, "response" .= input])
    return True

-- * Message Handlers (still in-memory for now, TODO: port to storage)

sessionMessageListHandler :: AppState -> Text -> Maybe Int -> Handler [Message]
sessionMessageListHandler st sid _mLimit = liftIO $ do
    -- Read messages from storage and sort by created time (oldest first)
    -- Secondary sort by role priority ensures user messages come before assistant messages
    -- when they have the same timestamp (which happens when created together)
    let key = ["message", sid]
    messages <-
        (Storage.list (stStorage st) key >>= mapM (Storage.read (stStorage st)))
            `catch` \(Storage.NotFoundError _) -> return []
    -- Sort by (created time, role priority) where user=0, assistant=1
    let rolePriority :: Text -> Int
        rolePriority "user" = 0
        rolePriority "assistant" = 1
        rolePriority _ = 2
    pure $ sortOn (\m -> (messageInfoCreatedTime (msgInfo m), rolePriority (messageInfoRole (msgInfo m)))) messages

sessionMessageCreateHandler :: AppState -> Text -> CreateMessageInput -> Handler Message
sessionMessageCreateHandler st sid input = liftIO $ do
    createMessageIO st sid input

createMessageIO :: AppState -> Text -> CreateMessageInput -> IO Message
createMessageIO st sid input = do
    let lg = Log.withNS (stLogger st) "message"

    now <- getCurrentTime
    let t = realToFrac (utcTimeToPOSIXSeconds now) * 1000
    let msgTime = MessageTime t Nothing -- created, no completed yet

    -- Major #5: Extract model and agent from input with defaults
    -- Model is now a {providerID, modelID} object
    let (providerId, modelId) = case cmiModel input of
            Just ms -> (msProviderID ms, msModelID ms)
            Nothing -> ("anthropic", "claude-opus-4-20250514")
    -- For OpenRouter, modelId already contains the full path (e.g., "moonshotai/kimi-k2.5")
    -- For other providers, we need to prefix with providerId
    let fullModelId = if providerId == "openrouter" then modelId else providerId <> "/" <> modelId
    let agentName = fromMaybe "armed" (cmiAgent input)

    -- Major #7: Look up agent to get system prompt
    mAgent <- Agent.get agentName
    let systemPrompt = mAgent >>= AT.agentPrompt

    -- Extract user text for logging
    let userText = extractUserText (cmiParts input)
    Log.logMsg lg Katip.InfoS $ "create session=" <> sid <> " model=" <> fullModelId <> " agent=" <> agentName <> " text=" <> T.take 50 userText

    -- Publish session.status busy event (Critical for TUI Ctrl+C support)
    Log.logMsg lg Katip.InfoS $ "publishing session.status busy for session: " <> sid
    Bus.publish (stBus st) "session.status" $
        object
            [ "sessionID" .= sid
            , "status" .= object ["type" .= ("busy" :: Text)]
            ]

    -- Get working directory for path info
    cwd <- getCurrentDirectory

    -- 1. User Message
    -- Use "msg" prefix to match TUI conventions for consistent sorting
    uMsgId <- Identifier.ascendingWithPrefix (stIdGen st) "msg"
    let uMsgInfo =
            UserMessageInfo
                { umiId = uMsgId
                , umiSessionId = sid
                , umiTime = msgTime
                , umiAgent = Just agentName
                }
    let uMsg =
            Message
                { msgInfo = UserInfo uMsgInfo
                , msgParts = cmiParts input
                }

    -- 2. Assistant Message (incomplete initially)
    -- Use same "msg" prefix as user message for consistent sorting
    aMsgId <- Identifier.ascendingWithPrefix (stIdGen st) "msg"
    partId <- Identifier.ascendingWithPrefix (stIdGen st) "part"
    let aMsgInfo =
            AssistantMessageInfo
                { amiId = aMsgId
                , amiSessionId = sid
                , amiTime = msgTime
                , amiParentId = uMsgId
                , amiModelId = modelId
                , amiProviderId = providerId
                , amiMode = "normal"
                , amiAgent = agentName
                , amiPath = MessagePath (pack cwd) (pack cwd)
                , amiCost = 0.0
                , amiTokens =
                    MessageTokens
                        { mtTotal = Nothing
                        , mtInput = 0
                        , mtOutput = 0
                        , mtReasoning = 0
                        , mtCache = TokenCache 0 0
                        }
                , amiSummary = Nothing
                , amiVariant = Nothing
                , amiFinish = Nothing
                , amiError = Nothing
                , amiStructured = Nothing
                }
    let aMsg =
            Message
                { msgInfo = AssistantInfo aMsgInfo
                , msgParts = []
                }

    -- Write to storage
    Storage.writeCached (stDirCache st) (stStorage st) ["message", sid, uMsgId] uMsg
    Storage.writeCached (stDirCache st) (stStorage st) ["message", sid, aMsgId] aMsg

    let todos = Todo.extractTodos (cmiParts input)
    unless (null todos) $
        Storage.writeCached (stDirCache st) (stStorage st) ["todo", sid] todos

    -- Publish user message event (send just info, not full message)
    -- Must include required fields: id, sessionID, role, time, agent, model
    let userInfo =
            object
                [ "id" .= uMsgId
                , "sessionID" .= sid
                , "role" .= ("user" :: Text)
                , "time" .= object ["created" .= t]
                , "parentID" .= (Nothing :: Maybe Text)
                , "agent" .= agentName
                , "model" .= object ["providerID" .= providerId, "modelID" .= modelId]
                ]
    Log.logMsg lg Katip.InfoS $ "publishing message.updated for user message: " <> uMsgId
    Bus.publish (stBus st) "message.updated" (object ["info" .= userInfo])
    Log.logMsg lg Katip.InfoS "message.updated published"

    -- Publish user message parts via SSE (Critical #3)
    forM_ (zip [(0 :: Int) ..] (cmiParts input)) $ \(idx, part) -> do
        -- Generate a part ID if not present
        partId' <- genId (stIdGen st)
        -- Add required fields: id, sessionID, messageID
        let partWithIds = case part of
                Object obj ->
                    Object $
                        -- Only add id if not present
                        (if KM.member "id" obj then Prelude.id else KM.insert "id" (String partId')) $
                            KM.insert "sessionID" (String sid) $
                                KM.insert "messageID" (String uMsgId) obj
                Array _arr ->
                    -- Array parts are not directly supported, wrap in object
                    object
                        [ "id" .= partId'
                        , "sessionID" .= sid
                        , "messageID" .= uMsgId
                        , "type" .= ("text" :: Text)
                        , "text" .= ("" :: Text)
                        ]
                String _str ->
                    -- String parts get converted to text parts
                    object
                        [ "id" .= partId'
                        , "sessionID" .= sid
                        , "messageID" .= uMsgId
                        , "type" .= ("text" :: Text)
                        , "text" .= ("" :: Text)
                        ]
                Number _num ->
                    object
                        [ "id" .= partId'
                        , "sessionID" .= sid
                        , "messageID" .= uMsgId
                        , "type" .= ("text" :: Text)
                        , "text" .= ("" :: Text)
                        ]
                Bool _b ->
                    object
                        [ "id" .= partId'
                        , "sessionID" .= sid
                        , "messageID" .= uMsgId
                        , "type" .= ("text" :: Text)
                        , "text" .= ("" :: Text)
                        ]
                Null ->
                    object
                        [ "id" .= partId'
                        , "sessionID" .= sid
                        , "messageID" .= uMsgId
                        , "type" .= ("text" :: Text)
                        , "text" .= ("" :: Text)
                        ]
        Log.logMsg lg Katip.InfoS $ "publishing message.part.updated for user part " <> T.pack (show idx)
        Bus.publish (stBus st) "message.part.updated" (object ["part" .= partWithIds])

    -- Publish assistant message (incomplete - no time.completed, no finish)
    -- This lets the TUI know there's an assistant message being generated
    let assistantInfo =
            object
                [ "id" .= aMsgId
                , "sessionID" .= sid
                , "role" .= ("assistant" :: Text)
                , "time" .= object ["created" .= t]
                , "parentID" .= uMsgId
                , "modelID" .= modelId
                , "providerID" .= providerId
                , "mode" .= ("build" :: Text)
                , "agent" .= agentName
                , "path" .= object ["cwd" .= stDirectory st, "root" .= stDirectory st]
                , "cost" .= (0 :: Double)
                , "tokens"
                    .= object
                        [ "input" .= (0 :: Int)
                        , "output" .= (0 :: Int)
                        , "reasoning" .= (0 :: Int)
                        , "cache" .= object ["read" .= (0 :: Int), "write" .= (0 :: Int)]
                        ]
                ]
    Log.logMsg lg Katip.InfoS $ "publishing message.updated for assistant message: " <> aMsgId
    Bus.publish (stBus st) "message.updated" (object ["info" .= assistantInfo])
    Log.logMsg lg Katip.InfoS "assistant message.updated published"

    -- Spawn LLM streaming task
    _ <-
        forkIO $
            ( do
                -- Register this thread for abort support
                tid <- myThreadId
                State.registerAgent st sid tid
                -- Always use OpenRouter as the unified LLM gateway
                -- OpenRouter can proxy to Anthropic, OpenAI, etc. using "provider/model" format
                mApiKey <- Provider.getApiKey (stStorage st) "openrouter"
                case mApiKey of
                    Nothing -> do
                        -- No API key - send error
                        let errPart =
                                object
                                    [ "id" .= partId
                                    , "sessionID" .= sid
                                    , "messageID" .= aMsgId
                                    , "type" .= ("text" :: Text)
                                    , "text" .= ("Error: No OpenRouter API key configured. Set OPENROUTER_API_KEY or add via provider auth. OpenRouter provides access to all LLM providers." :: Text)
                                    ]
                        Bus.publish (stBus st) "message.part.updated" (object ["part" .= errPart])
                        -- Minor #10: Persist error parts to storage
                        let updatedMsg = aMsg{msgParts = [errPart]}
                        Storage.writeCached (stDirCache st) (stStorage st) ["message", sid, aMsgId] updatedMsg
                        completeMessage st sid aMsgId uMsgId providerId modelId agentName t
                    Just key -> do
                        -- Always use OpenRouter - it can proxy to all providers
                        -- Use full model ID: "providerId/modelId"
                        client <- OpenRouter.newClient key
                        textRef <- newTVarIO ("" :: Text)
                        partsRef <- newTVarIO ([] :: [Value])
                        partIdRef <- newTVarIO partId

                        -- Critical #2: Load conversation history and send to LLM
                        priorMsgs <- loadConversationHistory st sid
                        let historyMessages = map messageToOpenRouter priorMsgs

                        -- Major #7: Add system prompt if agent has one
                        let systemMessage = case systemPrompt of
                                Just prompt -> [OpenRouter.simpleMessage OpenRouter.System prompt]
                                Nothing -> []
                        let initialMessages = systemMessage ++ historyMessages ++ [OpenRouter.simpleMessage OpenRouter.User userText]

                        -- Major #4 & #5: Use model from input and include tools
                        let tools = map OpenRouter.toolDefToOpenAI Tool.toolDefinitions
                        let toolCtx = ToolT.ToolContext sid aMsgId (T.unpack (stDirectory st))

                        -- Agent loop: run LLM -> execute tools -> loop until done
                        let agentLoop :: [OpenRouter.ChatMessage] -> IO ()
                            agentLoop msgs = do
                                currentPartId <- readTVarIO partIdRef
                                atomically $ writeTVar textRef ""

                                let req =
                                        OpenRouter.ChatRequest
                                            { OpenRouter.crModel = fullModelId -- Use full "provider/model" format for OpenRouter
                                            , OpenRouter.crMessages = msgs
                                            , OpenRouter.crMaxTokens = Just 4096
                                            , OpenRouter.crTemperature = Nothing
                                            , OpenRouter.crStream = True
                                            , OpenRouter.crTools = Just tools
                                            }

                                result <- OpenRouter.chatStreamWithTools client req $ \delta -> do
                                    -- Accumulate text
                                    atomically $ modifyTVar' textRef (<> delta)
                                    fullText <- readTVarIO textRef

                                    -- Publish text part update with accumulated text
                                    let textPart =
                                            object
                                                [ "id" .= currentPartId
                                                , "sessionID" .= sid
                                                , "messageID" .= aMsgId
                                                , "type" .= ("text" :: Text)
                                                , "text" .= fullText
                                                ]
                                    Bus.publish (stBus st) "message.part.updated" (object ["part" .= textPart, "delta" .= delta])

                                case result of
                                    Left err -> do
                                        -- Error occurred - append to text and finish
                                        fullText <- readTVarIO textRef
                                        let errText = fullText <> "\n\n[Error: " <> err <> "]"
                                        let textPart =
                                                object
                                                    [ "id" .= currentPartId
                                                    , "sessionID" .= sid
                                                    , "messageID" .= aMsgId
                                                    , "type" .= ("text" :: Text)
                                                    , "text" .= errText
                                                    ]
                                        Bus.publish (stBus st) "message.part.updated" (object ["part" .= textPart])
                                        atomically $ modifyTVar' partsRef (++ [textPart])
                                    Right streamResult -> do
                                        -- Check for tool calls
                                        let toolCalls = OpenRouter.srToolCalls streamResult
                                        fullText <- readTVarIO textRef

                                        -- Add text part if we have any
                                        unless (T.null fullText) $ do
                                            let textPart =
                                                    object
                                                        [ "id" .= currentPartId
                                                        , "sessionID" .= sid
                                                        , "messageID" .= aMsgId
                                                        , "type" .= ("text" :: Text)
                                                        , "text" .= fullText
                                                        ]
                                            atomically $ modifyTVar' partsRef (++ [textPart])

                                        if null toolCalls
                                            then pure () -- Done, no more tool calls
                                            else do
                                                -- Execute each tool call
                                                toolResults <- forM toolCalls $ \tc -> do
                                                    -- Generate part ID for this tool
                                                    toolPartId <- genId (stIdGen st)
                                                    startTime <- getCurrentTime
                                                    let startMs = realToFrac (utcTimeToPOSIXSeconds startTime) * 1000 :: Double
                                                    let toolName = OpenRouter.tcfName (OpenRouter.tcFunction tc)
                                                    let toolInput = parseToolInput (OpenRouter.tcfArguments (OpenRouter.tcFunction tc))

                                                    -- Emit tool part with "running" state
                                                    let runningPart =
                                                            object
                                                                [ "id" .= toolPartId
                                                                , "sessionID" .= sid
                                                                , "messageID" .= aMsgId
                                                                , "type" .= ("tool" :: Text)
                                                                , "tool" .= toolName
                                                                , "callID" .= OpenRouter.tcId tc
                                                                , "state"
                                                                    .= object
                                                                        [ "status" .= ("running" :: Text)
                                                                        , "input" .= toolInput
                                                                        , "time" .= object ["start" .= startMs]
                                                                        ]
                                                                ]
                                                    Bus.publish (stBus st) "message.part.updated" (object ["part" .= runningPart])
                                                    atomically $ modifyTVar' partsRef (++ [runningPart])

                                                    -- Convert to LLMTypes.ToolUse and execute with streaming
                                                    let toolUse =
                                                            LLMTypes.ToolUse
                                                                { LLMTypes.tuId = OpenRouter.tcId tc
                                                                , LLMTypes.tuName = toolName
                                                                , LLMTypes.tuInput = toolInput
                                                                }

                                                    -- Create streaming callback that emits partial output updates
                                                    let streamCallback accumulatedOutput = do
                                                            let streamingPart =
                                                                    object
                                                                        [ "id" .= toolPartId
                                                                        , "sessionID" .= sid
                                                                        , "messageID" .= aMsgId
                                                                        , "type" .= ("tool" :: Text)
                                                                        , "tool" .= toolName
                                                                        , "callID" .= OpenRouter.tcId tc
                                                                        , "state"
                                                                            .= object
                                                                                [ "status" .= ("running" :: Text)
                                                                                , "input" .= toolInput
                                                                                , "metadata" .= object ["output" .= accumulatedOutput]
                                                                                , "time" .= object ["start" .= startMs]
                                                                                ]
                                                                        ]
                                                            Bus.publish (stBus st) "message.part.updated" (object ["part" .= streamingPart])

                                                    toolResult <- ToolExec.executeToolUseStreaming toolCtx toolUse streamCallback

                                                    -- Emit tool part with "completed" or "error" state
                                                    endTime <- getCurrentTime
                                                    let endMs = realToFrac (utcTimeToPOSIXSeconds endTime) * 1000 :: Double
                                                    let completedPart =
                                                            if LLMTypes.trIsError toolResult
                                                                then
                                                                    object
                                                                        [ "id" .= toolPartId
                                                                        , "sessionID" .= sid
                                                                        , "messageID" .= aMsgId
                                                                        , "type" .= ("tool" :: Text)
                                                                        , "tool" .= toolName
                                                                        , "callID" .= OpenRouter.tcId tc
                                                                        , "state"
                                                                            .= object
                                                                                [ "status" .= ("error" :: Text)
                                                                                , "input" .= toolInput
                                                                                , "error" .= truncateToolOutput (LLMTypes.trContent toolResult)
                                                                                , "time" .= object ["start" .= startMs, "end" .= endMs]
                                                                                ]
                                                                        ]
                                                                else
                                                                    object
                                                                        [ "id" .= toolPartId
                                                                        , "sessionID" .= sid
                                                                        , "messageID" .= aMsgId
                                                                        , "type" .= ("tool" :: Text)
                                                                        , "tool" .= toolName
                                                                        , "callID" .= OpenRouter.tcId tc
                                                                        , "state"
                                                                            .= object
                                                                                [ "status" .= ("completed" :: Text)
                                                                                , "input" .= toolInput
                                                                                , "output" .= truncateToolOutput (LLMTypes.trContent toolResult)
                                                                                , "title" .= (toolName <> " completed")
                                                                                , "metadata" .= object []
                                                                                , "time" .= object ["start" .= startMs, "end" .= endMs]
                                                                                ]
                                                                        ]
                                                    Bus.publish (stBus st) "message.part.updated" (object ["part" .= completedPart])
                                                    -- Update the part in the parts list (replace the running one)
                                                    let updateToolPart pid newPart =
                                                            map
                                                                ( \p -> case p of
                                                                    Object obj -> case KM.lookup "id" obj of
                                                                        Just (String pid') | pid' == pid -> newPart
                                                                        Just (String _otherPid) -> p
                                                                        Just (Object _) -> p
                                                                        Just (Array _) -> p
                                                                        Just (Number _) -> p
                                                                        Just (Bool _) -> p
                                                                        Just Null -> p
                                                                        Nothing -> p
                                                                    Array _ -> p
                                                                    String _ -> p
                                                                    Number _ -> p
                                                                    Bool _ -> p
                                                                    Null -> p
                                                                )
                                                    atomically $ modifyTVar' partsRef (updateToolPart toolPartId completedPart)

                                                    pure toolResult

                                                -- Build assistant message with tool calls for history
                                                let assistantMsgWithTools =
                                                        OpenRouter.assistantMessageWithTools
                                                            (if T.null fullText then Nothing else Just fullText)
                                                            toolCalls

                                                -- Build tool result messages for OpenRouter (tool role)
                                                -- Truncate tool output for LLM context to avoid huge payloads
                                                let toolResultMsgs = map (\tr -> OpenRouter.toolResultMessage (LLMTypes.trToolUseId tr) (truncateToolOutputForLLM $ LLMTypes.trContent tr)) toolResults

                                                -- Update part ID for next iteration
                                                newPartId <- genId (stIdGen st)
                                                atomically $ writeTVar partIdRef newPartId

                                                -- Continue agent loop with updated messages
                                                -- Note: toolResultMsgs are ToolResultMessage, need special handling in ChatRequest
                                                -- For now, we pass them as Value in a modified request
                                                agentLoop (msgs ++ [assistantMsgWithTools] ++ map toolResultToMessage toolResultMsgs)

                        agentLoop initialMessages

                        -- Critical #1: Persist assistant message parts to storage
                        finalParts <- readTVarIO partsRef
                        let updatedMsg = aMsg{msgParts = finalParts}
                        Storage.writeCached (stDirCache st) (stStorage st) ["message", sid, aMsgId] updatedMsg

                        completeMessage st sid aMsgId uMsgId providerId modelId agentName t
                -- Unregister thread on normal completion
                State.unregisterAgent st sid
            )
                `catch` \(e :: SomeException) -> do
                    -- Always unregister thread on exception
                    State.unregisterAgent st sid
                    -- Check if this is an abort (ThreadKilled) - exit cleanly
                    let isAbort = fromException e == Just ThreadKilled
                    if isAbort
                        then do
                            let errLogger = Log.withNS (stLogger st) "message"
                            Log.logMsg errLogger Katip.InfoS "agent aborted by user"
                            -- Publish a text part indicating cancellation
                            let cancelPart =
                                    object
                                        [ "id" .= partId
                                        , "sessionID" .= sid
                                        , "messageID" .= aMsgId
                                        , "type" .= ("text" :: Text)
                                        , "text" .= ("[Cancelled by user]" :: Text)
                                        ]
                            Bus.publish (stBus st) "message.part.updated" (object ["part" .= cancelPart])
                            -- Persist the cancelled message
                            let updatedMsg = aMsg{msgParts = [cancelPart]}
                            Storage.writeCached (stDirCache st) (stStorage st) ["message", sid, aMsgId] updatedMsg
                            -- Complete the message normally so session goes back to idle
                            completeMessage st sid aMsgId uMsgId providerId modelId agentName t
                        else do
                            -- Minor #9: Handle streaming errors properly - publish error and complete
                            let errLogger = Log.withNS (stLogger st) "message"
                            Log.logMsg errLogger Katip.ErrorS $ "streaming error: " <> T.pack (show e)
                            let errPart =
                                    object
                                        [ "id" .= partId
                                        , "sessionID" .= sid
                                        , "messageID" .= aMsgId
                                        , "type" .= ("text" :: Text)
                                        , "text" .= ("Error: " <> T.pack (show e))
                                        ]
                            Bus.publish (stBus st) "message.part.updated" (object ["part" .= errPart])
                            -- Minor #10: Persist error parts to storage
                            let updatedMsg = aMsg{msgParts = [errPart]}
                            Storage.writeCached (stDirCache st) (stStorage st) ["message", sid, aMsgId] updatedMsg
                            completeMessage st sid aMsgId uMsgId providerId modelId agentName t

    return aMsg

sessionMessageGetHandler :: AppState -> Text -> Text -> Handler Message
sessionMessageGetHandler st sid msgId = do
    let key = ["message", sid, msgId]
    result <-
        liftIO $
            (Just <$> Storage.read (stStorage st) key)
                `catch` \(Storage.NotFoundError _) -> return Nothing
    case result of
        Nothing -> throwError err404
        Just msg -> return msg

sessionMessagePartDeleteHandler :: AppState -> Text -> Text -> Text -> Handler Bool
sessionMessagePartDeleteHandler st sid msgId partId = do
    let key = ["message", sid, msgId]
    result <- liftIO $ Storage.readMaybe (stStorage st) key
    case result of
        Nothing -> throwError err404
        Just msg -> do
            let updated = Parts.deletePart partId (msgParts msg)
            case updated of
                Nothing -> throwError err404
                Just parts -> do
                    let next = msg{msgParts = parts}
                    liftIO $ Storage.writeCached (stDirCache st) (stStorage st) key next
                    liftIO $ Bus.publish (stBus st) "message.part.removed" (object ["sessionID" .= sid, "messageID" .= msgId, "partID" .= partId])
                    return True

sessionMessagePartUpdateHandler :: AppState -> Text -> Text -> Text -> Value -> Handler Value
sessionMessagePartUpdateHandler st sid msgId partId input = do
    let key = ["message", sid, msgId]
    let bodySession = extractText input "sessionID"
    let bodyMessage = extractText input "messageID"
    let bodyPart = extractText input "id"
    case (bodySession, bodyMessage, bodyPart) of
        (Just s, Just m, Just p) ->
            when (s /= sid || m /= msgId || p /= partId) $ throwError err400
        (Nothing, _, _) -> throwError err400
        (_, Nothing, _) -> throwError err400
        (_, _, Nothing) -> throwError err400
    result <- liftIO $ Storage.readMaybe (stStorage st) key
    case result of
        Nothing -> throwError err404
        Just msg -> do
            let updated = replacePart partId input (msgParts msg)
            case updated of
                Nothing -> throwError err404
                Just parts -> do
                    let next = msg{msgParts = parts}
                    liftIO $ Storage.writeCached (stDirCache st) (stStorage st) key next
                    let mpart = Parts.findPart partId parts
                    case mpart of
                        Nothing -> throwError err404
                        Just part -> do
                            liftIO $ Bus.publish (stBus st) "message.part.updated" (object ["part" .= part])
                            return part
  where
    replacePart pid part parts =
        if any (\p -> partKey p == Just pid) parts
            then Just (map (\p -> if partKey p == Just pid then part else p) parts)
            else Nothing
    partKey (Object obj) = case KM.lookup "id" obj of
        Just (String t) -> Just t
        Just (Object _) -> fromPartID
        Just (Array _) -> fromPartID
        Just (Number _) -> fromPartID
        Just (Bool _) -> fromPartID
        Just Null -> fromPartID
        Nothing -> fromPartID
      where
        fromPartID = case KM.lookup "partID" obj of
            Just (String t) -> Just t
            Just (Object _) -> Nothing
            Just (Array _) -> Nothing
            Just (Number _) -> Nothing
            Just (Bool _) -> Nothing
            Just Null -> Nothing
            Nothing -> Nothing
    partKey (Array _) = Nothing
    partKey (String _) = Nothing
    partKey (Number _) = Nothing
    partKey (Bool _) = Nothing
    partKey Null = Nothing

sessionPromptAsyncHandler :: AppState -> Text -> CreateMessageInput -> Handler NoContent
sessionPromptAsyncHandler st sid input = liftIO $ do
    reqId <- RequestStore.generateId
    let job = PromptAsync.PromptAsyncJob reqId sid input
    let payload = PromptAsync.queuedPayload sid reqId input
    Storage.writeCached (stDirCache st) (stStorage st) (PromptAsync.promptAsyncKey sid reqId) payload
    appendPromptAsyncIndex (stStorage st) sid reqId
    atomically $ writeTQueue (stPromptAsyncQueue st) job
    Bus.publish (stBus st) "prompt.async.queued" payload
    return NoContent

startPromptAsyncWorker :: AppState -> IO ()
startPromptAsyncWorker st = do
    _ <- forkIO $ promptAsyncLoop st
    pure ()

promptAsyncLoop :: AppState -> IO ()
promptAsyncLoop st = do
    job <- atomically $ readTQueue (stPromptAsyncQueue st)
    processPromptAsync st job
    promptAsyncLoop st

processPromptAsync :: AppState -> PromptAsync.PromptAsyncJob -> IO ()
processPromptAsync st job = do
    let sid = PromptAsync.pajSessionId job
    let reqId = PromptAsync.pajRequestId job
    let started = PromptAsync.startedPayload sid reqId
    Storage.writeCached (stDirCache st) (stStorage st) (PromptAsync.promptAsyncKey sid reqId) started
    Bus.publish (stBus st) "prompt.async.started" started
    result <-
        (Just <$> createMessageIO st sid (PromptAsync.pajInput job))
            `catch` \(err :: SomeException) -> do
                let payload = PromptAsync.failedPayload sid reqId (T.pack (show err))
                Storage.writeCached (stDirCache st) (stStorage st) (PromptAsync.promptAsyncKey sid reqId) payload
                Bus.publish (stBus st) "prompt.async.failed" payload
                pure Nothing
    case result of
        Nothing -> pure ()
        Just msg -> do
            let mid = messageInfoId (msgInfo msg)
            let payload = PromptAsync.completedPayload sid reqId mid
            Storage.writeCached (stDirCache st) (stStorage st) (PromptAsync.promptAsyncKey sid reqId) payload
            Bus.publish (stBus st) "prompt.async.completed" payload

appendPromptAsyncIndex :: Storage.StorageConfig -> Text -> Text -> IO ()
appendPromptAsyncIndex storage sid reqId = do
    result <- Storage.readMaybe storage (PromptAsync.promptAsyncIndexKey sid)
    let ids = fromMaybe [] result
        next = if reqId `elem` ids then ids else ids ++ [reqId]
    Storage.write storage (PromptAsync.promptAsyncIndexKey sid) next

-- | Extract text content from user message parts
extractUserText :: [Value] -> Text
extractUserText parts = T.intercalate "\n" $ concatMap extractTextPart parts
  where
    extractTextPart (Object obj) = case KM.lookup "type" obj of
        Just (String "text") -> case KM.lookup "text" obj of
            Just (String txt) -> [txt]
            Just (Object _) -> []
            Just (Array _) -> []
            Just (Number _) -> []
            Just (Bool _) -> []
            Just Null -> []
            Nothing -> []
        Just (String _) -> []
        Just (Object _) -> []
        Just (Array _) -> []
        Just (Number _) -> []
        Just (Bool _) -> []
        Just Null -> []
        Nothing -> []
    extractTextPart (Array _) = []
    extractTextPart (String _) = []
    extractTextPart (Number _) = []
    extractTextPart (Bool _) = []
    extractTextPart Null = []

-- | Generate a unique part/tool ID using lexicographically sortable format
genId :: Identifier.IdGenState -> IO Text
genId idGen = Identifier.ascendingWithPrefix idGen "part"

-- | Parse tool input from JSON string (arguments come as string from OpenAI format)
parseToolInput :: Text -> Value
parseToolInput txt = case Data.Aeson.decodeStrict (TE.encodeUtf8 txt) of
    Just v -> v
    Nothing -> object [] -- fallback to empty object

{- | Maximum characters to include in tool output for display purposes.
Large outputs can cause issues with SSE payloads and client rendering.
-}
maxToolOutputDisplayChars :: Int
maxToolOutputDisplayChars = 100000 -- ~100KB limit for display

{- | Maximum characters to include in tool output for LLM context.
More aggressive truncation to manage token costs and avoid context overflow.
-}
maxToolOutputLLMChars :: Int
maxToolOutputLLMChars = 50000 -- ~50KB limit for LLM context

-- | Truncate tool output for display, keeping the end (most recent output)
truncateToolOutput :: Text -> Text
truncateToolOutput = truncateToolOutputTo maxToolOutputDisplayChars

-- | Truncate tool output for LLM context (more aggressive)
truncateToolOutputForLLM :: Text -> Text
truncateToolOutputForLLM = truncateToolOutputTo maxToolOutputLLMChars

-- | Truncate tool output to a specific character limit
truncateToolOutputTo :: Int -> Text -> Text
truncateToolOutputTo maxChars output
    | T.compareLength output maxChars == GT =
        "...(output truncated, showing last "
            <> T.pack (show maxChars)
            <> " chars)...\n"
            <> T.takeEnd maxChars output
    | otherwise = output

{- | Convert ToolResultMessage to ChatMessage format for continuing the conversation
NOTE: We truncate tool output for LLM context to avoid huge payloads
-}
toolResultToMessage :: OpenRouter.ToolResultMessage -> OpenRouter.ChatMessage
toolResultToMessage trm =
    OpenRouter.toolResultChatMessage (OpenRouter.trmToolCallId trm) (truncateToolOutputForLLM $ OpenRouter.trmContent trm)

-- | Load conversation history from storage (Critical #2)
loadConversationHistory :: AppState -> Text -> IO [Message]
loadConversationHistory st sid = do
    let key = ["message", sid]
    (Storage.list (stStorage st) key >>= mapM (Storage.read (stStorage st)))
        `catch` \(Storage.NotFoundError _) -> return []

-- | Convert a stored Message to OpenRouter.Message format (Critical #2)
messageToOpenRouter :: Message -> OpenRouter.ChatMessage
messageToOpenRouter msg =
    let role = case messageInfoRole (msgInfo msg) of
            "user" -> OpenRouter.User
            "assistant" -> OpenRouter.Assistant
            "system" -> OpenRouter.System
            _otherRole -> OpenRouter.User -- fallback for any other role string
        content = extractUserText (msgParts msg)
     in OpenRouter.simpleMessage role content

{- | Mark message as complete and publish idle event
Minor #8: parentID now correctly references the user message
Major #5: modelId and agentName now come from the request
Also updates the stored message with time.completed so the TUI can see it's done.
-}
completeMessage :: AppState -> Text -> Text -> Text -> Text -> Text -> Text -> Double -> IO ()
completeMessage st sid msgId parentMsgId providerId modelId agentName startTime = do
    let lg = Log.withNS (stLogger st) "message"

    now <- getCurrentTime
    let endTime = realToFrac (utcTimeToPOSIXSeconds now) * 1000 :: Double
    let duration = (endTime - startTime) / 1000 -- seconds
    Log.logMsg lg Katip.InfoS $ "complete session=" <> sid <> " msg=" <> msgId <> " duration=" <> T.pack (show duration) <> "s"

    -- Update the stored message with time.completed
    -- This is important so the TUI sees the message as complete when it syncs
    let msgKey = ["message", sid, msgId]
    mStoredMsg <- Storage.readMaybe (stStorage st) msgKey :: IO (Maybe Message)
    case mStoredMsg of
        Just storedMsg -> do
            -- Update the time in the message info to include completed
            let updatedInfo = case msgInfo storedMsg of
                    AssistantInfo ami ->
                        AssistantInfo ami{amiTime = MessageTime startTime (Just endTime), amiFinish = Just "end_turn"}
                    other -> other
            let updatedMsg = storedMsg{msgInfo = updatedInfo}
            Storage.writeCached (stDirCache st) (stStorage st) msgKey updatedMsg
        Nothing -> Log.logMsg lg Katip.WarningS $ "Could not find message to mark complete: " <> msgId

    -- Publish completed message info
    let completedInfo =
            object
                [ "id" .= msgId
                , "sessionID" .= sid
                , "role" .= ("assistant" :: Text)
                , "time" .= object ["created" .= startTime, "completed" .= endTime]
                , "parentID" .= parentMsgId
                , "modelID" .= modelId
                , "providerID" .= providerId
                , "mode" .= ("build" :: Text)
                , "agent" .= agentName
                , "path" .= object ["cwd" .= stDirectory st, "root" .= stDirectory st]
                , "cost" .= (0 :: Double)
                , "tokens"
                    .= object
                        [ "input" .= (0 :: Int)
                        , "output" .= (0 :: Int)
                        , "reasoning" .= (0 :: Int)
                        , "cache" .= object ["read" .= (0 :: Int), "write" .= (0 :: Int)]
                        ]
                , "finish" .= ("end_turn" :: Text)
                ]
    Bus.publish (stBus st) "message.updated" (object ["info" .= completedInfo])

    -- Publish session.status idle (for TUI to know processing is done)
    Bus.publish (stBus st) "session.status" $
        object
            [ "sessionID" .= sid
            , "status" .= object ["type" .= ("idle" :: Text)]
            ]

    -- Publish session idle (deprecated but kept for compatibility)
    Bus.publish (stBus st) "session.idle" (object ["sessionID" .= sid])

-- * File Handlers

fileListHandler :: Maybe Text -> Text -> Handler [FileNode]
fileListHandler mDir path = liftIO $ do
    fullPath <- resolvePath mDir path
    exists <- doesDirectoryExist fullPath
    if not exists
        then return []
        else do
            contents <- listDirectory fullPath
            forM contents $ \name -> do
                let itemPath = fullPath </> name
                isDir <- doesDirectoryExist itemPath
                let type_ = if isDir then FileTypeDirectory else FileTypeFile
                let relPath =
                        if unpack path == "" || unpack path == "." || unpack path == "/"
                            then name
                            else unpack path </> name
                return $
                    FileNode
                        { fnName = pack name
                        , fnPath = pack relPath
                        , fnAbsolute = pack itemPath
                        , fnType = type_
                        , fnIgnored = False
                        }

fileReadHandler :: Maybe Text -> Text -> Handler FileContent
fileReadHandler mDir path = do
    -- Validate path is not empty
    when (T.null path) $
        throwError err400{errBody = "Path cannot be empty"}
    fullPath <- liftIO $ resolvePath mDir path
    -- Validate path is a file, not a directory
    isDir <- liftIO $ doesDirectoryExist fullPath
    when isDir $
        throwError err400{errBody = "Path is a directory, not a file"}
    -- Check file exists
    exists <- liftIO $ doesFileExist fullPath
    unless exists $
        throwError err404{errBody = "File not found"}
    bytes <- liftIO $ BS.readFile fullPath
    case (hasNull bytes, TE.decodeUtf8' bytes) of
        (True, _) -> return $ FileContent ContentTypeBinary (encodeBase64 bytes)
        (False, Left _) -> return $ FileContent ContentTypeBinary (encodeBase64 bytes)
        (False, Right text) -> return $ FileContent ContentTypeText text

hasNull :: BS.ByteString -> Bool
hasNull = BS.any (== 0)

encodeBase64 :: BS.ByteString -> Text
encodeBase64 = TE.decodeUtf8 . B64.encode

-- * Stubs

lspHandler :: AppState -> Handler [Value]
lspHandler st = liftIO $ do
    LspStore.getDiagnostics (stStorage st)

vcsHandler :: AppState -> Handler VcsInfo
vcsHandler st = liftIO $ do
    let root = unpack (stDirectory st)
    branchName <- VcsStatus.loadBranch (stExeCache st) root
    return $ VcsInfo branchName

permissionHandler :: AppState -> Maybe Text -> Handler [Value]
permissionHandler st _mDir = liftIO $ do
    RequestStore.listRequests (stStorage st) "permission"

questionHandler :: AppState -> Maybe Text -> Handler [Value]
questionHandler st _mDir = liftIO $ do
    RequestStore.listRequests (stStorage st) "question"

-- | Generic handler for request responses (question/permission reply/reject)
requestResponseHandler :: Text -> Text -> Text -> Text -> AppState -> Text -> Maybe Text -> Value -> Handler Bool
requestResponseHandler requestType responseKey status eventName st rid _mDir input = liftIO $ do
    let payload = object ["requestID" .= rid, K.fromText responseKey .= input, "status" .= status]
    RequestStore.writeRequest (stStorage st) requestType rid payload
    Bus.publish (stBus st) eventName payload
    return True

questionReplyHandler :: AppState -> Text -> Maybe Text -> Value -> Handler Bool
questionReplyHandler = requestResponseHandler "question" "reply" "replied" "question.replied"

questionRejectHandler :: AppState -> Text -> Maybe Text -> Value -> Handler Bool
questionRejectHandler = requestResponseHandler "question" "reject" "rejected" "question.rejected"

permissionReplyHandler :: AppState -> Text -> Maybe Text -> Value -> Handler Bool
permissionReplyHandler = requestResponseHandler "permission" "reply" "replied" "permission.replied"

findHandler :: AppState -> Maybe Text -> Maybe Text -> Maybe Text -> Handler [Value]
findHandler st mQuery mPattern mDir = liftIO $ do
    let root = maybe (unpack (stDirectory st)) unpack mDir
    case (mQuery, mPattern) of
        (Just q, _) -> FindSearch.findText root q
        (Nothing, Just p) -> FindSearch.findText root p
        (Nothing, Nothing) -> pure []

findFileHandler :: AppState -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Int -> Handler [Value]
findFileHandler st mPattern mDir mDirs mType mLimit = liftIO $ do
    let root = maybe (unpack (stDirectory st)) unpack mDir
    let opts =
            FindSearch.FindFileOptions
                { FindSearch.ffoIncludeDirs = fromMaybe False mDirs
                , FindSearch.ffoFileType = mType
                , FindSearch.ffoLimit = mLimit
                }
    case mPattern of
        Nothing -> pure []
        Just p -> FindSearch.findFileWithOptions root p opts

findSymbolHandler :: AppState -> Maybe Text -> Maybe Text -> Handler [Value]
findSymbolHandler st mQuery mDir = liftIO $ do
    let root = maybe (unpack (stDirectory st)) unpack mDir
    case mQuery of
        Nothing -> pure []
        Just q -> FindSearch.findSymbol root q

fileStatusHandler :: AppState -> Maybe Text -> Maybe Text -> Handler [Value]
fileStatusHandler st mDir mPath = liftIO $ do
    let base = maybe (unpack (stDirectory st)) unpack mDir
    statuses <- VcsStatus.loadStatus (stExeCache st) base
    case mPath of
        Nothing -> return $ map Data.Aeson.toJSON statuses
        Just path -> do
            let filtered = filter (\s -> VcsStatus.fsPath s == path) statuses
            case filtered of
                [] -> do
                    fullPath <- resolvePath mDir path
                    exists <- doesFileExist fullPath
                    return [object ["path" .= path, "status" .= ("clean" :: Text), "exists" .= exists]]
                (_x : _xs) -> return $ map Data.Aeson.toJSON filtered

tuiAppendPromptHandler :: AppState -> Maybe Text -> Value -> Handler Bool
tuiAppendPromptHandler st _mDir input = liftIO $ do
    let text = case extractText input "text" of
            Just t -> t
            Nothing -> fromMaybe "" (extractText input "prompt")
    prompt <- TuiStore.appendPrompt (stStorage st) text
    let payload = object ["prompt" .= prompt]
    Bus.publish (stBus st) "tui.append-prompt" payload
    return True

tuiOpenHandler :: AppState -> Text -> Maybe Text -> Handler Bool
tuiOpenHandler st name _mDir = liftIO $ do
    let payload = object ["panel" .= name]
    TuiStore.setLast (stStorage st) payload
    Bus.publish (stBus st) ("tui." <> name) payload
    return True

tuiSubmitPromptHandler :: AppState -> Maybe Text -> Handler Bool
tuiSubmitPromptHandler st _mDir = liftIO $ do
    prompt <- TuiStore.submitPrompt (stStorage st)
    let payload = object ["prompt" .= prompt]
    Bus.publish (stBus st) "tui.submit-prompt" payload
    return True

tuiClearPromptHandler :: AppState -> Maybe Text -> Handler Bool
tuiClearPromptHandler st _mDir = liftIO $ do
    TuiStore.clearPrompt (stStorage st)
    Bus.publish (stBus st) "tui.clear-prompt" (object [])
    return True

-- | Generic TUI handler that stores input and publishes an event
tuiGenericHandler :: Text -> AppState -> Maybe Text -> Value -> Handler Bool
tuiGenericHandler eventName st _mDir input = liftIO $ do
    TuiStore.setLast (stStorage st) input
    Bus.publish (stBus st) eventName (object ["payload" .= input])
    return True

tuiExecuteCommandHandler :: AppState -> Maybe Text -> Value -> Handler Bool
tuiExecuteCommandHandler = tuiGenericHandler "tui.execute-command"

tuiShowToastHandler :: AppState -> Maybe Text -> Value -> Handler Bool
tuiShowToastHandler = tuiGenericHandler "tui.show-toast"

tuiPublishHandler :: AppState -> Maybe Text -> Value -> Handler Bool
tuiPublishHandler = tuiGenericHandler "tui.publish"

tuiSelectSessionHandler :: AppState -> Maybe Text -> Value -> Handler Bool
tuiSelectSessionHandler = tuiGenericHandler "tui.select-session"

tuiControlHandler :: AppState -> Text -> Maybe Text -> Value -> Handler Bool
tuiControlHandler st name _mDir input = liftIO $ do
    let payload = object ["control" .= name, "payload" .= input]
    TuiStore.setLast (stStorage st) payload
    Bus.publish (stBus st) ("tui.control." <> name) payload
    return True

instanceDisposeHandler :: AppState -> Handler Bool
instanceDisposeHandler st = liftIO $ do
    for_ (stProxy st) Proxy.stop
    Bus.publish (stBus st) "server.instance.disposed" (object [])
    return True

-- | Handler for /global/dispose (same as /instance/dispose)
globalDisposeHandler :: AppState -> Handler Bool
globalDisposeHandler = instanceDisposeHandler

-- | Handler for /event - accepts directory query param to filter events
eventHandler :: AppState -> Tagged Handler Application
eventHandler = Event.eventHandler

logHandler :: AppState -> Maybe Text -> Value -> Handler Bool
logHandler st _mDir input = liftIO $ do
    let lg = Log.withNS (stLogger st) "client"
    Log.logMsg lg Katip.InfoS $ "log " <> T.pack (show input)
    return True

skillHandler :: AppState -> Maybe Text -> Handler [Skill.SkillInfo]
skillHandler st mDir = liftIO $ do
    let dir = maybe (unpack (stDirectory st)) unpack mDir
    Skill.listSkills (stDhallCache st) dir

formatterHandler :: AppState -> Maybe Text -> Handler [Formatter.FormatterStatus]
formatterHandler st mDir = liftIO $ do
    let dir = maybe (unpack (stDirectory st)) unpack mDir
    Formatter.statusFor (stDhallCache st) (stExeCache st) dir

experimentalToolIdsHandler :: Handler [Text]
experimentalToolIdsHandler = return $ map ToolT.tdName Tool.allTools

-- | List tools with JSON schema for a specific provider/model (GET /experimental/tool)
experimentalToolListHandler :: AppState -> Text -> Text -> Maybe Text -> Handler [Value]
experimentalToolListHandler _st _provider _model _mDir = liftIO $ do
    -- Return tool definitions with their JSON schemas
    return Tool.toolDefinitions

experimentalToolHandler :: AppState -> Value -> Handler Value
experimentalToolHandler st input = liftIO $ do
    let name = fromMaybe "unknown" (extractText input "name")
    let payload = object ["name" .= name, "input" .= input]
    RequestStore.writeRequest (stStorage st) "experimental-tool" name payload
    return payload

experimentalWorktreeGetHandler :: AppState -> Maybe Text -> Handler [Text]
experimentalWorktreeGetHandler _st _mDir = liftIO $ do
    -- Return empty list - worktree listing not yet implemented
    return []

experimentalWorktreePostHandler :: AppState -> Value -> Handler Value
experimentalWorktreePostHandler st input = liftIO $ do
    Worktree.setInfo (stStorage st) input

experimentalWorktreeResetHandler :: AppState -> Maybe Text -> Handler Bool
experimentalWorktreeResetHandler st _mDir = liftIO $ do
    _ <- Worktree.resetInfo (stStorage st) (stDirectory st)
    return True

-- | Delete a worktree and its branch (DELETE /experimental/worktree)
experimentalWorktreeDeleteHandler :: AppState -> Maybe Text -> Value -> Handler Bool
experimentalWorktreeDeleteHandler st mDir input = liftIO $ do
    -- Use query param directory if provided, otherwise extract from body
    let dir = mDir <|> extractText input "directory"
    result <- Worktree.remove (stStorage st) (stDirectory st) dir
    case result of
        Left _err -> return False
        Right () -> return True

-- | List sessions globally across all projects (GET /experimental/session)
experimentalSessionListHandler ::
    AppState ->
    Maybe Text -> -- directory
    Maybe Bool -> -- roots
    Maybe Double -> -- start
    Maybe Double -> -- cursor
    Maybe Text -> -- search
    Maybe Int -> -- limit
    Maybe Bool -> -- archived
    Handler [GlobalSession]
experimentalSessionListHandler st mDir mRoots mStart mCursor mSearch mLimit mArchived = liftIO $ do
    let ctx = sessionContext st
    Sess.listGlobal ctx mDir mRoots mStart mCursor mSearch mLimit mArchived

-- * PTY Handlers (sandboxed terminals)

ptyListHandler :: AppState -> Handler [Value]
ptyListHandler st = liftIO $ do
    sessions <- Pty.list (stPtyManager st)
    return $ map Data.Aeson.toJSON sessions

ptyCreateHandler :: AppState -> Value -> Handler Value
ptyCreateHandler st input = liftIO $ do
    let ptyInput = PtyParse.parseInput input
    result <- Pty.create (stPtyManager st) ptyInput
    case result of
        Left err -> return $ errorResponse err
        Right info -> do
            -- Publish event
            Bus.publish (stBus st) "pty.created" (object ["info" .= info])
            return $ Data.Aeson.toJSON info

ptyGetHandler :: AppState -> Text -> Handler Value
ptyGetHandler st ptyId = liftIO $ do
    mInfo <- Pty.get (stPtyManager st) ptyId
    case mInfo of
        Nothing -> return $ errorResponse "PTY not found"
        Just info -> return $ Data.Aeson.toJSON info

ptyUpdateHandler :: AppState -> Text -> Value -> Handler Value
ptyUpdateHandler st ptyId input = liftIO $ do
    let parseInput = case Data.Aeson.fromJSON input of
            Data.Aeson.Success i -> Just i
            Data.Aeson.Error _errMsg -> Nothing

    case parseInput of
        Nothing -> return $ errorResponse "Invalid input"
        Just updateInput -> do
            mInfo <- Pty.update (stPtyManager st) ptyId updateInput
            case mInfo of
                Nothing -> return $ errorResponse "PTY not found"
                Just info -> do
                    Bus.publish (stBus st) "pty.updated" (object ["info" .= info])
                    return $ Data.Aeson.toJSON info

ptyDeleteHandler :: AppState -> Text -> Handler Bool
ptyDeleteHandler st ptyId = liftIO $ do
    success <- Pty.remove (stPtyManager st) ptyId
    when success $
        Bus.publish (stBus st) "pty.deleted" (object ["id" .= ptyId])
    return success

-- | Commit sandbox changes to real filesystem
ptyCommitHandler :: AppState -> Text -> Handler Value
ptyCommitHandler st ptyId = liftIO $ do
    result <- Pty.commitChanges (stPtyManager st) ptyId
    case result of
        Left err -> return $ errorResponse err
        Right () -> do
            Bus.publish (stBus st) "pty.committed" (object ["id" .= ptyId])
            return $ object ["success" .= True, "id" .= ptyId]

-- | Get list of changed files in sandbox
ptyChangesHandler :: AppState -> Text -> Handler Value
ptyChangesHandler st ptyId = liftIO $ do
    result <- Pty.getChangedFiles (stPtyManager st) ptyId
    case result of
        Left err -> return $ errorResponse err
        Right files -> return $ object ["id" .= ptyId, "changes" .= map pack files]

-- * LLM Handlers

-- | Simple chat completion handler for testing LLM integration
chatHandler :: AppState -> ChatInput -> Handler Value
chatHandler st input = liftIO $ do
    let model = fromMaybe "anthropic/claude-sonnet-4-20250514" (ciModel input)
    if "anthropic/" `T.isPrefixOf` model
        then chatWithAnthropic (stStorage st) model (ciMessage input)
        else chatWithOpenRouter (stStorage st) model (ciMessage input)

-- | Chat using Anthropic API
chatWithAnthropic :: Storage.StorageConfig -> Text -> Text -> IO Value
chatWithAnthropic storage model message = do
    mApiKey <- Provider.getApiKey storage "anthropic"
    case mApiKey of
        Nothing -> return $ errorResponse "No Anthropic API key configured. Set ANTHROPIC_API_KEY or add via provider auth."
        Just key -> do
            client <- Anthropic.newClient key
            let request =
                    LLMTypes.ChatRequest
                        { LLMTypes.crModel = dropPrefix "anthropic/" model
                        , LLMTypes.crMessages = [LLMTypes.Message LLMTypes.User (LLMTypes.SimpleContent message)]
                        , LLMTypes.crMaxTokens = 1024
                        , LLMTypes.crSystem = Nothing
                        , LLMTypes.crTemperature = Nothing
                        , LLMTypes.crTools = Nothing
                        , LLMTypes.crStream = False
                        }
            result <- Anthropic.chat client request
            case result of
                Left err -> return $ errorResponse err
                Right resp ->
                    let content = case LLMTypes.respContent resp of
                            (LLMTypes.TextBlock t : _rest) -> t
                            [] -> ""
                            (LLMTypes.ImageBlock{} : _rest) -> ""
                            (LLMTypes.ToolUseBlock{} : _rest) -> ""
                            (LLMTypes.ToolResultBlock{} : _rest) -> ""
                     in return $
                            object
                                [ "id" .= LLMTypes.respId resp
                                , "model" .= LLMTypes.respModel resp
                                , "content" .= content
                                , "usage" .= LLMTypes.respUsage resp
                                ]

-- | Chat using OpenRouter API
chatWithOpenRouter :: Storage.StorageConfig -> Text -> Text -> IO Value
chatWithOpenRouter storage model message = do
    mApiKey <- Provider.getApiKey storage "openrouter"
    case mApiKey of
        Nothing -> return $ errorResponse "No OpenRouter API key configured. Set OPENROUTER_API_KEY or add via provider auth."
        Just key -> do
            client <- OpenRouter.newClient key
            let request =
                    OpenRouter.ChatRequest
                        { OpenRouter.crModel = dropPrefix "openrouter/" model
                        , OpenRouter.crMessages = [OpenRouter.simpleMessage OpenRouter.User message]
                        , OpenRouter.crMaxTokens = Just 1024
                        , OpenRouter.crTemperature = Nothing
                        , OpenRouter.crStream = False
                        , OpenRouter.crTools = Nothing
                        }
            result <- OpenRouter.chat client request
            case result of
                Left err -> return $ errorResponse err
                Right resp ->
                    let content = case OpenRouter.respChoices resp of
                            (c : _) -> fromMaybe "" (OpenRouter.msgContent (OpenRouter.choiceMessage c))
                            [] -> ""
                     in return $
                            object
                                [ "id" .= OpenRouter.respId resp
                                , "model" .= OpenRouter.respModel resp
                                , "content" .= content
                                , "usage" .= OpenRouter.respUsage resp
                                ]

dropPrefix :: Text -> Text -> Text
dropPrefix prefix value = fromMaybe value (T.stripPrefix prefix value)

-- | Server Wiring - combines all handlers into a Servant Server
server :: AppState -> Server OpencodeAPI
server st =
    healthHandler st
        :<|> pathHandler st
        :<|> globalConfigHandler st
        :<|> projectListHandler st
        :<|> projectGetHandler st
        :<|> projectUpdateHandler st
        :<|> projectCurrentHandler st
        :<|> providerListHandler st
        :<|> providerAuthHandler st
        :<|> providerHandler st
        :<|> providerOauthAuthorizeHandler st
        :<|> providerOauthCallbackHandler st
        :<|> authCreateHandler st
        :<|> authUpdateHandler st
        :<|> authDeleteHandler st
        :<|> agentHandler
        :<|> configHandler st
        :<|> commandHandler
        :<|> sessionStatusHandler st
        :<|> sessionListHandler st
        :<|> sessionCreateHandler st
        :<|> sessionGetHandler st
        :<|> sessionDeleteHandler st
        :<|> sessionUpdateHandler st
        :<|> sessionChildrenHandler st
        :<|> sessionTodoHandler st
        :<|> sessionInitHandler st
        :<|> sessionForkHandler st
        :<|> sessionAbortHandler st
        :<|> sessionShareCreateHandler st
        :<|> sessionShareDeleteHandler st
        :<|> sessionDiffHandler st
        :<|> sessionSummarizeHandler st
        :<|> sessionCommandHandler st
        :<|> sessionShellHandler st
        :<|> sessionRevertHandler st
        :<|> sessionUnrevertHandler st
        :<|> sessionPermissionHandler st
        :<|> sessionMessageListHandler st
        :<|> sessionMessageCreateHandler st
        :<|> sessionMessageGetHandler st
        :<|> sessionMessagePartDeleteHandler st
        :<|> sessionMessagePartUpdateHandler st
        :<|> sessionPromptAsyncHandler st
        :<|> lspHandler st
        :<|> vcsHandler st
        :<|> permissionHandler st
        :<|> permissionReplyHandler st
        :<|> questionHandler st
        :<|> questionReplyHandler st
        :<|> questionRejectHandler st
        :<|> findHandler st
        :<|> findFileHandler st
        :<|> findSymbolHandler st
        :<|> fileListHandler
        :<|> fileReadHandler
        :<|> fileStatusHandler st
        :<|> Event.globalEventHandler st
        -- PTY handlers
        :<|> ptyListHandler st
        :<|> ptyCreateHandler st
        :<|> ptyGetHandler st
        :<|> ptyUpdateHandler st
        :<|> ptyDeleteHandler st
        :<|> PtyConnect.ptyConnectHandler st
        :<|> ptyCommitHandler st
        :<|> ptyChangesHandler st
        -- TUI handlers
        :<|> tuiAppendPromptHandler st
        :<|> tuiOpenHandler st "open-help"
        :<|> tuiOpenHandler st "open-sessions"
        :<|> tuiOpenHandler st "open-themes"
        :<|> tuiOpenHandler st "open-models"
        :<|> tuiSubmitPromptHandler st
        :<|> tuiClearPromptHandler st
        :<|> tuiExecuteCommandHandler st
        :<|> tuiShowToastHandler st
        :<|> tuiPublishHandler st
        :<|> tuiSelectSessionHandler st
        :<|> tuiControlHandler st "next"
        :<|> tuiControlHandler st "response"
        :<|> instanceDisposeHandler st
        :<|> globalDisposeHandler st
        :<|> eventHandler st
        :<|> logHandler st
        :<|> skillHandler st
        :<|> formatterHandler st
        :<|> experimentalToolIdsHandler
        :<|> experimentalToolListHandler st
        :<|> experimentalToolHandler st
        :<|> experimentalWorktreeGetHandler st
        :<|> experimentalWorktreePostHandler st
        :<|> experimentalWorktreeResetHandler st
        :<|> experimentalWorktreeDeleteHandler st
        :<|> experimentalSessionListHandler st
        -- LLM
        :<|> chatHandler st
