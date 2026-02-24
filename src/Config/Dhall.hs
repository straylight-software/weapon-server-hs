{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Dhall configuration loader
Loads and evaluates Dhall configuration files with defaults.

Uses MVar-based caching to avoid re-parsing Dhall files on every call.
The cache is initialized once per application and passed through AppState.
-}
module Config.Dhall (
    -- * Cache
    DhallCache,
    newDhallCache,

    -- * Loading (all cached)
    loadConfigCached,
    loadConfigFromFileCached,

    -- * Paths
    globalConfigPath,
    projectConfigPath,

    -- * Merging
    mergeConfigs,
) where

import Config.Types
import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (SomeException, try)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Dhall (auto, input, inputFile)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

-- | Get global config path
globalConfigPath :: IO FilePath
globalConfigPath = do
    home <- getHomeDirectory
    pure $ home </> ".config" </> "weapon" </> "weapon.dhall"

-- | Get project config path
projectConfigPath :: FilePath -> FilePath
projectConfigPath dir = dir </> "weapon.dhall"

-- | Get defaults path (shipped with binary)
defaultsPath :: FilePath
defaultsPath = "dhall/Defaults.dhall"

-- ════════════════════════════════════════════════════════════════════════════
-- Dhall Cache
-- ════════════════════════════════════════════════════════════════════════════

{- | Cache for parsed Dhall configurations.
Stores both the defaults and any loaded config files.
Thread-safe via MVar.
-}
data DhallCache = DhallCache
    { dcDefaults :: MVar (Maybe Config)
    -- ^ Cached defaults (Nothing = not yet loaded)
    , dcFiles :: MVar (Map FilePath (Maybe Config))
    -- ^ Cached config files by path
    }

-- | Create a new empty Dhall cache
newDhallCache :: IO DhallCache
newDhallCache = do
    defaults <- newMVar Nothing
    files <- newMVar Map.empty
    pure $ DhallCache defaults files

-- ════════════════════════════════════════════════════════════════════════════
-- Cached Loading Functions
-- ════════════════════════════════════════════════════════════════════════════

{- | Load defaults from Dhall file (cached).

This function caches the result - the Dhall file is parsed only once
per cache instance, regardless of how many times it's called.
-}
loadDefaultsCached :: DhallCache -> IO Config
loadDefaultsCached cache = modifyMVar (dcDefaults cache) $ \case
    Just cfg -> pure (Just cfg, cfg)
    Nothing -> do
        cfg <- loadDefaults
        pure (Just cfg, cfg)

{- | Load config from a specific Dhall file (cached).

Results are cached by filepath - each file is parsed only once per cache.
Returns Nothing if the file doesn't exist or fails to parse.
-}
loadConfigFromFileCached :: DhallCache -> FilePath -> IO (Maybe Config)
loadConfigFromFileCached cache path = modifyMVar (dcFiles cache) $ \files ->
    case Map.lookup path files of
        Just cfg -> pure (files, cfg)
        Nothing -> do
            cfg <- loadConfigFromFile path
            pure (Map.insert path cfg files, cfg)

-- | Load full config (global + project + defaults) using cache.
loadConfigCached :: DhallCache -> FilePath -> IO Config
loadConfigCached cache projectDir = do
    -- Load built-in defaults (cached)
    defaults <- loadDefaultsCached cache

    -- Load global config (cached)
    globalPath <- globalConfigPath
    globalCfg <- loadConfigFromFileCached cache globalPath

    -- Load project config (cached)
    let projectPath = projectConfigPath projectDir
    projectCfg <- loadConfigFromFileCached cache projectPath

    -- Merge: defaults <- global <- project
    let withGlobal = maybe defaults (mergeConfigs defaults) globalCfg
    let final = maybe withGlobal (mergeConfigs withGlobal) projectCfg

    pure final

-- ════════════════════════════════════════════════════════════════════════════
-- Uncached Loading Functions (for backwards compatibility)
-- ════════════════════════════════════════════════════════════════════════════

-- | Load defaults from Dhall file (uncached)
loadDefaults :: IO Config
loadDefaults = do
    exists <- doesFileExist defaultsPath
    if exists
        then do
            result <- try (inputFile auto defaultsPath) :: IO (Either SomeException Config)
            case result of
                Left _err -> pure defaultConfig
                Right cfg -> pure cfg
        else pure defaultConfig

-- | Load config from a specific Dhall file (uncached)
loadConfigFromFile :: FilePath -> IO (Maybe Config)
loadConfigFromFile path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            result <- try (inputFile auto path) :: IO (Either SomeException Config)
            case result of
                Left _err -> pure Nothing
                Right cfg -> pure (Just cfg)

-- | Load config with Dhall expression (for inline config)
_loadConfigFromText :: Text -> IO (Maybe Config)
_loadConfigFromText expr = do
    result <- try (input auto expr) :: IO (Either SomeException Config)
    case result of
        Left _err -> pure Nothing
        Right cfg -> pure (Just cfg)

-- | Merge two configs (second overrides first)
mergeConfigs :: Config -> Config -> Config
mergeConfigs base override =
    Config
        { cfgModel = cfgModel override <|> cfgModel base
        , cfgSystemPrompt = cfgSystemPrompt override <|> cfgSystemPrompt base
        , cfgMaxTokens = cfgMaxTokens override <|> cfgMaxTokens base
        , cfgLogLevel = cfgLogLevel override <|> cfgLogLevel base
        , cfgKeybinds = mergeKeybinds (cfgKeybinds base) (cfgKeybinds override)
        , cfgServer = mergeServer (cfgServer base) (cfgServer override)
        , cfgTui = mergeTUI (cfgTui base) (cfgTui override)
        , cfgPermission = mergePermission (cfgPermission base) (cfgPermission override)
        , cfgCompaction = mergeCompaction (cfgCompaction base) (cfgCompaction override)
        , cfgExperimental = mergeExperimental (cfgExperimental base) (cfgExperimental override)
        , cfgEnterprise = mergeEnterprise (cfgEnterprise base) (cfgEnterprise override)
        , cfgWatcher = mergeWatcher (cfgWatcher base) (cfgWatcher override)
        , cfgAgent = cfgAgent override <|> cfgAgent base
        , cfgProvider = cfgProvider override <|> cfgProvider base
        , cfgMcp = cfgMcp override <|> cfgMcp base
        , cfgFormatter = cfgFormatter override <|> cfgFormatter base
        , cfgLsp = cfgLsp override <|> cfgLsp base
        , cfgSkill = cfgSkill override <|> cfgSkill base
        , cfgCommand = cfgCommand override <|> cfgCommand base
        , cfgTheme = cfgTheme override <|> cfgTheme base
        , cfgThemes = cfgThemes override <|> cfgThemes base
        , cfgShare = cfgShare override <|> cfgShare base
        , cfgAutoUpdate = cfgAutoUpdate override <|> cfgAutoUpdate base
        , cfgDisabledTools = cfgDisabledTools override <|> cfgDisabledTools base
        , cfgInstrumentation = cfgInstrumentation override <|> cfgInstrumentation base
        }

-- | Merge keybinds (override wins for each field)
mergeKeybinds :: KeybindsConfig -> KeybindsConfig -> KeybindsConfig
mergeKeybinds base override =
    KeybindsConfig
        { kbLeader = kbLeader override <|> kbLeader base
        , kbAppExit = kbAppExit override <|> kbAppExit base
        , kbEditorOpen = kbEditorOpen override <|> kbEditorOpen base
        , kbThemeList = kbThemeList override <|> kbThemeList base
        , kbSidebarToggle = kbSidebarToggle override <|> kbSidebarToggle base
        , kbScrollbarToggle = kbScrollbarToggle override <|> kbScrollbarToggle base
        , kbUsernameToggle = kbUsernameToggle override <|> kbUsernameToggle base
        , kbStatusView = kbStatusView override <|> kbStatusView base
        , kbSessionExport = kbSessionExport override <|> kbSessionExport base
        , kbSessionNew = kbSessionNew override <|> kbSessionNew base
        , kbSessionList = kbSessionList override <|> kbSessionList base
        , kbSessionTimeline = kbSessionTimeline override <|> kbSessionTimeline base
        , kbSessionFork = kbSessionFork override <|> kbSessionFork base
        , kbSessionRename = kbSessionRename override <|> kbSessionRename base
        , kbSessionDelete = kbSessionDelete override <|> kbSessionDelete base
        , kbStashDelete = kbStashDelete override <|> kbStashDelete base
        , kbModelProviderList = kbModelProviderList override <|> kbModelProviderList base
        , kbModelFavoriteToggle = kbModelFavoriteToggle override <|> kbModelFavoriteToggle base
        , kbSessionShare = kbSessionShare override <|> kbSessionShare base
        , kbSessionUnshare = kbSessionUnshare override <|> kbSessionUnshare base
        , kbSessionInterrupt = kbSessionInterrupt override <|> kbSessionInterrupt base
        , kbSessionCompact = kbSessionCompact override <|> kbSessionCompact base
        , kbMessagesPageUp = kbMessagesPageUp override <|> kbMessagesPageUp base
        , kbMessagesPageDown = kbMessagesPageDown override <|> kbMessagesPageDown base
        , kbMessagesLineUp = kbMessagesLineUp override <|> kbMessagesLineUp base
        , kbMessagesLineDown = kbMessagesLineDown override <|> kbMessagesLineDown base
        , kbMessagesHalfPageUp = kbMessagesHalfPageUp override <|> kbMessagesHalfPageUp base
        , kbMessagesHalfPageDown = kbMessagesHalfPageDown override <|> kbMessagesHalfPageDown base
        , kbMessagesFirst = kbMessagesFirst override <|> kbMessagesFirst base
        , kbMessagesLast = kbMessagesLast override <|> kbMessagesLast base
        , kbMessagesNext = kbMessagesNext override <|> kbMessagesNext base
        , kbMessagesPrevious = kbMessagesPrevious override <|> kbMessagesPrevious base
        , kbMessagesLastUser = kbMessagesLastUser override <|> kbMessagesLastUser base
        , kbMessagesCopy = kbMessagesCopy override <|> kbMessagesCopy base
        , kbMessagesUndo = kbMessagesUndo override <|> kbMessagesUndo base
        , kbMessagesRedo = kbMessagesRedo override <|> kbMessagesRedo base
        , kbMessagesToggleConceal = kbMessagesToggleConceal override <|> kbMessagesToggleConceal base
        , kbToolDetails = kbToolDetails override <|> kbToolDetails base
        , kbModelList = kbModelList override <|> kbModelList base
        , kbModelCycleRecent = kbModelCycleRecent override <|> kbModelCycleRecent base
        , kbModelCycleRecentReverse = kbModelCycleRecentReverse override <|> kbModelCycleRecentReverse base
        , kbModelCycleFavorite = kbModelCycleFavorite override <|> kbModelCycleFavorite base
        , kbModelCycleFavoriteReverse = kbModelCycleFavoriteReverse override <|> kbModelCycleFavoriteReverse base
        , kbCommandList = kbCommandList override <|> kbCommandList base
        , kbAgentList = kbAgentList override <|> kbAgentList base
        , kbAgentCycle = kbAgentCycle override <|> kbAgentCycle base
        , kbAgentCycleReverse = kbAgentCycleReverse override <|> kbAgentCycleReverse base
        , kbVariantCycle = kbVariantCycle override <|> kbVariantCycle base
        , kbInputClear = kbInputClear override <|> kbInputClear base
        , kbInputPaste = kbInputPaste override <|> kbInputPaste base
        , kbInputSubmit = kbInputSubmit override <|> kbInputSubmit base
        , kbInputNewline = kbInputNewline override <|> kbInputNewline base
        , kbInputMoveLeft = kbInputMoveLeft override <|> kbInputMoveLeft base
        , kbInputMoveRight = kbInputMoveRight override <|> kbInputMoveRight base
        , kbInputMoveUp = kbInputMoveUp override <|> kbInputMoveUp base
        , kbInputMoveDown = kbInputMoveDown override <|> kbInputMoveDown base
        , kbInputSelectLeft = kbInputSelectLeft override <|> kbInputSelectLeft base
        , kbInputSelectRight = kbInputSelectRight override <|> kbInputSelectRight base
        , kbInputSelectUp = kbInputSelectUp override <|> kbInputSelectUp base
        , kbInputSelectDown = kbInputSelectDown override <|> kbInputSelectDown base
        , kbInputLineHome = kbInputLineHome override <|> kbInputLineHome base
        , kbInputLineEnd = kbInputLineEnd override <|> kbInputLineEnd base
        , kbInputSelectLineHome = kbInputSelectLineHome override <|> kbInputSelectLineHome base
        , kbInputSelectLineEnd = kbInputSelectLineEnd override <|> kbInputSelectLineEnd base
        , kbInputVisualLineHome = kbInputVisualLineHome override <|> kbInputVisualLineHome base
        , kbInputVisualLineEnd = kbInputVisualLineEnd override <|> kbInputVisualLineEnd base
        , kbInputSelectVisualLineHome = kbInputSelectVisualLineHome override <|> kbInputSelectVisualLineHome base
        , kbInputSelectVisualLineEnd = kbInputSelectVisualLineEnd override <|> kbInputSelectVisualLineEnd base
        , kbInputBufferHome = kbInputBufferHome override <|> kbInputBufferHome base
        , kbInputBufferEnd = kbInputBufferEnd override <|> kbInputBufferEnd base
        , kbInputSelectBufferHome = kbInputSelectBufferHome override <|> kbInputSelectBufferHome base
        , kbInputSelectBufferEnd = kbInputSelectBufferEnd override <|> kbInputSelectBufferEnd base
        , kbInputDeleteLine = kbInputDeleteLine override <|> kbInputDeleteLine base
        , kbInputDeleteToLineEnd = kbInputDeleteToLineEnd override <|> kbInputDeleteToLineEnd base
        , kbInputDeleteToLineStart = kbInputDeleteToLineStart override <|> kbInputDeleteToLineStart base
        , kbInputBackspace = kbInputBackspace override <|> kbInputBackspace base
        , kbInputDelete = kbInputDelete override <|> kbInputDelete base
        , kbInputUndo = kbInputUndo override <|> kbInputUndo base
        , kbInputRedo = kbInputRedo override <|> kbInputRedo base
        , kbInputWordForward = kbInputWordForward override <|> kbInputWordForward base
        , kbInputWordBackward = kbInputWordBackward override <|> kbInputWordBackward base
        , kbInputSelectWordForward = kbInputSelectWordForward override <|> kbInputSelectWordForward base
        , kbInputSelectWordBackward = kbInputSelectWordBackward override <|> kbInputSelectWordBackward base
        , kbInputDeleteWordForward = kbInputDeleteWordForward override <|> kbInputDeleteWordForward base
        , kbInputDeleteWordBackward = kbInputDeleteWordBackward override <|> kbInputDeleteWordBackward base
        , kbHistoryPrevious = kbHistoryPrevious override <|> kbHistoryPrevious base
        , kbHistoryNext = kbHistoryNext override <|> kbHistoryNext base
        , kbSessionChildCycle = kbSessionChildCycle override <|> kbSessionChildCycle base
        , kbSessionChildCycleReverse = kbSessionChildCycleReverse override <|> kbSessionChildCycleReverse base
        , kbSessionParent = kbSessionParent override <|> kbSessionParent base
        , kbTerminalSuspend = kbTerminalSuspend override <|> kbTerminalSuspend base
        , kbTerminalTitleToggle = kbTerminalTitleToggle override <|> kbTerminalTitleToggle base
        , kbTipsToggle = kbTipsToggle override <|> kbTipsToggle base
        , kbDisplayThinking = kbDisplayThinking override <|> kbDisplayThinking base
        }

-- | Merge server config
mergeServer :: ServerConfig -> ServerConfig -> ServerConfig
mergeServer base override =
    ServerConfig
        { scHostname = scHostname override <|> scHostname base
        , scPort = scPort override <|> scPort base
        , scMdns = scMdns override <|> scMdns base
        , scCors = scCors override <|> scCors base
        }

-- | Merge TUI config
mergeTUI :: TUIConfig -> TUIConfig -> TUIConfig
mergeTUI base override =
    TUIConfig
        { tuiScrollSpeed = tuiScrollSpeed override <|> tuiScrollSpeed base
        , tuiScrollAcceleration = tuiScrollAcceleration override <|> tuiScrollAcceleration base
        , tuiDiffStyle = tuiDiffStyle override <|> tuiDiffStyle base
        }

-- | Merge permission config
mergePermission :: PermissionConfig -> PermissionConfig -> PermissionConfig
mergePermission base override =
    PermissionConfig
        { permRead = permRead override <|> permRead base
        , permEdit = permEdit override <|> permEdit base
        , permGlob = permGlob override <|> permGlob base
        , permGrep = permGrep override <|> permGrep base
        , permList = permList override <|> permList base
        , permBash = permBash override <|> permBash base
        , permTask = permTask override <|> permTask base
        , permExternalDirectory = permExternalDirectory override <|> permExternalDirectory base
        , permTodowrite = permTodowrite override <|> permTodowrite base
        , permTodoread = permTodoread override <|> permTodoread base
        , permQuestion = permQuestion override <|> permQuestion base
        , permWebfetch = permWebfetch override <|> permWebfetch base
        , permWebsearch = permWebsearch override <|> permWebsearch base
        , permCodesearch = permCodesearch override <|> permCodesearch base
        , permLsp = permLsp override <|> permLsp base
        , permDoomLoop = permDoomLoop override <|> permDoomLoop base
        , permSkill = permSkill override <|> permSkill base
        }

-- | Merge compaction config
mergeCompaction :: CompactionConfig -> CompactionConfig -> CompactionConfig
mergeCompaction base override =
    CompactionConfig
        { compAuto = compAuto override <|> compAuto base
        , compPrune = compPrune override <|> compPrune base
        , compReserved = compReserved override <|> compReserved base
        }

-- | Merge experimental config
mergeExperimental :: ExperimentalConfig -> ExperimentalConfig -> ExperimentalConfig
mergeExperimental base override =
    ExperimentalConfig
        { expThinking = expThinking override <|> expThinking base
        , expWorktree = expWorktree override <|> expWorktree base
        , expFileCache = expFileCache override <|> expFileCache base
        , expParallelTools = expParallelTools override <|> expParallelTools base
        , expStreaming = expStreaming override <|> expStreaming base
        }

-- | Merge enterprise config
mergeEnterprise :: EnterpriseConfig -> EnterpriseConfig -> EnterpriseConfig
mergeEnterprise base override =
    EnterpriseConfig
        { entUrl = entUrl override <|> entUrl base
        , entApiKey = entApiKey override <|> entApiKey base
        }

-- | Merge watcher config
mergeWatcher :: WatcherConfig -> WatcherConfig -> WatcherConfig
mergeWatcher base override =
    WatcherConfig
        { watchIgnore = watchIgnore override <|> watchIgnore base
        }

-- Silence unused import warnings
_unusedT :: T.Text
_unusedT = T.empty

_unusedTIO :: IO T.Text
_unusedTIO = TIO.getLine
