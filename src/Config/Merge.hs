{- |
Module      : Config.Merge
Description : Pure configuration merging logic

This module provides pure functions for merging configuration values.
These functions are separated from IO operations to enable easy testing.

= Merge Semantics

Configuration merging follows these rules:

1. 'Maybe' fields: Override wins if 'Just', otherwise base value used
2. Nested records: Fields merged recursively
3. Maps: Override completely replaces base (no deep merge)

= Usage

@
let merged = mergeConfigs baseConfig overrideConfig
@

The override config takes precedence for all set fields.
-}
module Config.Merge (
    -- * Main Merge Function
    mergeConfigs,

    -- * Nested Config Merging
    mergeKeybinds,
    mergeServer,
    mergeTUI,
    mergePermission,
    mergeCompaction,
    mergeExperimental,
    mergeEnterprise,
    mergeWatcher,

    -- * Merge Helpers

    -- | Low-level helpers for building custom merge functions
    mergeOptional,
) where

import Config.Types
import Control.Applicative ((<|>))

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Merge Helpers
-- ════════════════════════════════════════════════════════════════════════════

{- | Merge two optional values, preferring the override.

This is the fundamental merge operation. If the override is 'Just',
it wins; otherwise the base value is used.

@
mergeOptional (Just "a") (Just "b") == Just "b"
mergeOptional (Just "a") Nothing    == Just "a"
mergeOptional Nothing    (Just "b") == Just "b"
mergeOptional Nothing    Nothing    == Nothing
@

This is equivalent to @flip ('<|>')@ but with clearer semantics for
configuration merging where "override wins".
-}
mergeOptional :: Maybe a -> Maybe a -> Maybe a
mergeOptional base override = override <|> base
{-# INLINE mergeOptional #-}

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Main Merge
-- ════════════════════════════════════════════════════════════════════════════

{- | Merge two configurations, with the second overriding the first.

This is the primary merge function used when layering configurations:
defaults <- global <- project.

All 'Maybe' fields use 'mergeOptional' semantics. Nested records
are merged recursively using their respective merge functions.

= Example

@
let base = defaultConfig { cfgModel = Just "gpt-4" }
let override = defaultConfig { cfgModel = Just "claude-3", cfgLogLevel = Just DEBUG }
let merged = mergeConfigs base override
-- merged.cfgModel == Just "claude-3"
-- merged.cfgLogLevel == Just DEBUG
@
-}
mergeConfigs :: Config -> Config -> Config
mergeConfigs base override =
    Config
        { cfgModel = mergeOptional (cfgModel base) (cfgModel override)
        , cfgSystemPrompt = mergeOptional (cfgSystemPrompt base) (cfgSystemPrompt override)
        , cfgMaxTokens = mergeOptional (cfgMaxTokens base) (cfgMaxTokens override)
        , cfgLogLevel = mergeOptional (cfgLogLevel base) (cfgLogLevel override)
        , cfgKeybinds = mergeKeybinds (cfgKeybinds base) (cfgKeybinds override)
        , cfgServer = mergeServer (cfgServer base) (cfgServer override)
        , cfgTui = mergeTUI (cfgTui base) (cfgTui override)
        , cfgPermission = mergePermission (cfgPermission base) (cfgPermission override)
        , cfgCompaction = mergeCompaction (cfgCompaction base) (cfgCompaction override)
        , cfgExperimental = mergeExperimental (cfgExperimental base) (cfgExperimental override)
        , cfgEnterprise = mergeEnterprise (cfgEnterprise base) (cfgEnterprise override)
        , cfgWatcher = mergeWatcher (cfgWatcher base) (cfgWatcher override)
        , cfgAgent = mergeOptional (cfgAgent base) (cfgAgent override)
        , cfgProvider = mergeOptional (cfgProvider base) (cfgProvider override)
        , cfgMcp = mergeOptional (cfgMcp base) (cfgMcp override)
        , cfgFormatter = mergeOptional (cfgFormatter base) (cfgFormatter override)
        , cfgLsp = mergeOptional (cfgLsp base) (cfgLsp override)
        , cfgSkill = mergeOptional (cfgSkill base) (cfgSkill override)
        , cfgCommand = mergeOptional (cfgCommand base) (cfgCommand override)
        , cfgTheme = mergeOptional (cfgTheme base) (cfgTheme override)
        , cfgThemes = mergeOptional (cfgThemes base) (cfgThemes override)
        , cfgShare = mergeOptional (cfgShare base) (cfgShare override)
        , cfgAutoUpdate = mergeOptional (cfgAutoUpdate base) (cfgAutoUpdate override)
        , cfgDisabledTools = mergeOptional (cfgDisabledTools base) (cfgDisabledTools override)
        , cfgInstrumentation = mergeOptional (cfgInstrumentation base) (cfgInstrumentation override)
        }

-- ════════════════════════════════════════════════════════════════════════════
--                                                         Keybinds Merge
-- ════════════════════════════════════════════════════════════════════════════

{- | Merge keybind configurations.

Each keybind field is merged independently using 'mergeOptional'.
This allows partial overrides - you can override just the leader key
without affecting other keybinds.
-}
mergeKeybinds :: KeybindsConfig -> KeybindsConfig -> KeybindsConfig
mergeKeybinds base override =
    KeybindsConfig
        { kbLeader = mergeOptional (kbLeader base) (kbLeader override)
        , kbAppExit = mergeOptional (kbAppExit base) (kbAppExit override)
        , kbEditorOpen = mergeOptional (kbEditorOpen base) (kbEditorOpen override)
        , kbThemeList = mergeOptional (kbThemeList base) (kbThemeList override)
        , kbSidebarToggle = mergeOptional (kbSidebarToggle base) (kbSidebarToggle override)
        , kbScrollbarToggle = mergeOptional (kbScrollbarToggle base) (kbScrollbarToggle override)
        , kbUsernameToggle = mergeOptional (kbUsernameToggle base) (kbUsernameToggle override)
        , kbStatusView = mergeOptional (kbStatusView base) (kbStatusView override)
        , kbSessionExport = mergeOptional (kbSessionExport base) (kbSessionExport override)
        , kbSessionNew = mergeOptional (kbSessionNew base) (kbSessionNew override)
        , kbSessionList = mergeOptional (kbSessionList base) (kbSessionList override)
        , kbSessionTimeline = mergeOptional (kbSessionTimeline base) (kbSessionTimeline override)
        , kbSessionFork = mergeOptional (kbSessionFork base) (kbSessionFork override)
        , kbSessionRename = mergeOptional (kbSessionRename base) (kbSessionRename override)
        , kbSessionDelete = mergeOptional (kbSessionDelete base) (kbSessionDelete override)
        , kbStashDelete = mergeOptional (kbStashDelete base) (kbStashDelete override)
        , kbModelProviderList = mergeOptional (kbModelProviderList base) (kbModelProviderList override)
        , kbModelFavoriteToggle = mergeOptional (kbModelFavoriteToggle base) (kbModelFavoriteToggle override)
        , kbSessionShare = mergeOptional (kbSessionShare base) (kbSessionShare override)
        , kbSessionUnshare = mergeOptional (kbSessionUnshare base) (kbSessionUnshare override)
        , kbSessionInterrupt = mergeOptional (kbSessionInterrupt base) (kbSessionInterrupt override)
        , kbSessionCompact = mergeOptional (kbSessionCompact base) (kbSessionCompact override)
        , kbMessagesPageUp = mergeOptional (kbMessagesPageUp base) (kbMessagesPageUp override)
        , kbMessagesPageDown = mergeOptional (kbMessagesPageDown base) (kbMessagesPageDown override)
        , kbMessagesLineUp = mergeOptional (kbMessagesLineUp base) (kbMessagesLineUp override)
        , kbMessagesLineDown = mergeOptional (kbMessagesLineDown base) (kbMessagesLineDown override)
        , kbMessagesHalfPageUp = mergeOptional (kbMessagesHalfPageUp base) (kbMessagesHalfPageUp override)
        , kbMessagesHalfPageDown = mergeOptional (kbMessagesHalfPageDown base) (kbMessagesHalfPageDown override)
        , kbMessagesFirst = mergeOptional (kbMessagesFirst base) (kbMessagesFirst override)
        , kbMessagesLast = mergeOptional (kbMessagesLast base) (kbMessagesLast override)
        , kbMessagesNext = mergeOptional (kbMessagesNext base) (kbMessagesNext override)
        , kbMessagesPrevious = mergeOptional (kbMessagesPrevious base) (kbMessagesPrevious override)
        , kbMessagesLastUser = mergeOptional (kbMessagesLastUser base) (kbMessagesLastUser override)
        , kbMessagesCopy = mergeOptional (kbMessagesCopy base) (kbMessagesCopy override)
        , kbMessagesUndo = mergeOptional (kbMessagesUndo base) (kbMessagesUndo override)
        , kbMessagesRedo = mergeOptional (kbMessagesRedo base) (kbMessagesRedo override)
        , kbMessagesToggleConceal = mergeOptional (kbMessagesToggleConceal base) (kbMessagesToggleConceal override)
        , kbToolDetails = mergeOptional (kbToolDetails base) (kbToolDetails override)
        , kbModelList = mergeOptional (kbModelList base) (kbModelList override)
        , kbModelCycleRecent = mergeOptional (kbModelCycleRecent base) (kbModelCycleRecent override)
        , kbModelCycleRecentReverse = mergeOptional (kbModelCycleRecentReverse base) (kbModelCycleRecentReverse override)
        , kbModelCycleFavorite = mergeOptional (kbModelCycleFavorite base) (kbModelCycleFavorite override)
        , kbModelCycleFavoriteReverse = mergeOptional (kbModelCycleFavoriteReverse base) (kbModelCycleFavoriteReverse override)
        , kbCommandList = mergeOptional (kbCommandList base) (kbCommandList override)
        , kbAgentList = mergeOptional (kbAgentList base) (kbAgentList override)
        , kbAgentCycle = mergeOptional (kbAgentCycle base) (kbAgentCycle override)
        , kbAgentCycleReverse = mergeOptional (kbAgentCycleReverse base) (kbAgentCycleReverse override)
        , kbVariantCycle = mergeOptional (kbVariantCycle base) (kbVariantCycle override)
        , kbInputClear = mergeOptional (kbInputClear base) (kbInputClear override)
        , kbInputPaste = mergeOptional (kbInputPaste base) (kbInputPaste override)
        , kbInputSubmit = mergeOptional (kbInputSubmit base) (kbInputSubmit override)
        , kbInputNewline = mergeOptional (kbInputNewline base) (kbInputNewline override)
        , kbInputMoveLeft = mergeOptional (kbInputMoveLeft base) (kbInputMoveLeft override)
        , kbInputMoveRight = mergeOptional (kbInputMoveRight base) (kbInputMoveRight override)
        , kbInputMoveUp = mergeOptional (kbInputMoveUp base) (kbInputMoveUp override)
        , kbInputMoveDown = mergeOptional (kbInputMoveDown base) (kbInputMoveDown override)
        , kbInputSelectLeft = mergeOptional (kbInputSelectLeft base) (kbInputSelectLeft override)
        , kbInputSelectRight = mergeOptional (kbInputSelectRight base) (kbInputSelectRight override)
        , kbInputSelectUp = mergeOptional (kbInputSelectUp base) (kbInputSelectUp override)
        , kbInputSelectDown = mergeOptional (kbInputSelectDown base) (kbInputSelectDown override)
        , kbInputLineHome = mergeOptional (kbInputLineHome base) (kbInputLineHome override)
        , kbInputLineEnd = mergeOptional (kbInputLineEnd base) (kbInputLineEnd override)
        , kbInputSelectLineHome = mergeOptional (kbInputSelectLineHome base) (kbInputSelectLineHome override)
        , kbInputSelectLineEnd = mergeOptional (kbInputSelectLineEnd base) (kbInputSelectLineEnd override)
        , kbInputVisualLineHome = mergeOptional (kbInputVisualLineHome base) (kbInputVisualLineHome override)
        , kbInputVisualLineEnd = mergeOptional (kbInputVisualLineEnd base) (kbInputVisualLineEnd override)
        , kbInputSelectVisualLineHome = mergeOptional (kbInputSelectVisualLineHome base) (kbInputSelectVisualLineHome override)
        , kbInputSelectVisualLineEnd = mergeOptional (kbInputSelectVisualLineEnd base) (kbInputSelectVisualLineEnd override)
        , kbInputBufferHome = mergeOptional (kbInputBufferHome base) (kbInputBufferHome override)
        , kbInputBufferEnd = mergeOptional (kbInputBufferEnd base) (kbInputBufferEnd override)
        , kbInputSelectBufferHome = mergeOptional (kbInputSelectBufferHome base) (kbInputSelectBufferHome override)
        , kbInputSelectBufferEnd = mergeOptional (kbInputSelectBufferEnd base) (kbInputSelectBufferEnd override)
        , kbInputDeleteLine = mergeOptional (kbInputDeleteLine base) (kbInputDeleteLine override)
        , kbInputDeleteToLineEnd = mergeOptional (kbInputDeleteToLineEnd base) (kbInputDeleteToLineEnd override)
        , kbInputDeleteToLineStart = mergeOptional (kbInputDeleteToLineStart base) (kbInputDeleteToLineStart override)
        , kbInputBackspace = mergeOptional (kbInputBackspace base) (kbInputBackspace override)
        , kbInputDelete = mergeOptional (kbInputDelete base) (kbInputDelete override)
        , kbInputUndo = mergeOptional (kbInputUndo base) (kbInputUndo override)
        , kbInputRedo = mergeOptional (kbInputRedo base) (kbInputRedo override)
        , kbInputWordForward = mergeOptional (kbInputWordForward base) (kbInputWordForward override)
        , kbInputWordBackward = mergeOptional (kbInputWordBackward base) (kbInputWordBackward override)
        , kbInputSelectWordForward = mergeOptional (kbInputSelectWordForward base) (kbInputSelectWordForward override)
        , kbInputSelectWordBackward = mergeOptional (kbInputSelectWordBackward base) (kbInputSelectWordBackward override)
        , kbInputDeleteWordForward = mergeOptional (kbInputDeleteWordForward base) (kbInputDeleteWordForward override)
        , kbInputDeleteWordBackward = mergeOptional (kbInputDeleteWordBackward base) (kbInputDeleteWordBackward override)
        , kbHistoryPrevious = mergeOptional (kbHistoryPrevious base) (kbHistoryPrevious override)
        , kbHistoryNext = mergeOptional (kbHistoryNext base) (kbHistoryNext override)
        , kbSessionChildCycle = mergeOptional (kbSessionChildCycle base) (kbSessionChildCycle override)
        , kbSessionChildCycleReverse = mergeOptional (kbSessionChildCycleReverse base) (kbSessionChildCycleReverse override)
        , kbSessionParent = mergeOptional (kbSessionParent base) (kbSessionParent override)
        , kbTerminalSuspend = mergeOptional (kbTerminalSuspend base) (kbTerminalSuspend override)
        , kbTerminalTitleToggle = mergeOptional (kbTerminalTitleToggle base) (kbTerminalTitleToggle override)
        , kbTipsToggle = mergeOptional (kbTipsToggle base) (kbTipsToggle override)
        , kbDisplayThinking = mergeOptional (kbDisplayThinking base) (kbDisplayThinking override)
        }

-- ════════════════════════════════════════════════════════════════════════════
--                                                       Simple Config Merges
-- ════════════════════════════════════════════════════════════════════════════

-- | Merge server configurations.
mergeServer :: ServerConfig -> ServerConfig -> ServerConfig
mergeServer base override =
    ServerConfig
        { scHostname = mergeOptional (scHostname base) (scHostname override)
        , scPort = mergeOptional (scPort base) (scPort override)
        , scMdns = mergeOptional (scMdns base) (scMdns override)
        , scCors = mergeOptional (scCors base) (scCors override)
        }

-- | Merge TUI configurations.
mergeTUI :: TUIConfig -> TUIConfig -> TUIConfig
mergeTUI base override =
    TUIConfig
        { tuiScrollSpeed = mergeOptional (tuiScrollSpeed base) (tuiScrollSpeed override)
        , tuiScrollAcceleration = mergeOptional (tuiScrollAcceleration base) (tuiScrollAcceleration override)
        , tuiDiffStyle = mergeOptional (tuiDiffStyle base) (tuiDiffStyle override)
        }

-- | Merge permission configurations.
mergePermission :: PermissionConfig -> PermissionConfig -> PermissionConfig
mergePermission base override =
    PermissionConfig
        { permRead = mergeOptional (permRead base) (permRead override)
        , permEdit = mergeOptional (permEdit base) (permEdit override)
        , permGlob = mergeOptional (permGlob base) (permGlob override)
        , permGrep = mergeOptional (permGrep base) (permGrep override)
        , permList = mergeOptional (permList base) (permList override)
        , permBash = mergeOptional (permBash base) (permBash override)
        , permTask = mergeOptional (permTask base) (permTask override)
        , permExternalDirectory = mergeOptional (permExternalDirectory base) (permExternalDirectory override)
        , permTodowrite = mergeOptional (permTodowrite base) (permTodowrite override)
        , permTodoread = mergeOptional (permTodoread base) (permTodoread override)
        , permQuestion = mergeOptional (permQuestion base) (permQuestion override)
        , permWebfetch = mergeOptional (permWebfetch base) (permWebfetch override)
        , permWebsearch = mergeOptional (permWebsearch base) (permWebsearch override)
        , permCodesearch = mergeOptional (permCodesearch base) (permCodesearch override)
        , permLsp = mergeOptional (permLsp base) (permLsp override)
        , permDoomLoop = mergeOptional (permDoomLoop base) (permDoomLoop override)
        , permSkill = mergeOptional (permSkill base) (permSkill override)
        }

-- | Merge compaction configurations.
mergeCompaction :: CompactionConfig -> CompactionConfig -> CompactionConfig
mergeCompaction base override =
    CompactionConfig
        { compAuto = mergeOptional (compAuto base) (compAuto override)
        , compPrune = mergeOptional (compPrune base) (compPrune override)
        , compReserved = mergeOptional (compReserved base) (compReserved override)
        }

-- | Merge experimental configurations.
mergeExperimental :: ExperimentalConfig -> ExperimentalConfig -> ExperimentalConfig
mergeExperimental base override =
    ExperimentalConfig
        { expThinking = mergeOptional (expThinking base) (expThinking override)
        , expWorktree = mergeOptional (expWorktree base) (expWorktree override)
        , expFileCache = mergeOptional (expFileCache base) (expFileCache override)
        , expParallelTools = mergeOptional (expParallelTools base) (expParallelTools override)
        , expStreaming = mergeOptional (expStreaming base) (expStreaming override)
        }

-- | Merge enterprise configurations.
mergeEnterprise :: EnterpriseConfig -> EnterpriseConfig -> EnterpriseConfig
mergeEnterprise base override =
    EnterpriseConfig
        { entUrl = mergeOptional (entUrl base) (entUrl override)
        , entApiKey = mergeOptional (entApiKey base) (entApiKey override)
        }

-- | Merge watcher configurations.
mergeWatcher :: WatcherConfig -> WatcherConfig -> WatcherConfig
mergeWatcher base override =
    WatcherConfig
        { watchIgnore = mergeOptional (watchIgnore base) (watchIgnore override)
        }
