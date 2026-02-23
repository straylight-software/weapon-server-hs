{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Config type definitions
Matches the Dhall configuration schema exactly
-}
module Config.Types (
    -- * Main Config
    Config (..),

    -- * Enums
    LogLevel (..),
    ShareMode (..),
    Layout (..),
    PermissionAction (..),
    DiffStyle (..),
    AgentMode (..),
    ModelStatus (..),
    AutoUpdate (..),

    -- * Nested Configs
    KeybindsConfig (..),
    ServerConfig (..),
    TUIConfig (..),
    PermissionConfig (..),
    PermissionRule (..),
    CompactionConfig (..),
    ExperimentalConfig (..),
    EnterpriseConfig (..),
    WatcherConfig (..),

    -- * Agent
    AgentConfig (..),
    AgentColor (..),

    -- * Provider
    ProviderConfig (..),
    ProviderModel (..),
    ProviderOptions (..),
    ProviderTimeout (..),

    -- * MCP
    MCPConfig (..),
    MCPLocal (..),
    MCPRemote (..),

    -- * Formatter & LSP
    FormatterConfig (..),
    FormatterEntry (..),
    LSPConfig (..),
    LSPEntry (..),

    -- * Theme
    ThemeConfig (..),
    Color (..),

    -- * Skill & Command
    SkillConfig (..),
    CommandConfig (..),

    -- * Defaults
    defaultConfig,
    defaultKeybinds,
) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                      Enums
-- ════════════════════════════════════════════════════════════════════════════

data LogLevel = DEBUG | INFO | WARN | ERROR
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON LogLevel where
    toJSON DEBUG = String "DEBUG"
    toJSON INFO = String "INFO"
    toJSON WARN = String "WARN"
    toJSON ERROR = String "ERROR"

instance FromJSON LogLevel where
    parseJSON = withText "LogLevel" $ \case
        "DEBUG" -> pure DEBUG
        "INFO" -> pure INFO
        "WARN" -> pure WARN
        "ERROR" -> pure ERROR
        other -> fail $ "Unknown LogLevel: " <> show other

data ShareMode = ShareManual | ShareAuto | ShareDisabled
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ShareMode where
    toJSON ShareManual = String "manual"
    toJSON ShareAuto = String "auto"
    toJSON ShareDisabled = String "disabled"

instance FromJSON ShareMode where
    parseJSON = withText "ShareMode" $ \case
        "manual" -> pure ShareManual
        "auto" -> pure ShareAuto
        "disabled" -> pure ShareDisabled
        other -> fail $ "Unknown ShareMode: " <> show other

data Layout = LayoutAuto | LayoutStretch
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON Layout where
    toJSON LayoutAuto = String "auto"
    toJSON LayoutStretch = String "stretch"

instance FromJSON Layout where
    parseJSON = withText "Layout" $ \case
        "auto" -> pure LayoutAuto
        "stretch" -> pure LayoutStretch
        other -> fail $ "Unknown Layout: " <> show other

data PermissionAction = PermAsk | PermAllow | PermDeny
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON PermissionAction where
    toJSON PermAsk = String "ask"
    toJSON PermAllow = String "allow"
    toJSON PermDeny = String "deny"

instance FromJSON PermissionAction where
    parseJSON = withText "PermissionAction" $ \case
        "ask" -> pure PermAsk
        "allow" -> pure PermAllow
        "deny" -> pure PermDeny
        other -> fail $ "Unknown PermissionAction: " <> show other

data DiffStyle = DiffAuto | DiffStacked
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON DiffStyle where
    toJSON DiffAuto = String "auto"
    toJSON DiffStacked = String "stacked"

instance FromJSON DiffStyle where
    parseJSON = withText "DiffStyle" $ \case
        "auto" -> pure DiffAuto
        "stacked" -> pure DiffStacked
        other -> fail $ "Unknown DiffStyle: " <> show other

data AgentMode = AgentSubagent | AgentPrimary | AgentAll
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON AgentMode where
    toJSON AgentSubagent = String "subagent"
    toJSON AgentPrimary = String "primary"
    toJSON AgentAll = String "all"

instance FromJSON AgentMode where
    parseJSON = withText "AgentMode" $ \case
        "subagent" -> pure AgentSubagent
        "primary" -> pure AgentPrimary
        "all" -> pure AgentAll
        other -> fail $ "Unknown AgentMode: " <> show other

data ModelStatus = ModelAlpha | ModelBeta | ModelDeprecated | ModelStable
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ModelStatus where
    toJSON ModelAlpha = String "alpha"
    toJSON ModelBeta = String "beta"
    toJSON ModelDeprecated = String "deprecated"
    toJSON ModelStable = String "stable"

instance FromJSON ModelStatus where
    parseJSON = withText "ModelStatus" $ \case
        "alpha" -> pure ModelAlpha
        "beta" -> pure ModelBeta
        "deprecated" -> pure ModelDeprecated
        "stable" -> pure ModelStable
        other -> fail $ "Unknown ModelStatus: " <> show other

data AutoUpdate = AutoUpdateEnabled | AutoUpdateDisabled | AutoUpdateNotify
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON AutoUpdate where
    toJSON AutoUpdateEnabled = Bool True
    toJSON AutoUpdateDisabled = Bool False
    toJSON AutoUpdateNotify = String "notify"

instance FromJSON AutoUpdate where
    parseJSON (Bool True) = pure AutoUpdateEnabled
    parseJSON (Bool False) = pure AutoUpdateDisabled
    parseJSON (String "notify") = pure AutoUpdateNotify
    parseJSON other = fail $ "Unknown AutoUpdate: " <> show other

-- ════════════════════════════════════════════════════════════════════════════
--                                                                   Keybinds
-- ════════════════════════════════════════════════════════════════════════════

-- | Keybinds configuration - all 70+ keybinds
data KeybindsConfig = KeybindsConfig
    { kbLeader :: Maybe Text
    , kbAppExit :: Maybe Text
    , kbEditorOpen :: Maybe Text
    , kbThemeList :: Maybe Text
    , kbSidebarToggle :: Maybe Text
    , kbScrollbarToggle :: Maybe Text
    , kbUsernameToggle :: Maybe Text
    , kbStatusView :: Maybe Text
    , kbSessionExport :: Maybe Text
    , kbSessionNew :: Maybe Text
    , kbSessionList :: Maybe Text
    , kbSessionTimeline :: Maybe Text
    , kbSessionFork :: Maybe Text
    , kbSessionRename :: Maybe Text
    , kbSessionDelete :: Maybe Text
    , kbStashDelete :: Maybe Text
    , kbModelProviderList :: Maybe Text
    , kbModelFavoriteToggle :: Maybe Text
    , kbSessionShare :: Maybe Text
    , kbSessionUnshare :: Maybe Text
    , kbSessionInterrupt :: Maybe Text
    , kbSessionCompact :: Maybe Text
    , kbMessagesPageUp :: Maybe Text
    , kbMessagesPageDown :: Maybe Text
    , kbMessagesLineUp :: Maybe Text
    , kbMessagesLineDown :: Maybe Text
    , kbMessagesHalfPageUp :: Maybe Text
    , kbMessagesHalfPageDown :: Maybe Text
    , kbMessagesFirst :: Maybe Text
    , kbMessagesLast :: Maybe Text
    , kbMessagesNext :: Maybe Text
    , kbMessagesPrevious :: Maybe Text
    , kbMessagesLastUser :: Maybe Text
    , kbMessagesCopy :: Maybe Text
    , kbMessagesUndo :: Maybe Text
    , kbMessagesRedo :: Maybe Text
    , kbMessagesToggleConceal :: Maybe Text
    , kbToolDetails :: Maybe Text
    , kbModelList :: Maybe Text
    , kbModelCycleRecent :: Maybe Text
    , kbModelCycleRecentReverse :: Maybe Text
    , kbModelCycleFavorite :: Maybe Text
    , kbModelCycleFavoriteReverse :: Maybe Text
    , kbCommandList :: Maybe Text
    , kbAgentList :: Maybe Text
    , kbAgentCycle :: Maybe Text
    , kbAgentCycleReverse :: Maybe Text
    , kbVariantCycle :: Maybe Text
    , kbInputClear :: Maybe Text
    , kbInputPaste :: Maybe Text
    , kbInputSubmit :: Maybe Text
    , kbInputNewline :: Maybe Text
    , kbInputMoveLeft :: Maybe Text
    , kbInputMoveRight :: Maybe Text
    , kbInputMoveUp :: Maybe Text
    , kbInputMoveDown :: Maybe Text
    , kbInputSelectLeft :: Maybe Text
    , kbInputSelectRight :: Maybe Text
    , kbInputSelectUp :: Maybe Text
    , kbInputSelectDown :: Maybe Text
    , kbInputLineHome :: Maybe Text
    , kbInputLineEnd :: Maybe Text
    , kbInputSelectLineHome :: Maybe Text
    , kbInputSelectLineEnd :: Maybe Text
    , kbInputVisualLineHome :: Maybe Text
    , kbInputVisualLineEnd :: Maybe Text
    , kbInputSelectVisualLineHome :: Maybe Text
    , kbInputSelectVisualLineEnd :: Maybe Text
    , kbInputBufferHome :: Maybe Text
    , kbInputBufferEnd :: Maybe Text
    , kbInputSelectBufferHome :: Maybe Text
    , kbInputSelectBufferEnd :: Maybe Text
    , kbInputDeleteLine :: Maybe Text
    , kbInputDeleteToLineEnd :: Maybe Text
    , kbInputDeleteToLineStart :: Maybe Text
    , kbInputBackspace :: Maybe Text
    , kbInputDelete :: Maybe Text
    , kbInputUndo :: Maybe Text
    , kbInputRedo :: Maybe Text
    , kbInputWordForward :: Maybe Text
    , kbInputWordBackward :: Maybe Text
    , kbInputSelectWordForward :: Maybe Text
    , kbInputSelectWordBackward :: Maybe Text
    , kbInputDeleteWordForward :: Maybe Text
    , kbInputDeleteWordBackward :: Maybe Text
    , kbHistoryPrevious :: Maybe Text
    , kbHistoryNext :: Maybe Text
    , kbSessionChildCycle :: Maybe Text
    , kbSessionChildCycleReverse :: Maybe Text
    , kbSessionParent :: Maybe Text
    , kbTerminalSuspend :: Maybe Text
    , kbTerminalTitleToggle :: Maybe Text
    , kbTipsToggle :: Maybe Text
    , kbDisplayThinking :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON KeybindsConfig where
    toJSON kb =
        object
            [ "leader" .= kbLeader kb
            , "app_exit" .= kbAppExit kb
            , "editor_open" .= kbEditorOpen kb
            , "theme_list" .= kbThemeList kb
            , "sidebar_toggle" .= kbSidebarToggle kb
            , "scrollbar_toggle" .= kbScrollbarToggle kb
            , "username_toggle" .= kbUsernameToggle kb
            , "status_view" .= kbStatusView kb
            , "session_export" .= kbSessionExport kb
            , "session_new" .= kbSessionNew kb
            , "session_list" .= kbSessionList kb
            , "session_timeline" .= kbSessionTimeline kb
            , "session_fork" .= kbSessionFork kb
            , "session_rename" .= kbSessionRename kb
            , "session_delete" .= kbSessionDelete kb
            , "stash_delete" .= kbStashDelete kb
            , "model_provider_list" .= kbModelProviderList kb
            , "model_favorite_toggle" .= kbModelFavoriteToggle kb
            , "session_share" .= kbSessionShare kb
            , "session_unshare" .= kbSessionUnshare kb
            , "session_interrupt" .= kbSessionInterrupt kb
            , "session_compact" .= kbSessionCompact kb
            , "messages_page_up" .= kbMessagesPageUp kb
            , "messages_page_down" .= kbMessagesPageDown kb
            , "messages_line_up" .= kbMessagesLineUp kb
            , "messages_line_down" .= kbMessagesLineDown kb
            , "messages_half_page_up" .= kbMessagesHalfPageUp kb
            , "messages_half_page_down" .= kbMessagesHalfPageDown kb
            , "messages_first" .= kbMessagesFirst kb
            , "messages_last" .= kbMessagesLast kb
            , "messages_next" .= kbMessagesNext kb
            , "messages_previous" .= kbMessagesPrevious kb
            , "messages_last_user" .= kbMessagesLastUser kb
            , "messages_copy" .= kbMessagesCopy kb
            , "messages_undo" .= kbMessagesUndo kb
            , "messages_redo" .= kbMessagesRedo kb
            , "messages_toggle_conceal" .= kbMessagesToggleConceal kb
            , "tool_details" .= kbToolDetails kb
            , "model_list" .= kbModelList kb
            , "model_cycle_recent" .= kbModelCycleRecent kb
            , "model_cycle_recent_reverse" .= kbModelCycleRecentReverse kb
            , "model_cycle_favorite" .= kbModelCycleFavorite kb
            , "model_cycle_favorite_reverse" .= kbModelCycleFavoriteReverse kb
            , "command_list" .= kbCommandList kb
            , "agent_list" .= kbAgentList kb
            , "agent_cycle" .= kbAgentCycle kb
            , "agent_cycle_reverse" .= kbAgentCycleReverse kb
            , "variant_cycle" .= kbVariantCycle kb
            , "input_clear" .= kbInputClear kb
            , "input_paste" .= kbInputPaste kb
            , "input_submit" .= kbInputSubmit kb
            , "input_newline" .= kbInputNewline kb
            , "input_move_left" .= kbInputMoveLeft kb
            , "input_move_right" .= kbInputMoveRight kb
            , "input_move_up" .= kbInputMoveUp kb
            , "input_move_down" .= kbInputMoveDown kb
            , "input_select_left" .= kbInputSelectLeft kb
            , "input_select_right" .= kbInputSelectRight kb
            , "input_select_up" .= kbInputSelectUp kb
            , "input_select_down" .= kbInputSelectDown kb
            , "input_line_home" .= kbInputLineHome kb
            , "input_line_end" .= kbInputLineEnd kb
            , "input_select_line_home" .= kbInputSelectLineHome kb
            , "input_select_line_end" .= kbInputSelectLineEnd kb
            , "input_visual_line_home" .= kbInputVisualLineHome kb
            , "input_visual_line_end" .= kbInputVisualLineEnd kb
            , "input_select_visual_line_home" .= kbInputSelectVisualLineHome kb
            , "input_select_visual_line_end" .= kbInputSelectVisualLineEnd kb
            , "input_buffer_home" .= kbInputBufferHome kb
            , "input_buffer_end" .= kbInputBufferEnd kb
            , "input_select_buffer_home" .= kbInputSelectBufferHome kb
            , "input_select_buffer_end" .= kbInputSelectBufferEnd kb
            , "input_delete_line" .= kbInputDeleteLine kb
            , "input_delete_to_line_end" .= kbInputDeleteToLineEnd kb
            , "input_delete_to_line_start" .= kbInputDeleteToLineStart kb
            , "input_backspace" .= kbInputBackspace kb
            , "input_delete" .= kbInputDelete kb
            , "input_undo" .= kbInputUndo kb
            , "input_redo" .= kbInputRedo kb
            , "input_word_forward" .= kbInputWordForward kb
            , "input_word_backward" .= kbInputWordBackward kb
            , "input_select_word_forward" .= kbInputSelectWordForward kb
            , "input_select_word_backward" .= kbInputSelectWordBackward kb
            , "input_delete_word_forward" .= kbInputDeleteWordForward kb
            , "input_delete_word_backward" .= kbInputDeleteWordBackward kb
            , "history_previous" .= kbHistoryPrevious kb
            , "history_next" .= kbHistoryNext kb
            , "session_child_cycle" .= kbSessionChildCycle kb
            , "session_child_cycle_reverse" .= kbSessionChildCycleReverse kb
            , "session_parent" .= kbSessionParent kb
            , "terminal_suspend" .= kbTerminalSuspend kb
            , "terminal_title_toggle" .= kbTerminalTitleToggle kb
            , "tips_toggle" .= kbTipsToggle kb
            , "display_thinking" .= kbDisplayThinking kb
            ]

instance FromJSON KeybindsConfig where
    parseJSON = withObject "KeybindsConfig" $ \v ->
        KeybindsConfig
            <$> v .:? "leader"
            <*> v .:? "app_exit"
            <*> v .:? "editor_open"
            <*> v .:? "theme_list"
            <*> v .:? "sidebar_toggle"
            <*> v .:? "scrollbar_toggle"
            <*> v .:? "username_toggle"
            <*> v .:? "status_view"
            <*> v .:? "session_export"
            <*> v .:? "session_new"
            <*> v .:? "session_list"
            <*> v .:? "session_timeline"
            <*> v .:? "session_fork"
            <*> v .:? "session_rename"
            <*> v .:? "session_delete"
            <*> v .:? "stash_delete"
            <*> v .:? "model_provider_list"
            <*> v .:? "model_favorite_toggle"
            <*> v .:? "session_share"
            <*> v .:? "session_unshare"
            <*> v .:? "session_interrupt"
            <*> v .:? "session_compact"
            <*> v .:? "messages_page_up"
            <*> v .:? "messages_page_down"
            <*> v .:? "messages_line_up"
            <*> v .:? "messages_line_down"
            <*> v .:? "messages_half_page_up"
            <*> v .:? "messages_half_page_down"
            <*> v .:? "messages_first"
            <*> v .:? "messages_last"
            <*> v .:? "messages_next"
            <*> v .:? "messages_previous"
            <*> v .:? "messages_last_user"
            <*> v .:? "messages_copy"
            <*> v .:? "messages_undo"
            <*> v .:? "messages_redo"
            <*> v .:? "messages_toggle_conceal"
            <*> v .:? "tool_details"
            <*> v .:? "model_list"
            <*> v .:? "model_cycle_recent"
            <*> v .:? "model_cycle_recent_reverse"
            <*> v .:? "model_cycle_favorite"
            <*> v .:? "model_cycle_favorite_reverse"
            <*> v .:? "command_list"
            <*> v .:? "agent_list"
            <*> v .:? "agent_cycle"
            <*> v .:? "agent_cycle_reverse"
            <*> v .:? "variant_cycle"
            <*> v .:? "input_clear"
            <*> v .:? "input_paste"
            <*> v .:? "input_submit"
            <*> v .:? "input_newline"
            <*> v .:? "input_move_left"
            <*> v .:? "input_move_right"
            <*> v .:? "input_move_up"
            <*> v .:? "input_move_down"
            <*> v .:? "input_select_left"
            <*> v .:? "input_select_right"
            <*> v .:? "input_select_up"
            <*> v .:? "input_select_down"
            <*> v .:? "input_line_home"
            <*> v .:? "input_line_end"
            <*> v .:? "input_select_line_home"
            <*> v .:? "input_select_line_end"
            <*> v .:? "input_visual_line_home"
            <*> v .:? "input_visual_line_end"
            <*> v .:? "input_select_visual_line_home"
            <*> v .:? "input_select_visual_line_end"
            <*> v .:? "input_buffer_home"
            <*> v .:? "input_buffer_end"
            <*> v .:? "input_select_buffer_home"
            <*> v .:? "input_select_buffer_end"
            <*> v .:? "input_delete_line"
            <*> v .:? "input_delete_to_line_end"
            <*> v .:? "input_delete_to_line_start"
            <*> v .:? "input_backspace"
            <*> v .:? "input_delete"
            <*> v .:? "input_undo"
            <*> v .:? "input_redo"
            <*> v .:? "input_word_forward"
            <*> v .:? "input_word_backward"
            <*> v .:? "input_select_word_forward"
            <*> v .:? "input_select_word_backward"
            <*> v .:? "input_delete_word_forward"
            <*> v .:? "input_delete_word_backward"
            <*> v .:? "history_previous"
            <*> v .:? "history_next"
            <*> v .:? "session_child_cycle"
            <*> v .:? "session_child_cycle_reverse"
            <*> v .:? "session_parent"
            <*> v .:? "terminal_suspend"
            <*> v .:? "terminal_title_toggle"
            <*> v .:? "tips_toggle"
            <*> v .:? "display_thinking"

-- | Default keybinds (critical for Ctrl+C to work!)
defaultKeybinds :: KeybindsConfig
defaultKeybinds =
    KeybindsConfig
        { kbLeader = Just "ctrl+x"
        , kbAppExit = Just "ctrl+c,ctrl+d,<leader>q"
        , kbEditorOpen = Just "<leader>e"
        , kbThemeList = Just "<leader>t"
        , kbSidebarToggle = Just "<leader>b"
        , kbScrollbarToggle = Just "none"
        , kbUsernameToggle = Just "none"
        , kbStatusView = Just "<leader>s"
        , kbSessionExport = Just "<leader>x"
        , kbSessionNew = Just "<leader>n"
        , kbSessionList = Just "<leader>l"
        , kbSessionTimeline = Just "<leader>g"
        , kbSessionFork = Just "none"
        , kbSessionRename = Just "ctrl+r"
        , kbSessionDelete = Just "ctrl+d"
        , kbStashDelete = Just "ctrl+d"
        , kbModelProviderList = Just "ctrl+a"
        , kbModelFavoriteToggle = Just "ctrl+f"
        , kbSessionShare = Just "none"
        , kbSessionUnshare = Just "none"
        , kbSessionInterrupt = Just "escape"
        , kbSessionCompact = Just "<leader>c"
        , kbMessagesPageUp = Just "pageup,ctrl+alt+b"
        , kbMessagesPageDown = Just "pagedown,ctrl+alt+f"
        , kbMessagesLineUp = Just "ctrl+alt+y"
        , kbMessagesLineDown = Just "ctrl+alt+e"
        , kbMessagesHalfPageUp = Just "ctrl+alt+u"
        , kbMessagesHalfPageDown = Just "ctrl+alt+d"
        , kbMessagesFirst = Just "ctrl+g,home"
        , kbMessagesLast = Just "ctrl+alt+g,end"
        , kbMessagesNext = Just "none"
        , kbMessagesPrevious = Just "none"
        , kbMessagesLastUser = Just "none"
        , kbMessagesCopy = Just "<leader>y"
        , kbMessagesUndo = Just "<leader>u"
        , kbMessagesRedo = Just "<leader>r"
        , kbMessagesToggleConceal = Just "<leader>h"
        , kbToolDetails = Just "none"
        , kbModelList = Just "<leader>m"
        , kbModelCycleRecent = Just "f2"
        , kbModelCycleRecentReverse = Just "shift+f2"
        , kbModelCycleFavorite = Just "none"
        , kbModelCycleFavoriteReverse = Just "none"
        , kbCommandList = Just "ctrl+p"
        , kbAgentList = Just "<leader>a"
        , kbAgentCycle = Just "tab"
        , kbAgentCycleReverse = Just "shift+tab"
        , kbVariantCycle = Just "ctrl+t"
        , kbInputClear = Just "ctrl+c"
        , kbInputPaste = Just "ctrl+v"
        , kbInputSubmit = Just "return"
        , kbInputNewline = Just "shift+return,ctrl+return,alt+return,ctrl+j"
        , kbInputMoveLeft = Just "left,ctrl+b"
        , kbInputMoveRight = Just "right,ctrl+f"
        , kbInputMoveUp = Just "up"
        , kbInputMoveDown = Just "down"
        , kbInputSelectLeft = Just "shift+left"
        , kbInputSelectRight = Just "shift+right"
        , kbInputSelectUp = Just "shift+up"
        , kbInputSelectDown = Just "shift+down"
        , kbInputLineHome = Just "ctrl+a"
        , kbInputLineEnd = Just "ctrl+e"
        , kbInputSelectLineHome = Just "ctrl+shift+a"
        , kbInputSelectLineEnd = Just "ctrl+shift+e"
        , kbInputVisualLineHome = Just "alt+a"
        , kbInputVisualLineEnd = Just "alt+e"
        , kbInputSelectVisualLineHome = Just "alt+shift+a"
        , kbInputSelectVisualLineEnd = Just "alt+shift+e"
        , kbInputBufferHome = Just "home"
        , kbInputBufferEnd = Just "end"
        , kbInputSelectBufferHome = Just "shift+home"
        , kbInputSelectBufferEnd = Just "shift+end"
        , kbInputDeleteLine = Just "ctrl+shift+d"
        , kbInputDeleteToLineEnd = Just "ctrl+k"
        , kbInputDeleteToLineStart = Just "ctrl+u"
        , kbInputBackspace = Just "backspace,shift+backspace"
        , kbInputDelete = Just "ctrl+d,delete,shift+delete"
        , kbInputUndo = Just "ctrl+-,super+z"
        , kbInputRedo = Just "ctrl+.,super+shift+z"
        , kbInputWordForward = Just "alt+f,alt+right,ctrl+right"
        , kbInputWordBackward = Just "alt+b,alt+left,ctrl+left"
        , kbInputSelectWordForward = Just "alt+shift+f,alt+shift+right"
        , kbInputSelectWordBackward = Just "alt+shift+b,alt+shift+left"
        , kbInputDeleteWordForward = Just "alt+d,alt+delete,ctrl+delete"
        , kbInputDeleteWordBackward = Just "ctrl+w,ctrl+backspace,alt+backspace"
        , kbHistoryPrevious = Just "up"
        , kbHistoryNext = Just "down"
        , kbSessionChildCycle = Just "<leader>right"
        , kbSessionChildCycleReverse = Just "<leader>left"
        , kbSessionParent = Just "<leader>up"
        , kbTerminalSuspend = Just "ctrl+z"
        , kbTerminalTitleToggle = Just "none"
        , kbTipsToggle = Just "<leader>h"
        , kbDisplayThinking = Just "none"
        }

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Server Config
-- ════════════════════════════════════════════════════════════════════════════

data ServerConfig = ServerConfig
    { scHostname :: Maybe Text
    , scPort :: Maybe Int
    , scMdns :: Maybe Bool
    , scCors :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ServerConfig where
    toJSON sc =
        object
            [ "hostname" .= scHostname sc
            , "port" .= scPort sc
            , "mdns" .= scMdns sc
            , "cors" .= scCors sc
            ]

instance FromJSON ServerConfig where
    parseJSON = withObject "ServerConfig" $ \v ->
        ServerConfig
            <$> v .:? "hostname"
            <*> v .:? "port"
            <*> v .:? "mdns"
            <*> v .:? "cors"

-- ════════════════════════════════════════════════════════════════════════════
--                                                                 TUI Config
-- ════════════════════════════════════════════════════════════════════════════

data TUIConfig = TUIConfig
    { tuiScrollSpeed :: Maybe Int
    , tuiScrollAcceleration :: Maybe Int
    , tuiDiffStyle :: Maybe DiffStyle
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON TUIConfig where
    toJSON t =
        object
            [ "scroll_speed" .= tuiScrollSpeed t
            , "scroll_acceleration" .= tuiScrollAcceleration t
            , "diff_style" .= tuiDiffStyle t
            ]

instance FromJSON TUIConfig where
    parseJSON = withObject "TUIConfig" $ \v ->
        TUIConfig
            <$> v .:? "scroll_speed"
            <*> v .:? "scroll_acceleration"
            <*> v .:? "diff_style"

-- ════════════════════════════════════════════════════════════════════════════
--                                                          Permission Config
-- ════════════════════════════════════════════════════════════════════════════

data PermissionRule
    = PermAction PermissionAction
    | PermByPath (Map Text PermissionAction)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON PermissionRule where
    toJSON (PermAction a) = toJSON a
    toJSON (PermByPath m) = toJSON m

instance FromJSON PermissionRule where
    parseJSON v =
        (PermAction <$> parseJSON v)
            <|> (PermByPath <$> parseJSON v)

data PermissionConfig = PermissionConfig
    { permRead :: Maybe PermissionRule
    , permEdit :: Maybe PermissionRule
    , permGlob :: Maybe PermissionRule
    , permGrep :: Maybe PermissionRule
    , permList :: Maybe PermissionRule
    , permBash :: Maybe PermissionRule
    , permTask :: Maybe PermissionRule
    , permExternalDirectory :: Maybe PermissionRule
    , permTodowrite :: Maybe PermissionAction
    , permTodoread :: Maybe PermissionAction
    , permQuestion :: Maybe PermissionAction
    , permWebfetch :: Maybe PermissionAction
    , permWebsearch :: Maybe PermissionAction
    , permCodesearch :: Maybe PermissionAction
    , permLsp :: Maybe PermissionRule
    , permDoomLoop :: Maybe PermissionAction
    , permSkill :: Maybe PermissionRule
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON PermissionConfig where
    toJSON p =
        object
            [ "read" .= permRead p
            , "edit" .= permEdit p
            , "glob" .= permGlob p
            , "grep" .= permGrep p
            , "list" .= permList p
            , "bash" .= permBash p
            , "task" .= permTask p
            , "external_directory" .= permExternalDirectory p
            , "todowrite" .= permTodowrite p
            , "todoread" .= permTodoread p
            , "question" .= permQuestion p
            , "webfetch" .= permWebfetch p
            , "websearch" .= permWebsearch p
            , "codesearch" .= permCodesearch p
            , "lsp" .= permLsp p
            , "doom_loop" .= permDoomLoop p
            , "skill" .= permSkill p
            ]

instance FromJSON PermissionConfig where
    parseJSON = withObject "PermissionConfig" $ \v ->
        PermissionConfig
            <$> v .:? "read"
            <*> v .:? "edit"
            <*> v .:? "glob"
            <*> v .:? "grep"
            <*> v .:? "list"
            <*> v .:? "bash"
            <*> v .:? "task"
            <*> v .:? "external_directory"
            <*> v .:? "todowrite"
            <*> v .:? "todoread"
            <*> v .:? "question"
            <*> v .:? "webfetch"
            <*> v .:? "websearch"
            <*> v .:? "codesearch"
            <*> v .:? "lsp"
            <*> v .:? "doom_loop"
            <*> v .:? "skill"

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Other Configs
-- ════════════════════════════════════════════════════════════════════════════

data CompactionConfig = CompactionConfig
    { compAuto :: Maybe Bool
    , compPrune :: Maybe Bool
    , compReserved :: Maybe Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON CompactionConfig where
    toJSON c =
        object
            [ "auto" .= compAuto c
            , "prune" .= compPrune c
            , "reserved" .= compReserved c
            ]

instance FromJSON CompactionConfig where
    parseJSON = withObject "CompactionConfig" $ \v ->
        CompactionConfig
            <$> v .:? "auto"
            <*> v .:? "prune"
            <*> v .:? "reserved"

data ExperimentalConfig = ExperimentalConfig
    { expThinking :: Maybe Bool
    , expWorktree :: Maybe Bool
    , expFileCache :: Maybe Bool
    , expParallelTools :: Maybe Bool
    , expStreaming :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ExperimentalConfig where
    toJSON e =
        object
            [ "thinking" .= expThinking e
            , "worktree" .= expWorktree e
            , "fileCache" .= expFileCache e
            , "parallelTools" .= expParallelTools e
            , "streaming" .= expStreaming e
            ]

instance FromJSON ExperimentalConfig where
    parseJSON = withObject "ExperimentalConfig" $ \v ->
        ExperimentalConfig
            <$> v .:? "thinking"
            <*> v .:? "worktree"
            <*> v .:? "fileCache"
            <*> v .:? "parallelTools"
            <*> v .:? "streaming"

data EnterpriseConfig = EnterpriseConfig
    { entUrl :: Maybe Text
    , entApiKey :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON EnterpriseConfig where
    toJSON e =
        object
            [ "url" .= entUrl e
            , "apiKey" .= entApiKey e
            ]

instance FromJSON EnterpriseConfig where
    parseJSON = withObject "EnterpriseConfig" $ \v ->
        EnterpriseConfig
            <$> v .:? "url"
            <*> v .:? "apiKey"

newtype WatcherConfig = WatcherConfig
    { watchIgnore :: Maybe [Text]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON WatcherConfig where
    toJSON w = object ["ignore" .= watchIgnore w]

instance FromJSON WatcherConfig where
    parseJSON = withObject "WatcherConfig" $ \v ->
        WatcherConfig <$> v .:? "ignore"

-- ════════════════════════════════════════════════════════════════════════════
--                                                                      Agent
-- ════════════════════════════════════════════════════════════════════════════

data AgentColor
    = AgentColorHex Text
    | AgentColorTheme Text
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON AgentColor where
    toJSON (AgentColorHex h) = String h
    toJSON (AgentColorTheme t) = String t

instance FromJSON AgentColor where
    parseJSON = withText "AgentColor" (pure . AgentColorHex)

data AgentConfig = AgentConfig
    { acModel :: Maybe Text
    , acMaxTokens :: Maybe Int
    , acSystemPrompt :: Maybe Text
    , acTools :: Maybe [Text]
    , acMode :: Maybe AgentMode
    , acColor :: Maybe AgentColor
    , acDescription :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON AgentConfig where
    toJSON ac =
        object
            [ "model" .= acModel ac
            , "maxTokens" .= acMaxTokens ac
            , "systemPrompt" .= acSystemPrompt ac
            , "tools" .= acTools ac
            , "mode" .= acMode ac
            , "color" .= acColor ac
            , "description" .= acDescription ac
            ]

instance FromJSON AgentConfig where
    parseJSON = withObject "AgentConfig" $ \v ->
        AgentConfig
            <$> v .:? "model"
            <*> v .:? "maxTokens"
            <*> v .:? "systemPrompt"
            <*> v .:? "tools"
            <*> v .:? "mode"
            <*> v .:? "color"
            <*> v .:? "description"

-- ════════════════════════════════════════════════════════════════════════════
--                                                                    Provider
-- ════════════════════════════════════════════════════════════════════════════

data ProviderTimeout
    = ProviderTimeoutDisabled
    | ProviderTimeoutSeconds Int
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ProviderTimeout where
    toJSON ProviderTimeoutDisabled = Bool False
    toJSON (ProviderTimeoutSeconds n) = Number (fromIntegral n)

instance FromJSON ProviderTimeout where
    parseJSON (Bool False) = pure ProviderTimeoutDisabled
    parseJSON (Number n) = pure $ ProviderTimeoutSeconds (round n)
    parseJSON other = fail $ "Unknown ProviderTimeout: " <> show other

data ProviderModel = ProviderModel
    { pmId :: Text
    , pmName :: Maybe Text
    , pmContextLength :: Maybe Int
    , pmMaxOutput :: Maybe Int
    , pmStatus :: Maybe ModelStatus
    , pmHidden :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ProviderModel where
    toJSON pm =
        object
            [ "id" .= pmId pm
            , "name" .= pmName pm
            , "contextLength" .= pmContextLength pm
            , "maxOutput" .= pmMaxOutput pm
            , "status" .= pmStatus pm
            , "hidden" .= pmHidden pm
            ]

instance FromJSON ProviderModel where
    parseJSON = withObject "ProviderModel" $ \v ->
        ProviderModel
            <$> v .: "id"
            <*> v .:? "name"
            <*> v .:? "contextLength"
            <*> v .:? "maxOutput"
            <*> v .:? "status"
            <*> v .:? "hidden"

data ProviderOptions = ProviderOptions
    { poThinking :: Maybe Bool
    , poVersion :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ProviderOptions where
    toJSON po =
        object
            [ "thinking" .= poThinking po
            , "version" .= poVersion po
            ]

instance FromJSON ProviderOptions where
    parseJSON = withObject "ProviderOptions" $ \v ->
        ProviderOptions
            <$> v .:? "thinking"
            <*> v .:? "version"

data ProviderConfig = ProviderConfig
    { pcApi :: Maybe Text
    , pcModels :: Maybe [ProviderModel]
    , pcOptions :: Maybe ProviderOptions
    , pcTimeout :: Maybe ProviderTimeout
    , pcDisabled :: Maybe Bool
    , pcName :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ProviderConfig where
    toJSON pc =
        object
            [ "api" .= pcApi pc
            , "models" .= pcModels pc
            , "options" .= pcOptions pc
            , "timeout" .= pcTimeout pc
            , "disabled" .= pcDisabled pc
            , "name" .= pcName pc
            ]

instance FromJSON ProviderConfig where
    parseJSON = withObject "ProviderConfig" $ \v ->
        ProviderConfig
            <$> v .:? "api"
            <*> v .:? "models"
            <*> v .:? "options"
            <*> v .:? "timeout"
            <*> v .:? "disabled"
            <*> v .:? "name"

-- ════════════════════════════════════════════════════════════════════════════
--                                                                         MCP
-- ════════════════════════════════════════════════════════════════════════════

data MCPLocal = MCPLocal
    { mcplCommand :: [Text]
    , mcplEnvironment :: Maybe (Map Text Text)
    , mcplEnabled :: Maybe Bool
    , mcplTimeout :: Maybe Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON MCPLocal where
    toJSON m =
        object
            [ "type" .= ("local" :: Text)
            , "command" .= mcplCommand m
            , "environment" .= mcplEnvironment m
            , "enabled" .= mcplEnabled m
            , "timeout" .= mcplTimeout m
            ]

instance FromJSON MCPLocal where
    parseJSON = withObject "MCPLocal" $ \v ->
        MCPLocal
            <$> v .: "command"
            <*> v .:? "environment"
            <*> v .:? "enabled"
            <*> v .:? "timeout"

data MCPRemote = MCPRemote
    { mcprUrl :: Text
    , mcprEnabled :: Maybe Bool
    , mcprHeaders :: Maybe (Map Text Text)
    , mcprTimeout :: Maybe Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON MCPRemote where
    toJSON m =
        object
            [ "type" .= ("remote" :: Text)
            , "url" .= mcprUrl m
            , "enabled" .= mcprEnabled m
            , "headers" .= mcprHeaders m
            , "timeout" .= mcprTimeout m
            ]

instance FromJSON MCPRemote where
    parseJSON = withObject "MCPRemote" $ \v ->
        MCPRemote
            <$> v .: "url"
            <*> v .:? "enabled"
            <*> v .:? "headers"
            <*> v .:? "timeout"

data MCPConfig
    = MCPConfigLocal MCPLocal
    | MCPConfigRemote MCPRemote
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON MCPConfig where
    toJSON (MCPConfigLocal l) = toJSON l
    toJSON (MCPConfigRemote r) = toJSON r

instance FromJSON MCPConfig where
    parseJSON v@(Object o) = do
        mType <- o .:? "type" :: Parser (Maybe Text)
        case mType of
            Just "remote" -> MCPConfigRemote <$> parseJSON v
            Just "local" -> MCPConfigLocal <$> parseJSON v
            Just _unknownType -> MCPConfigLocal <$> parseJSON v -- Default to local for unknown types
            Nothing -> MCPConfigLocal <$> parseJSON v -- Default to local when type is not specified
    parseJSON other = fail $ "Unknown MCP config: " <> show other

-- ════════════════════════════════════════════════════════════════════════════
--                                                           Formatter & LSP
-- ════════════════════════════════════════════════════════════════════════════

data FormatterEntry = FormatterEntry
    { feCommand :: [Text]
    , feTimeout :: Maybe Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON FormatterEntry where
    toJSON fe =
        object
            [ "command" .= feCommand fe
            , "timeout" .= feTimeout fe
            ]

instance FromJSON FormatterEntry where
    parseJSON = withObject "FormatterEntry" $ \v ->
        FormatterEntry
            <$> v .: "command"
            <*> v .:? "timeout"

data FormatterConfig
    = FormatterDisabled
    | FormatterEnabled (Map Text FormatterEntry)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON FormatterConfig where
    toJSON FormatterDisabled = Bool False
    toJSON (FormatterEnabled m) = toJSON m

instance FromJSON FormatterConfig where
    parseJSON (Bool False) = pure FormatterDisabled
    parseJSON v@(Object _) = FormatterEnabled <$> parseFormatterMap v
    parseJSON other = fail $ "Unknown FormatterConfig: " <> show other

parseFormatterMap :: Value -> Parser (Map Text FormatterEntry)
parseFormatterMap = withObject "FormatterMap" $ \obj ->
    Map.fromList
        <$> traverse
            (\(k, val) -> (Key.toText k,) <$> parseJSON val)
            (KM.toList obj)

data LSPEntry = LSPEntry
    { lspCommand :: [Text]
    , lspArgs :: Maybe [Text]
    , lspInitializationOptions :: Maybe Text
    , lspRootUri :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON LSPEntry where
    toJSON e =
        object
            [ "command" .= lspCommand e
            , "args" .= lspArgs e
            , "initializationOptions" .= lspInitializationOptions e
            , "rootUri" .= lspRootUri e
            ]

instance FromJSON LSPEntry where
    parseJSON = withObject "LSPEntry" $ \v ->
        LSPEntry
            <$> v .: "command"
            <*> v .:? "args"
            <*> v .:? "initializationOptions"
            <*> v .:? "rootUri"

data LSPConfig
    = LSPDisabled
    | LSPEnabled (Map Text LSPEntry)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON LSPConfig where
    toJSON LSPDisabled = Bool False
    toJSON (LSPEnabled m) = toJSON m

instance FromJSON LSPConfig where
    parseJSON (Bool False) = pure LSPDisabled
    parseJSON v@(Object _) = LSPEnabled <$> parseLSPMap v
    parseJSON other = fail $ "Unknown LSPConfig: " <> show other

parseLSPMap :: Value -> Parser (Map Text LSPEntry)
parseLSPMap = withObject "LSPMap" $ \obj ->
    Map.fromList
        <$> traverse
            (\(k, val) -> (Key.toText k,) <$> parseJSON val)
            (KM.toList obj)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                       Theme
-- ════════════════════════════════════════════════════════════════════════════

data Color = Color
    { colorR :: Double
    , colorG :: Double
    , colorB :: Double
    , colorA :: Double
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON Color where
    toJSON c =
        object
            [ "r" .= colorR c
            , "g" .= colorG c
            , "b" .= colorB c
            , "a" .= colorA c
            ]

instance FromJSON Color where
    parseJSON = withObject "Color" $ \v ->
        Color
            <$> v .: "r"
            <*> v .: "g"
            <*> v .: "b"
            <*> v .: "a"

-- | Theme config (simplified - full theme has 50+ colors)
data ThemeConfig = ThemeConfig
    { thPrimary :: Color
    , thSecondary :: Color
    , thAccent :: Color
    , thError :: Color
    , thWarning :: Color
    , thSuccess :: Color
    , thInfo :: Color
    , thText :: Color
    , thTextMuted :: Color
    , thBackground :: Color
    , thBackgroundPanel :: Color
    , thBackgroundElement :: Color
    , thBorder :: Color
    , thBorderActive :: Color
    , thBorderSubtle :: Color
    , thThinkingOpacity :: Double
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ThemeConfig where
    toJSON t =
        object
            [ "primary" .= thPrimary t
            , "secondary" .= thSecondary t
            , "accent" .= thAccent t
            , "error" .= thError t
            , "warning" .= thWarning t
            , "success" .= thSuccess t
            , "info" .= thInfo t
            , "text" .= thText t
            , "textMuted" .= thTextMuted t
            , "background" .= thBackground t
            , "backgroundPanel" .= thBackgroundPanel t
            , "backgroundElement" .= thBackgroundElement t
            , "border" .= thBorder t
            , "borderActive" .= thBorderActive t
            , "borderSubtle" .= thBorderSubtle t
            , "thinkingOpacity" .= thThinkingOpacity t
            ]

instance FromJSON ThemeConfig where
    parseJSON = withObject "ThemeConfig" $ \v ->
        ThemeConfig
            <$> v .: "primary"
            <*> v .: "secondary"
            <*> v .: "accent"
            <*> v .: "error"
            <*> v .: "warning"
            <*> v .: "success"
            <*> v .: "info"
            <*> v .: "text"
            <*> v .: "textMuted"
            <*> v .: "background"
            <*> v .: "backgroundPanel"
            <*> v .: "backgroundElement"
            <*> v .: "border"
            <*> v .: "borderActive"
            <*> v .: "borderSubtle"
            <*> v .: "thinkingOpacity"

-- ════════════════════════════════════════════════════════════════════════════
--                                                            Skill & Command
-- ════════════════════════════════════════════════════════════════════════════

data SkillConfig = SkillConfig
    { skillName :: Text
    , skillDescription :: Text
    , skillPrompt :: Text
    , skillTools :: Maybe [Text]
    , skillAgent :: Maybe Text
    , skillModel :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON SkillConfig where
    toJSON s =
        object
            [ "name" .= skillName s
            , "description" .= skillDescription s
            , "prompt" .= skillPrompt s
            , "tools" .= skillTools s
            , "agent" .= skillAgent s
            , "model" .= skillModel s
            ]

instance FromJSON SkillConfig where
    parseJSON = withObject "SkillConfig" $ \v ->
        SkillConfig
            <$> v .: "name"
            <*> v .: "description"
            <*> v .: "prompt"
            <*> v .:? "tools"
            <*> v .:? "agent"
            <*> v .:? "model"

data CommandConfig = CommandConfig
    { cmdCommand :: Text
    , cmdDescription :: Maybe Text
    , cmdEnvironment :: Maybe (Map Text Text)
    , cmdWorkdir :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON CommandConfig where
    toJSON c =
        object
            [ "command" .= cmdCommand c
            , "description" .= cmdDescription c
            , "environment" .= cmdEnvironment c
            , "workdir" .= cmdWorkdir c
            ]

instance FromJSON CommandConfig where
    parseJSON = withObject "CommandConfig" $ \v ->
        CommandConfig
            <$> v .: "command"
            <*> v .:? "description"
            <*> v .:? "environment"
            <*> v .:? "workdir"

-- ════════════════════════════════════════════════════════════════════════════
--                                                                 Full Config
-- ════════════════════════════════════════════════════════════════════════════

-- | Full configuration type matching Dhall schema
data Config = Config
    { -- Core settings
      cfgModel :: Maybe Text
    , cfgSystemPrompt :: Maybe Text
    , cfgMaxTokens :: Maybe Int
    , cfgLogLevel :: Maybe LogLevel
    , -- Nested configs
      cfgKeybinds :: KeybindsConfig
    , cfgServer :: ServerConfig
    , cfgTui :: TUIConfig
    , cfgPermission :: PermissionConfig
    , cfgCompaction :: CompactionConfig
    , cfgExperimental :: ExperimentalConfig
    , cfgEnterprise :: EnterpriseConfig
    , cfgWatcher :: WatcherConfig
    , -- Map fields
      cfgAgent :: Maybe (Map Text AgentConfig)
    , cfgProvider :: Maybe (Map Text ProviderConfig)
    , cfgMcp :: Maybe (Map Text MCPConfig)
    , cfgFormatter :: Maybe FormatterConfig
    , cfgLsp :: Maybe LSPConfig
    , cfgSkill :: Maybe (Map Text SkillConfig)
    , cfgCommand :: Maybe (Map Text CommandConfig)
    , -- Theme settings
      cfgTheme :: Maybe Text
    , cfgThemes :: Maybe (Map Text ThemeConfig)
    , -- Share settings
      cfgShare :: Maybe ShareMode
    , -- Auto-update
      cfgAutoUpdate :: Maybe AutoUpdate
    , -- Disabled tools
      cfgDisabledTools :: Maybe [Text]
    , -- Instrumentation
      cfgInstrumentation :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON Config where
    toJSON c =
        object
            [ "model" .= cfgModel c
            , "systemPrompt" .= cfgSystemPrompt c
            , "maxTokens" .= cfgMaxTokens c
            , "logLevel" .= cfgLogLevel c
            , "keybinds" .= cfgKeybinds c
            , "server" .= cfgServer c
            , "tui" .= cfgTui c
            , "permission" .= cfgPermission c
            , "compaction" .= cfgCompaction c
            , "experimental" .= cfgExperimental c
            , "enterprise" .= cfgEnterprise c
            , "watcher" .= cfgWatcher c
            , "agent" .= cfgAgent c
            , "provider" .= cfgProvider c
            , "mcp" .= cfgMcp c
            , "formatter" .= cfgFormatter c
            , "lsp" .= cfgLsp c
            , "skill" .= cfgSkill c
            , "command" .= cfgCommand c
            , "theme" .= cfgTheme c
            , "themes" .= cfgThemes c
            , "share" .= cfgShare c
            , "autoUpdate" .= cfgAutoUpdate c
            , "disabledTools" .= cfgDisabledTools c
            , "instrumentation" .= cfgInstrumentation c
            ]

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v ->
        Config
            <$> v .:? "model"
            <*> v .:? "systemPrompt"
            <*> v .:? "maxTokens"
            <*> v .:? "logLevel"
            <*> v .:? "keybinds" .!= defaultKeybinds
            <*> v .:? "server" .!= defaultServer
            <*> v .:? "tui" .!= defaultTUI
            <*> v .:? "permission" .!= defaultPermission
            <*> v .:? "compaction" .!= defaultCompaction
            <*> v .:? "experimental" .!= defaultExperimental
            <*> v .:? "enterprise" .!= defaultEnterprise
            <*> v .:? "watcher" .!= defaultWatcher
            <*> v .:? "agent"
            <*> v .:? "provider"
            <*> v .:? "mcp"
            <*> v .:? "formatter"
            <*> v .:? "lsp"
            <*> v .:? "skill"
            <*> v .:? "command"
            <*> v .:? "theme"
            <*> v .:? "themes"
            <*> v .:? "share"
            <*> v .:? "autoUpdate"
            <*> v .:? "disabledTools"
            <*> v .:? "instrumentation"

-- ════════════════════════════════════════════════════════════════════════════
--                                                                    Defaults
-- ════════════════════════════════════════════════════════════════════════════

defaultServer :: ServerConfig
defaultServer =
    ServerConfig
        { scHostname = Just "localhost"
        , scPort = Just 4096
        , scMdns = Just False
        , scCors = Just True
        }

defaultTUI :: TUIConfig
defaultTUI =
    TUIConfig
        { tuiScrollSpeed = Just 1
        , tuiScrollAcceleration = Just 1
        , tuiDiffStyle = Just DiffAuto
        }

defaultPermission :: PermissionConfig
defaultPermission =
    PermissionConfig
        { permRead = Nothing
        , permEdit = Nothing
        , permGlob = Nothing
        , permGrep = Nothing
        , permList = Nothing
        , permBash = Nothing
        , permTask = Nothing
        , permExternalDirectory = Nothing
        , permTodowrite = Nothing
        , permTodoread = Nothing
        , permQuestion = Nothing
        , permWebfetch = Nothing
        , permWebsearch = Nothing
        , permCodesearch = Nothing
        , permLsp = Nothing
        , permDoomLoop = Nothing
        , permSkill = Nothing
        }

defaultCompaction :: CompactionConfig
defaultCompaction =
    CompactionConfig
        { compAuto = Just False
        , compPrune = Just False
        , compReserved = Just 8192
        }

defaultExperimental :: ExperimentalConfig
defaultExperimental =
    ExperimentalConfig
        { expThinking = Just False
        , expWorktree = Just False
        , expFileCache = Just True
        , expParallelTools = Just True
        , expStreaming = Just True
        }

defaultEnterprise :: EnterpriseConfig
defaultEnterprise =
    EnterpriseConfig
        { entUrl = Nothing
        , entApiKey = Nothing
        }

defaultWatcher :: WatcherConfig
defaultWatcher =
    WatcherConfig
        { watchIgnore =
            Just
                [ "node_modules"
                , ".git"
                , "dist"
                , "build"
                , ".next"
                , "target"
                , "__pycache__"
                , ".venv"
                , "venv"
                ]
        }

-- | Default config with all keybinds populated
defaultConfig :: Config
defaultConfig =
    Config
        { cfgModel = Nothing
        , cfgSystemPrompt = Nothing
        , cfgMaxTokens = Nothing
        , cfgLogLevel = Just INFO
        , cfgKeybinds = defaultKeybinds
        , cfgServer = defaultServer
        , cfgTui = defaultTUI
        , cfgPermission = defaultPermission
        , cfgCompaction = defaultCompaction
        , cfgExperimental = defaultExperimental
        , cfgEnterprise = defaultEnterprise
        , cfgWatcher = defaultWatcher
        , cfgAgent = Nothing
        , cfgProvider = Nothing
        , cfgMcp = Nothing
        , cfgFormatter = Nothing
        , cfgLsp = Nothing
        , cfgSkill = Nothing
        , cfgCommand = Nothing
        , cfgTheme = Just "ono-sendai"
        , cfgThemes = Nothing
        , cfgShare = Nothing
        , cfgAutoUpdate = Just AutoUpdateNotify
        , cfgDisabledTools = Nothing
        , cfgInstrumentation = Just False
        }
