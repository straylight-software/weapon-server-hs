{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Config.Types
Description : Configuration type definitions for weapon server

This module defines all configuration types used by the weapon server.
These types match the Dhall configuration schema exactly and provide
JSON serialization for API responses.

= Type Hierarchy

The main 'Config' type contains:

* Core settings (model, systemPrompt, maxTokens, logLevel)
* Nested config records ('KeybindsConfig', 'ServerConfig', etc.)
* Optional map fields for agents, providers, MCP servers
* Theme and share settings

= JSON Encoding

All types provide 'ToJSON' and 'FromJSON' instances that use snake_case
for JSON keys to match the TypeScript client expectations. The 'FromDhall'
instances use the Dhall naming conventions.

= Defaults

Use 'defaultConfig' to get a fully-populated configuration with sensible
defaults. Individual default records are also exported for partial overrides.
-}
module Config.Types (
    -- * Main Config
    Config (..),

    -- * Enums

    -- | Enumeration types for configuration options
    LogLevel (..),
    ShareMode (..),
    Layout (..),
    PermissionAction (..),
    DiffStyle (..),
    AgentMode (..),
    ModelStatus (..),
    AutoUpdate (..),

    -- * Nested Configs

    -- | Record types for grouped configuration settings
    KeybindsConfig (..),
    ServerConfig (..),
    TUIConfig (..),
    ScrollAccelerationConfig (..),
    PermissionConfig (..),
    PermissionRule (..),
    CompactionConfig (..),
    ExperimentalConfig (..),
    EnterpriseConfig (..),
    WatcherConfig (..),

    -- * Agent

    -- | Agent configuration for custom AI personas
    AgentConfig (..),
    AgentColor (..),

    -- * Provider

    -- | LLM provider configuration
    ProviderConfig (..),
    ProviderModel (..),
    ProviderOptions (..),
    ProviderTimeout (..),

    -- * MCP

    -- | Model Context Protocol server configuration
    MCPConfig (..),
    MCPLocal (..),
    MCPRemote (..),

    -- * Formatter & LSP

    -- | Code formatter and LSP server configuration
    FormatterConfig (..),
    FormatterEntry (..),
    LSPConfig (..),
    LSPEntry (..),

    -- * Theme

    -- | UI theme configuration
    ThemeConfig (..),
    Color (..),

    -- * Skill & Command

    -- | Custom skill and command definitions
    SkillConfig (..),
    CommandConfig (..),

    -- * Telemetry

    -- | Telemetry and R2 storage configuration
    TelemetryConfig (..),
    R2StorageConfig (..),

    -- * Defaults

    -- | Default configuration values
    defaultConfig,
    defaultKeybinds,
    defaultServer,
    defaultTUI,
    defaultPermission,
    defaultCompaction,
    defaultExperimental,
    defaultEnterprise,
    defaultWatcher,
) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair, Parser)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)

{- | Try two optional parsers and return the first Just result
Unlike <|>, this combines the Maybe results, not the Parser success/failure
-}
firstJust :: Parser (Maybe a) -> Parser (Maybe a) -> Parser (Maybe a)
firstJust = liftA2 (<|>)

-- ════════════════════════════════════════════════════════════════════════════
--                                                                      Enums
-- ════════════════════════════════════════════════════════════════════════════

{- | Log level for controlling verbosity of server output.

Log levels are ordered by severity: DEBUG < INFO < WARN < ERROR
-}
data LogLevel
    = -- | Detailed debugging information
      DEBUG
    | -- | Normal operational messages
      INFO
    | -- | Warning conditions that may need attention
      WARN
    | -- | Error conditions that require attention
      ERROR
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

-- | Session sharing mode for collaborative features.
data ShareMode
    = -- | Require explicit user action to share
      ShareManual
    | -- | Automatically share sessions
      ShareAuto
    | -- | Disable sharing entirely
      ShareDisabled
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

-- | Layout mode for UI elements.
data Layout
    = -- | Automatically adjust layout based on content
      LayoutAuto
    | -- | Stretch to fill available space
      LayoutStretch
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

{- | Permission action for tool execution requests.

Used to control whether tools require user approval.
-}
data PermissionAction
    = -- | Prompt the user for approval
      PermAsk
    | -- | Automatically allow the action
      PermAllow
    | -- | Automatically deny the action
      PermDeny
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

-- | Diff display style in the TUI.
data DiffStyle
    = -- | Automatically choose based on terminal width
      DiffAuto
    | -- | Always show diffs in stacked (unified) format
      DiffStacked
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

{- | Agent visibility mode.

Controls where an agent appears in the UI and how it can be used.
-}
data AgentMode
    = -- | Only available as a subagent (Task tool)
      AgentSubagent
    | -- | Only available as the primary agent
      AgentPrimary
    | -- | Available in both contexts
      AgentAll
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

{- | Model release status.

Indicates the maturity level of an LLM model.
-}
data ModelStatus
    = -- | Early testing, may have significant issues
      ModelAlpha
    | -- | Beta testing, mostly stable
      ModelBeta
    | -- | No longer recommended for use
      ModelDeprecated
    | -- | Production ready
      ModelStable
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

-- | Auto-update behavior for the application.
data AutoUpdate
    = -- | Automatically download and install updates
      AutoUpdateEnabled
    | -- | Do not check for or install updates
      AutoUpdateDisabled
    | -- | Check for updates and notify, but don't auto-install
      AutoUpdateNotify
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

{- | Keybinds configuration for TUI keyboard shortcuts.

Contains all 90+ keybinds that control the application. Each keybind
is optional ('Maybe Text') and can contain comma-separated alternatives.

= Keybind Format

Keybinds use a simple string format:

* @"ctrl+c"@ - Ctrl+C
* @"ctrl+c,ctrl+d"@ - Either Ctrl+C or Ctrl+D
* @"\<leader\>q"@ - Leader key followed by Q
* @"none"@ - Explicitly disabled

= Critical Keybinds

The most important keybinds that should always be set:

* 'kbAppExit' - Exit the application (default: @"ctrl+c,ctrl+d,\<leader\>q"@)
* 'kbSessionInterrupt' - Interrupt current operation (default: @"escape"@)
* 'kbLeader' - Leader key prefix (default: @"ctrl+x"@)

See 'defaultKeybinds' for all default values.
-}
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

{- | HTTP server configuration.

Controls how the weapon server binds to network interfaces and
handles cross-origin requests.

@
server:
  hostname: "localhost"
  port: 4096
  mdns: false
  cors: ["https://example.com"]
@
-}
data ServerConfig = ServerConfig
    { scHostname :: Maybe Text
    -- ^ Hostname to bind to (default: "localhost")
    , scPort :: Maybe Int
    -- ^ Port number (default: 4096)
    , scMdns :: Maybe Bool
    -- ^ Enable mDNS discovery (default: false)
    , scMdnsDomain :: Maybe Text
    -- ^ Custom domain name for mDNS service
    , scCors :: Maybe [Text]
    -- ^ Additional domains to allow for CORS
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ServerConfig where
    toJSON sc =
        object $
            catMaybes
                [ fmap ("hostname" .=) (scHostname sc)
                , fmap ("port" .=) (scPort sc)
                , fmap ("mdns" .=) (scMdns sc)
                , fmap ("mdnsDomain" .=) (scMdnsDomain sc)
                , fmap ("cors" .=) (scCors sc)
                ]

instance FromJSON ServerConfig where
    parseJSON = withObject "ServerConfig" $ \v ->
        ServerConfig
            <$> v .:? "hostname"
            <*> v .:? "port"
            <*> v .:? "mdns"
            <*> v .:? "mdnsDomain"
            <*> v .:? "cors"

-- ════════════════════════════════════════════════════════════════════════════
--                                                                 TUI Config
-- ════════════════════════════════════════════════════════════════════════════

-- | Scroll acceleration configuration.
newtype ScrollAccelerationConfig = ScrollAccelerationConfig
    { sacEnabled :: Bool
    -- ^ Whether scroll acceleration is enabled
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ScrollAccelerationConfig where
    toJSON s = object ["enabled" .= sacEnabled s]

instance FromJSON ScrollAccelerationConfig where
    parseJSON = withObject "ScrollAccelerationConfig" $ \v ->
        ScrollAccelerationConfig <$> v .: "enabled"

{- | Terminal UI configuration.

Controls scrolling behavior and diff display in the TUI.
-}
data TUIConfig = TUIConfig
    { tuiScrollSpeed :: Maybe Double
    -- ^ Scroll speed (default: 1.0)
    , tuiScrollAcceleration :: Maybe ScrollAccelerationConfig
    -- ^ Scroll acceleration settings
    , tuiDiffStyle :: Maybe DiffStyle
    -- ^ How to display file diffs (default: 'DiffAuto')
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON TUIConfig where
    toJSON t =
        object $
            catMaybes
                [ fmap ("scroll_speed" .=) (tuiScrollSpeed t)
                , fmap ("scroll_acceleration" .=) (tuiScrollAcceleration t)
                , fmap ("diff_style" .=) (tuiDiffStyle t)
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

{- | A permission rule that can be either a single action or path-based.

Path-based rules allow different permissions for different paths:

@
permission:
  bash:
    "/home/user/safe": "allow"
    "*": "ask"
@
-}
data PermissionRule
    = -- | A single permission for all paths
      PermAction PermissionAction
    | -- | Path-specific permissions (glob patterns supported)
      PermByPath (Map Text PermissionAction)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON PermissionRule where
    toJSON (PermAction a) = toJSON a
    toJSON (PermByPath m) = toJSON m

instance FromJSON PermissionRule where
    parseJSON v =
        (PermAction <$> parseJSON v)
            <|> (PermByPath <$> parseJSON v)

{- | Tool permission configuration.

Controls which tools require user approval before execution.
Tools that modify files or execute commands typically require
more restrictive permissions.
-}
data PermissionConfig = PermissionConfig
    { permRead :: Maybe PermissionRule
    -- ^ Read tool - read file contents
    , permEdit :: Maybe PermissionRule
    -- ^ Edit tool - modify file contents
    , permGlob :: Maybe PermissionRule
    -- ^ Glob tool - search for files by pattern
    , permGrep :: Maybe PermissionRule
    -- ^ Grep tool - search file contents
    , permList :: Maybe PermissionRule
    -- ^ List tool - list directory contents
    , permBash :: Maybe PermissionRule
    -- ^ Bash tool - execute shell commands
    , permTask :: Maybe PermissionRule
    -- ^ Task tool - spawn subagent tasks
    , permExternalDirectory :: Maybe PermissionRule
    -- ^ Access directories outside project root
    , permTodowrite :: Maybe PermissionAction
    -- ^ TodoWrite tool - manage task lists
    , permTodoread :: Maybe PermissionAction
    -- ^ TodoRead tool - read task lists
    , permQuestion :: Maybe PermissionAction
    -- ^ Question tool - ask user questions
    , permWebfetch :: Maybe PermissionAction
    -- ^ WebFetch tool - fetch web content
    , permWebsearch :: Maybe PermissionAction
    -- ^ WebSearch tool - search the web
    , permCodesearch :: Maybe PermissionAction
    -- ^ CodeSearch tool - semantic code search
    , permLsp :: Maybe PermissionRule
    -- ^ LSP tool - language server operations
    , permDoomLoop :: Maybe PermissionAction
    -- ^ Allow agent to continue after repeated failures
    , permSkill :: Maybe PermissionRule
    -- ^ Skill tool - execute custom skills
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON PermissionConfig where
    toJSON p =
        object $
            catMaybes
                [ fmap ("read" .=) (permRead p)
                , fmap ("edit" .=) (permEdit p)
                , fmap ("glob" .=) (permGlob p)
                , fmap ("grep" .=) (permGrep p)
                , fmap ("list" .=) (permList p)
                , fmap ("bash" .=) (permBash p)
                , fmap ("task" .=) (permTask p)
                , fmap ("external_directory" .=) (permExternalDirectory p)
                , fmap ("todowrite" .=) (permTodowrite p)
                , fmap ("todoread" .=) (permTodoread p)
                , fmap ("question" .=) (permQuestion p)
                , fmap ("webfetch" .=) (permWebfetch p)
                , fmap ("websearch" .=) (permWebsearch p)
                , fmap ("codesearch" .=) (permCodesearch p)
                , fmap ("lsp" .=) (permLsp p)
                , fmap ("doom_loop" .=) (permDoomLoop p)
                , fmap ("skill" .=) (permSkill p)
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

{- | Context compaction configuration.

Controls automatic context management when conversation history
exceeds the model's context window.
-}
data CompactionConfig = CompactionConfig
    { compAuto :: Maybe Bool
    -- ^ Enable automatic compaction (default: false)
    , compPrune :: Maybe Bool
    -- ^ Prune old messages instead of summarizing (default: false)
    , compReserved :: Maybe Int
    -- ^ Tokens to reserve for response (default: 8192)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON CompactionConfig where
    toJSON c =
        object $
            catMaybes
                [ fmap ("auto" .=) (compAuto c)
                , fmap ("prune" .=) (compPrune c)
                , fmap ("reserved" .=) (compReserved c)
                ]

instance FromJSON CompactionConfig where
    parseJSON = withObject "CompactionConfig" $ \v ->
        CompactionConfig
            <$> v .:? "auto"
            <*> v .:? "prune"
            <*> v .:? "reserved"

{- | Experimental features configuration.

These features may change or be removed in future versions.
Enable at your own risk. Matches TypeScript Config.experimental schema.
-}
data ExperimentalConfig = ExperimentalConfig
    { expDisablePasteSummary :: Maybe Bool
    -- ^ Disable paste summary feature
    , expBatchTool :: Maybe Bool
    -- ^ Enable the batch tool
    , expOpenTelemetry :: Maybe Bool
    -- ^ Enable OpenTelemetry spans for AI SDK calls
    , expPrimaryTools :: Maybe [Text]
    -- ^ Tools that should only be available to primary agents
    , expContinueLoopOnDeny :: Maybe Bool
    -- ^ Continue the agent loop when a tool call is denied
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ExperimentalConfig where
    toJSON e =
        object $
            catMaybes
                [ fmap ("disable_paste_summary" .=) (expDisablePasteSummary e)
                , fmap ("batch_tool" .=) (expBatchTool e)
                , fmap ("openTelemetry" .=) (expOpenTelemetry e)
                , fmap ("primary_tools" .=) (expPrimaryTools e)
                , fmap ("continue_loop_on_deny" .=) (expContinueLoopOnDeny e)
                ]

instance FromJSON ExperimentalConfig where
    parseJSON = withObject "ExperimentalConfig" $ \v ->
        ExperimentalConfig
            <$> v .:? "disable_paste_summary"
            <*> v .:? "batch_tool"
            <*> v .:? "openTelemetry"
            <*> v .:? "primary_tools"
            <*> v .:? "continue_loop_on_deny"

{- | Enterprise/cloud deployment configuration.

For self-hosted or enterprise deployments that connect to
a central management server.
-}
newtype EnterpriseConfig = EnterpriseConfig
    { entUrl :: Maybe Text
    -- ^ Enterprise server URL
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON EnterpriseConfig where
    toJSON e =
        object $
            catMaybes
                [ fmap ("url" .=) (entUrl e)
                ]

instance FromJSON EnterpriseConfig where
    parseJSON = withObject "EnterpriseConfig" $ \v ->
        EnterpriseConfig
            <$> v .:? "url"

{- | File watcher configuration.

Controls which directories are ignored by the file watcher
to reduce noise and improve performance.
-}
newtype WatcherConfig = WatcherConfig
    { watchIgnore :: Maybe [Text]
    -- ^ Glob patterns for directories to ignore
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

{- | Agent color specification.

Can be either a hex color code or a reference to a theme color.
-}
data AgentColor
    = -- | Hex color code (e.g., "#FF5733")
      AgentColorHex Text
    | -- | Theme color reference (e.g., "primary", "accent")
      AgentColorTheme Text
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON AgentColor where
    toJSON (AgentColorHex h) = String h
    toJSON (AgentColorTheme t) = String t

instance FromJSON AgentColor where
    parseJSON = withText "AgentColor" (pure . AgentColorHex)

{- | Custom agent configuration.

Defines a custom AI persona with specific model, tools, and behavior.

@
agent:
  code-reviewer:
    model: "claude-3-opus"
    systemPrompt: "You are a code review expert..."
    tools: ["read", "grep", "glob"]
    mode: "subagent"
@
-}
data AgentConfig = AgentConfig
    { acModel :: Maybe Text
    -- ^ Model to use (overrides global default)
    , acMaxTokens :: Maybe Int
    -- ^ Max tokens for responses
    , acSystemPrompt :: Maybe Text
    -- ^ Custom system prompt
    , acTools :: Maybe [Text]
    -- ^ List of allowed tools (empty = all)
    , acMode :: Maybe AgentMode
    -- ^ Where this agent can be used
    , acColor :: Maybe AgentColor
    -- ^ UI color for this agent
    , acDescription :: Maybe Text
    -- ^ Human-readable description
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

{- | Provider request timeout configuration.

Controls how long to wait for provider responses.
-}
data ProviderTimeout
    = -- | No timeout (wait indefinitely)
      ProviderTimeoutDisabled
    | -- | Timeout after specified seconds
      ProviderTimeoutSeconds Int
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON ProviderTimeout where
    toJSON ProviderTimeoutDisabled = Bool False
    toJSON (ProviderTimeoutSeconds n) = Number (fromIntegral n)

instance FromJSON ProviderTimeout where
    parseJSON (Bool False) = pure ProviderTimeoutDisabled
    parseJSON (Number n) = pure $ ProviderTimeoutSeconds (round n)
    parseJSON other = fail $ "Unknown ProviderTimeout: " <> show other

{- | Model definition within a provider.

Allows overriding model metadata like context length and visibility.
-}
data ProviderModel = ProviderModel
    { pmId :: Text
    -- ^ Model identifier (e.g., "gpt-4", "claude-3-opus")
    , pmName :: Maybe Text
    -- ^ Human-readable name (defaults to id)
    , pmContextLength :: Maybe Int
    -- ^ Override context window size
    , pmMaxOutput :: Maybe Int
    -- ^ Override max output tokens
    , pmStatus :: Maybe ModelStatus
    -- ^ Model maturity status
    , pmHidden :: Maybe Bool
    -- ^ Hide from model selection UI
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

{- | Provider-specific options.

Extra configuration passed to the provider API.
-}
data ProviderOptions = ProviderOptions
    { poThinking :: Maybe Bool
    -- ^ Enable extended thinking (Claude-specific)
    , poVersion :: Maybe Text
    -- ^ API version string
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

{- | LLM provider configuration.

Configures an LLM provider like OpenAI, Anthropic, or a custom endpoint.

@
provider:
  anthropic:
    api: "https://api.anthropic.com"
    timeout: 300
  custom:
    api: "https://my-llm.example.com/v1"
    name: "My Custom LLM"
@
-}
data ProviderConfig = ProviderConfig
    { pcApi :: Maybe Text
    -- ^ API endpoint URL
    , pcModels :: Maybe [ProviderModel]
    -- ^ Custom model definitions
    , pcOptions :: Maybe ProviderOptions
    -- ^ Provider-specific options
    , pcTimeout :: Maybe ProviderTimeout
    -- ^ Request timeout
    , pcDisabled :: Maybe Bool
    -- ^ Disable this provider
    , pcName :: Maybe Text
    -- ^ Human-readable provider name
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

{- | Local MCP server configuration.

Runs an MCP server as a subprocess using stdio transport.
-}
data MCPLocal = MCPLocal
    { mcplCommand :: [Text]
    -- ^ Command and arguments to run
    , mcplEnvironment :: Maybe (Map Text Text)
    -- ^ Environment variables to set
    , mcplEnabled :: Maybe Bool
    -- ^ Enable/disable this server
    , mcplTimeout :: Maybe Int
    -- ^ Startup timeout in milliseconds
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

{- | Remote MCP server configuration.

Connects to an MCP server over HTTP/SSE transport.
-}
data MCPRemote = MCPRemote
    { mcprUrl :: Text
    -- ^ Server URL (HTTP or HTTPS)
    , mcprEnabled :: Maybe Bool
    -- ^ Enable/disable this server
    , mcprHeaders :: Maybe (Map Text Text)
    -- ^ Additional HTTP headers (e.g., auth)
    , mcprTimeout :: Maybe Int
    -- ^ Connection timeout in milliseconds
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

{- | MCP server configuration (local or remote).

Model Context Protocol servers extend the agent with custom tools.

@
mcp:
  filesystem:
    type: "local"
    command: ["npx", "-y", "\@modelcontextprotocol/server-filesystem", "/home"]
  weather:
    type: "remote"
    url: "https://mcp.example.com/weather"
@
-}
data MCPConfig
    = -- | Local subprocess server
      MCPConfigLocal MCPLocal
    | -- | Remote HTTP server
      MCPConfigRemote MCPRemote
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

{- | Code formatter entry for a file extension.

Defines the command to format files of a specific type.
-}
data FormatterEntry = FormatterEntry
    { feCommand :: [Text]
    -- ^ Formatter command and arguments (file path appended)
    , feTimeout :: Maybe Int
    -- ^ Timeout in milliseconds
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

{- | Code formatter configuration.

Maps file extensions to formatter commands.

@
formatter:
  ".hs": { command: ["ormolu", "--mode", "inplace"] }
  ".py": { command: ["black"] }
@

Set to @false@ to disable formatting entirely.
-}
data FormatterConfig
    = -- | Formatting disabled
      FormatterDisabled
    | -- | Extension-to-formatter mapping
      FormatterEnabled (Map Text FormatterEntry)
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

{- | LSP server entry for a language.

Configures how to start the language server for a specific language.
-}
data LSPEntry = LSPEntry
    { lspCommand :: [Text]
    -- ^ Command and arguments to start the LSP server
    , lspArgs :: Maybe [Text]
    -- ^ Additional arguments
    , lspInitializationOptions :: Maybe Text
    -- ^ JSON initialization options
    , lspRootUri :: Maybe Text
    -- ^ Override workspace root URI
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

{- | LSP configuration.

Maps languages to their LSP server configurations.

@
lsp:
  haskell: { command: ["haskell-language-server-wrapper", "--lsp"] }
  python: { command: ["pylsp"] }
@

Set to @false@ to disable LSP integration entirely.
-}
data LSPConfig
    = -- | LSP integration disabled
      LSPDisabled
    | -- | Language-to-LSP-server mapping
      LSPEnabled (Map Text LSPEntry)
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

{- | RGBA color value.

Components are in the range [0.0, 1.0].
-}
data Color = Color
    { colorR :: Double
    -- ^ Red component [0.0, 1.0]
    , colorG :: Double
    -- ^ Green component [0.0, 1.0]
    , colorB :: Double
    -- ^ Blue component [0.0, 1.0]
    , colorA :: Double
    -- ^ Alpha component [0.0, 1.0] (1.0 = opaque)
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

{- | UI theme configuration.

Defines colors for the TUI. This is a simplified subset of the
full theme which has 50+ color definitions.
-}
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

{- | Custom skill configuration.

Skills are reusable prompts that can be invoked with @/skill name@.

@
skill:
  code-review:
    name: "Code Review"
    description: "Review code for best practices"
    prompt: "Please review this code for..."
    tools: ["read", "grep"]
@
-}
data SkillConfig = SkillConfig
    { skillName :: Text
    -- ^ Display name for the skill
    , skillDescription :: Text
    -- ^ Brief description shown in skill list
    , skillPrompt :: Text
    -- ^ System prompt or instructions
    , skillTools :: Maybe [Text]
    -- ^ Allowed tools (empty = inherit from agent)
    , skillAgent :: Maybe Text
    -- ^ Agent to use for this skill
    , skillModel :: Maybe Text
    -- ^ Model override for this skill
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

{- | Custom command configuration.

Commands are AI prompt templates that can be invoked with @/name@.

@
command:
  review:
    template: "Review the changes in $1"
    description: "Review changes in a commit, branch, or PR"
    agent: "code-reviewer"
@
-}
data CommandConfig = CommandConfig
    { cmdTemplate :: Text
    -- ^ Prompt template with placeholders like $1, $ARGUMENTS
    , cmdDescription :: Maybe Text
    -- ^ Brief description
    , cmdAgent :: Maybe Text
    -- ^ Agent to use for this command
    , cmdModel :: Maybe Text
    -- ^ Model to use for this command
    , cmdSubtask :: Maybe Bool
    -- ^ Whether this command runs as a subtask
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON CommandConfig where
    toJSON c =
        object $
            ("template" .= cmdTemplate c)
                : catMaybes
                    [ ("description" .=) <$> cmdDescription c
                    , ("agent" .=) <$> cmdAgent c
                    , ("model" .=) <$> cmdModel c
                    , ("subtask" .=) <$> cmdSubtask c
                    ]

instance FromJSON CommandConfig where
    parseJSON = withObject "CommandConfig" $ \v ->
        CommandConfig
            <$> v .: "template"
            <*> v .:? "description"
            <*> v .:? "agent"
            <*> v .:? "model"
            <*> v .:? "subtask"

-- ════════════════════════════════════════════════════════════════════════════
--                                                                   Telemetry
-- ════════════════════════════════════════════════════════════════════════════

{- | R2 (Cloudflare) storage configuration for telemetry uploads.

Contains credentials and endpoint information for uploading telemetry
data to R2 object storage. All fields are optional in Dhall config;
required fields are validated at runtime by Telemetry.R2.configFromDhall.
-}
data R2StorageConfig = R2StorageConfig
    { r2sAccountId :: Maybe Text
    -- ^ Cloudflare account ID
    , r2sAccessKeyId :: Maybe Text
    -- ^ R2 access key ID
    , r2sSecretKey :: Maybe Text
    -- ^ R2 secret access key
    , r2sBucket :: Maybe Text
    -- ^ R2 bucket name (default: weapon-telemetry)
    , r2sPrefix :: Maybe Text
    -- ^ Key prefix (default: telemetry)
    , r2sEndpoint :: Maybe Text
    -- ^ Optional custom endpoint (defaults to R2 standard endpoint)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON R2StorageConfig where
    toJSON r =
        object $
            catMaybes
                [ fmap ("accountId" .=) (r2sAccountId r)
                , fmap ("accessKeyId" .=) (r2sAccessKeyId r)
                , fmap ("secretKey" .=) (r2sSecretKey r)
                , fmap ("bucket" .=) (r2sBucket r)
                , fmap ("prefix" .=) (r2sPrefix r)
                , fmap ("endpoint" .=) (r2sEndpoint r)
                ]

instance FromJSON R2StorageConfig where
    parseJSON = withObject "R2StorageConfig" $ \v ->
        -- Support both Dhall field names (r2s prefix) and API names
        R2StorageConfig
            <$> firstJust (v .:? "r2sAccountId") (v .:? "accountId")
            <*> firstJust (v .:? "r2sAccessKeyId") (v .:? "accessKeyId")
            <*> firstJust (v .:? "r2sSecretKey") (v .:? "secretKey")
            <*> firstJust (v .:? "r2sBucket") (v .:? "bucket")
            <*> firstJust (v .:? "r2sPrefix") (v .:? "prefix")
            <*> firstJust (v .:? "r2sEndpoint") (v .:? "endpoint")

{- | Telemetry configuration for full-take capture.

Presence of this config means telemetry is enabled. The type is:

@
Maybe TelemetryConfig  -- in Config record
@

* @Nothing@ = telemetry disabled
* @Just cfg@ = telemetry enabled with R2 storage config

This avoids the @Maybe Bool@ anti-pattern where @Nothing@, @Just True@,
and @Just False@ create ambiguous semantics.
-}
newtype TelemetryConfig = TelemetryConfig
    { telR2 :: R2StorageConfig
    -- ^ R2 storage configuration (required when telemetry is enabled)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON TelemetryConfig where
    toJSON t = object ["r2" .= telR2 t]

instance FromJSON TelemetryConfig where
    parseJSON = withObject "TelemetryConfig" $ \v ->
        -- Support both "telR2" (Dhall-to-JSON) and "r2" (API/JSON)
        TelemetryConfig <$> (v .: "telR2" <|> v .: "r2")

-- ════════════════════════════════════════════════════════════════════════════
--                                                                 Full Config
-- ════════════════════════════════════════════════════════════════════════════

{- | Main configuration type for the weapon server.

This type matches the Dhall configuration schema exactly. Configuration
is loaded in layers:

1. Built-in defaults ('defaultConfig')
2. Global config (~\/.config\/weapon\/weapon.dhall)
3. Project config (project\/weapon.dhall)

Each layer overrides the previous, with project config having highest priority.

= Example Dhall Configuration

@
{ model = Some "claude-3-opus"
, logLevel = Some \<INFO\>
, server = { port = Some 8080 }
}
@

= Usage

@
cache <- newDhallCache
config <- loadConfigCached cache "\/path\/to\/project"
@

See 'Config.Config.load' for the primary loading function.
-}
data Config = Config
    { -- Core settings
      cfgModel :: Maybe Text
    -- ^ Default model identifier (e.g., "claude-3-opus")
    , cfgSystemPrompt :: Maybe Text
    -- ^ Override the default system prompt
    , cfgMaxTokens :: Maybe Int
    -- ^ Maximum tokens for model responses
    , cfgLogLevel :: Maybe LogLevel
    -- ^ Logging verbosity (default: 'INFO')
    , -- Nested configs
      cfgKeybinds :: KeybindsConfig
    -- ^ TUI keyboard shortcuts
    , cfgServer :: ServerConfig
    -- ^ HTTP server settings
    , cfgTui :: TUIConfig
    -- ^ Terminal UI settings
    , cfgPermission :: PermissionConfig
    -- ^ Tool permission rules
    , cfgCompaction :: CompactionConfig
    -- ^ Context compaction settings
    , cfgExperimental :: ExperimentalConfig
    -- ^ Experimental feature flags
    , cfgEnterprise :: EnterpriseConfig
    -- ^ Enterprise deployment settings
    , cfgWatcher :: WatcherConfig
    -- ^ File watcher settings
    , -- Map fields
      cfgAgent :: Maybe (Map Text AgentConfig)
    -- ^ Custom agent definitions
    , cfgProvider :: Maybe (Map Text ProviderConfig)
    -- ^ LLM provider configurations
    , cfgMcp :: Maybe (Map Text MCPConfig)
    -- ^ MCP server configurations
    , cfgFormatter :: Maybe FormatterConfig
    -- ^ Code formatter settings
    , cfgLsp :: Maybe LSPConfig
    -- ^ Language server settings
    , cfgSkill :: Maybe (Map Text SkillConfig)
    -- ^ Custom skill definitions
    , cfgCommand :: Maybe (Map Text CommandConfig)
    -- ^ Custom command definitions
    , -- Theme settings
      cfgTheme :: Maybe Text
    -- ^ Active theme name (default: "ono-sendai")
    , cfgThemes :: Maybe (Map Text ThemeConfig)
    -- ^ Custom theme definitions
    , -- Share settings
      cfgShare :: Maybe ShareMode
    -- ^ Session sharing mode
    , -- Auto-update
      cfgAutoUpdate :: Maybe AutoUpdate
    -- ^ Auto-update behavior (default: 'AutoUpdateNotify')
    , -- Telemetry
      cfgTelemetry :: Maybe TelemetryConfig
    -- ^ Telemetry capture settings (Nothing = disabled, Just = enabled)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)

instance ToJSON Config where
    toJSON c =
        -- Only include fields that exist in the TypeScript/OpenAPI Config schema
        -- Use catMaybes to omit null optional fields entirely
        object $
            catMaybes
                [ optField "model" (cfgModel c)
                , optField "systemPrompt" (cfgSystemPrompt c)
                , optField "maxTokens" (cfgMaxTokens c)
                , optField "logLevel" (cfgLogLevel c)
                , optField "keybinds" (Just $ cfgKeybinds c)
                , optField "server" (Just $ cfgServer c)
                , optField "tui" (Just $ cfgTui c)
                , optField "permission" (Just $ cfgPermission c)
                , optField "compaction" (Just $ cfgCompaction c)
                , optField "experimental" (Just $ cfgExperimental c)
                , optField "enterprise" (Just $ cfgEnterprise c)
                , optField "watcher" (Just $ cfgWatcher c)
                , optField "agent" (cfgAgent c)
                , optField "provider" (cfgProvider c)
                , optField "mcp" (cfgMcp c)
                , optField "formatter" (cfgFormatter c)
                , optField "lsp" (cfgLsp c)
                , optField "skill" (cfgSkill c)
                , optField "command" (cfgCommand c)
                , optField "theme" (cfgTheme c)
                , optField "themes" (cfgThemes c)
                , optField "share" (cfgShare c)
                , optField "autoupdate" (cfgAutoUpdate c)
                , optField "telemetry" (cfgTelemetry c)
                ]
      where
        optField :: (ToJSON v) => Key -> Maybe v -> Maybe Pair
        optField k = fmap (k .=)

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
            <*> v .:? "autoupdate"
            <*> firstJust (v .:? "cfgTelemetry") (v .:? "telemetry")

-- ════════════════════════════════════════════════════════════════════════════
--                                                                    Defaults
-- ════════════════════════════════════════════════════════════════════════════

{- | Default server configuration.

Binds to localhost:4096 with CORS enabled and mDNS disabled.
-}
defaultServer :: ServerConfig
defaultServer =
    ServerConfig
        { scHostname = Just "localhost"
        , scPort = Just 4096
        , scMdns = Just False
        , scMdnsDomain = Nothing
        , scCors = Nothing
        }

{- | Default TUI configuration.

Standard scroll speed with automatic diff style.
-}
defaultTUI :: TUIConfig
defaultTUI =
    TUIConfig
        { tuiScrollSpeed = Just 1.0
        , tuiScrollAcceleration = Nothing
        , tuiDiffStyle = Just DiffAuto
        }

{- | Default permission configuration.

All permissions are unset (will use system defaults).
-}
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

{- | Default compaction configuration.

Compaction disabled, 8192 tokens reserved for responses.
-}
defaultCompaction :: CompactionConfig
defaultCompaction =
    CompactionConfig
        { compAuto = Just False
        , compPrune = Just False
        , compReserved = Just 8192
        }

{- | Default experimental features configuration.

All experimental features disabled by default.
-}
defaultExperimental :: ExperimentalConfig
defaultExperimental =
    ExperimentalConfig
        { expDisablePasteSummary = Nothing
        , expBatchTool = Nothing
        , expOpenTelemetry = Nothing
        , expPrimaryTools = Nothing
        , expContinueLoopOnDeny = Nothing
        }

{- | Default enterprise configuration.

No enterprise server configured.
-}
defaultEnterprise :: EnterpriseConfig
defaultEnterprise =
    EnterpriseConfig
        { entUrl = Nothing
        }

{- | Default watcher configuration.

Ignores common build output and dependency directories.
-}
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
        , cfgTelemetry = Nothing -- Telemetry disabled by default
        }
