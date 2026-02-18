-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                   // weapon-server // api/tui
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- Terminal UI (TUI) API endpoints. Controls the terminal interface for prompt
-- management, navigation, and UI state.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Tui
    ( -- * TUI API Endpoints
      TuiAppendPromptAPI
    , TuiOpenHelpAPI
    , TuiOpenSessionsAPI
    , TuiOpenThemesAPI
    , TuiOpenModelsAPI
    , TuiSubmitPromptAPI
    , TuiClearPromptAPI
    , TuiExecuteCommandAPI
    , TuiShowToastAPI
    , TuiPublishAPI
    , TuiSelectSessionAPI
    , TuiControlNextAPI
    , TuiControlResponseAPI
    ) where

import Data.Aeson (Value)
import Servant


-- ═══════════════════════════════════════════════════════════════════════════
-- // api type definitions //
-- ═══════════════════════════════════════════════════════════════════════════

-- prompt management
type TuiAppendPromptAPI = "tui" :> "append-prompt" :> ReqBody '[JSON] Value :> Post '[JSON] Value
type TuiSubmitPromptAPI = "tui" :> "submit-prompt" :> ReqBody '[JSON] Value :> Post '[JSON] Value
type TuiClearPromptAPI = "tui" :> "clear-prompt" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- navigation panels
type TuiOpenHelpAPI = "tui" :> "open-help" :> ReqBody '[JSON] Value :> Post '[JSON] Value
type TuiOpenSessionsAPI = "tui" :> "open-sessions" :> ReqBody '[JSON] Value :> Post '[JSON] Value
type TuiOpenThemesAPI = "tui" :> "open-themes" :> ReqBody '[JSON] Value :> Post '[JSON] Value
type TuiOpenModelsAPI = "tui" :> "open-models" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- command execution
type TuiExecuteCommandAPI = "tui" :> "execute-command" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- notifications
type TuiShowToastAPI = "tui" :> "show-toast" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- publishing
type TuiPublishAPI = "tui" :> "publish" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- session selection
type TuiSelectSessionAPI = "tui" :> "select-session" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- control flow
type TuiControlNextAPI = "tui" :> "control" :> "next" :> ReqBody '[JSON] Value :> Post '[JSON] Value
type TuiControlResponseAPI = "tui" :> "control" :> "response" :> ReqBody '[JSON] Value :> Post '[JSON] Value
