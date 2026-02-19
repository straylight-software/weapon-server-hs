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
import Data.Text (Text)
import Servant


-- ═══════════════════════════════════════════════════════════════════════════
-- // api type definitions //
-- ═══════════════════════════════════════════════════════════════════════════

-- prompt management
-- Note: OpenAPI spec expects Bool responses for all TUI endpoints
type TuiAppendPromptAPI = "tui" :> "append-prompt" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool
type TuiSubmitPromptAPI = "tui" :> "submit-prompt" :> QueryParam "directory" Text :> Post '[JSON] Bool
type TuiClearPromptAPI = "tui" :> "clear-prompt" :> QueryParam "directory" Text :> Post '[JSON] Bool

-- navigation panels (no request body required per OpenAPI spec)
type TuiOpenHelpAPI = "tui" :> "open-help" :> QueryParam "directory" Text :> Post '[JSON] Bool
type TuiOpenSessionsAPI = "tui" :> "open-sessions" :> QueryParam "directory" Text :> Post '[JSON] Bool
type TuiOpenThemesAPI = "tui" :> "open-themes" :> QueryParam "directory" Text :> Post '[JSON] Bool
type TuiOpenModelsAPI = "tui" :> "open-models" :> QueryParam "directory" Text :> Post '[JSON] Bool

-- command execution
type TuiExecuteCommandAPI = "tui" :> "execute-command" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- notifications
type TuiShowToastAPI = "tui" :> "show-toast" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- publishing
type TuiPublishAPI = "tui" :> "publish" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- session selection
type TuiSelectSessionAPI = "tui" :> "select-session" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- control flow
type TuiControlNextAPI = "tui" :> "control" :> "next" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool
type TuiControlResponseAPI = "tui" :> "control" :> "response" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool
