{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{- | ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                  // weapon-server // api/tui
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Terminal UI (TUI) API endpoints. Controls the terminal interface for prompt
management, navigation, and UI state.

= Overview

The TUI API enables programmatic control of the terminal user interface:

* __Prompt Management__ - Append, submit, and clear prompt text
* __Navigation__ - Open help, sessions, themes, and model panels
* __Command Execution__ - Execute UI commands
* __Notifications__ - Show toast messages
* __Session Selection__ - Switch between sessions
* __Control Flow__ - Handle next/response control events

All TUI endpoints return @Bool@ indicating success/failure.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-}
module Api.Tui (
    -- * TUI API Endpoints

    -- ** Prompt Management
    TuiAppendPromptAPI,
    TuiSubmitPromptAPI,
    TuiClearPromptAPI,

    -- ** Navigation Panels
    TuiOpenHelpAPI,
    TuiOpenSessionsAPI,
    TuiOpenThemesAPI,
    TuiOpenModelsAPI,

    -- ** Command Execution
    TuiExecuteCommandAPI,

    -- ** Notifications
    TuiShowToastAPI,

    -- ** Publishing
    TuiPublishAPI,

    -- ** Session Selection
    TuiSelectSessionAPI,

    -- ** Control Flow
    TuiControlNextAPI,
    TuiControlResponseAPI,
) where

import Data.Aeson (Value)
import Data.Text (Text)
import Servant (
    JSON,
    Post,
    QueryParam,
    ReqBody,
    type (:>),
 )

-- ═══════════════════════════════════════════════════════════════════════════
-- // prompt management //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @POST /tui/append-prompt@ - Append text to the current prompt.

Adds text to the end of the current prompt input without submitting.
Useful for inserting code snippets, file references, or suggestions.
-}
type TuiAppendPromptAPI = "tui" :> "append-prompt" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

{- | @POST /tui/submit-prompt@ - Submit the current prompt.

Sends the current prompt text to the active session for processing.
Equivalent to pressing Enter in the TUI.
-}
type TuiSubmitPromptAPI = "tui" :> "submit-prompt" :> QueryParam "directory" Text :> Post '[JSON] Bool

{- | @POST /tui/clear-prompt@ - Clear the current prompt.

Removes all text from the prompt input field.
-}
type TuiClearPromptAPI = "tui" :> "clear-prompt" :> QueryParam "directory" Text :> Post '[JSON] Bool

-- ═══════════════════════════════════════════════════════════════════════════
-- // navigation panels //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @POST /tui/open-help@ - Open the help panel.

Displays keyboard shortcuts and available commands.
-}
type TuiOpenHelpAPI = "tui" :> "open-help" :> QueryParam "directory" Text :> Post '[JSON] Bool

{- | @POST /tui/open-sessions@ - Open the sessions panel.

Shows the session list for browsing and selecting conversations.
-}
type TuiOpenSessionsAPI = "tui" :> "open-sessions" :> QueryParam "directory" Text :> Post '[JSON] Bool

{- | @POST /tui/open-themes@ - Open the themes panel.

Displays available color themes for selection.
-}
type TuiOpenThemesAPI = "tui" :> "open-themes" :> QueryParam "directory" Text :> Post '[JSON] Bool

{- | @POST /tui/open-models@ - Open the models panel.

Shows available AI models for selection.
-}
type TuiOpenModelsAPI = "tui" :> "open-models" :> QueryParam "directory" Text :> Post '[JSON] Bool

-- ═══════════════════════════════════════════════════════════════════════════
-- // command execution //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @POST /tui/execute-command@ - Execute a TUI command.

Runs a named command (e.g., "new-session", "toggle-diff").
The request body should contain @{"command": "command-name"}@.
-}
type TuiExecuteCommandAPI = "tui" :> "execute-command" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- ═══════════════════════════════════════════════════════════════════════════
-- // notifications //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @POST /tui/show-toast@ - Show a toast notification.

Displays a temporary notification message in the TUI.
The request body should contain @{"message": "text", "type": "info|warning|error"}@.
-}
type TuiShowToastAPI = "tui" :> "show-toast" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- ═══════════════════════════════════════════════════════════════════════════
-- // publishing //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @POST /tui/publish@ - Publish content from the TUI.

Publishes the current session or selected content.
-}
type TuiPublishAPI = "tui" :> "publish" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- ═══════════════════════════════════════════════════════════════════════════
-- // session selection //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @POST /tui/select-session@ - Select a session.

Switches the TUI to display the specified session.
The request body should contain @{"sessionID": "ses_xxx"}@.
-}
type TuiSelectSessionAPI = "tui" :> "select-session" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

-- ═══════════════════════════════════════════════════════════════════════════
-- // control flow //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @POST /tui/control/next@ - Handle control next event.

Advances to the next step in a multi-step workflow.
-}
type TuiControlNextAPI = "tui" :> "control" :> "next" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool

{- | @POST /tui/control/response@ - Handle control response event.

Provides a response for a control flow prompt (e.g., permission dialog).
-}
type TuiControlResponseAPI = "tui" :> "control" :> "response" :> QueryParam "directory" Text :> ReqBody '[JSON] Value :> Post '[JSON] Bool
