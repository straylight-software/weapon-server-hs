{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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

    -- * Input Types (strict JSON parsing)
    AppendPromptInput (..),
    ExecuteCommandInput (..),
    ShowToastInput (..),
    PublishInput (..),
    PublishPromptAppendProps (..),
    PublishCommandExecuteProps (..),
    PublishToastShowProps (..),
    PublishSessionSelectProps (..),
    SelectSessionInput (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?))
import Data.Aeson qualified as A
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import Json.Strict (withStrictObject, (.:!?))
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
type TuiAppendPromptAPI = "tui" :> "append-prompt" :> QueryParam "directory" Text :> ReqBody '[JSON] AppendPromptInput :> Post '[JSON] Bool

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
type TuiExecuteCommandAPI = "tui" :> "execute-command" :> QueryParam "directory" Text :> ReqBody '[JSON] ExecuteCommandInput :> Post '[JSON] Bool

-- ═══════════════════════════════════════════════════════════════════════════
-- // notifications //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @POST /tui/show-toast@ - Show a toast notification.

Displays a temporary notification message in the TUI.
The request body should contain @{"message": "text", "variant": "info|warning|error"}@.
-}
type TuiShowToastAPI = "tui" :> "show-toast" :> QueryParam "directory" Text :> ReqBody '[JSON] ShowToastInput :> Post '[JSON] Bool

-- ═══════════════════════════════════════════════════════════════════════════
-- // publishing //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @POST /tui/publish@ - Publish content from the TUI.

Publishes the current session or selected content.
-}
type TuiPublishAPI = "tui" :> "publish" :> QueryParam "directory" Text :> ReqBody '[JSON] PublishInput :> Post '[JSON] Bool

-- ═══════════════════════════════════════════════════════════════════════════
-- // session selection //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @POST /tui/select-session@ - Select a session.

Switches the TUI to display the specified session.
The request body should contain @{"sessionID": "ses_xxx"}@.
-}
type TuiSelectSessionAPI = "tui" :> "select-session" :> QueryParam "directory" Text :> ReqBody '[JSON] SelectSessionInput :> Post '[JSON] Bool

-- ═══════════════════════════════════════════════════════════════════════════
-- // control flow //
-- ═══════════════════════════════════════════════════════════════════════════
-- // input types //
-- ═══════════════════════════════════════════════════════════════════════════

-- | Input for append-prompt endpoint
newtype AppendPromptInput = AppendPromptInput
    { apiText :: Text
    -- ^ Text to append (required)
    }
    deriving (Show, Eq, Generic)

instance FromJSON AppendPromptInput where
    parseJSON = withStrictObject "AppendPromptInput" ["text"] $ \v ->
        AppendPromptInput <$> v .: "text"

instance ToJSON AppendPromptInput where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = drop 3}

-- | Input for execute-command endpoint
newtype ExecuteCommandInput = ExecuteCommandInput
    { eciCommand :: Text
    -- ^ Command to execute (required)
    }
    deriving (Show, Eq, Generic)

instance FromJSON ExecuteCommandInput where
    parseJSON = withStrictObject "ExecuteCommandInput" ["command"] $ \v ->
        ExecuteCommandInput <$> v .: "command"

instance ToJSON ExecuteCommandInput where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = drop 3}

-- | Input for show-toast endpoint
data ShowToastInput = ShowToastInput
    { stiMessage :: Text
    -- ^ Toast message text (required)
    , stiVariant :: Text
    -- ^ Toast variant: info, warning, error, success (required)
    , stiTitle :: Maybe Text
    -- ^ Optional title (not nullable)
    , stiDuration :: Maybe Double
    -- ^ Optional duration in ms (not nullable)
    }
    deriving (Show, Eq, Generic)

instance FromJSON ShowToastInput where
    parseJSON = withStrictObject "ShowToastInput" ["message", "variant", "title", "duration"] $ \v ->
        ShowToastInput
            <$> v .: "message"
            <*> v .: "variant"
            <*> v .:!? "title"
            <*> v .:!? "duration"

instance ToJSON ShowToastInput where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = drop 3}

{- | Input for publish endpoint - discriminated union of TUI events

Matches TypeScript: z.union([...TuiEvent types])
Each event has a "type" discriminator and "properties" object.
-}
data PublishInput
    = PublishPromptAppend !PublishPromptAppendProps
    | PublishCommandExecute !PublishCommandExecuteProps
    | PublishToastShow !PublishToastShowProps
    | PublishSessionSelect !PublishSessionSelectProps
    deriving (Show, Eq, Generic)

-- | Properties for tui.prompt.append event
newtype PublishPromptAppendProps = PublishPromptAppendProps
    { ppapText :: Text
    }
    deriving (Show, Eq, Generic)

-- | Properties for tui.command.execute event
newtype PublishCommandExecuteProps = PublishCommandExecuteProps
    { pcepCommand :: Text
    }
    deriving (Show, Eq, Generic)

-- | Properties for tui.toast.show event
data PublishToastShowProps = PublishToastShowProps
    { ptspTitle :: Maybe Text
    , ptspMessage :: Text
    , ptspVariant :: Text
    , ptspDuration :: Maybe Int
    }
    deriving (Show, Eq, Generic)

-- | Properties for tui.session.select event
newtype PublishSessionSelectProps = PublishSessionSelectProps
    { psspSessionID :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON PublishInput where
    parseJSON = withStrictObject "PublishInput" ["type", "properties"] $ \v -> do
        eventType <- v A..: "type" :: Parser Text
        props <- v A..: "properties"
        case eventType of
            "tui.prompt.append" -> PublishPromptAppend <$> A.parseJSON props
            "tui.command.execute" -> PublishCommandExecute <$> A.parseJSON props
            "tui.toast.show" -> PublishToastShow <$> A.parseJSON props
            "tui.session.select" -> PublishSessionSelect <$> A.parseJSON props
            other -> fail $ "Unknown event type: " <> show other

instance ToJSON PublishInput where
    toJSON (PublishPromptAppend props) =
        A.object ["type" A..= ("tui.prompt.append" :: Text), "properties" A..= props]
    toJSON (PublishCommandExecute props) =
        A.object ["type" A..= ("tui.command.execute" :: Text), "properties" A..= props]
    toJSON (PublishToastShow props) =
        A.object ["type" A..= ("tui.toast.show" :: Text), "properties" A..= props]
    toJSON (PublishSessionSelect props) =
        A.object ["type" A..= ("tui.session.select" :: Text), "properties" A..= props]

instance FromJSON PublishPromptAppendProps where
    parseJSON = withStrictObject "PublishPromptAppendProps" ["text"] $ \v ->
        PublishPromptAppendProps <$> v A..: "text"

instance ToJSON PublishPromptAppendProps where
    toJSON (PublishPromptAppendProps t) = A.object ["text" A..= t]

instance FromJSON PublishCommandExecuteProps where
    parseJSON = withStrictObject "PublishCommandExecuteProps" ["command"] $ \v ->
        PublishCommandExecuteProps <$> v A..: "command"

instance ToJSON PublishCommandExecuteProps where
    toJSON (PublishCommandExecuteProps c) = A.object ["command" A..= c]

instance FromJSON PublishToastShowProps where
    parseJSON = withStrictObject "PublishToastShowProps" ["title", "message", "variant", "duration"] $ \v ->
        PublishToastShowProps
            <$> v .:? "title"
            <*> v A..: "message"
            <*> v A..: "variant"
            <*> v .:? "duration"

instance ToJSON PublishToastShowProps where
    toJSON (PublishToastShowProps title msg var dur) =
        A.object $
            ["message" A..= msg, "variant" A..= var]
                ++ maybe [] (\t -> ["title" A..= t]) title
                ++ maybe [] (\d -> ["duration" A..= d]) dur

instance FromJSON PublishSessionSelectProps where
    parseJSON = withStrictObject "PublishSessionSelectProps" ["sessionID"] $ \v ->
        PublishSessionSelectProps <$> v A..: "sessionID"

instance ToJSON PublishSessionSelectProps where
    toJSON (PublishSessionSelectProps sid) = A.object ["sessionID" A..= sid]

-- | Input for select-session endpoint
newtype SelectSessionInput = SelectSessionInput
    { ssiSessionID :: Text
    -- ^ Session ID to select (required)
    }
    deriving (Show, Eq, Generic)

instance FromJSON SelectSessionInput where
    parseJSON = withStrictObject "SelectSessionInput" ["sessionID"] $ \v ->
        SelectSessionInput <$> v .: "sessionID"

instance ToJSON SelectSessionInput where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = drop 3}
