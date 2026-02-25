{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Bus.Event
Description : Event type definitions for the pub/sub event system

This module defines the event types that can be published on the event bus.
It provides:

* 'EventType' - An enumeration of all valid event types in the system
* 'Event' - A wrapper combining event type with properties
* JSON serialization for wire format compatibility

Event types follow a dotted naming convention (e.g., @session.created@,
@message.updated@) that maps to the TypeScript client's event handling.

@since 0.1.0
-}
module Bus.Event (
    -- * Event Types
    Event (..),
    EventType (..),

    -- * Pure Helpers
    eventTypeToText,
    textToEventType,
    mkEvent,

    -- * Re-exports for convenience
    toJSON,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Ix (Ix)

{- | All event types that can be published on the bus.

These map to the TypeScript event names with a dotted notation:

* @server.*@ - Server lifecycle events
* @session.*@ - Session state changes
* @message.*@ - Message updates
* @permission.*@ - Permission requests/responses
* @question.*@ - Interactive question handling
* @todo.*@ - Todo list updates
* @file.*@ - File system events
* @vcs.*@ - Version control events
* @lsp.*@ - Language server protocol events
* @project.*@ - Project state changes
* @installation.*@ - Installation/update events
* @command.*@ - Command execution events
* @pty.*@ - Pseudo-terminal events
* @worktree.*@ - Git worktree events
* @global.*@ - Global system events
-}
data EventType
    = -- | Server has connected and is ready
      ServerConnected
    | -- | Periodic heartbeat signal
      ServerHeartbeat
    | -- | Server instance has been disposed
      ServerInstanceDisposed
    | -- | A new session was created
      SessionCreated
    | -- | Session state was updated
      SessionUpdated
    | -- | Session was deleted
      SessionDeleted
    | -- | Session diff applied
      SessionDiff
    | -- | Session encountered an error
      SessionError
    | -- | Session status changed
      SessionStatus
    | -- | Session became idle
      SessionIdle
    | -- | Session history was compacted
      SessionCompacted
    | -- | A message was updated
      MessageUpdated
    | -- | A message was removed
      MessageRemoved
    | -- | A message part was updated
      MessagePartUpdated
    | -- | A message part was removed
      MessagePartRemoved
    | -- | Permission was requested
      PermissionAsked
    | -- | Permission request was answered
      PermissionReplied
    | -- | A question was asked to the user
      QuestionAsked
    | -- | User replied to a question
      QuestionReplied
    | -- | Question was rejected
      QuestionRejected
    | -- | Todo list was updated
      TodoUpdated
    | -- | A file was edited
      FileEdited
    | -- | File watcher state changed
      FileWatcherUpdated
    | -- | VCS branch was updated
      VcsBranchUpdated
    | -- | LSP state was updated
      LspUpdated
    | -- | LSP client diagnostics received
      LspClientDiagnostics
    | -- | Project state was updated
      ProjectUpdated
    | -- | Installation was updated
      InstallationUpdated
    | -- | A new update is available
      InstallationUpdateAvailable
    | -- | A command was executed
      CommandExecuted
    | -- | A PTY was created
      PtyCreated
    | -- | PTY output was updated
      PtyUpdated
    | -- | PTY process exited
      PtyExited
    | -- | PTY was deleted
      PtyDeleted
    | -- | Git worktree is ready
      WorktreeReady
    | -- | Git worktree operation failed
      WorktreeFailed
    | -- | Global system disposed
      GlobalDisposed
    deriving (Show, Eq, Generic, Ord, Bounded, Ix)

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure conversion functions (testable without IO)
-- ═══════════════════════════════════════════════════════════════════════════

{- | Convert an 'EventType' to its wire format 'Text' representation.

This is a pure function suitable for unit testing.

==== __Examples__

>>> eventTypeToText SessionCreated
"session.created"

>>> eventTypeToText MessagePartUpdated
"message.part.updated"
-}
eventTypeToText :: EventType -> Text
eventTypeToText ServerConnected = "server.connected"
eventTypeToText ServerHeartbeat = "server.heartbeat"
eventTypeToText ServerInstanceDisposed = "server.instance.disposed"
eventTypeToText SessionCreated = "session.created"
eventTypeToText SessionUpdated = "session.updated"
eventTypeToText SessionDeleted = "session.deleted"
eventTypeToText SessionDiff = "session.diff"
eventTypeToText SessionError = "session.error"
eventTypeToText SessionStatus = "session.status"
eventTypeToText SessionIdle = "session.idle"
eventTypeToText SessionCompacted = "session.compacted"
eventTypeToText MessageUpdated = "message.updated"
eventTypeToText MessageRemoved = "message.removed"
eventTypeToText MessagePartUpdated = "message.part.updated"
eventTypeToText MessagePartRemoved = "message.part.removed"
eventTypeToText PermissionAsked = "permission.asked"
eventTypeToText PermissionReplied = "permission.replied"
eventTypeToText QuestionAsked = "question.asked"
eventTypeToText QuestionReplied = "question.replied"
eventTypeToText QuestionRejected = "question.rejected"
eventTypeToText TodoUpdated = "todo.updated"
eventTypeToText FileEdited = "file.edited"
eventTypeToText FileWatcherUpdated = "file.watcher.updated"
eventTypeToText VcsBranchUpdated = "vcs.branch.updated"
eventTypeToText LspUpdated = "lsp.updated"
eventTypeToText LspClientDiagnostics = "lsp.client.diagnostics"
eventTypeToText ProjectUpdated = "project.updated"
eventTypeToText InstallationUpdated = "installation.updated"
eventTypeToText InstallationUpdateAvailable = "installation.update-available"
eventTypeToText CommandExecuted = "command.executed"
eventTypeToText PtyCreated = "pty.created"
eventTypeToText PtyUpdated = "pty.updated"
eventTypeToText PtyExited = "pty.exited"
eventTypeToText PtyDeleted = "pty.deleted"
eventTypeToText WorktreeReady = "worktree.ready"
eventTypeToText WorktreeFailed = "worktree.failed"
eventTypeToText GlobalDisposed = "global.disposed"

{- | Parse a wire format 'Text' back to an 'EventType'.

Returns 'Nothing' if the text doesn't match any known event type.

==== __Examples__

>>> textToEventType "session.created"
Just SessionCreated

>>> textToEventType "unknown.event"
Nothing
-}
textToEventType :: Text -> Maybe EventType
textToEventType "server.connected" = Just ServerConnected
textToEventType "server.heartbeat" = Just ServerHeartbeat
textToEventType "server.instance.disposed" = Just ServerInstanceDisposed
textToEventType "session.created" = Just SessionCreated
textToEventType "session.updated" = Just SessionUpdated
textToEventType "session.deleted" = Just SessionDeleted
textToEventType "session.diff" = Just SessionDiff
textToEventType "session.error" = Just SessionError
textToEventType "session.status" = Just SessionStatus
textToEventType "session.idle" = Just SessionIdle
textToEventType "session.compacted" = Just SessionCompacted
textToEventType "message.updated" = Just MessageUpdated
textToEventType "message.removed" = Just MessageRemoved
textToEventType "message.part.updated" = Just MessagePartUpdated
textToEventType "message.part.removed" = Just MessagePartRemoved
textToEventType "permission.asked" = Just PermissionAsked
textToEventType "permission.replied" = Just PermissionReplied
textToEventType "question.asked" = Just QuestionAsked
textToEventType "question.replied" = Just QuestionReplied
textToEventType "question.rejected" = Just QuestionRejected
textToEventType "todo.updated" = Just TodoUpdated
textToEventType "file.edited" = Just FileEdited
textToEventType "file.watcher.updated" = Just FileWatcherUpdated
textToEventType "vcs.branch.updated" = Just VcsBranchUpdated
textToEventType "lsp.updated" = Just LspUpdated
textToEventType "lsp.client.diagnostics" = Just LspClientDiagnostics
textToEventType "project.updated" = Just ProjectUpdated
textToEventType "installation.updated" = Just InstallationUpdated
textToEventType "installation.update-available" = Just InstallationUpdateAvailable
textToEventType "command.executed" = Just CommandExecuted
textToEventType "pty.created" = Just PtyCreated
textToEventType "pty.updated" = Just PtyUpdated
textToEventType "pty.exited" = Just PtyExited
textToEventType "pty.deleted" = Just PtyDeleted
textToEventType "worktree.ready" = Just WorktreeReady
textToEventType "worktree.failed" = Just WorktreeFailed
textToEventType "global.disposed" = Just GlobalDisposed
textToEventType _ = Nothing

instance ToJSON EventType where
    toJSON = toJSON . eventTypeToText

instance FromJSON EventType where
    parseJSON v = case v of
        String t -> case textToEventType t of
            Just et -> pure et
            Nothing -> fail $ "Unknown EventType: " <> show t
        Object _ -> fail "EventType must be a JSON string"
        Array _ -> fail "EventType must be a JSON string"
        Number _ -> fail "EventType must be a JSON string"
        Bool _ -> fail "EventType must be a JSON string"
        Null -> fail "EventType must be a JSON string"

-- ═══════════════════════════════════════════════════════════════════════════
-- Event type
-- ═══════════════════════════════════════════════════════════════════════════

{- | A bus event combining an event type string with JSON properties.

This is the wire format representation of events sent via SSE.
The 'eventType' field uses the dotted notation (e.g., @"session.created"@).
-}
data Event = Event
    { eventType :: Text
    -- ^ The event type in dotted notation (e.g., @"session.created"@)
    , eventProperties :: Value
    -- ^ JSON payload containing event-specific data
    }
    deriving (Show, Eq, Generic)

{- | Create an 'Event' from a typed 'EventType' and properties.

This is a pure function that converts the typed event to wire format.

==== __Examples__

>>> import Data.Aeson (object, (.=))
>>> mkEvent SessionCreated (object ["sessionID" .= "abc123"])
Event {eventType = "session.created", eventProperties = ...}
-}
mkEvent :: EventType -> Value -> Event
mkEvent et = Event (eventTypeToText et)

instance ToJSON Event where
    toJSON e =
        object
            [ "type" .= eventType e
            , "properties" .= eventProperties e
            ]

instance FromJSON Event where
    parseJSON = withObject "Event" $ \v ->
        Event
            <$> v .: "type"
            <*> v .: "properties"
