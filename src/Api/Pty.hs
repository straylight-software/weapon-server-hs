{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{- | ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                  // weapon-server // api/pty
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Pseudo-terminal (PTY) API endpoints. Manages sandboxed terminal sessions
for safe command execution with filesystem isolation.

= Overview

The PTY API provides:

* __Session Management__ - Create, list, get, update, and delete PTY sessions
* __WebSocket Connection__ - Real-time terminal I/O via WebSocket
* __Sandbox Integration__ - Commit changes from sandbox to real filesystem
* __Change Tracking__ - View files modified within the sandbox

= Sandbox Isolation

PTY sessions run in sandboxed environments using bubblewrap (bwrap).
File changes are tracked and can be committed to the real filesystem
only when explicitly requested.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-}
module Api.Pty (
    -- * PTY API Endpoints

    -- ** Session CRUD
    PtyListAPI,
    PtyCreateAPI,
    PtyGetAPI,
    PtyUpdateAPI,
    PtyDeleteAPI,

    -- ** Terminal Connection
    PtyConnectAPI,

    -- ** Sandbox Operations
    PtyCommitAPI,
    PtyChangesAPI,
) where

import Data.Aeson (Value)
import Data.Text (Text)
import Pty.Types (CreatePtyInput)
import Servant (
    Capture,
    Delete,
    Get,
    JSON,
    Post,
    Put,
    Raw,
    ReqBody,
    type (:>),
 )

-- ═══════════════════════════════════════════════════════════════════════════
-- // session crud //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @GET /pty@ - List all PTY sessions.

Returns an array of active PTY session objects.
-}
type PtyListAPI = "pty" :> Get '[JSON] [Value]

{- | @POST /pty@ - Create a new PTY session.

Creates a sandboxed terminal session. The request body should include:

* @directory@ - Working directory for the session
* @shell@ - Shell to use (optional, defaults to user's shell)
* @rows@ / @cols@ - Terminal dimensions
-}
type PtyCreateAPI = "pty" :> ReqBody '[JSON] CreatePtyInput :> Post '[JSON] Value

{- | @GET /pty/:ptyID@ - Get a specific PTY session.

Returns the session details including state, dimensions, and sandbox info.
-}
type PtyGetAPI = "pty" :> Capture "ptyID" Text :> Get '[JSON] Value

{- | @PUT /pty/:ptyID@ - Update a PTY session.

Updates session properties such as terminal dimensions.
-}
type PtyUpdateAPI = "pty" :> Capture "ptyID" Text :> ReqBody '[JSON] Value :> Put '[JSON] Value

{- | @DELETE /pty/:ptyID@ - Delete a PTY session.

Terminates the PTY process and cleans up resources.
Returns @true@ on success.
-}
type PtyDeleteAPI = "pty" :> Capture "ptyID" Text :> Delete '[JSON] Bool

-- ═══════════════════════════════════════════════════════════════════════════
-- // terminal connection //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @GET /pty/:ptyID/connect@ - WebSocket connection for terminal I/O.

Upgrades to a WebSocket connection for bidirectional terminal communication.

__WebSocket Protocol:__

* Client sends: Raw terminal input (keystrokes)
* Server sends: Terminal output (ANSI escape sequences)
* Resize messages: JSON @{"type": "resize", "rows": N, "cols": M}@
-}
type PtyConnectAPI = "pty" :> Capture "ptyID" Text :> "connect" :> Raw

-- ═══════════════════════════════════════════════════════════════════════════
-- // sandbox operations //
-- ═══════════════════════════════════════════════════════════════════════════

{- | @POST /pty/:ptyID/commit@ - Commit sandbox changes to real filesystem.

Applies all file changes made within the sandbox to the actual filesystem.
This is the only way for sandbox modifications to persist.

Returns the list of committed files.
-}
type PtyCommitAPI = "pty" :> Capture "ptyID" Text :> "commit" :> Post '[JSON] Value

{- | @GET /pty/:ptyID/changes@ - Get list of changed files in sandbox.

Returns an array of files that have been modified within the sandbox
environment, including their change status (added, modified, deleted).
-}
type PtyChangesAPI = "pty" :> Capture "ptyID" Text :> "changes" :> Get '[JSON] Value
