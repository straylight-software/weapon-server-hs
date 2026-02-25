# `// api reference`

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                     // weapon-server // api
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

   "Get just wasted enough, find yourself in some desperate but strangely
    arbitrary kind of trouble, and it was possible to see Ninsei as a field
    of data, the way the matrix had once reminded him of proteins linking to
    distinguish cell specialities."

                                                                 — Neuromancer
```

Complete HTTP API reference for the Weapon Haskell server. Server listens on
port 4096 by default.

## `// global`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /global/health`

Health check endpoint.

**Response:**

```json
{
  "healthy": true,
  "version": "0.1.0"
}
```

### `GET /global/event`

Server-sent events stream. All events are broadcast here.

**Headers:**

- `Accept: text/event-stream`

**Query Parameters:**

- `directory` (optional) — filter events by directory

**Response:** SSE stream

```
data: {"type":"server.connected","properties":{}}

data: {"type":"session.created","properties":{"info":{...}}}
```

### `GET /global/config`

Get global configuration.

**Response:**

```json
{
  "providers": {...},
  "agents": {...},
  "defaults": {...}
}
```

### `PATCH /global/config`

Update global configuration. Merges with existing config.

**Request Body:**

```json
{
  "defaults": {
    "provider": "anthropic"
  }
}
```

### `POST /global/dispose`

Gracefully shutdown the server instance.

## `// project`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /project`

List all known projects.

**Response:**

```json
[
  {
    "id": "proj_abc123",
    "name": "my-project",
    "directory": "/home/user/my-project"
  }
]
```

### `GET /project/current`

Get the current active project.

### `GET /project/{projectID}`

Get project by ID.

### `PATCH /project/{projectID}`

Update project properties.

## `// session`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /session`

List sessions.

**Query Parameters:**

- `start` (optional) — filter sessions updated on or after timestamp
- `search` (optional) — filter sessions by title (case-insensitive)

**Headers:**

- `x-weapon-directory` (optional) — filter by directory

**Response:**

```json
[
  {
    "id": "ses_abc123",
    "slug": "my-session",
    "title": "My Session",
    "projectID": "proj_xyz",
    "directory": "/home/user/project",
    "version": "1.0.0",
    "time": {
      "created": 1708300000,
      "updated": 1708301000
    }
  }
]
```

### `POST /session`

Create a new session.

**Request Body:**

```json
{
  "title": "My New Session",
  "parentID": "ses_parent123"
}
```

**Response:** `Session` object

### `GET /session/status`

Get status of all sessions.

**Response:**

```json
{
  "ses_abc123": "idle",
  "ses_def456": "running"
}
```

### `GET /session/{sessionID}`

Get session by ID.

### `DELETE /session/{sessionID}`

Delete session.

### `GET /session/{sessionID}/children`

Get child sessions (forks).

### `POST /session/{sessionID}/fork`

Fork a session.

**Request Body:**

```json
{
  "messageID": "msg_abc123"
}
```

### `POST /session/{sessionID}/abort`

Abort the current operation in session.

### `GET /session/{sessionID}/diff`

Get diff of changes made in session.

**Query Parameters:**

- `messageID` (optional) — get diff for specific message

### `POST /session/{sessionID}/summarize`

Trigger session summarization.

**Response:** `true` if summarization started

### `POST /session/{sessionID}/share`

Create share link for session.

### `DELETE /session/{sessionID}/share`

Remove share link.

### `POST /session/{sessionID}/revert`

Revert session to a previous state.

### `POST /session/{sessionID}/unrevert`

Undo a revert.

### `GET /session/{sessionID}/todo`

Get todos for session.

**Response:**

```json
[
  {
    "id": "todo_1",
    "content": "Implement feature X",
    "status": "pending",
    "priority": "high"
  }
]
```

## `// message`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /session/{sessionID}/message`

List messages in session.

**Response:**

```json
[
  {
    "id": "msg_abc123",
    "sessionID": "ses_xyz",
    "role": "user",
    "time": {
      "created": 1708300000
    }
  },
  {
    "id": "msg_def456",
    "sessionID": "ses_xyz",
    "role": "assistant",
    "time": {
      "created": 1708300100,
      "completed": 1708300200
    },
    "modelID": "claude-3-opus",
    "providerID": "anthropic",
    "cost": 0.0123
  }
]
```

### `POST /session/{sessionID}/message`

Send a prompt to the session.

**Request Body:**

```json
{
  "parts": [
    {
      "type": "text",
      "text": "Please explain this code"
    }
  ]
}
```

### `GET /session/{sessionID}/message/{messageID}`

Get specific message.

### `DELETE /session/{sessionID}/message/{messageID}`

Delete message.

### `GET /session/{sessionID}/message/{messageID}/part/{partID}`

Get specific message part.

### `POST /session/{sessionID}/prompt_async`

Send prompt asynchronously (returns immediately).

### `POST /session/{sessionID}/command`

Execute a slash command.

**Request Body:**

```json
{
  "command": "/help"
}
```

### `POST /session/{sessionID}/shell`

Execute a shell command (via PTY).

## `// permission`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /permission`

List pending permission requests.

**Response:**

```json
[
  {
    "id": "perm_abc123",
    "sessionID": "ses_xyz",
    "tool": "bash",
    "input": {"command": "rm -rf /tmp/test"}
  }
]
```

### `POST /permission/{requestID}/reply`

Reply to permission request.

**Request Body:**

```json
{
  "allowed": true
}
```

### `PUT /session/{sessionID}/permissions/{permissionID}`

Update permission rule for session.

## `// question`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /question`

List pending questions.

**Response:**

```json
[
  {
    "id": "q_abc123",
    "sessionID": "ses_xyz",
    "question": "Which framework should I use?",
    "options": [
      {"label": "React", "description": "Popular frontend framework"},
      {"label": "Vue", "description": "Progressive framework"}
    ]
  }
]
```

### `POST /question/{requestID}/reply`

Reply to question.

**Request Body:**

```json
{
  "answer": ["React"]
}
```

### `POST /question/{requestID}/reject`

Reject question (cancel).

## `// pty`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /pty`

List PTY sessions.

### `POST /pty`

Create new PTY session.

**Request Body:**

```json
{
  "command": "/bin/bash",
  "rows": 24,
  "cols": 80
}
```

### `GET /pty/{ptyID}`

Get PTY session info.

### `PUT /pty/{ptyID}`

Update PTY (resize).

**Request Body:**

```json
{
  "rows": 30,
  "cols": 120
}
```

### `DELETE /pty/{ptyID}`

Delete PTY session.

### `GET /pty/{ptyID}/connect`

WebSocket endpoint for PTY I/O.

**Protocol:** Binary WebSocket

- client → server: raw terminal input
- server → client: terminal output

### `POST /pty/{ptyID}/commit`

Commit PTY sandbox changes to filesystem.

### `GET /pty/{ptyID}/changes`

List uncommitted changes in PTY sandbox.

## `// file`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /file`

List files in directory.

**Query Parameters:**

- `path` — directory path

### `GET /file/content`

Get file content.

**Query Parameters:**

- `path` — file path

### `GET /file/status`

Get file status (modified, untracked, etc.).

## `// find`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /find`

Search for text in files.

**Query Parameters:**

- `query` — search term
- `include` (optional) — file pattern to include
- `limit` (optional) — max results

### `GET /find/file`

Search for files by name.

**Query Parameters:**

- `query` — file name pattern
- `dirs` (optional) — include directories ("true"/"false")
- `type` (optional) — filter by type ("file"/"directory")
- `limit` (optional) — max results (1-200)

### `GET /find/symbol`

Search for symbols (functions, types, etc.).

**Query Parameters:**

- `query` — symbol name

## `// provider`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /provider`

List configured providers.

### `GET /provider/auth`

Get provider authentication status.

### `GET /provider/{providerID}/oauth/authorize`

Start OAuth flow for provider.

### `GET /provider/{providerID}/oauth/callback`

OAuth callback endpoint.

## `// config`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /config`

Get configuration.

### `PATCH /config`

Update configuration.

### `GET /config/providers`

Get provider configuration.

## `// experimental`

```
════════════════════════════════════════════════════════════════════════════════
```

### `GET /experimental/tool`

List available tools with JSON schemas.

**Response:**

```json
[
  {
    "name": "read",
    "description": "Read file contents",
    "input_schema": {
      "type": "object",
      "properties": {
        "path": {"type": "string"}
      }
    }
  }
]
```

### `GET /experimental/tool/ids`

List tool IDs.

### `GET /experimental/worktree`

Get worktree state.

### `POST /experimental/worktree`

Set worktree state.

### `DELETE /experimental/worktree`

Remove worktree.

### `POST /experimental/worktree/reset`

Reset worktree to clean state.

## `// tui`

```
════════════════════════════════════════════════════════════════════════════════
```

Endpoints for controlling the TUI (terminal user interface).

### `POST /tui/append-prompt`

Append text to prompt input.

### `POST /tui/submit-prompt`

Submit the current prompt.

### `POST /tui/clear-prompt`

Clear the prompt input.

### `POST /tui/execute-command`

Execute a TUI command.

### `POST /tui/show-toast`

Show a toast notification.

### `POST /tui/open-help`

Open help dialog.

### `POST /tui/open-sessions`

Open sessions dialog.

### `POST /tui/open-themes`

Open themes dialog.

### `POST /tui/open-models`

Open models dialog.

### `POST /tui/publish`

Publish TUI event.

## `// event types`

```
════════════════════════════════════════════════════════════════════════════════
```

Events sent via SSE at `/global/event`:

| Event Type | Description |
|------------|-------------|
| `server.connected` | initial connection event |
| `server.heartbeat` | periodic heartbeat |
| `session.created` | new session created |
| `session.updated` | session metadata changed |
| `session.deleted` | session deleted |
| `session.status` | session status changed |
| `session.idle` | session became idle |
| `session.error` | session error occurred |
| `message.updated` | message content changed |
| `message.removed` | message deleted |
| `message.part.updated` | message part changed |
| `message.part.removed` | message part deleted |
| `permission.asked` | permission request pending |
| `permission.replied` | permission request answered |
| `question.asked` | question pending |
| `question.replied` | question answered |
| `question.rejected` | question cancelled |
| `todo.updated` | todos changed |
| `project.updated` | project changed |

**Event Format:**

```json
{
  "type": "session.created",
  "properties": {
    "info": {...}
  }
}
```

```
────────────────────────────────────────────────────────────────────────────────

   "And in the bloodlit dark behind his eyes, silver phosphenes boiling in from
    the edge of space, hypnagogic images jerking past like film compiled from
    random frames."

                                                                 — Neuromancer
────────────────────────────────────────────────────────────────────────────────
```
