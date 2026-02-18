# `// architecture`

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                    // weapon-server // modules
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

   "Cyberspace. A consensual hallucination experienced daily by billions of
    legitimate operators, in every nation, by children being taught mathematical
    concepts..."

                                                                 — Neuromancer
```

This document describes the internal architecture of the Weapon Haskell server.

## `// overview`

The server is structured as a Servant application with STM-based concurrency.
Key design principles:

- **type-safe routing** via Servant's type-level API definitions
- **composable concurrency** via STM for shared state
- **structured effects** via ReaderT pattern for configuration
- **explicit error handling** via Either and ExceptT

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                                  HTTP                                       │
│                            (Warp + Servant)                                 │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              Handlers.hs                                    │
│                         (request/response logic)                            │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
            ┌───────────────────────┼───────────────────────┐
            ▼                       ▼                       ▼
┌───────────────────┐   ┌───────────────────┐   ┌───────────────────┐
│     Session/      │   │       LLM/        │   │       Tool/       │
│   (lifecycle)     │   │   (providers)     │   │   (execution)     │
└───────────────────┘   └───────────────────┘   └───────────────────┘
            │                       │                       │
            └───────────────────────┼───────────────────────┘
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                               State.hs                                      │
│                    (AppState: Bus, Storage, PtyManager)                     │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
            ┌───────────────────────┼───────────────────────┐
            ▼                       ▼                       ▼
┌───────────────────┐   ┌───────────────────┐   ┌───────────────────┐
│       Bus/        │   │     Storage/      │   │       Pty/        │
│  (event pubsub)   │   │  (persistence)    │   │   (terminals)     │
└───────────────────┘   └───────────────────┘   └───────────────────┘
```

## `// core modules`

### `// Api.hs`

```
────────────────────────────────────────────────────────────────────────────────
```

Servant type-level API definition. Every endpoint is expressed as a type,
enabling compile-time verification of routing and handler signatures.

```haskell
type API =
  "global" :> "health" :> Get '[JSON] HealthResponse
  :<|> "session" :> Get '[JSON] [Session]
  :<|> "session" :> ReqBody '[JSON] CreateSessionInput :> Post '[JSON] Session
  -- ... 95 endpoints total
```

Key types:

- `HealthResponse` — server health status
- `Session` — session metadata and state
- `Message` — user/assistant messages with parts
- `Part` — text, tool calls, reasoning content

### `// Handlers.hs`

```
────────────────────────────────────────────────────────────────────────────────
```

Request handlers implementing the API. Each handler receives `AppState` and
request data, returns a response or throws a Servant error.

Pattern used throughout:

```haskell
sessionCreateHandler :: AppState -> Maybe Text -> CreateSessionInput -> Handler Session
sessionCreateHandler appState directoryHeader createInput = do
  let directory = fromMaybe (stDirectory appState) directoryHeader
  session <- liftIO $ Session.create (stStorage appState) createInput directory
  liftIO $ Bus.publish (stBus appState) "session.created" (toJSON session)
  return session
```

### `// State.hs`

```
────────────────────────────────────────────────────────────────────────────────
```

Application state container. Uses STM for thread-safe concurrent access.

```haskell
data AppState = AppState
  { stBus :: Bus                    -- event pub/sub
  , stStorage :: Storage            -- persistence layer
  , stPtyManager :: PtyManager      -- terminal sessions
  , stDirectory :: Text             -- working directory
  , stProjectID :: Text             -- current project
  , stLogger :: Logger              -- structured logging
  , stConfig :: TVar Config         -- mutable configuration
  }
```

State is created once at startup and threaded through all handlers.

## `// domain modules`

### `// Session/`

```
════════════════════════════════════════════════════════════════════════════════
```

Session lifecycle management.

| Module | Purpose |
|--------|---------|
| `Session.hs` | create, list, get, delete, archive operations |
| `Status.hs` | session status tracking (idle, running, error) |
| `Types.hs` | session data types |

Sessions are stored as JSON files in the storage directory. Each session
maintains:

- unique ID (prefixed `ses_`)
- title (auto-generated or user-provided)
- timestamps (created, updated, archived)
- message history
- diff state for revert functionality

### `// Message/`

```
════════════════════════════════════════════════════════════════════════════════
```

Message handling and content parts.

| Module | Purpose |
|--------|---------|
| `Types.hs` | Message, Part, Role definitions |
| `Parts.hs` | part manipulation (add, update, remove) |
| `Todo.hs` | todo item extraction from messages |

Messages are stored as append-only logs per session. Parts support:

- `TextPart` — plain text content
- `ToolPart` — tool invocations with input/output
- `ReasoningPart` — model reasoning traces
- `FilePart` — file references

### `// Tool/`

```
════════════════════════════════════════════════════════════════════════════════
```

Tool execution framework for LLM tool calls.

| Module | Purpose |
|--------|---------|
| `Types.hs` | input types (ReadInput, BashInput, etc.) |
| `Defs.hs` | Anthropic API tool definitions with JSON schemas |
| `Exec.hs` | tool executors (file ops, bash, fd, rg) |
| `Tool.hs` | re-export module |

Execution flow:

```haskell
executeToolUse :: ToolUse -> IO ToolResult
executeToolUse toolUse = case toolUseName toolUse of
  "read"  -> executeRead (toolUseInput toolUse)
  "write" -> executeWrite (toolUseInput toolUse)
  "edit"  -> executeEdit (toolUseInput toolUse)
  "bash"  -> executeBash (toolUseInput toolUse)
  "glob"  -> executeGlob (toolUseInput toolUse)
  "grep"  -> executeGrep (toolUseInput toolUse)
  other   -> return $ ToolError $ "Unknown tool: " <> other
```

Tool definitions follow the Anthropic tool use specification, providing
JSON schemas that inform the model about available parameters.

### `// LLM/`

```
════════════════════════════════════════════════════════════════════════════════
```

LLM provider integrations.

| Module | Purpose |
|--------|---------|
| `Types.hs` | ChatRequest, ChatResponse, ToolUse, ToolResult |
| `Anthropic.hs` | direct Anthropic API client |
| `OpenRouter.hs` | OpenRouter API client with streaming |

Both providers support:

- streaming responses via SSE
- tool use with automatic parsing
- token counting and cost tracking
- error handling with retries

OpenRouter uses `curl -4` for IPv4 streaming (works around IPv6 issues).

### `// Bus/`

```
════════════════════════════════════════════════════════════════════════════════
```

Event pub/sub for SSE streaming.

| Module | Purpose |
|--------|---------|
| `Bus.hs` | publish/subscribe implementation |
| `Event.hs` | event type definitions |

The bus uses STM broadcast channels for fan-out to multiple SSE clients:

```haskell
data Bus = Bus
  { busChannel :: TChan Event
  , busSubscribers :: TVar (Map SubscriberId (TChan Event))
  }

publish :: Bus -> Text -> Value -> IO ()
subscribe :: Bus -> IO (SubscriberId, TChan Event)
unsubscribe :: Bus -> SubscriberId -> IO ()
```

Event types include:

- `session.created`, `session.updated`, `session.deleted`
- `message.updated`, `message.part.updated`
- `permission.asked`, `permission.replied`
- `question.asked`, `question.replied`
- `server.heartbeat`

### `// Pty/`

```
════════════════════════════════════════════════════════════════════════════════
```

PTY terminal session management with WebSocket bridge.

| Module | Purpose |
|--------|---------|
| `Types.hs` | PtySession, PtyManager, PtyConnection |
| `Pty.hs` | PTY creation, resize, I/O |
| `Connect.hs` | WebSocket bridge handler |
| `Parse.hs` | terminal output parsing |

PTY sessions provide sandboxed shell access:

```haskell
data PtyManager = PtyManager
  { pmSessions :: TVar (Map PtyId PtySession)
  , pmSandbox :: Maybe SandboxConfig
  }

create :: PtyManager -> CreatePtyInput -> IO PtySession
connect :: PtyManager -> PtyId -> IO (Maybe PtyConnection)
resize :: PtyManager -> PtyId -> Int -> Int -> IO ()
```

WebSocket bridge enables bidirectional PTY <-> browser communication.

### `// Sandbox/`

```
════════════════════════════════════════════════════════════════════════════════
```

Sandboxed execution via bubblewrap and overlayfs.

| Module | Purpose |
|--------|---------|
| `Types.hs` | SandboxConfig, SandboxState |
| `Sandbox.hs` | namespace setup, overlay management |

Provides isolation for:

- PTY sessions (user shell commands)
- bash tool execution
- file operations within sandbox boundary

Uses Linux namespaces (user, mount, network) with overlayfs for
copy-on-write file system changes.

### `// Storage/`

```
════════════════════════════════════════════════════════════════════════════════
```

Persistence layer for sessions and configuration.

Storage uses a simple file-based approach:

```
.weapon/storage/
├── sessions/
│   ├── ses_abc123/
│   │   ├── session.json
│   │   └── messages/
│   │       ├── msg_001.json
│   │       └── msg_002.json
│   └── ses_def456/
│       └── ...
└── config.json
```

Operations are atomic via write-to-temp-then-rename pattern.

### `// Config/`

```
════════════════════════════════════════════════════════════════════════════════
```

Configuration parsing and validation.

| Module | Purpose |
|--------|---------|
| `Types.hs` | Config, ProviderConfig, AgentConfig |
| `Config.hs` | loading, merging, validation |

Configuration supports:

- provider settings (API keys, endpoints)
- agent definitions (system prompts, permissions)
- default behaviors
- formatting preferences

### `// Provider/`

```
════════════════════════════════════════════════════════════════════════════════
```

Provider definitions and OAuth flows.

| Module | Purpose |
|--------|---------|
| `Types.hs` | Provider, ProviderAuth |
| `Provider.hs` | provider registration and lookup |
| `OAuth.hs` | OAuth authentication flows |

### `// Agent/`

```
════════════════════════════════════════════════════════════════════════════════
```

Agent definitions with permission rulesets.

| Module | Purpose |
|--------|---------|
| `Types.hs` | Agent, PermissionRuleset, Permission |
| `Agent.hs` | built-in agents (armed, locked, explore, etc.) |

Agents define:

- system prompt templates
- available tools
- permission rules (auto-approve, ask, deny)
- model preferences

### `// Log.hs`

```
════════════════════════════════════════════════════════════════════════════════
```

Structured logging via Katip with JSON output.

```haskell
logMsg :: Logger -> Severity -> Text -> IO ()
logMsg logger severity message = do
  Katip.logMsg logger severity $ object
    [ "msg" .= message
    , "ns" .= loggerNamespace logger
    ]
```

Log output is JSON lines format for easy parsing:

```json
{"at":"2026-02-18T21:03:25Z","msg":"listening on port 4096","sev":"Info","ns":["weapon","server"]}
```

## `// concurrency model`

```
────────────────────────────────────────────────────────────────────────────────
```

The server uses STM for all shared state:

- `TVar` for mutable configuration and session state
- `TChan` for event bus broadcast
- `TMVar` for request/response synchronization

This provides:

- composable transactions (multiple reads/writes atomically)
- automatic retry on conflicts
- no deadlocks by construction
- easy reasoning about concurrent operations

Example pattern:

```haskell
-- atomic read-modify-write
updateSession :: Storage -> SessionId -> (Session -> Session) -> IO Session
updateSession storage sessionId updateFn = atomically $ do
  sessions <- readTVar (storageSessions storage)
  case Map.lookup sessionId sessions of
    Nothing -> throwSTM SessionNotFound
    Just session -> do
      let updated = updateFn session
      writeTVar (storageSessions storage) (Map.insert sessionId updated sessions)
      return updated
```

## `// error handling`

```
────────────────────────────────────────────────────────────────────────────────
```

Errors flow through the type system:

- `Either` for pure operations that can fail
- `ExceptT` in monadic contexts
- `Handler` (Servant's type) for HTTP errors

Pattern:

```haskell
getSession :: Storage -> SessionId -> Handler Session
getSession storage sessionId = do
  maybeSession <- liftIO $ Storage.get storage sessionId
  case maybeSession of
    Nothing -> throwError err404 { errBody = "Session not found" }
    Just session -> return session
```

## `// extension points`

```
────────────────────────────────────────────────────────────────────────────────
```

### adding a new tool

1. add input type to `Tool/Types.hs`
2. add `FromJSON` instance for parsing
3. add tool definition to `Tool/Defs.hs` with JSON schema
4. add executor to `Tool/Exec.hs`
5. wire into `execute` dispatcher

### adding a new endpoint

1. add route type to `Api.hs`
2. add handler to `Handlers.hs`
3. wire handler into `server` function

### adding a new event type

1. add constructor to `Event` type in `Bus/Event.hs`
2. add JSON serialization
3. publish from relevant handlers

```
────────────────────────────────────────────────────────────────────────────────

   "A year here and he still dreamed of cyberspace, hope fading nightly."

                                                                 — Neuromancer
────────────────────────────────────────────────────────────────────────────────
```
