# `// weapon-server-hs`

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                        // haskell // server
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

   "The matrix has its roots in primitive arcade games," said the voice-over,
    "in early graphics programs and military experimentation with cranial jacks."

                                                                 — Neuromancer
```

Haskell implementation of the Weapon AI coding agent server. Full API parity
with the TypeScript reference implementation, 100% endpoint coverage, property
tested with Hedgehog.

## `// quick start`

```bash
# build
nix build

# run
nix run

# develop
nix develop
cabal build
cabal run weapon-server
```

Server listens on port 4096.

## `// what is this`

Weapon is an AI coding agent. This server provides the backend API that:

- manages coding sessions with persistent storage
- orchestrates LLM interactions (Anthropic, OpenRouter)
- executes tools (file operations, shell commands, search)
- streams events via SSE for real-time UI updates
- manages PTY terminals with WebSocket bridge
- supports sandboxed execution via bubblewrap

## `// api coverage`

Full parity with the OpenAPI specification:

| Category | Endpoints | Status |
|----------|-----------|--------|
| Health & Config | 10 | ✓ |
| Sessions | 18 | ✓ |
| Messages | 8 | ✓ |
| Files | 6 | ✓ |
| PTY Terminals | 8 | ✓ |
| Events (SSE) | 2 | ✓ |
| Experimental | 12 | ✓ |
| **Total** | **95** | **100%** |

See [API.md](./docs/API.md) for complete endpoint documentation.

## `// architecture`

```
src/
├── Api.hs                 # servant api type definitions
├── Handlers.hs            # request handlers
├── State.hs               # appstate with bus, storage, pty manager
│
├── Agent/                 # agent definitions and permissions
├── Bus/                   # event bus for sse streaming
├── Config/                # configuration parsing
├── LLM/                   # provider integrations (anthropic, openrouter)
├── Message/               # message types and parts
├── Project/               # project discovery
├── Provider/              # provider definitions and oauth
├── Proxy/                 # mitm proxy for traffic inspection
├── Pty/                   # pty session management
├── Sandbox/               # bwrap + overlayfs isolation
├── Session/               # session lifecycle
├── Storage/               # persistence layer
├── Tool/                  # tool execution framework
└── Log.hs                 # structured logging (katip)
```

See [ARCHITECTURE.md](./docs/ARCHITECTURE.md) for detailed module documentation.

## `// tool execution`

The server executes LLM tool calls with the following tools:

| Tool | Description |
|------|-------------|
| `read` | read file/directory contents with line numbers |
| `write` | write content to file |
| `edit` | replace oldString with newString in file |
| `bash` | execute shell command with timeout |
| `glob` | find files by pattern (uses `fd`) |
| `grep` | search file contents (uses `rg`) |

Tool execution flow:

1. LLM returns `stop_reason: "tool_use"` with `tool_use` content blocks
1. server parses `ToolUse` from response
1. `Tool.Exec.executeToolUse` runs the tool and returns `ToolResult`
1. results sent back as `tool_result` content blocks
1. conversation continues until `stop_reason: "end_turn"`

## `// environment`

| Variable | Description |
|----------|-------------|
| `OPENROUTER_API_KEY` | OpenRouter API key for LLM calls |
| `ANTHROPIC_API_KEY` | direct Anthropic API key (alternative) |

## `// dependencies`

Runtime tools used by executors:

- `fd` — fast file finder (glob)
- `rg` — ripgrep (grep)
- `curl` — HTTP client (streaming with IPv4 flag)
- `bwrap` — bubblewrap for sandboxing (optional)

All dependencies are provided by the nix flake.

## `// testing`

```bash
# run all tests
cabal test

# run with verbose output
cabal test --test-show-details=direct
```

Test suite includes:

- 221 property tests with Hedgehog
- API compatibility verification against TypeScript server
- Haskemathesis OpenAPI property testing
- Unit tests for serialization and parsing

## `// contributing`

See [CONTRIBUTING.md](./CONTRIBUTING.md) for code style and contribution guidelines.

```
────────────────────────────────────────────────────────────────────────────────

   "He'd operated on an almost permanent adrenaline high, a byproduct of youth
    and proficiency, jacked into a custom cyberspace deck that projected his
    disembodied consciousness into the consensual hallucination that was the
    matrix."

                                                                 — Neuromancer
────────────────────────────────────────────────────────────────────────────────
```

## `// license`

MIT. See [LICENSE](./LICENSE).
