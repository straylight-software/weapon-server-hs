# Weapon Server

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                    // weapon-server // haskell
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

   "The matrix has its roots in primitive arcade games," said the voice-over,
    "in early graphics programs and military experimentation with cranial
    jacks."

                                                               — Neuromancer
```

Weapon Server is a high-performance Haskell backend for AI coding agents. It provides:

- **Type-safe HTTP API** via Servant
- **Real-time events** via Server-Sent Events (SSE)
- **PTY management** for terminal sessions
- **Tool execution** with sandboxing support
- **Multi-provider LLM integration** (Anthropic, OpenRouter, etc.)
- **Dhall-based configuration** with full type safety

## Key Features

### High Performance

Built on a custom `io_uring`-based HTTP server (`evring-wai`) for maximum throughput on Linux. Falls back to standard Warp on other platforms.

**Benchmark** (aarch64, 12 cores, 10k requests @ 100 concurrent):

| Backend | req/s | p50 | p99 |
|---------|-------|-----|-----|
| io_uring | 37,765 | 1.2ms | 13ms |
| warp | 35,048 | 1.4ms | 13ms |

io_uring provides ~8% higher throughput with slightly lower median latency.

### Functional Design

The codebase emphasizes:

- Pure functions where possible
- Explicit effect handling via `IO` and `STM`
- Type-driven development with rich domain types
- Property-based testing with Hedgehog

### Agent System

Agents define tool permissions and system prompts:

- `armed` — Full tool access (default)
- `locked` — Read-only mode
- `explore` — Fast codebase search
- `general` — Multi-purpose subagent

### Nix Integration

First-class Nix support:

- `nix build` — Build the server
- `nix develop` — Development shell with all tools
- NixOS module for deployment

## Quick Start

```bash
# Clone and enter dev shell
git clone git@github.com:straylight-software/weapon-server-hs.git
cd weapon-server-hs
nix develop

# Build and run
cabal build
cabal run exe:weapon-server -- --port 4096

# Run tests
cabal test
```

## Documentation Overview

- **[Getting Started](./getting-started.md)** — Installation and first steps
- **[Configuration](./configuration.md)** — Dhall config system
- **[API Reference](./api.md)** — Complete HTTP API docs
- **[Architecture](./architecture.md)** — Internal design and modules
