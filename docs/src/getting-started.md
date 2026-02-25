# Getting Started

## Prerequisites

- **Nix** (recommended) — For reproducible builds
- **GHC 9.10+** — If building without Nix
- **Linux** — Required for `io_uring` support (other platforms use Warp fallback)

## Installation

### With Nix (Recommended)

```bash
# Run directly
nix run github:straylight-software/weapon-server-hs

# Or add to your flake
{
  inputs.weapon-server.url = "github:straylight-software/weapon-server-hs";
}
```

### From Source

```bash
git clone git@github.com:straylight-software/weapon-server-hs.git
cd weapon-server-hs

# Enter development shell
nix develop

# Build
cabal build exe:weapon-server

# Run
cabal run exe:weapon-server -- --port 4096
```

## Verifying Installation

```bash
# Health check
curl http://localhost:4096/global/health

# Expected response
{"healthy":true,"version":"0.1.0"}
```

## Development Setup

The Nix development shell includes:

- GHC with all dependencies
- cabal-install
- haskell-language-server
- ghcid (for fast recompilation)
- stan (static analysis)
- mdbook (documentation)
- ripgrep, fd, git (runtime dependencies)

```bash
# Enter dev shell
nix develop

# Watch mode with ghcid
ghcid --command "cabal repl exe:weapon-server"

# Run tests
cabal test --test-show-details=direct

# Check code quality
stan --hiedir=dist-newstyle

# Format code
nix fmt
```

## NixOS Deployment

Add the module to your NixOS configuration:

```nix
{
  imports = [ weapon-server.nixosModules.default ];

  services.weapon-server = {
    enable = true;
    port = 4096;
  };
}
```

## Next Steps

- [Configuration](./configuration.md) — Set up Dhall config
- [API Reference](./api.md) — Explore the HTTP API
- [Architecture](./architecture.md) — Understand the codebase
