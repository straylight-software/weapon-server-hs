# Contributing

## Development Philosophy

This project follows functional programming principles:

- **Favor Haskell** for the backend, **PureScript** for frontends
- **Build with Nix** for reproducibility
- **Never settle for bad abstractions** — refactor until types tell the story
- **Use the type system** to make invalid states unrepresentable
- **Prefer pure functions**, explicit effects, and composable designs
- **Value correctness over convenience**
- **Write code that is easy to delete**, not easy to extend

## Getting Started

```bash
# Clone the repo
git clone git@github.com:straylight-software/weapon-server-hs.git
cd weapon-server-hs

# Enter dev shell
nix develop

# Build everything
cabal build all

# Run tests
cabal test --test-show-details=direct
```

## Code Quality

### Tests

We use property-based testing with Hedgehog:

```bash
# Run all tests
cabal test

# Run specific test pattern
cabal test --test-option="--pattern=Agent"

# Run with verbose output
cabal test --test-show-details=direct
```

### Static Analysis

Stan checks for common issues:

```bash
# Run stan
stan --hiedir=dist-newstyle

# Should show 100% health
```

### Formatting

Code is formatted with Ormolu via treefmt:

```bash
# Check and fix formatting
nix fmt

# Should report no changes needed
```

## Project Structure

```
src/
├── Agent/           # Agent definitions and context
├── Api/             # Servant API types
├── Bus/             # Event bus (pub/sub)
├── Config/          # Dhall configuration
├── Evring/          # io_uring HTTP server
├── LLM/             # LLM provider integrations
├── Pty/             # PTY session management
├── Session/         # Session lifecycle
├── Storage/         # Persistence layer
├── Tool/            # Tool execution
└── Handlers.hs      # HTTP request handlers

test/
├── Property/        # Hedgehog property tests
├── Unit/            # HSpec unit tests
└── Integration/     # Integration tests

dhall/
├── Types.dhall      # Type definitions
├── Defaults.dhall   # Default values
└── Types/           # Individual type modules
```

## Making Changes

1. **Create a branch** for your changes
1. **Write tests first** — property tests preferred
1. **Implement the feature** with clear types
1. **Run the full test suite** — all tests must pass
1. **Check Stan health** — should be 100%
1. **Format code** — `nix fmt` should report no changes
1. **Submit PR** with clear description

## Commit Guidelines

- Use clear, descriptive commit messages
- Focus on "why" not "what"
- Keep commits atomic and focused

## Documentation

Documentation uses mdbook:

```bash
# Serve docs locally
cd docs
mdbook serve

# Build docs
mdbook build
```
