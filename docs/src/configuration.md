# Configuration

Weapon Server uses [Dhall](https://dhall-lang.org/) for type-safe configuration. Dhall provides:

- **Full type safety** — Configuration errors caught at load time
- **Composable configs** — Import and merge configurations
- **Built-in defaults** — Override only what you need
- **IDE support** — Syntax highlighting and completion

## Configuration Files

The server looks for configuration in these locations (in order):

1. `./weapon.dhall` — Project-local config
1. `~/.config/weapon/weapon.dhall` — User global config

Project config takes precedence over global config, with values merged.

## Quick Start

Create a minimal `weapon.dhall`:

```dhall
let Defaults = ./dhall/Defaults.dhall

in Defaults // {
  server = Defaults.server // {
    port = 8080
  }
}
```

## Configuration Structure

```dhall
{ server : Server
, tui : TUI
, providers : List Provider
, agents : List Agent
, keybinds : Keybinds
, mcp : List MCP
, permissions : List Permission
, theme : Optional Theme
}
```

### Server Config

```dhall
{ port : Natural
, host : Text
, logLevel : LogLevel  -- Debug | Info | Warning | Error
}
```

### Provider Config

```dhall
{ id : Text
, name : Text
, baseUrl : Text
, envVar : Text
, models : List ProviderModel
}
```

### Agent Config

```dhall
{ name : Text
, description : Optional Text
, mode : AgentMode  -- Primary | Subagent | AllModes
, prompt : Optional Text
, temperature : Optional Double
, permission : List PermissionRule
}
```

## Detailed Reference

See [Dhall Config System](./configuration/dhall.md) for the complete configuration reference including all types, defaults, and examples.
