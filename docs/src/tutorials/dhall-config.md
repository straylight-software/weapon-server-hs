# Configuring Weapon with Dhall

This tutorial walks through configuring Weapon using Dhall, a typed configuration
language that provides safety, composability, and excellent error messages.

## Why Dhall?

- **Type safety**: Catch config errors before runtime
- **Composability**: Import and merge configs from files or URLs
- **Defaults**: Override only what you need, get sensible defaults for everything else
- **Functions**: Create reusable config templates
- **No surprises**: Dhall is total (always terminates) and pure (no side effects)

## File Locations

Weapon looks for configuration in two places:

| Location | Purpose |
|----------|---------|
| `~/.config/weapon/weapon.dhall` | User-wide defaults |
| `./weapon.dhall` | Project-specific settings |

Project config takes precedence over global config, which takes precedence over
built-in defaults.

## Your First Config

Create `~/.config/weapon/weapon.dhall`:

```dhall
{ model = Some "anthropic/claude-sonnet-4-20250514" }
```

That's it! All other settings use defaults. Dhall's `Some` wraps optional values.

## Setting Multiple Options

```dhall
{ model = Some "anthropic/claude-sonnet-4-20250514"
, provider = Some "anthropic"
, autoupdate = Some True
}
```

## Customizing Keybinds

Override specific keybinds while keeping defaults for the rest:

```dhall
let Defaults = ./dhall/Defaults.dhall

in  Defaults
    // { keybinds = Defaults.keybinds
           // { leader = Some "ctrl+space"
              , session_new = Some "<leader>n"
              , model_list = Some "<leader>m"
              }
       }
```

The `//` operator merges records, with right-side values taking precedence.

### Disabling a Keybind

Set it to `"none"`:

```dhall
{ keybinds = { session_share = Some "none" } }
```

### Multiple Keys for One Action

Use comma-separated values:

```dhall
{ keybinds = { app_exit = Some "ctrl+c,ctrl+d,<leader>q" } }
```

### Using the Leader Key

Reference `<leader>` in keybinds and it expands to your leader key sequence:

```dhall
{ keybinds =
    { leader = Some "ctrl+x"
    , session_new = Some "<leader>n"  -- becomes ctrl+x n
    }
}
```

## Configuring Agents

Customize agent behavior:

```dhall
{ agent = toMap
    { armed =
        { model = Some "anthropic/claude-sonnet-4-20250514"
        , maxTokens = Some 8192
        }
    , explore =
        { model = Some "openai/gpt-4o-mini"  -- faster model for exploration
        }
    }
}
```

### Agent Colors

Agents can have custom colors for the TUI:

```dhall
{ agent = toMap
    { armed =
        { color = Some "primary"  -- use theme color
        }
    , explore =
        { color = Some "#ff6b6b"  -- use hex color
        }
    }
}
```

## Configuring Providers

Set provider-specific options:

```dhall
{ providers = toMap
    { anthropic =
        { models = toMap
            { "claude-sonnet-4-20250514" =
                { maxTokens = Some 8192
                , temperature = Some 0.7
                }
            }
        }
    , openai =
        { baseUrl = Some "https://api.openai.com/v1"
        }
    }
}
```

## Adding MCP Servers

MCP (Model Context Protocol) servers extend the agent with external tools.

### Local MCP Server

```dhall
{ mcp = toMap
    { filesystem =
        { type = "local"
        , command = [ "npx", "-y", "@modelcontextprotocol/server-filesystem", "/home/user" ]
        , enabled = Some True
        , timeout = Some 5000
        }
    }
}
```

### Remote MCP Server

```dhall
{ mcp = toMap
    { remote-tools =
        { type = "remote"
        , url = "https://mcp.example.com/sse"
        , headers = toMap { Authorization = "Bearer token123" }
        , enabled = Some True
        }
    }
}
```

## Permission Rules

Control what tools can do without asking:

```dhall
let PermissionAction = < ask | allow | deny >

in  { permissions =
        { read = Some (PermissionAction.allow)
        , edit = Some (PermissionAction.ask)
        , bash = Some (PermissionAction.ask)
        , webfetch = Some (PermissionAction.allow)
        }
    }
```

### Path-Based Permissions

Allow/deny based on file paths:

```dhall
{ permissions =
    { edit = Some
        { byPath = toMap
            { "/home/user/project" = PermissionAction.allow
            , "/etc" = PermissionAction.deny
            }
        }
    }
}
```

## Defining Custom Skills

Skills are reusable prompts with optional tool restrictions:

```dhall
{ skills = toMap
    { code-review =
        { name = "code-review"
        , description = "Review code for issues and improvements"
        , prompt = ''
            Review the provided code for:
            - Potential bugs
            - Performance issues
            - Security vulnerabilities
            - Code style improvements
            
            Be specific and provide actionable suggestions.
          ''
        , tools = Some [ "read", "glob", "grep" ]
        }
    , explain =
        { name = "explain"
        , description = "Explain code in detail"
        , prompt = ''
            Explain this code in detail. Cover:
            - What it does at a high level
            - How each part works
            - Any notable patterns or techniques used
          ''
        , tools = Some [ "read" ]
        , agent = Some "locked"  -- read-only mode
        }
    }
}
```

Use skills in the TUI with `/skill code-review` or programmatically.

## Importing Configurations

### From Local Files

Split your config into modules:

```dhall
-- ~/.config/weapon/weapon.dhall
let keybinds = ./keybinds.dhall
let agents = ./agents.dhall

in  { keybinds = keybinds
    , agent = agents
    }
```

### From URLs

Import shared configs:

```dhall
let teamDefaults = https://internal.example.com/weapon-config.dhall

in  teamDefaults // { model = Some "anthropic/claude-sonnet-4-20250514" }
```

Dhall caches imports and verifies integrity with optional SHA256 hashes:

```dhall
let teamDefaults =
      https://internal.example.com/weapon-config.dhall
        sha256:abc123...

in  teamDefaults
```

## Project-Specific Configs

Create `./weapon.dhall` in your project root:

```dhall
-- Project: my-haskell-app
{ model = Some "anthropic/claude-sonnet-4-20250514"
, permissions =
    { bash = Some { allow = True }  -- trust bash in this project
    }
, mcp = toMap
    { haskell-lsp =
        { type = "local"
        , command = [ "haskell-language-server-wrapper", "--lsp" ]
        }
    }
}
```

## TUI Settings

Customize the terminal interface:

```dhall
{ tui =
    { scrollSpeed = Some 3
    , scrollAcceleration = Some 1.5
    , diffStyle = Some "stacked"
    }
}
```

## Server Settings

Configure the HTTP server:

```dhall
{ server =
    { hostname = Some "127.0.0.1"
    , port = Some 4096
    , cors = Some True
    }
}
```

## Complete Example

Here's a full configuration showing multiple features:

```dhall
let PermissionAction = < ask | allow | deny >

in  { -- Model defaults
      model = Some "anthropic/claude-sonnet-4-20250514"
    , provider = Some "anthropic"
    
    -- Keybinds
    , keybinds =
        { leader = Some "ctrl+space"
        , session_new = Some "<leader>n"
        , session_list = Some "<leader>l"
        , model_list = Some "<leader>m"
        , app_exit = Some "ctrl+c,ctrl+d"
        }
    
    -- Agent customization
    , agent = toMap
        { armed =
            { maxTokens = Some 8192 }
        , explore =
            { model = Some "openai/gpt-4o-mini" }
        }
    
    -- Permissions
    , permissions =
        { read = Some PermissionAction.allow
        , glob = Some PermissionAction.allow
        , grep = Some PermissionAction.allow
        , edit = Some PermissionAction.ask
        , bash = Some PermissionAction.ask
        }
    
    -- MCP servers
    , mcp = toMap
        { filesystem =
            { type = "local"
            , command = [ "npx", "-y", "@modelcontextprotocol/server-filesystem", "/home/user" ]
            }
        }
    
    -- Custom skills
    , skills = toMap
        { review =
            { name = "review"
            , description = "Code review"
            , prompt = "Review this code for bugs and improvements."
            , tools = Some [ "read", "glob", "grep" ]
            }
        }
    
    -- Server
    , server =
        { port = Some 4096
        , cors = Some True
        }
    }
```

## Validating Your Config

Use the `dhall` CLI to check your config:

```bash
# Type-check
dhall type --file ~/.config/weapon/weapon.dhall

# Format (normalizes style)
dhall format --inplace ~/.config/weapon/weapon.dhall

# Resolve imports and show final config
dhall --file ~/.config/weapon/weapon.dhall
```

## Common Patterns

### Environment-Based Config

```dhall
let env = env:WEAPON_ENV ? "development"

in  if env == "production"
    then { model = Some "anthropic/claude-sonnet-4-20250514" }
    else { model = Some "openai/gpt-4o-mini" }
```

### Config Functions

Create reusable templates:

```dhall
let makeAgent = \(model : Text) -> \(maxTokens : Natural) ->
      { model = Some model
      , maxTokens = Some maxTokens
      }

in  { agent = toMap
        { armed = makeAgent "anthropic/claude-sonnet-4-20250514" 8192
        , explore = makeAgent "openai/gpt-4o-mini" 4096
        }
    }
```

### Conditional Features

```dhall
let useExperimental = True

in  { experimental =
        if useExperimental
        then Some { worktree = True, sandbox = True }
        else None { worktree : Bool, sandbox : Bool }
    }
```

## Troubleshooting

### "Type mismatch" Errors

Dhall has strict types. Common issues:

```dhall
-- Wrong: bare string where Optional Text expected
{ model = "claude-sonnet-4-20250514" }

-- Right: wrap in Some
{ model = Some "claude-sonnet-4-20250514" }
```

### "Unbound variable" Errors

Check that imports resolve and variables are defined:

```dhall
-- If using Defaults, make sure the path is correct
let Defaults = ./dhall/Defaults.dhall  -- relative to config file
```

### Config Not Taking Effect

1. Check file location: `~/.config/weapon/weapon.dhall` or `./weapon.dhall`
1. Validate syntax: `dhall type --file weapon.dhall`
1. Restart the server after config changes

## Next Steps

- See [Configuration Reference](../configuration/dhall.md) for all available options
- Explore [API Cookbook](../api-cookbook.md) for programmatic config updates
- Check [Built-in Agents](./custom-agent.md) to understand agent configuration
