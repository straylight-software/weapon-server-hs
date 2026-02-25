# Building a Custom Agent

This tutorial walks you through creating a custom agent for Weapon Server. Agents define how the AI assistant behaves, what tools it can access, and what system prompt it uses.

## Understanding Agents

An agent consists of:

- **Name** — Unique identifier (e.g., "code-reviewer")
- **Mode** — `Primary` (user-facing) or `Subagent` (spawned by other agents)
- **Permissions** — Which tools the agent can use
- **System Prompt** — Instructions that shape agent behavior
- **Optional Settings** — Temperature, model override, color, etc.

## Method 1: Dhall Configuration

The easiest way to create a custom agent is via your `weapon.dhall` config:

```dhall
let Types = ./dhall/Types.dhall

let myAgent =
      { model = Some "anthropic/claude-sonnet-4-20250514"
      , maxTokens = Some 4096
      , systemPrompt = Some
          ''
          You are a code reviewer focused on Haskell best practices.
          Review code for:
          - Type safety and proper use of the type system
          - Purity and effect handling
          - Performance considerations
          - Idiomatic Haskell patterns
          
          Be constructive but thorough. Suggest concrete improvements.
          ''
      , tools = Some [ "read", "glob", "grep" ]  -- Read-only tools
      , mode = Some Types.Enums.AgentMode.Primary
      , color = Some (Types.Agent.AgentColor.Hex "#4CAF50")
      , description = Some "Haskell code review specialist"
      }

in  Types.Config.default
    // { agent = Some
           [ { mapKey = "code-reviewer", mapValue = myAgent }
           ]
       }
```

## Method 2: Haskell Definition

For built-in agents, define them in `src/Agent/Agent.hs`:

```haskell
{- | Code review agent focused on Haskell best practices. -}
codeReviewerAgent :: Agent
codeReviewerAgent =
    (defaultAgent "code-reviewer" Primary (mkSimpleRuleset permissions))
        { agentDescription = Just "Haskell code review specialist"
        , agentPrompt = Just $ T.unlines
            [ "You are a code reviewer focused on Haskell best practices."
            , "Review code for:"
            , "- Type safety and proper use of the type system"
            , "- Purity and effect handling"
            , "- Performance considerations"
            , "- Idiomatic Haskell patterns"
            , ""
            , "Be constructive but thorough. Suggest concrete improvements."
            ]
        , agentColor = Just "#4CAF50"
        }
  where
    permissions =
        [ ("*", Deny)        -- Deny all by default
        , ("read", Allow)    -- Allow reading files
        , ("glob", Allow)    -- Allow file search
        , ("grep", Allow)    -- Allow content search
        ]
```

Then add it to `builtinAgents`:

```haskell
builtinAgents :: [Agent]
builtinAgents =
    [ armedAgent
    , lockedAgent
    , generalAgent
    , exploreAgent
    , codeReviewerAgent  -- Add your agent
    , compactionAgent
    , titleAgent
    , summaryAgent
    ]
```

## Permission System

Permissions control tool access using a ruleset pattern:

```haskell
-- Allow all tools
allowAllRuleset :: PermissionRuleset
allowAllRuleset = mkSimpleRuleset [("*", Allow)]

-- Deny all tools
denyAllRuleset :: PermissionRuleset
denyAllRuleset = mkSimpleRuleset [("*", Deny)]

-- Mixed permissions (order matters - more specific rules override)
mixedPermissions = mkSimpleRuleset
    [ ("*", Allow)       -- Allow everything by default
    , ("bash", Ask)      -- Ask before running shell commands
    , ("write", Deny)    -- Never allow writing files
    ]
```

### Permission Actions

| Action | Behavior |
|--------|----------|
| `Allow` | Tool executes immediately |
| `Deny` | Tool is blocked |
| `Ask` | User is prompted for confirmation |

### Available Tools

| Tool | Description |
|------|-------------|
| `read` | Read file contents |
| `write` | Write/create files |
| `edit` | Edit existing files |
| `glob` | Search for files by pattern |
| `grep` | Search file contents |
| `bash` | Execute shell commands |
| `webfetch` | Fetch web content |
| `task` | Spawn subagent |
| `todoread` | Read todo list |
| `todowrite` | Modify todo list |
| `question` | Ask user a question |

## Agent Modes

### Primary Mode

Primary agents interact directly with users:

```haskell
myAgent = defaultAgent "my-agent" Primary permissions
```

### Subagent Mode

Subagents are spawned by other agents for specific tasks:

```haskell
researchAgent = defaultAgent "researcher" Subagent permissions
```

Subagents:

- Cannot be invoked directly by users
- Inherit context from parent agent
- Should report results back concisely

## Context Injection

The server automatically injects environment context into agent prompts:

```xml
<env>
  Working directory: /home/user/project
  Is directory a git repo: yes
  Git branch: main
  Platform: linux
  Today's date: Wed Feb 25 2026
</env>

<files>
  README.md
  project.cabal
  flake.nix
</files>

<philosophy>
  [Functional programming principles...]
</philosophy>

[Your agent's system prompt here]
```

You don't need to include this in your prompt — it's added automatically for coding agents.

## Testing Your Agent

1. **Build the server:**

   ```bash
   cabal build exe:weapon-server
   ```

1. **Start with your config:**

   ```bash
   cabal run exe:weapon-server -- --port 4096
   ```

1. **Test via API:**

   ```bash
   curl -X POST http://localhost:4096/session \
     -H "Content-Type: application/json" \
     -d '{"agent": "code-reviewer"}'
   ```

1. **Add property tests** in `test/Property/AgentAgentProps.hs`:

   ```haskell
   prop_codeReviewer_exists :: Property
   prop_codeReviewer_exists = property $ do
       agents <- liftIO Agent.list
       let names = map agentName agents
       assert $ "code-reviewer" `elem` names
   ```

## Best Practices

1. **Be specific in prompts** — Vague prompts lead to inconsistent behavior
1. **Limit tool access** — Only grant permissions the agent actually needs
1. **Test with edge cases** — Try malformed input, empty files, etc.
1. **Use subagents** — Break complex tasks into focused subagents
1. **Document behavior** — Add `agentDescription` for UI display
