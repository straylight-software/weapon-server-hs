# Dhall Configuration System

## Overview

This document describes the plan to replace the current JSON-based configuration system with a Dhall-based system that provides:

- **Full type safety** with Dhall's type system
- **1:1 representation** of the TypeScript config schema from OpenCode
- **Built-in defaults** via a `Defaults.dhall` file
- **Composable configs** using Dhall imports and let bindings
- **Skills and themes as native Dhall values** (fetchable from URLs via Dhall's built-in fetchers)

## Motivation

The current config system returns `null` for missing config sections (like keybinds), which causes the TUI to malfunction (e.g., Ctrl+C doesn't work because keybind defaults aren't applied server-side).

The TypeScript server uses Zod schemas with `.default()` values that are automatically applied. We need equivalent functionality in Haskell, and Dhall provides this elegantly.

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Config format | Dhall only | Clean break, full type safety |
| File locations | `~/.config/weapon/weapon.dhall` (global), `./weapon.dhall` (project) | Match TypeScript paths |
| Defaults handling | Ship `Defaults.dhall` | Users can import and override |
| Union types | Native Dhall unions | `< Disabled \| Enabled : {...} >` syntax |
| Validation | Strict at load time | Fail fast with clear Dhall type errors |
| Themes/Skills | Pure Dhall records | Fetchable via Dhall imports |
| Composition | Full Dhall | Allow imports, let bindings, functions |
| Dependency | Add `dhall` Haskell package | Direct integration, no shelling out |

## Directory Structure

```
dhall/
├── Types.dhall              # Re-exports all types
├── Defaults.dhall           # Default config values (imports Types)
├── package.dhall            # Package file for easy importing
├── Types/
│   ├── Enums.dhall          # LogLevel, ShareMode, Layout, PermissionAction, etc.
│   ├── Keybinds.dhall       # All 70+ keybind fields with defaults
│   ├── Server.dhall         # Server configuration
│   ├── TUI.dhall            # TUI-specific settings
│   ├── Permission.dhall     # Permission rules (with union types)
│   ├── Agent.dhall          # Agent config (including AgentColor union)
│   ├── Provider.dhall       # Provider config (ProviderModel, ProviderOptions)
│   ├── MCP.dhall            # MCP servers (discriminated union: local | remote)
│   ├── Formatter.dhall      # Formatter config (union: Disabled | Config)
│   ├── LSP.dhall            # LSP config (union: Disabled | Config)
│   ├── Theme.dhall          # Theme color definitions
│   ├── Skill.dhall          # Skill definitions
│   ├── Command.dhall        # Custom command definitions
│   ├── Watcher.dhall        # File watcher config
│   ├── Compaction.dhall     # Compaction settings
│   ├── Experimental.dhall   # Experimental features
│   └── Enterprise.dhall     # Enterprise configuration
└── Themes/                  # Built-in themes
    ├── Default.dhall
    ├── Dark.dhall
    └── Light.dhall
```

## Config File Locations

| Type | Path | Purpose |
|------|------|---------|
| Global | `~/.config/weapon/weapon.dhall` | User-wide defaults |
| Project | `./weapon.dhall` | Project-specific overrides |
| Built-in | `dhall/Defaults.dhall` | Factory defaults (shipped with binary) |

**Precedence**: Project > Global > Built-in Defaults

## Implementation Phases

### Phase 1: Dependencies & Infrastructure

1. Add `dhall` package to `weapon-server.cabal`
1. Create `dhall/` directory structure
1. Update `flake.nix` if needed for dhall tooling

### Phase 2: Dhall Type Definitions

Create all type definition files:

4. `dhall/Types/Enums.dhall` - LogLevel, ShareMode, Layout, PermissionAction, DiffStyle, AgentMode, ModelStatus
1. `dhall/Types/Keybinds.dhall` - All 70+ keybind fields
1. `dhall/Types/Server.dhall` - hostname, port, mdns, cors
1. `dhall/Types/TUI.dhall` - scroll_speed, scroll_acceleration, diff_style
1. `dhall/Types/Permission.dhall` - PermissionAction, PermissionRule union
1. `dhall/Types/Agent.dhall` - AgentConfig, AgentColor union
1. `dhall/Types/Provider.dhall` - ProviderConfig, ProviderModel, ProviderOptions, ProviderTimeout union
1. `dhall/Types/MCP.dhall` - McpLocal, McpRemote, MCP discriminated union
1. `dhall/Types/Formatter.dhall` - FormatterEntry, Formatter union (Disabled | Config)
1. `dhall/Types/LSP.dhall` - LSPEntry, LSP union (Disabled | Config)
1. `dhall/Types/Theme.dhall` - ThemeColors record (50+ color fields)
1. `dhall/Types/Skill.dhall` - Skill record (name, description, prompt, tools)
1. `dhall/Types/Command.dhall` - Custom command definition
1. `dhall/Types/Watcher.dhall` - File watcher ignore patterns
1. `dhall/Types/Compaction.dhall` - auto, prune, reserved
1. `dhall/Types/Experimental.dhall` - Experimental feature flags
1. `dhall/Types/Enterprise.dhall` - Enterprise URL config
1. `dhall/Types.dhall` - Re-export all types

### Phase 3: Defaults

22. Create `dhall/Defaults.dhall` with all default values
    - Full keybinds defaults (critical for Ctrl+C fix)
    - Sensible defaults for all optional fields

### Phase 4: Haskell Integration

23. Create `src/Config/Dhall.hs` - Dhall loading and evaluation
    - `loadConfig :: FilePath -> IO Config`
    - `loadConfigWithDefaults :: FilePath -> FilePath -> IO Config`
    - Error handling with clear messages
01. Update `src/Config/Types.hs` - Expand types to match full schema
    - Add `FromDhall` instances (or use `Dhall.input auto`)
01. Update `src/Config/Config.hs` - Use Dhall loader instead of JSON
01. Handle embedded defaults (compile Defaults.dhall into binary or load from data dir)

### Phase 5: API Integration

27. Update `configHandler` to return fully-populated config with defaults
01. Ensure keybinds are always present in the response (fixing Ctrl+C)
01. Convert Dhall config to JSON for API responses (TUI expects JSON)

### Phase 6: Testing & Validation

30. Property tests (see below)
01. Integration tests for config merging
01. Golden tests for default config output

______________________________________________________________________

## Type Definitions

### Enums.dhall

```dhall
let LogLevel = < DEBUG | INFO | WARN | ERROR >

let ShareMode = < manual | auto | disabled >

let Layout = < auto | stretch >

let PermissionAction = < ask | allow | deny >

let DiffStyle = < auto | stacked >

let AgentMode = < subagent | primary | all >

let ModelStatus = < alpha | beta | deprecated >

let ThemeColorName =
      < primary
      | secondary
      | accent
      | success
      | warning
      | error
      | info
      >

in  { LogLevel
    , ShareMode
    , Layout
    , PermissionAction
    , DiffStyle
    , AgentMode
    , ModelStatus
    , ThemeColorName
    }
```

### Keybinds.dhall

```dhall
let Keybinds =
      { Type =
          { leader : Optional Text
          , app_exit : Optional Text
          , editor_open : Optional Text
          , theme_list : Optional Text
          , sidebar_toggle : Optional Text
          , scrollbar_toggle : Optional Text
          , username_toggle : Optional Text
          , status_view : Optional Text
          , session_export : Optional Text
          , session_new : Optional Text
          , session_list : Optional Text
          , session_timeline : Optional Text
          , session_fork : Optional Text
          , session_rename : Optional Text
          , session_delete : Optional Text
          , stash_delete : Optional Text
          , model_provider_list : Optional Text
          , model_favorite_toggle : Optional Text
          , session_share : Optional Text
          , session_unshare : Optional Text
          , session_interrupt : Optional Text
          , session_compact : Optional Text
          , messages_page_up : Optional Text
          , messages_page_down : Optional Text
          , messages_line_up : Optional Text
          , messages_line_down : Optional Text
          , messages_half_page_up : Optional Text
          , messages_half_page_down : Optional Text
          , messages_first : Optional Text
          , messages_last : Optional Text
          , messages_next : Optional Text
          , messages_previous : Optional Text
          , messages_last_user : Optional Text
          , messages_copy : Optional Text
          , messages_undo : Optional Text
          , messages_redo : Optional Text
          , messages_toggle_conceal : Optional Text
          , tool_details : Optional Text
          , model_list : Optional Text
          , model_cycle_recent : Optional Text
          , model_cycle_recent_reverse : Optional Text
          , model_cycle_favorite : Optional Text
          , model_cycle_favorite_reverse : Optional Text
          , command_list : Optional Text
          , agent_list : Optional Text
          , agent_cycle : Optional Text
          , agent_cycle_reverse : Optional Text
          , variant_cycle : Optional Text
          , input_clear : Optional Text
          , input_paste : Optional Text
          , input_submit : Optional Text
          , input_newline : Optional Text
          , input_move_left : Optional Text
          , input_move_right : Optional Text
          , input_move_up : Optional Text
          , input_move_down : Optional Text
          , input_select_left : Optional Text
          , input_select_right : Optional Text
          , input_select_up : Optional Text
          , input_select_down : Optional Text
          , input_line_home : Optional Text
          , input_line_end : Optional Text
          , input_select_line_home : Optional Text
          , input_select_line_end : Optional Text
          , input_visual_line_home : Optional Text
          , input_visual_line_end : Optional Text
          , input_select_visual_line_home : Optional Text
          , input_select_visual_line_end : Optional Text
          , input_buffer_home : Optional Text
          , input_buffer_end : Optional Text
          , input_select_buffer_home : Optional Text
          , input_select_buffer_end : Optional Text
          , input_delete_line : Optional Text
          , input_delete_to_line_end : Optional Text
          , input_delete_to_line_start : Optional Text
          , input_backspace : Optional Text
          , input_delete : Optional Text
          , input_undo : Optional Text
          , input_redo : Optional Text
          , input_word_forward : Optional Text
          , input_word_backward : Optional Text
          , input_select_word_forward : Optional Text
          , input_select_word_backward : Optional Text
          , input_delete_word_forward : Optional Text
          , input_delete_word_backward : Optional Text
          , history_previous : Optional Text
          , history_next : Optional Text
          , session_child_cycle : Optional Text
          , session_child_cycle_reverse : Optional Text
          , session_parent : Optional Text
          , terminal_suspend : Optional Text
          , terminal_title_toggle : Optional Text
          , tips_toggle : Optional Text
          , display_thinking : Optional Text
          }
      , default =
          { leader = Some "ctrl+x"
          , app_exit = Some "ctrl+c,ctrl+d,<leader>q"
          , editor_open = Some "<leader>e"
          , theme_list = Some "<leader>t"
          , sidebar_toggle = Some "<leader>b"
          , scrollbar_toggle = Some "none"
          , username_toggle = Some "none"
          , status_view = Some "<leader>s"
          , session_export = Some "<leader>x"
          , session_new = Some "<leader>n"
          , session_list = Some "<leader>l"
          , session_timeline = Some "<leader>g"
          , session_fork = Some "none"
          , session_rename = Some "ctrl+r"
          , session_delete = Some "ctrl+d"
          , stash_delete = Some "ctrl+d"
          , model_provider_list = Some "ctrl+a"
          , model_favorite_toggle = Some "ctrl+f"
          , session_share = Some "none"
          , session_unshare = Some "none"
          , session_interrupt = Some "escape"
          , session_compact = Some "<leader>c"
          , messages_page_up = Some "pageup,ctrl+alt+b"
          , messages_page_down = Some "pagedown,ctrl+alt+f"
          , messages_line_up = Some "ctrl+alt+y"
          , messages_line_down = Some "ctrl+alt+e"
          , messages_half_page_up = Some "ctrl+alt+u"
          , messages_half_page_down = Some "ctrl+alt+d"
          , messages_first = Some "ctrl+g,home"
          , messages_last = Some "ctrl+alt+g,end"
          , messages_next = Some "none"
          , messages_previous = Some "none"
          , messages_last_user = Some "none"
          , messages_copy = Some "<leader>y"
          , messages_undo = Some "<leader>u"
          , messages_redo = Some "<leader>r"
          , messages_toggle_conceal = Some "<leader>h"
          , tool_details = Some "none"
          , model_list = Some "<leader>m"
          , model_cycle_recent = Some "f2"
          , model_cycle_recent_reverse = Some "shift+f2"
          , model_cycle_favorite = Some "none"
          , model_cycle_favorite_reverse = Some "none"
          , command_list = Some "ctrl+p"
          , agent_list = Some "<leader>a"
          , agent_cycle = Some "tab"
          , agent_cycle_reverse = Some "shift+tab"
          , variant_cycle = Some "ctrl+t"
          , input_clear = Some "ctrl+c"
          , input_paste = Some "ctrl+v"
          , input_submit = Some "return"
          , input_newline = Some "shift+return,ctrl+return,alt+return,ctrl+j"
          , input_move_left = Some "left,ctrl+b"
          , input_move_right = Some "right,ctrl+f"
          , input_move_up = Some "up"
          , input_move_down = Some "down"
          , input_select_left = Some "shift+left"
          , input_select_right = Some "shift+right"
          , input_select_up = Some "shift+up"
          , input_select_down = Some "shift+down"
          , input_line_home = Some "ctrl+a"
          , input_line_end = Some "ctrl+e"
          , input_select_line_home = Some "ctrl+shift+a"
          , input_select_line_end = Some "ctrl+shift+e"
          , input_visual_line_home = Some "alt+a"
          , input_visual_line_end = Some "alt+e"
          , input_select_visual_line_home = Some "alt+shift+a"
          , input_select_visual_line_end = Some "alt+shift+e"
          , input_buffer_home = Some "home"
          , input_buffer_end = Some "end"
          , input_select_buffer_home = Some "shift+home"
          , input_select_buffer_end = Some "shift+end"
          , input_delete_line = Some "ctrl+shift+d"
          , input_delete_to_line_end = Some "ctrl+k"
          , input_delete_to_line_start = Some "ctrl+u"
          , input_backspace = Some "backspace,shift+backspace"
          , input_delete = Some "ctrl+d,delete,shift+delete"
          , input_undo = Some "ctrl+-,super+z"
          , input_redo = Some "ctrl+.,super+shift+z"
          , input_word_forward = Some "alt+f,alt+right,ctrl+right"
          , input_word_backward = Some "alt+b,alt+left,ctrl+left"
          , input_select_word_forward = Some "alt+shift+f,alt+shift+right"
          , input_select_word_backward = Some "alt+shift+b,alt+shift+left"
          , input_delete_word_forward = Some "alt+d,alt+delete,ctrl+delete"
          , input_delete_word_backward = Some "ctrl+w,ctrl+backspace,alt+backspace"
          , history_previous = Some "up"
          , history_next = Some "down"
          , session_child_cycle = Some "<leader>right"
          , session_child_cycle_reverse = Some "<leader>left"
          , session_parent = Some "<leader>up"
          , terminal_suspend = Some "ctrl+z"
          , terminal_title_toggle = Some "none"
          , tips_toggle = Some "<leader>h"
          , display_thinking = Some "none"
          }
      }

in  Keybinds
```

### Permission.dhall (Union Type Example)

```dhall
let Enums = ./Enums.dhall

let PermissionAction = Enums.PermissionAction

-- PermissionRule can be either a simple action or a record of path -> action
let PermissionRule =
      < Action : PermissionAction
      | ByPath : List { mapKey : Text, mapValue : PermissionAction }
      >

let Permission =
      { Type =
          { read : Optional PermissionRule
          , edit : Optional PermissionRule
          , glob : Optional PermissionRule
          , grep : Optional PermissionRule
          , list : Optional PermissionRule
          , bash : Optional PermissionRule
          , task : Optional PermissionRule
          , external_directory : Optional PermissionRule
          , todowrite : Optional PermissionAction
          , todoread : Optional PermissionAction
          , question : Optional PermissionAction
          , webfetch : Optional PermissionAction
          , websearch : Optional PermissionAction
          , codesearch : Optional PermissionAction
          , lsp : Optional PermissionRule
          , doom_loop : Optional PermissionAction
          , skill : Optional PermissionRule
          }
      , default =
          { read = None PermissionRule
          , edit = None PermissionRule
          , glob = None PermissionRule
          , grep = None PermissionRule
          , list = None PermissionRule
          , bash = None PermissionRule
          , task = None PermissionRule
          , external_directory = None PermissionRule
          , todowrite = None PermissionAction
          , todoread = None PermissionAction
          , question = None PermissionAction
          , webfetch = None PermissionAction
          , websearch = None PermissionAction
          , codesearch = None PermissionAction
          , lsp = None PermissionRule
          , doom_loop = None PermissionAction
          , skill = None PermissionRule
          }
      }

in  { PermissionAction, PermissionRule, Permission }
```

### MCP.dhall (Discriminated Union Example)

```dhall
let McpLocal =
      { Type =
          { command : List Text
          , environment : Optional (List { mapKey : Text, mapValue : Text })
          , enabled : Optional Bool
          , timeout : Optional Natural
          }
      , default =
          { environment = None (List { mapKey : Text, mapValue : Text })
          , enabled = Some True
          , timeout = Some 5000
          }
      }

let McpRemote =
      { Type =
          { url : Text
          , enabled : Optional Bool
          , headers : Optional (List { mapKey : Text, mapValue : Text })
          , timeout : Optional Natural
          }
      , default =
          { enabled = Some True
          , headers = None (List { mapKey : Text, mapValue : Text })
          , timeout = Some 5000
          }
      }

let MCP = < Local : McpLocal.Type | Remote : McpRemote.Type >

in  { McpLocal, McpRemote, MCP }
```

### Theme.dhall

```dhall
-- RGBA color representation
let Color = { r : Double, g : Double, b : Double, a : Double }

let ThemeColors =
      { Type =
          { primary : Color
          , secondary : Color
          , accent : Color
          , error : Color
          , warning : Color
          , success : Color
          , info : Color
          , text : Color
          , textMuted : Color
          , selectedListItemText : Optional Color
          , background : Color
          , backgroundPanel : Color
          , backgroundElement : Color
          , backgroundMenu : Optional Color
          , border : Color
          , borderActive : Color
          , borderSubtle : Color
          , diffAdded : Color
          , diffRemoved : Color
          , diffContext : Color
          , diffHunkHeader : Color
          , diffHighlightAdded : Color
          , diffHighlightRemoved : Color
          , diffAddedBg : Color
          , diffRemovedBg : Color
          , diffContextBg : Color
          , diffLineNumber : Color
          , diffAddedLineNumberBg : Color
          , diffRemovedLineNumberBg : Color
          , markdownText : Color
          , markdownHeading : Color
          , markdownLink : Color
          , markdownLinkText : Color
          , markdownCode : Color
          , markdownBlockQuote : Color
          , markdownEmph : Color
          , markdownStrong : Color
          , markdownHorizontalRule : Color
          , markdownListItem : Color
          , markdownListEnumeration : Color
          , markdownImage : Color
          , markdownImageText : Color
          , markdownCodeBlock : Color
          , syntaxComment : Color
          , syntaxKeyword : Color
          , syntaxFunction : Color
          , syntaxVariable : Color
          , syntaxString : Color
          , syntaxNumber : Color
          , syntaxType : Color
          , syntaxOperator : Color
          , syntaxPunctuation : Color
          }
      }

let Theme =
      { Type = ThemeColors.Type //\\ { thinkingOpacity : Double }
      }

in  { Color, ThemeColors, Theme }
```

### Skill.dhall

```dhall
let Skill =
      { Type =
          { name : Text
          , description : Text
          , prompt : Text
          , tools : Optional (List Text)
          , agent : Optional Text
          , model : Optional Text
          }
      , default =
          { tools = None (List Text)
          , agent = None Text
          , model = None Text
          }
      }

in  Skill
```

______________________________________________________________________

## Theme Structure

Based on TypeScript `ThemeColors` type:

| Category | Colors |
|----------|--------|
| **Semantic** | primary, secondary, accent, error, warning, success, info |
| **Text** | text, textMuted, selectedListItemText |
| **Background** | background, backgroundPanel, backgroundElement, backgroundMenu |
| **Border** | border, borderActive, borderSubtle |
| **Diff** | diffAdded, diffRemoved, diffContext, diffHunkHeader, diffHighlightAdded, diffHighlightRemoved, diffAddedBg, diffRemovedBg, diffContextBg, diffLineNumber, diffAddedLineNumberBg, diffRemovedLineNumberBg |
| **Markdown** | markdownText, markdownHeading, markdownLink, markdownLinkText, markdownCode, markdownBlockQuote, markdownEmph, markdownStrong, markdownHorizontalRule, markdownListItem, markdownListEnumeration, markdownImage, markdownImageText, markdownCodeBlock |
| **Syntax** | syntaxComment, syntaxKeyword, syntaxFunction, syntaxVariable, syntaxString, syntaxNumber, syntaxType, syntaxOperator, syntaxPunctuation |

______________________________________________________________________

## Property Tests

### Config Loading Tests

```haskell
-- | Property: Loading defaults produces valid config with all keybinds
prop_defaultsHasAllKeybinds :: Property

-- | Property: Empty user config merged with defaults equals defaults
prop_emptyConfigMergeEqualsDefaults :: Property

-- | Property: User config overrides take precedence over defaults
prop_userOverridesTakePrecedence :: Property

-- | Property: Config round-trip (Dhall -> Haskell -> JSON -> parse) preserves values
prop_configRoundTrip :: Property

-- | Property: Invalid Dhall syntax produces clear error (not crash)
prop_invalidDhallProducesError :: Property

-- | Property: Missing optional fields use defaults
prop_missingFieldsUseDefaults :: Property
```

### Keybinds Tests

```haskell
-- | Property: All keybind fields have defaults (no Nothing after merge)
prop_allKeybindsHaveDefaults :: Property

-- | Property: Keybind values are valid key sequences
prop_keybindValuesAreValidSequences :: Property

-- | Property: Leader substitution works correctly (<leader>x -> ctrl+x x)
prop_leaderSubstitutionWorks :: Property

-- | Property: Multiple keybinds (comma-separated) are all valid
prop_multipleKeybindsAllValid :: Property

-- | Property: "none" keybind disables the action
prop_noneKeybindDisables :: Property
```

### Permission Tests

```haskell
-- | Property: PermissionAction JSON round-trip
prop_permissionActionRoundTrip :: Property

-- | Property: PermissionRule union serializes correctly
prop_permissionRuleUnionSerializes :: Property

-- | Property: Permission with ByPath has valid path patterns
prop_permissionByPathValidPatterns :: Property

-- | Property: Default permissions are all None (not restrictive by default)
prop_defaultPermissionsAreNone :: Property
```

### Union Type Tests

```haskell
-- | Property: MCP Local discriminated union serializes with type="local"
prop_mcpLocalHasTypeField :: Property

-- | Property: MCP Remote discriminated union serializes with type="remote"
prop_mcpRemoteHasTypeField :: Property

-- | Property: Formatter Disabled serializes to false
prop_formatterDisabledSerializesToFalse :: Property

-- | Property: Formatter Config serializes to record
prop_formatterConfigSerializesToRecord :: Property

-- | Property: LSP union round-trip preserves structure
prop_lspUnionRoundTrip :: Property

-- | Property: AutoUpdate union handles both Bool and "notify"
prop_autoUpdateUnionBothVariants :: Property

-- | Property: AgentColor handles both hex strings and theme enums
prop_agentColorBothVariants :: Property

-- | Property: ProviderTimeout handles both Natural and Disabled
prop_providerTimeoutBothVariants :: Property
```

### Theme Tests

```haskell
-- | Property: Theme colors are valid RGBA (0-1 range for each component)
prop_themeColorsValidRGBA :: Property

-- | Property: Theme round-trip preserves all 50+ color fields
prop_themeRoundTrip :: Property

-- | Property: Theme with missing optional fields uses sensible fallbacks
prop_themeMissingFieldsFallback :: Property

-- | Property: ThinkingOpacity is in valid range (0-1)
prop_thinkingOpacityValidRange :: Property
```

### Skill Tests

```haskell
-- | Property: Skill name is non-empty
prop_skillNameNonEmpty :: Property

-- | Property: Skill prompt is non-empty
prop_skillPromptNonEmpty :: Property

-- | Property: Skill tools list contains valid tool names
prop_skillToolsValidNames :: Property

-- | Property: Skill round-trip preserves all fields
prop_skillRoundTrip :: Property
```

### Config Merge Tests

```haskell
-- | Property: Merging two configs is associative
prop_configMergeAssociative :: Property

-- | Property: Merging with empty config is identity
prop_configMergeEmptyIdentity :: Property

-- | Property: Project config fields override global config fields
prop_projectOverridesGlobal :: Property

-- | Property: Nested record merging works correctly (keybinds within config)
prop_nestedRecordMerging :: Property

-- | Property: List fields are replaced, not appended
prop_listFieldsReplaced :: Property
```

### Dhall Import Tests

```haskell
-- | Property: Local file imports resolve correctly
prop_localImportResolves :: Property

-- | Property: Relative imports from config dir work
prop_relativeImportsWork :: Property

-- | Property: Circular imports are detected and rejected
prop_circularImportsRejected :: Property

-- | Property: Missing imports produce clear errors
prop_missingImportsError :: Property
```

### API Response Tests

```haskell
-- | Property: /config endpoint returns JSON with all keybind defaults
prop_configEndpointHasKeybinds :: Property

-- | Property: /config response matches TypeScript schema shape
prop_configResponseMatchesSchema :: Property

-- | Property: Config response has no null values for required defaults
prop_configResponseNoNullDefaults :: Property
```

### Haskemathesis Integration Tests

```haskell
-- | Property: Config endpoint response validates against OpenAPI schema
prop_configConformsToOpenAPI :: Property

-- | Property: All config union types serialize to valid OpenAPI discriminated unions
prop_configUnionsConformToOpenAPI :: Property
```

______________________________________________________________________

## Example User Config

```dhall
-- weapon.dhall
let Defaults = https://raw.githubusercontent.com/straylight-software/weapon-server-hs/main/dhall/Defaults.dhall

let myTheme = https://example.com/my-custom-theme.dhall

in  Defaults
    // { keybinds = Defaults.keybinds // { leader = Some "ctrl+space" }
       , theme = Some "custom"
       , model = Some "anthropic/claude-sonnet-4-20250514"
       , agent =
           toMap
             { armed = Defaults.Agent::{ model = Some "openai/gpt-4o" }
             }
       , mcp =
           toMap
             { filesystem =
                 Defaults.MCP.Local
                   { command = [ "npx", "-y", "@modelcontextprotocol/server-filesystem", "/home/user" ]
                   }
             }
       }
```

______________________________________________________________________

## Migration Path

1. **Phase 0 (Immediate fix)**: Hardcode keybind defaults in `configHandler` to fix Ctrl+C
1. **Phase 1**: Implement Dhall type definitions
1. **Phase 2**: Implement Dhall loading in Haskell
1. **Phase 3**: Ship `dhall/Defaults.dhall` with the binary
1. **Phase 4**: Document migration from JSON to Dhall for users
1. **Phase 5**: Deprecate JSON config support (future)

______________________________________________________________________

## Open Questions

1. **Embedding Defaults**: Should `Defaults.dhall` be embedded in the binary (via Template Haskell or file-embed) or loaded from a data directory?

1. **Config Caching**: Should we cache evaluated Dhall configs to avoid re-evaluation on every `/config` request?

1. **URL Fetching**: Should we enable Dhall's URL imports by default, or require explicit opt-in for security?

1. **Validation Timing**: Should config be validated at server startup (fail fast) or lazily on first access?

______________________________________________________________________

## References

- [Dhall Language](https://dhall-lang.org/)
- [Dhall Haskell Library](https://hackage.haskell.org/package/dhall)
- [TypeScript Config Schema](/home/luke/Projects/opencode/packages/opencode/src/config/config.ts)
- [Current Haskell Config Types](../src/Config/Types.hs)
