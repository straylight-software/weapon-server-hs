-- Types.dhall
-- Re-exports all type definitions
let Enums = ./Types/Enums.dhall

let Keybinds = ./Types/Keybinds.dhall

let Server = ./Types/Server.dhall

let TUI = ./Types/TUI.dhall

let Permission = ./Types/Permission.dhall

let Agent = ./Types/Agent.dhall

let Provider = ./Types/Provider.dhall

let MCP = ./Types/MCP.dhall

let Formatter = ./Types/Formatter.dhall

let LSP = ./Types/LSP.dhall

let Theme = ./Types/Theme.dhall

let Skill = ./Types/Skill.dhall

let Command = ./Types/Command.dhall

let Watcher = ./Types/Watcher.dhall

let Compaction = ./Types/Compaction.dhall

let Experimental = ./Types/Experimental.dhall

let Enterprise = ./Types/Enterprise.dhall

let AutoUpdate = < Enabled | Disabled | Notify >

let Config =
      { Type =
          { -- Core settings
            model : Optional Text
          , systemPrompt : Optional Text
          , maxTokens : Optional Natural
          , logLevel : Optional Enums.LogLevel
          , -- Nested configs
            keybinds : Keybinds.Type
          , server : Server.Type
          , tui : TUI.Type
          , permission : Permission.Permission.Type
          , compaction : Compaction.Type
          , experimental : Experimental.Type
          , enterprise : Enterprise.Type
          , watcher : Watcher.Type
          , -- Map fields (name -> config)
            agent :
              Optional
                (List { mapKey : Text, mapValue : Agent.AgentConfig.Type })
          , provider :
              Optional
                ( List
                    { mapKey : Text, mapValue : Provider.ProviderConfig.Type }
                )
          , mcp : Optional (List { mapKey : Text, mapValue : MCP.MCP })
          , formatter : Optional Formatter.Formatter
          , lsp : Optional LSP.LSP
          , skill : Optional (List { mapKey : Text, mapValue : Skill.Type })
          , command : Optional (List { mapKey : Text, mapValue : Command.Type })
          , -- Theme settings
            theme : Optional Text
          , themes :
              Optional (List { mapKey : Text, mapValue : Theme.Theme.Type })
          , -- Share settings
            share : Optional Enums.ShareMode
          , -- Auto-update
            autoUpdate : Optional AutoUpdate
          , -- Disabled tools
            disabledTools : Optional (List Text)
          , -- Instrumentation
            instrumentation : Optional Bool
          }
      , default =
        { model = None Text
        , systemPrompt = None Text
        , maxTokens = None Natural
        , logLevel = Some Enums.LogLevel.INFO
        , keybinds = Keybinds.default
        , server = Server.default
        , tui = TUI.default
        , permission = Permission.Permission.default
        , compaction = Compaction.default
        , experimental = Experimental.default
        , enterprise = Enterprise.default
        , watcher = Watcher.default
        , agent =
            None (List { mapKey : Text, mapValue : Agent.AgentConfig.Type })
        , provider =
            None
              (List { mapKey : Text, mapValue : Provider.ProviderConfig.Type })
        , mcp = None (List { mapKey : Text, mapValue : MCP.MCP })
        , formatter = None Formatter.Formatter
        , lsp = None LSP.LSP
        , skill = None (List { mapKey : Text, mapValue : Skill.Type })
        , command = None (List { mapKey : Text, mapValue : Command.Type })
        , theme = None Text
        , themes = None (List { mapKey : Text, mapValue : Theme.Theme.Type })
        , share = None Enums.ShareMode
        , autoUpdate = Some AutoUpdate.Notify
        , disabledTools = None (List Text)
        , instrumentation = Some False
        }
      }

in  { -- Re-export all modules
      Enums
    , Keybinds
    , Server
    , TUI
    , Permission
    , Agent
    , Provider
    , MCP
    , Formatter
    , LSP
    , Theme
    , Skill
    , Command
    , Watcher
    , Compaction
    , Experimental
    , Enterprise
    , AutoUpdate
    , Config
    }
