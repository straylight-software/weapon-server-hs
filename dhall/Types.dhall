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

let Telemetry = ./Types/Telemetry.dhall

let AutoUpdate = < AutoUpdateEnabled | AutoUpdateDisabled | AutoUpdateNotify >

let Config =
      { Type =
          { -- Core settings
            cfgModel : Optional Text
          , cfgSystemPrompt : Optional Text
          , cfgMaxTokens : Optional Integer
          , cfgLogLevel : Optional Enums.LogLevel
          , -- Nested configs
            cfgKeybinds : Keybinds.Type
          , cfgServer : Server.Type
          , cfgTui : TUI.Type
          , cfgPermission : Permission.Permission.Type
          , cfgCompaction : Compaction.Type
          , cfgExperimental : Experimental.Type
          , cfgEnterprise : Enterprise.Type
          , cfgWatcher : Watcher.Type
          , -- Map fields (name -> config)
            cfgAgent :
              Optional
                (List { mapKey : Text, mapValue : Agent.AgentConfig.Type })
          , cfgProvider :
              Optional
                ( List
                    { mapKey : Text, mapValue : Provider.ProviderConfig.Type }
                )
          , cfgMcp : Optional (List { mapKey : Text, mapValue : MCP.MCP })
          , cfgFormatter : Optional Formatter.Formatter
          , cfgLsp : Optional LSP.LSP
          , cfgSkill : Optional (List { mapKey : Text, mapValue : Skill.Type })
          , cfgCommand :
              Optional (List { mapKey : Text, mapValue : Command.Type })
          , -- Theme settings
            cfgTheme : Optional Text
          , cfgThemes :
              Optional (List { mapKey : Text, mapValue : Theme.Theme.Type })
          , -- Share settings
            cfgShare : Optional Enums.ShareMode
          , -- Auto-update
            cfgAutoUpdate : Optional AutoUpdate
          , -- Telemetry
            cfgTelemetry : Telemetry.Telemetry.Type
          }
      , default =
        { cfgModel = None Text
        , cfgSystemPrompt = None Text
        , cfgMaxTokens = None Integer
        , cfgLogLevel = Some Enums.LogLevel.INFO
        , cfgKeybinds = Keybinds.default
        , cfgServer = Server.default
        , cfgTui = TUI.default
        , cfgPermission = Permission.Permission.default
        , cfgCompaction = Compaction.default
        , cfgExperimental = Experimental.default
        , cfgEnterprise = Enterprise.default
        , cfgWatcher = Watcher.default
        , cfgAgent =
            None (List { mapKey : Text, mapValue : Agent.AgentConfig.Type })
        , cfgProvider =
            None
              (List { mapKey : Text, mapValue : Provider.ProviderConfig.Type })
        , cfgMcp = None (List { mapKey : Text, mapValue : MCP.MCP })
        , cfgFormatter = None Formatter.Formatter
        , cfgLsp = None LSP.LSP
        , cfgSkill = None (List { mapKey : Text, mapValue : Skill.Type })
        , cfgCommand = None (List { mapKey : Text, mapValue : Command.Type })
        , cfgTheme = None Text
        , cfgThemes = None (List { mapKey : Text, mapValue : Theme.Theme.Type })
        , cfgShare = None Enums.ShareMode
        , cfgAutoUpdate = Some AutoUpdate.AutoUpdateNotify
        , cfgTelemetry = Telemetry.Telemetry.default
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
    , Telemetry
    , AutoUpdate
    , Config
    }
