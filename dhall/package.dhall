-- package.dhall
-- Main entry point for weapon config
let Types = ./Types.dhall

in  { Types
    , Defaults = ./Defaults.dhall
    , -- Convenient re-exports
      Config = Types.Config
    , Keybinds = Types.Keybinds
    , Server = Types.Server
    , Agent = Types.Agent
    , Provider = Types.Provider
    , MCP = Types.MCP
    , Permission = Types.Permission
    , Theme = Types.Theme
    , Skill = Types.Skill
    }
