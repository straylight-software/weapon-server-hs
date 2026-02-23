-- Agent.dhall
-- Agent configuration types
let Enums = ./Enums.dhall

let AgentColor = < Hex : Text | Theme : Text >

let AgentConfig =
      { Type =
          { model : Optional Text
          , maxTokens : Optional Natural
          , systemPrompt : Optional Text
          , tools : Optional (List Text)
          , mode : Optional Enums.AgentMode
          , color : Optional AgentColor
          , description : Optional Text
          }
      , default =
        { model = None Text
        , maxTokens = None Natural
        , systemPrompt = None Text
        , tools = None (List Text)
        , mode = None Enums.AgentMode
        , color = None AgentColor
        , description = None Text
        }
      }

in  { AgentColor, AgentConfig }
