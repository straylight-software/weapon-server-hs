-- Provider.dhall
-- LLM Provider configuration
let Enums = ./Enums.dhall

let ProviderTimeout = < Disabled | Seconds : Natural >

let ProviderModel =
      { Type =
          { id : Text
          , name : Optional Text
          , contextLength : Optional Natural
          , maxOutput : Optional Natural
          , status : Optional Enums.ModelStatus
          , hidden : Optional Bool
          }
      , default =
        { name = None Text
        , contextLength = None Natural
        , maxOutput = None Natural
        , status = None Enums.ModelStatus
        , hidden = None Bool
        }
      }

let ProviderOptions =
      { Type = { thinking : Optional Bool, version : Optional Text }
      , default = { thinking = None Bool, version = None Text }
      }

let ProviderConfig =
      { Type =
          { api : Optional Text
          , models : Optional (List ProviderModel.Type)
          , options : Optional ProviderOptions.Type
          , timeout : Optional ProviderTimeout
          , disabled : Optional Bool
          , name : Optional Text
          }
      , default =
        { api = None Text
        , models = None (List ProviderModel.Type)
        , options = None ProviderOptions.Type
        , timeout = None ProviderTimeout
        , disabled = None Bool
        , name = None Text
        }
      }

in  { ProviderTimeout, ProviderModel, ProviderOptions, ProviderConfig }
