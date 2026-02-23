-- Enterprise.dhall
-- Enterprise configuration
let Enterprise =
      { Type = { url : Optional Text, apiKey : Optional Text }
      , default = { url = None Text, apiKey = None Text }
      }

in  Enterprise
