-- Command.dhall
-- Custom command definitions
let Command =
      { Type =
          { command : Text
          , description : Optional Text
          , environment : Optional (List { mapKey : Text, mapValue : Text })
          , workdir : Optional Text
          }
      , default =
        { description = None Text
        , environment = None (List { mapKey : Text, mapValue : Text })
        , workdir = None Text
        }
      }

in  Command
