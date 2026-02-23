-- LSP.dhall
-- Language Server Protocol configuration
let LSPEntry =
      { Type =
          { command : List Text
          , args : Optional (List Text)
          , initializationOptions : Optional Text
          , rootUri : Optional Text
          }
      , default =
        { args = None (List Text)
        , initializationOptions = None Text
        , rootUri = None Text
        }
      }

let LSP =
      < Disabled | Config : List { mapKey : Text, mapValue : LSPEntry.Type } >

in  { LSPEntry, LSP }
