-- Formatter.dhall
-- Code formatter configuration
let FormatterEntry =
      { Type = { command : List Text, timeout : Optional Natural }
      , default.timeout = Some 5000
      }

let Formatter =
      < Disabled
      | Config : List { mapKey : Text, mapValue : FormatterEntry.Type }
      >

in  { FormatterEntry, Formatter }
