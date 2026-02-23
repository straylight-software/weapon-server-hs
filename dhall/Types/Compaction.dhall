-- Compaction.dhall
-- Message compaction settings
let Compaction =
      { Type =
          { auto : Optional Bool
          , prune : Optional Bool
          , reserved : Optional Natural
          }
      , default =
        { auto = Some False, prune = Some False, reserved = Some 8192 }
      }

in  Compaction
