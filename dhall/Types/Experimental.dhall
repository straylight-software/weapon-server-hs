-- Experimental.dhall
-- Experimental feature flags
let Experimental =
      { Type =
          { thinking : Optional Bool
          , worktree : Optional Bool
          , fileCache : Optional Bool
          , parallelTools : Optional Bool
          , streaming : Optional Bool
          }
      , default =
        { thinking = Some False
        , worktree = Some False
        , fileCache = Some True
        , parallelTools = Some True
        , streaming = Some True
        }
      }

in  Experimental
