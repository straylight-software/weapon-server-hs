-- Watcher.dhall
-- File watcher configuration
let Watcher =
      { Type = { ignore : Optional (List Text) }
      , default.ignore
        = Some
        [ "node_modules"
        , ".git"
        , "dist"
        , "build"
        , ".next"
        , "target"
        , "__pycache__"
        , ".venv"
        , "venv"
        ]
      }

in  Watcher
