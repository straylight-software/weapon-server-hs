-- TUI.dhall
-- Terminal User Interface configuration
let Enums = ./Enums.dhall

let TUI =
      { Type =
          { scroll_speed : Optional Natural
          , scroll_acceleration : Optional Natural
          , diff_style : Optional Enums.DiffStyle
          }
      , default =
        { scroll_speed = Some 1
        , scroll_acceleration = Some 1
        , diff_style = Some Enums.DiffStyle.auto
        }
      }

in  TUI
