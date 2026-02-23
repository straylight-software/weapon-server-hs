-- Skill.dhall
-- Skill definitions for custom commands
let Skill =
      { Type =
          { name : Text
          , description : Text
          , prompt : Text
          , tools : Optional (List Text)
          , agent : Optional Text
          , model : Optional Text
          }
      , default =
        { tools = None (List Text), agent = None Text, model = None Text }
      }

in  Skill
