-- Permission.dhall
-- Permission rules for tool access
let Enums = ./Enums.dhall

let PermissionAction = Enums.PermissionAction

let PermissionRule =
      < Action : PermissionAction
      | ByPath : List { mapKey : Text, mapValue : PermissionAction }
      >

let Permission =
      { Type =
          { read : Optional PermissionRule
          , edit : Optional PermissionRule
          , glob : Optional PermissionRule
          , grep : Optional PermissionRule
          , list : Optional PermissionRule
          , bash : Optional PermissionRule
          , task : Optional PermissionRule
          , external_directory : Optional PermissionRule
          , todowrite : Optional PermissionAction
          , todoread : Optional PermissionAction
          , question : Optional PermissionAction
          , webfetch : Optional PermissionAction
          , websearch : Optional PermissionAction
          , codesearch : Optional PermissionAction
          , lsp : Optional PermissionRule
          , doom_loop : Optional PermissionAction
          , skill : Optional PermissionRule
          }
      , default =
        { read = None PermissionRule
        , edit = None PermissionRule
        , glob = None PermissionRule
        , grep = None PermissionRule
        , list = None PermissionRule
        , bash = None PermissionRule
        , task = None PermissionRule
        , external_directory = None PermissionRule
        , todowrite = None PermissionAction
        , todoread = None PermissionAction
        , question = None PermissionAction
        , webfetch = None PermissionAction
        , websearch = None PermissionAction
        , codesearch = None PermissionAction
        , lsp = None PermissionRule
        , doom_loop = None PermissionAction
        , skill = None PermissionRule
        }
      }

in  { PermissionAction, PermissionRule, Permission }
