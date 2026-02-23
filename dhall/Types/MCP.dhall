-- MCP.dhall
-- Model Context Protocol server configuration
let McpLocal =
      { Type =
          { command : List Text
          , environment : Optional (List { mapKey : Text, mapValue : Text })
          , enabled : Optional Bool
          , timeout : Optional Natural
          }
      , default =
        { environment = None (List { mapKey : Text, mapValue : Text })
        , enabled = Some True
        , timeout = Some 5000
        }
      }

let McpRemote =
      { Type =
          { url : Text
          , enabled : Optional Bool
          , headers : Optional (List { mapKey : Text, mapValue : Text })
          , timeout : Optional Natural
          }
      , default =
        { enabled = Some True
        , headers = None (List { mapKey : Text, mapValue : Text })
        , timeout = Some 5000
        }
      }

let MCP = < Local : McpLocal.Type | Remote : McpRemote.Type >

in  { McpLocal, McpRemote, MCP }
