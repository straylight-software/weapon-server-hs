-- Server.dhall
-- Server configuration types
let Server =
      { Type =
          { hostname : Optional Text
          , port : Optional Natural
          , mdns : Optional Bool
          , cors : Optional Bool
          }
      , default =
        { hostname = Some "localhost"
        , port = Some 4096
        , mdns = Some False
        , cors = Some True
        }
      }

in  Server
