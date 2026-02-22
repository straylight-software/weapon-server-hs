{-# LANGUAGE OverloadedStrings #-}

module Server.ErrorFormatters
    ( errorFormattersContext
    ) where

import Data.Aeson (encode, object, (.=))
import Data.Text (Text)
import Network.HTTP.Types (hContentType)
import Network.Wai (Request)
import Servant

errorFormattersContext :: Context '[ErrorFormatters]
errorFormattersContext = jsonErrorFormatters :. EmptyContext

jsonErrorFormatters :: ErrorFormatters
jsonErrorFormatters =
    defaultErrorFormatters
        { notFoundErrorFormatter = notFoundJson
        }

notFoundJson :: Request -> ServerError
notFoundJson _req =
    err404
        { errHeaders = [(hContentType, "application/json")]
        , errBody = encode $ object
            [ "name" .= ("NotFoundError" :: Text)
            , "data" .= object ["message" .= ("Not found" :: Text)]
            ]
        }
