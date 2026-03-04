{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Server.ErrorFormatters
Description : JSON error formatters for Servant API responses

This module provides consistent JSON error formatting for the OpenCode API server.
All HTTP error responses are formatted as JSON objects with a standardized structure
to ensure consistent error handling on the client side.

== Error Response Structure

All errors follow this JSON structure:

@
{
  "name": "ErrorTypeName",
  "data": {
    "message": "Human-readable error description"
  }
}
@

== Usage

The 'errorFormattersContext' should be passed to Servant's 'serveWithContext':

@
app :: Application
app = serveWithContext api errorFormattersContext server
@
-}
module Server.ErrorFormatters (
    -- * Servant Context
    errorFormattersContext,

    -- * Error Formatters
    jsonErrorFormatters,
    notFoundJson,
    badRequestJson,
    unauthorizedJson,
    forbiddenJson,
    internalErrorJson,

    -- * Pure Error Body Builders

    {- | These functions construct JSON error bodies without IO,
    making them easy to test and compose.
    -}
    errorBody,
    notFoundBody,
    badRequestBody,
    unauthorizedBody,
    forbiddenBody,
    internalErrorBody,

    -- * Handler Error Helpers

    {- | These functions return ServerError values suitable for use with
    'throwError' in Servant handlers, with properly formatted JSON bodies.
    -}
    notFoundError,
    notFoundErrorWithMsg,
    badRequestError,

    -- * Generic Error Response Helpers
    jsonError,
    errorResponse,

    -- * Re-exports for convenience
    ServerError,
    err400,
    err401,
    err403,
    err404,
    err500,
) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack)
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types qualified as HTTP
import Network.Wai (Request)
import Servant

-- ═══════════════════════════════════════════════════════════════════════════
-- Servant Context
-- ═══════════════════════════════════════════════════════════════════════════

{- | Servant context with JSON error formatters.

Use this context when serving the API to ensure all error responses
are formatted as JSON:

@
app :: Application
app = serveWithContext api errorFormattersContext server
@
-}
errorFormattersContext :: Context '[ErrorFormatters]
errorFormattersContext = jsonErrorFormatters :. EmptyContext

-- ═══════════════════════════════════════════════════════════════════════════
-- Error Formatters
-- ═══════════════════════════════════════════════════════════════════════════

{- | Custom error formatters that return JSON instead of plain text.

Overrides all Servant error formatters to ensure consistent JSON responses:

* 'notFoundErrorFormatter' - Returns JSON for 404 errors
* 'bodyParserErrorFormatter' - Returns JSON for request body parsing errors
* 'urlParseErrorFormatter' - Returns JSON for URL/query parameter parsing errors
-}
jsonErrorFormatters :: ErrorFormatters
jsonErrorFormatters =
    defaultErrorFormatters
        { notFoundErrorFormatter = notFoundJson
        , bodyParserErrorFormatter = bodyParserJson
        , urlParseErrorFormatter = urlParseJson
        }

{- | Format 404 Not Found errors as JSON.

Returns a JSON object with the structure:

@
{
  "name": "NotFoundError",
  "data": { "message": "Not found" }
}
@
-}
notFoundJson :: NotFoundErrorFormatter
notFoundJson _req =
    err404
        { errHeaders = jsonContentType
        , errBody = notFoundBody
        }

{- | Format request body parsing errors as JSON.

Handles errors from malformed JSON bodies, missing required fields, etc.
-}
bodyParserJson :: ErrorFormatter
bodyParserJson _typeRep _req msg =
    err400
        { errHeaders = jsonContentType
        , errBody = badRequestBody (pack msg)
        }

{- | Format URL/query parameter parsing errors as JSON.

Handles errors from invalid query parameters, missing required params, etc.
-}
urlParseJson :: ErrorFormatter
urlParseJson _typeRep _req msg =
    err400
        { errHeaders = jsonContentType
        , errBody = badRequestBody (pack msg)
        }

{- | Format 400 Bad Request errors as JSON.

@since 0.2.0
-}
badRequestJson :: Text -> Request -> ServerError
badRequestJson msg _req =
    err400
        { errHeaders = jsonContentType
        , errBody = badRequestBody msg
        }

{- | Format 401 Unauthorized errors as JSON.

@since 0.2.0
-}
unauthorizedJson :: Text -> Request -> ServerError
unauthorizedJson msg _req =
    err401
        { errHeaders = jsonContentType
        , errBody = unauthorizedBody msg
        }

{- | Format 403 Forbidden errors as JSON.

@since 0.2.0
-}
forbiddenJson :: Text -> Request -> ServerError
forbiddenJson msg _req =
    err403
        { errHeaders = jsonContentType
        , errBody = forbiddenBody msg
        }

{- | Format 500 Internal Server Error as JSON.

@since 0.2.0
-}
internalErrorJson :: Text -> Request -> ServerError
internalErrorJson msg _req =
    err500
        { errHeaders = jsonContentType
        , errBody = internalErrorBody msg
        }

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Error Body Builders
-- ═══════════════════════════════════════════════════════════════════════════

{- | Build a JSON error body with the standard structure.

This is the core pure function that constructs error JSON.
All other error body builders delegate to this function.

==== __Examples__

>>> errorBody "ValidationError" "Invalid email format"
"{\"name\":\"ValidationError\",\"data\":{\"message\":\"Invalid email format\"}}"

>>> errorBody "RateLimitError" "Too many requests"
"{\"name\":\"RateLimitError\",\"data\":{\"message\":\"Too many requests\"}}"
-}
errorBody :: Text -> Text -> ByteString
errorBody errorName message =
    encode $
        object
            [ "name" .= errorName
            , "data" .= object ["message" .= message]
            ]

{- | Build a 404 Not Found error body.

Uses the default "Not found" message.

==== __Examples__

>>> notFoundBody
"{\"name\":\"NotFoundError\",\"data\":{\"message\":\"Not found\"}}"
-}
notFoundBody :: ByteString
notFoundBody = errorBody "NotFoundError" "Not found"

{- | Build a 400 Bad Request error body with a custom message.

==== __Examples__

>>> badRequestBody "Missing required field: name"
"{\"name\":\"BadRequestError\",\"data\":{\"message\":\"Missing required field: name\"}}"
-}
badRequestBody :: Text -> ByteString
badRequestBody = errorBody "BadRequestError"

{- | Build a 401 Unauthorized error body with a custom message.

==== __Examples__

>>> unauthorizedBody "Invalid API key"
"{\"name\":\"UnauthorizedError\",\"data\":{\"message\":\"Invalid API key\"}}"
-}
unauthorizedBody :: Text -> ByteString
unauthorizedBody = errorBody "UnauthorizedError"

{- | Build a 403 Forbidden error body with a custom message.

==== __Examples__

>>> forbiddenBody "Access denied to this resource"
"{\"name\":\"ForbiddenError\",\"data\":{\"message\":\"Access denied to this resource\"}}"
-}
forbiddenBody :: Text -> ByteString
forbiddenBody = errorBody "ForbiddenError"

{- | Build a 500 Internal Server Error body with a custom message.

==== __Examples__

>>> internalErrorBody "Database connection failed"
"{\"name\":\"InternalServerError\",\"data\":{\"message\":\"Database connection failed\"}}"
-}
internalErrorBody :: Text -> ByteString
internalErrorBody = errorBody "InternalServerError"

-- ═══════════════════════════════════════════════════════════════════════════
-- Generic Error Response Helpers
-- ═══════════════════════════════════════════════════════════════════════════

{- | Create a JSON error response value.

This is a simpler format for inline error responses where the
full error structure is not needed.

==== __Examples__

>>> errorResponse "File not found"
Object (fromList [("error",String "File not found")])
-}
errorResponse :: Text -> Value
errorResponse msg = object ["error" .= msg]

{- | Create a custom ServerError with JSON body and additional fields.

This allows constructing server errors with arbitrary JSON data
beyond the standard error structure.

==== __Examples__

@
let customErr = jsonError err422 [("field", "email"), ("reason", "invalid format")]
@
-}
jsonError :: ServerError -> [Pair] -> ServerError
jsonError baseErr pairs =
    baseErr
        { errHeaders = jsonContentType
        , errBody = encode (object pairs)
        }

-- ═══════════════════════════════════════════════════════════════════════════
-- Handler Error Helpers
-- ═══════════════════════════════════════════════════════════════════════════

{- | Create a 404 Not Found error for use with 'throwError'.

Returns a ServerError with JSON body matching the OpenAPI NotFoundError schema.

==== __Examples__

@
handler :: Handler Session
handler = do
    msession <- getSession sid
    case msession of
        Nothing -> throwError notFoundError
        Just s  -> return s
@
-}
notFoundError :: ServerError
notFoundError =
    err404
        { errHeaders = jsonContentType
        , errBody = notFoundBody
        }

{- | Create a 404 Not Found error with a custom message.

==== __Examples__

@
handler :: Handler Session
handler = do
    msession <- getSession sid
    case msession of
        Nothing -> throwError $ notFoundErrorWithMsg "Session not found"
        Just s  -> return s
@
-}
notFoundErrorWithMsg :: Text -> ServerError
notFoundErrorWithMsg msg =
    err404
        { errHeaders = jsonContentType
        , errBody = errorBody "NotFoundError" msg
        }

{- | Create a 400 Bad Request error with a custom message.

==== __Examples__

@
handler :: Handler ()
handler = do
    when (isInvalid input) $
        throwError $ badRequestError "Invalid input format"
@
-}
badRequestError :: Text -> ServerError
badRequestError msg =
    err400
        { errHeaders = jsonContentType
        , errBody = badRequestBody msg
        }

-- ═══════════════════════════════════════════════════════════════════════════
-- Internal Helpers
-- ═══════════════════════════════════════════════════════════════════════════

-- | Standard JSON content type header.
jsonContentType :: [HTTP.Header]
jsonContentType = [(hContentType, "application/json")]
