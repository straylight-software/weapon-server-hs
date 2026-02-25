{-# LANGUAGE OverloadedStrings #-}

{- | ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                            // weapon-server // api/validation
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Input validation helpers for API handlers. These ensure that requests with
invalid parameters (empty path captures, invalid JSON bodies, missing required
query parameters) return proper 4xx error responses.

= Overview

Servant's default behavior allows some invalid requests to pass through:

* Empty path captures (@\/session\/\/abort@) - Captured as empty string
* Invalid JSON bodies (@"oops"@ instead of object) - Accepted as 'Value'
* Missing required query params - Passed as 'Nothing'

This module provides validation helpers to reject these at the handler level.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-}
module Api.Validation (
    -- * Path Parameter Validation
    requireNonEmptyId,
    requireNonEmptyIdM,
    validateSessionId,
    validatePtyId,
    validateRequestId,
    validateProviderId,
    validateMessageId,

    -- * Query Parameter Validation
    requireQueryParam,
    validateEnumParam,
    validateFileTypeEnum,

    -- * Request Body Validation
    requireJsonObject,
    requireJsonObjectWith,

    -- * Validation Result Type
    ValidationError (..),
    throwValidation,
) where

import Data.Aeson (FromJSON, Object, Result (..), Value (..), fromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Servant (Handler, ServerError (..), err400, throwError)
import Server.ErrorFormatters (badRequestBody)

-- ═══════════════════════════════════════════════════════════════════════════
-- Validation Error Type
-- ═══════════════════════════════════════════════════════════════════════════

-- | Validation error with a descriptive message.
newtype ValidationError = ValidationError Text
    deriving (Eq, Show)

-- | Throw a validation error as a 400 Bad Request.
throwValidation :: Text -> Handler a
throwValidation msg =
    throwError $
        err400
            { errBody = badRequestBody msg
            , errHeaders = [("Content-Type", "application/json")]
            }

-- ═══════════════════════════════════════════════════════════════════════════
-- Path Parameter Validation
-- ═══════════════════════════════════════════════════════════════════════════

{- | Require a non-empty path parameter.

Returns 400 Bad Request if the ID is empty.

@
sessionGetHandler st sessionId = do
    sid <- requireNonEmptyId "sessionID" sessionId
    -- ... use sid ...
@
-}
requireNonEmptyId :: Text -> Text -> Handler Text
requireNonEmptyId paramName value
    | T.null value = throwValidation $ "Missing required path parameter: " <> paramName
    | otherwise = pure value

{- | Monadic version of 'requireNonEmptyId' for use in IO blocks.

Returns 400 Bad Request if the ID is empty, otherwise continues with the value.

@
sessionAbortHandler st sessionId mDir = do
    sid <- requireNonEmptyIdM "sessionID" sessionId
    liftIO $ do
        -- ... use sid ...
@
-}
requireNonEmptyIdM :: Text -> Text -> Handler Text
requireNonEmptyIdM = requireNonEmptyId

-- | Validate a session ID (non-empty).
validateSessionId :: Text -> Handler Text
validateSessionId = requireNonEmptyId "sessionID"

-- | Validate a PTY ID (non-empty).
validatePtyId :: Text -> Handler Text
validatePtyId = requireNonEmptyId "ptyID"

-- | Validate a request ID (non-empty, for questions/permissions).
validateRequestId :: Text -> Handler Text
validateRequestId = requireNonEmptyId "requestID"

-- | Validate a provider ID (non-empty).
validateProviderId :: Text -> Handler Text
validateProviderId = requireNonEmptyId "providerID"

-- | Validate a message ID (non-empty).
validateMessageId :: Text -> Handler Text
validateMessageId = requireNonEmptyId "messageID"

-- ═══════════════════════════════════════════════════════════════════════════
-- Query Parameter Validation
-- ═══════════════════════════════════════════════════════════════════════════

{- | Require a query parameter that was declared optional in the API type.

Returns 400 Bad Request if the parameter is missing.

@
findHandler st mQuery mPattern mDir = do
    query <- requireQueryParam "query" mQuery
    -- ... use query ...
@
-}
requireQueryParam :: Text -> Maybe a -> Handler a
requireQueryParam paramName Nothing =
    throwValidation $ "Missing required query parameter: " <> paramName
requireQueryParam _ (Just value) = pure value

{- | Validate that an optional query parameter is one of the allowed values.

Returns 400 Bad Request if the value is present but not in the allowed list.
Returns Nothing if the value is not present.

@
findFileHandler st mQuery mDir mDirs mType mLimit = do
    validType <- validateEnumParam "type" ["file", "directory"] mType
    -- ... use validType ...
@
-}
validateEnumParam :: Text -> [Text] -> Maybe Text -> Handler (Maybe Text)
validateEnumParam _ _ Nothing = pure Nothing
validateEnumParam paramName allowed (Just value)
    | value `elem` allowed = pure (Just value)
    | otherwise =
        throwValidation $
            "Invalid value for parameter '"
                <> paramName
                <> "': expected one of ["
                <> T.intercalate ", " allowed
                <> "]"

{- | Validate the 'type' parameter for find.files endpoint.

Must be either "file" or "directory" if present.
-}
validateFileTypeEnum :: Maybe Text -> Handler (Maybe Text)
validateFileTypeEnum = validateEnumParam "type" ["file", "directory"]

-- ═══════════════════════════════════════════════════════════════════════════
-- Request Body Validation
-- ═══════════════════════════════════════════════════════════════════════════

{- | Require the request body to be a JSON object.

Returns 400 Bad Request if the body is not an object (e.g., string, array, null).

@
ptyCreateHandler st input = do
    obj <- requireJsonObject input
    -- ... use obj ...
@
-}
requireJsonObject :: Value -> Handler Object
requireJsonObject (Object obj) = pure obj
requireJsonObject _ = throwValidation "Request body must be a JSON object"

{- | Require the request body to be a JSON object that parses to a specific type.

Returns 400 Bad Request if the body is not an object or fails to parse.

@
sessionInitHandler st sessionId input = do
    initReq <- requireJsonObjectWith @InitRequest input
    -- ... use initReq ...
@
-}
requireJsonObjectWith :: (FromJSON a) => Value -> Handler a
requireJsonObjectWith val@(Object _) =
    case fromJSON val of
        Success a -> pure a
        Error msg -> throwValidation $ "Invalid request body: " <> T.pack msg
requireJsonObjectWith _ = throwValidation "Request body must be a JSON object"
