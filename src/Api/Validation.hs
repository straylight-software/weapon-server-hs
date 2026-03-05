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
    validatePermissionId,
    validatePartId,
    validateProviderId,
    validateMessageId,

    -- * Request Body Field Validation
    validateBodyMessageId,
    validateBodyMessageIdRequired,
    validateBodySessionId,
    validateBodySessionIdRequired,

    -- * Query Parameter Validation
    requireQueryParam,
    requireNonEmptyTextParam,
    validateEnumParam,
    validateFileTypeEnum,
    validateBoolParam,
    validateIntParam,

    -- * Request Body Validation
    requireJsonObject,
    requireJsonObjectWith,

    -- * Validation Result Type
    ValidationError (..),
    throwValidation,
) where

import Data.Aeson (FromJSON, Object, Result (..), Value (..), fromJSON)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
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

-- | Validate a session ID (non-empty and matches pattern ^ses[a-zA-Z0-9_-]*$).
validateSessionId :: Text -> Handler Text
validateSessionId value
    | T.null value = throwValidation "Missing required path parameter: sessionID"
    | not (T.isPrefixOf "ses" value) = throwValidation "Invalid sessionID: must start with 'ses'"
    | not (T.all isValidIdChar (T.drop 3 value)) = throwValidation "Invalid sessionID: must contain only alphanumeric characters, underscores, and hyphens after 'ses'"
    | otherwise = pure value

-- | Validate a PTY ID (non-empty and matches pattern ^pty_.*).
validatePtyId :: Text -> Handler Text
validatePtyId value
    | T.null value = throwValidation "Missing required path parameter: ptyID"
    | not (T.isPrefixOf "pty_" value) = throwValidation "Invalid ptyID: must start with 'pty_'"
    | otherwise = pure value

-- | Validate a request ID (non-empty, alphanumeric with underscore and hyphen).
validateRequestId :: Text -> Handler Text
validateRequestId value
    | T.null value = throwValidation "Missing required path parameter: requestID"
    | not (T.all isValidIdChar value) = throwValidation "Invalid requestID: must contain only alphanumeric characters, underscores, and hyphens"
    | otherwise = pure value

-- | Validate a permission ID (non-empty, alphanumeric with underscore and hyphen).
validatePermissionId :: Text -> Handler Text
validatePermissionId value
    | T.null value = throwValidation "Missing required path parameter: permissionID"
    | not (T.all isValidIdChar value) = throwValidation "Invalid permissionID: must contain only alphanumeric characters, underscores, and hyphens"
    | otherwise = pure value

-- | Validate a part ID (non-empty, alphanumeric with underscore and hyphen).
validatePartId :: Text -> Handler Text
validatePartId value
    | T.null value = throwValidation "Missing required path parameter: partID"
    | not (T.all isValidIdChar value) = throwValidation "Invalid partID: must contain only alphanumeric characters, underscores, and hyphens"
    | otherwise = pure value

{- | Check if a character is valid for IDs (ASCII alphanumeric, underscore, hyphen).
Matches the OpenAPI pattern ^[a-zA-Z0-9_-]+$
-}
isValidIdChar :: Char -> Bool
isValidIdChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c == '_' || c == '-'

-- | Validate a provider ID (non-empty, lowercase alphanumeric and hyphens only, must start with letter or digit).
validateProviderId :: Text -> Handler Text
validateProviderId value
    | T.null value = throwValidation "Missing required path parameter: providerID"
    | not (isValidFirstChar (T.head value)) = throwValidation "Invalid providerID: must start with a lowercase letter or digit"
    | not (T.all isValidProviderChar value) = throwValidation "Invalid providerID: must contain only lowercase letters, digits, and hyphens"
    | otherwise = pure value
  where
    -- Valid provider ID characters: lowercase letters, digits, and hyphens
    isValidProviderChar c = isAsciiLower c || isDigit c || c == '-'
    -- First character must be letter or digit (not hyphen)
    isValidFirstChar c = isAsciiLower c || isDigit c

-- | Validate a message ID (non-empty and matches pattern ^msg[a-zA-Z0-9_-]*$).
validateMessageId :: Text -> Handler Text
validateMessageId value
    | T.null value = throwValidation "Missing required path parameter: messageID"
    | not (T.isPrefixOf "msg" value) = throwValidation "Invalid messageID: must start with 'msg'"
    | not (T.all isValidIdChar (T.drop 3 value)) = throwValidation "Invalid messageID: must contain only alphanumeric characters, underscores, and hyphens after 'msg'"
    | otherwise = pure value

-- ═══════════════════════════════════════════════════════════════════════════
-- Request Body Field Validation
-- ═══════════════════════════════════════════════════════════════════════════

-- | Validate an optional messageID in request body (pattern ^msg[a-zA-Z0-9_-]*$).
validateBodyMessageId :: Maybe Text -> Handler (Maybe Text)
validateBodyMessageId Nothing = pure Nothing
validateBodyMessageId (Just value)
    | T.null value = pure Nothing -- Empty string treated as absent
    | not (T.isPrefixOf "msg" value) = throwValidation "Invalid messageID: must start with 'msg'"
    | not (T.all isValidIdChar (T.drop 3 value)) = throwValidation "Invalid messageID: must contain only alphanumeric characters, underscores, and hyphens after 'msg'"
    | otherwise = pure (Just value)

-- | Validate a required messageID in request body (pattern ^msg[a-zA-Z0-9_-]*$).
validateBodyMessageIdRequired :: Text -> Handler Text
validateBodyMessageIdRequired value
    | T.null value = throwValidation "Missing required field: messageID"
    | not (T.isPrefixOf "msg" value) = throwValidation "Invalid messageID: must start with 'msg'"
    | not (T.all isValidIdChar (T.drop 3 value)) = throwValidation "Invalid messageID: must contain only alphanumeric characters, underscores, and hyphens after 'msg'"
    | otherwise = pure value

-- | Validate an optional sessionID/parentID in request body (pattern ^ses[a-zA-Z0-9_-]*$).
validateBodySessionId :: Maybe Text -> Handler (Maybe Text)
validateBodySessionId Nothing = pure Nothing
validateBodySessionId (Just value)
    | T.null value = pure Nothing -- Empty string treated as absent
    | not (T.isPrefixOf "ses" value) = throwValidation "Invalid sessionID: must start with 'ses'"
    | not (T.all isValidIdChar (T.drop 3 value)) = throwValidation "Invalid sessionID: must contain only alphanumeric characters, underscores, and hyphens after 'ses'"
    | otherwise = pure (Just value)

-- | Validate a required sessionID in request body (pattern ^ses[a-zA-Z0-9_-]*$).
validateBodySessionIdRequired :: Text -> Handler Text
validateBodySessionIdRequired value
    | T.null value = throwValidation "Missing required field: sessionID"
    | not (T.isPrefixOf "ses" value) = throwValidation "Invalid sessionID: must start with 'ses'"
    | not (T.all isValidIdChar (T.drop 3 value)) = throwValidation "Invalid sessionID: must contain only alphanumeric characters, underscores, and hyphens after 'ses'"
    | otherwise = pure value

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

{- | Require a non-empty text query parameter.

Returns 400 Bad Request if the parameter is missing or empty.
This is appropriate for search/pattern parameters where an empty
string would match everything or cause unbounded work.
-}
requireNonEmptyTextParam :: Text -> Maybe Text -> Handler Text
requireNonEmptyTextParam paramName Nothing =
    throwValidation $ "Missing required query parameter: " <> paramName
requireNonEmptyTextParam paramName (Just value)
    | T.null value = throwValidation $ "Query parameter '" <> paramName <> "' must not be empty"
    | otherwise = pure value

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
validateEnumParam _ _ (Just "") = pure Nothing -- Treat empty string as absent
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

{- | Validate an optional boolean query parameter.

Accepts "true" or "false" (case-sensitive). Empty strings are treated as absent.
Any other value returns a 400 error.
-}
validateBoolParam :: Text -> Maybe Text -> Handler (Maybe Bool)
validateBoolParam _ Nothing = pure Nothing
validateBoolParam _ (Just "") = pure Nothing
validateBoolParam _ (Just "true") = pure (Just True)
validateBoolParam _ (Just "false") = pure (Just False)
validateBoolParam paramName (Just val) =
    throwValidation $
        "Error parsing query parameter "
            <> paramName
            <> " failed: could not parse: `"
            <> val
            <> "'"

{- | Validate an optional integer query parameter.

Empty strings are treated as absent. Non-integer values return a 400 error.
-}
validateIntParam :: Text -> Maybe Text -> Handler (Maybe Int)
validateIntParam _ Nothing = pure Nothing
validateIntParam _ (Just "") = pure Nothing
validateIntParam paramName (Just val) =
    case reads (T.unpack val) of
        [(n, "")] -> pure (Just n)
        _other ->
            throwValidation $
                "Error parsing query parameter "
                    <> paramName
                    <> " failed: could not parse: `"
                    <> val
                    <> "'"

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
