-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                 // weapon-server // middleware
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- WAI middleware for the Weapon server.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Middleware (
    -- * Existing middleware
    supplyEmptyBody,
    requestLogger,
    rejectEmptyPathSegments,

    -- * RFC 9110 compliance
    addAllowHeader,

    -- * Strict request validation
    rejectDuplicateQueryParams,
    rejectUnknownQueryParams,
    limitJsonDepth,
    rejectInvalidCharset,
    rejectInvalidContentType,
    rejectNullBytePaths,
    rejectDoubleEncodedPaths,
    requireContentType,
    rejectHeadMethod,
    rejectUnsupportedMethods,
    rejectMethodMismatch,

    -- * Route information
    RouteSpec (..),
    routeSpecs,
) where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Katip qualified
import Log (Logger, logMsg)
import Network.HTTP.Types (
    Header,
    HeaderName,
    Method,
    hContentType,
    methodDelete,
    methodGet,
    methodPatch,
    methodPost,
    methodPut,
    status400,
    status405,
    status415,
 )
import Network.Wai (
    Middleware,
    Request (..),
    RequestBodyLength (..),
    Response,
    mapResponseHeaders,
    pathInfo,
    queryString,
    requestBodyLength,
    requestMethod,
    responseLBS,
    responseStatus,
    setRequestBodyChunks,
    strictRequestBody,
 )

-- | Allow header name (not exported by http-types)
hAllow :: HeaderName
hAllow = "Allow"

{- | Check if a method is in our supported set (GET, POST, PUT, PATCH, DELETE)
Used to skip validation for unsupported methods so Servant can return 405.
-}
isSupportedMethod :: Method -> Bool
isSupportedMethod m = m `elem` [methodGet, methodPost, methodPut, methodPatch, methodDelete]

-- | Middleware to log all incoming requests
requestLogger :: Logger -> Middleware
requestLogger logger app req callback = do
    let method = TE.decodeUtf8 (requestMethod req)
        path = "/" <> T.intercalate "/" (pathInfo req)
    logMsg logger Katip.InfoS $ "HTTP " <> method <> " " <> path
    app req callback

{- | Middleware to supply an empty JSON body when no body is provided.

For POST, PUT, PATCH, and DELETE requests without a body, this middleware
supplies an empty JSON object `{}` and sets Content-Type to application/json
to avoid 415 Unsupported Media Type errors from Servant.
-}
supplyEmptyBody :: Middleware
supplyEmptyBody app req callback
    | needsBody && hasNoBody = do
        -- Create a mutable ref to track if body was read
        bodyReadRef <- newIORef False
        let emptyJsonBody = "{}"
            bodyChunks = do
                wasRead <- readIORef bodyReadRef
                if wasRead
                    then pure ""
                    else do
                        writeIORef bodyReadRef True
                        pure emptyJsonBody
            -- Add Content-Type header if not present
            headers' =
                if hasContentType
                    then requestHeaders req
                    else (hContentType, "application/json") : requestHeaders req
            req' = (setRequestBodyChunks bodyChunks req){requestHeaders = headers'}
        app req' callback
    | otherwise = app req callback
  where
    method = requestMethod req
    needsBody = method `elem` [methodPost, methodPut, methodPatch, methodDelete]
    -- Only supply body when Content-Length is explicitly 0 or missing entirely
    -- ChunkedBody means body exists but size unknown - don't supply default
    hasNoBody = case requestBodyLength req of
        KnownLength 0 -> True
        ChunkedBody -> False -- Chunked encoding implies body exists
        KnownLength _len -> False
    hasContentType = any (\(h, _) -> h == hContentType) (requestHeaders req)

{- | Middleware to reject requests with empty path segments.

Paths like @\/session\/@ or @\/pty\/@ have a trailing slash that creates an
empty path segment. These should return 400 Bad Request rather than being
routed to list endpoints.

This handles the case where a fuzzer removes a required path parameter like
@sessionID@ from @\/session\/{sessionID}@, resulting in @\/session\/@.

Note: We skip validation for unsupported methods so Servant can return 405.
-}
rejectEmptyPathSegments :: Middleware
rejectEmptyPathSegments app req callback
    -- Skip validation for unsupported methods - let Servant return 405
    | not (isSupportedMethod (requestMethod req)) = app req callback
    | shouldReject =
        callback $
            responseLBS
                status400
                [(hContentType, "application/json")]
                (LBS.fromStrict "{\"name\":\"BadRequestError\",\"data\":{\"message\":\"Invalid path: empty path segment\"}}")
    | otherwise = app req callback
  where
    -- WAI's pathInfo includes empty segments for trailing slashes
    -- e.g., "/session/" -> ["session", ""]
    --       "/session" -> ["session"]
    -- We reject paths that have a trailing empty segment on routes that expect a capture
    segments = pathInfo req

    -- Check if the path has a trailing empty segment (from trailing slash)
    -- Use explicit pattern matching to avoid partial 'last' (STAN-0004)
    hasTrailingEmpty = go segments
      where
        go [] = False
        go [x] = T.null x
        go (_x : xs) = go xs

    -- Get the non-empty segments for pattern matching
    nonEmptySegments = filter (not . T.null) segments

    -- Paths where a trailing slash indicates a missing required capture
    shouldReject = hasTrailingEmpty && isCapturePath nonEmptySegments

    isCapturePath :: [T.Text] -> Bool
    isCapturePath ["session"] = True -- /session/ missing sessionID
    isCapturePath ["pty"] = True -- /pty/ missing ptyID
    isCapturePath ["session", _sessionId, "message"] = True -- /session/x/message/ missing messageID
    isCapturePath _otherPaths = False

-- ═══════════════════════════════════════════════════════════════════════════
-- RFC 9110 Compliance: Allow Header on 405
-- ═══════════════════════════════════════════════════════════════════════════

{- | Middleware to add Allow header to 405 Method Not Allowed responses.

RFC 9110 Section 15.5.6 requires the Allow header field in 405 responses.
This middleware intercepts responses and adds the Allow header based on
the route specification.
-}
addAllowHeader :: Middleware
addAllowHeader app req callback =
    app req $ \resp ->
        if responseStatus resp == status405
            then callback $ addAllowToResponse (pathInfo req) resp
            else callback resp
  where
    addAllowToResponse :: [T.Text] -> Middleware.Response -> Middleware.Response
    addAllowToResponse pathSegs = mapResponseHeaders (addAllowHeaderIfMissing pathSegs)

    addAllowHeaderIfMissing :: [T.Text] -> [Header] -> [Header]
    addAllowHeaderIfMissing pathSegs headers
        | any (\(h, _) -> h == hAllow) headers = headers
        | otherwise = (hAllow, allowValue pathSegs) : headers

    -- Determine allowed methods for a path
    allowValue :: [T.Text] -> BS.ByteString
    allowValue pathSegs =
        let methods = lookupAllowedMethods pathSegs
         in BS.intercalate ", " methods

    -- Look up allowed methods from route specs, falling back to common methods
    lookupAllowedMethods :: [T.Text] -> [Method]
    lookupAllowedMethods pathSegs =
        case findMatchingRoute pathSegs of
            Just spec -> rsMethods spec
            -- Fallback: list common methods (Servant will still reject unsupported ones)
            Nothing -> [methodGet, methodPost, methodPut, methodPatch, methodDelete]

-- | Response type alias for clarity
type Response = Network.Wai.Response

-- ═══════════════════════════════════════════════════════════════════════════
-- Strict Query Parameter Validation
-- ═══════════════════════════════════════════════════════════════════════════

{- | Middleware to reject requests with duplicate query parameters.

Parameter pollution attacks send the same parameter multiple times.
This middleware rejects such requests with 400 Bad Request.

RFC 3986 doesn't forbid duplicate params, but for security we require uniqueness.

Note: We skip validation for unsupported methods so Servant can return 405.
-}
rejectDuplicateQueryParams :: Middleware
rejectDuplicateQueryParams app req callback
    -- Skip validation for unsupported methods - let Servant return 405
    | not (isSupportedMethod (requestMethod req)) = app req callback
    | hasDuplicates =
        callback $
            responseLBS
                status400
                [(hContentType, "application/json")]
                (errorJson "Duplicate query parameter" $ "Parameter appears multiple times: " <> duplicateParam)
    | otherwise = app req callback
  where
    params = queryString req
    paramNames = fmap fst params

    -- Check for duplicates in O(n) by tracking seen items
    -- Returns Just duplicateName if found, Nothing otherwise
    findDuplicate :: [BS.ByteString] -> Maybe BS.ByteString
    findDuplicate = go Set.empty
      where
        go _seen [] = Nothing
        go seen (x : rest)
            | x `Set.member` seen = Just x
            | otherwise = go (Set.insert x seen) rest

    hasDuplicates = case findDuplicate paramNames of
        Just _dup -> True
        Nothing -> False

    -- Find the first duplicate parameter name
    duplicateParam :: LBS.ByteString
    duplicateParam = maybe "unknown" LBS.fromStrict (findDuplicate paramNames)

{- | Middleware to reject requests with unknown query parameters.

Strict API validation requires rejecting parameters not defined in the
OpenAPI specification. This prevents typos and injection attempts.

Note: We skip validation for methods not in our supported set (GET, POST, PUT, PATCH, DELETE)
so that Servant can properly return 405 Method Not Allowed for unsupported methods like TRACE.
-}
rejectUnknownQueryParams :: Middleware
rejectUnknownQueryParams app req callback
    -- Skip validation for unsupported methods - let Servant return 405
    | not (isSupportedMethod (requestMethod req)) = app req callback
    | otherwise =
        case lookupAllowedParams (pathInfo req) (requestMethod req) of
            -- Method not allowed for this route - skip validation, let Servant return 405
            Nothing -> app req callback
            Just allowedParams ->
                let params = queryString req
                    actualParams = Set.fromList $ fmap fst params
                    unknownParams = Set.toList $ actualParams `Set.difference` allowedParams
                 in case unknownParams of
                        (p : _) ->
                            callback $
                                responseLBS
                                    status400
                                    [(hContentType, "application/json")]
                                    (errorJson "Unknown query parameter" $ "Unexpected parameter: " <> LBS.fromStrict p)
                        [] -> app req callback
  where
    lookupAllowedParams :: [T.Text] -> Method -> Maybe (Set.Set BS.ByteString)
    lookupAllowedParams pathSegs method =
        case findMatchingRoute pathSegs of
            Just spec ->
                -- If method not in route's allowed methods, return Nothing to skip validation
                if method `elem` rsMethods spec
                    then Just $ Map.findWithDefault Set.empty method (rsQueryParams spec)
                    else Nothing -- Let Servant return 405
                    -- Route not found - skip validation, let Servant return 404
            Nothing -> Nothing

-- ═══════════════════════════════════════════════════════════════════════════
-- JSON Depth Limiting
-- ═══════════════════════════════════════════════════════════════════════════

{- | Middleware to limit JSON nesting depth.

Deeply nested JSON can cause stack overflows or excessive memory usage.
This middleware rejects requests with JSON bodies exceeding the depth limit.

Default depth limit: 50 levels
-}
limitJsonDepth :: Middleware
limitJsonDepth = limitJsonDepthN 50

-- | Middleware to limit JSON nesting depth with configurable limit.
limitJsonDepthN :: Int -> Middleware
limitJsonDepthN maxDepth app req callback
    -- Skip for unsupported methods - let Servant return 405
    | not (isSupportedMethod method) = app req callback
    | not needsBodyCheck = app req callback
    | otherwise = do
        body <- strictRequestBody req
        -- Always reconstruct the request with the body we read,
        -- since strictRequestBody consumes the stream
        let reconstructRequest = do
                bodyReadRef <- newIORef False
                let bodyChunks = do
                        wasRead <- readIORef bodyReadRef
                        if wasRead
                            then pure ""
                            else do
                                writeIORef bodyReadRef True
                                pure $ LBS.toStrict body
                pure $ setRequestBodyChunks bodyChunks req

        case A.decode body of
            Nothing -> do
                -- Body isn't valid JSON - let Servant handle parse errors
                req' <- reconstructRequest
                app req' callback
            Just (val :: A.Value) ->
                if jsonDepth val > maxDepth
                    then
                        callback $
                            responseLBS
                                status400
                                [(hContentType, "application/json")]
                                (errorJson "JSON too deeply nested" $ "Maximum nesting depth is " <> LBS.fromStrict (TE.encodeUtf8 $ T.pack $ show maxDepth))
                    else do
                        req' <- reconstructRequest
                        app req' callback
  where
    method = requestMethod req
    needsBodyCheck = method `elem` [methodPost, methodPut, methodPatch]

    -- Calculate maximum depth of a JSON value
    -- Use foldr max 0 instead of maximum to avoid partial function warning
    jsonDepth :: A.Value -> Int
    jsonDepth (A.Object obj) =
        let childDepths = fmap jsonDepth (KM.elems obj)
         in 1 + foldr max 0 childDepths
    jsonDepth (A.Array arr) =
        let childDepths = fmap jsonDepth (toList arr)
         in 1 + foldr max 0 childDepths
    jsonDepth _primitive = 1

    toList :: A.Array -> [A.Value]
    toList = foldr (:) []

-- ═══════════════════════════════════════════════════════════════════════════
-- Charset Validation
-- ═══════════════════════════════════════════════════════════════════════════

{- | Middleware to reject requests with invalid charset in Content-Type.

Only UTF-8 is valid for JSON. This rejects requests with explicit non-UTF-8
charset like charset=utf-16 or charset=iso-8859-1.

Note: We skip validation for unsupported methods so Servant can return 405.
-}
rejectInvalidCharset :: Middleware
rejectInvalidCharset app req callback
    -- Skip for unsupported methods - let Servant return 405
    | not (isSupportedMethod method) = app req callback
    | not hasContentType = app req callback
    | otherwise = case invalidCharset of
        Just charset ->
            callback $
                responseLBS
                    status400
                    [(hContentType, "application/json")]
                    (errorJson "Invalid charset" $ "Only UTF-8 is supported, got: " <> LBS.fromStrict charset)
        Nothing -> app req callback
  where
    method = requestMethod req
    headers = requestHeaders req
    contentType = lookup hContentType headers
    hasContentType = case contentType of
        Just _ct -> True
        Nothing -> False

    -- Extract charset from Content-Type header
    -- Content-Type: application/json; charset=utf-8
    invalidCharset :: Maybe BS.ByteString
    invalidCharset = do
        ct <- contentType
        -- Find charset parameter
        let parts = BS.split 0x3B ct -- split on ';'
        charset <- findCharset parts
        -- Check if it's NOT utf-8 (case insensitive)
        let normalized = BS.map toLowerAscii charset
        if normalized == "utf-8" || normalized == "utf8"
            then Nothing
            else Just charset

    findCharset :: [BS.ByteString] -> Maybe BS.ByteString
    findCharset [] = Nothing
    findCharset (p : ps) =
        let trimmed = BS.dropWhile (== 0x20) p -- drop leading spaces
         in if "charset=" `BS.isPrefixOf` BS.map toLowerAscii trimmed
                then Just $ BS.drop 8 trimmed -- drop "charset="
                else findCharset ps

    -- ASCII lowercase conversion
    toLowerAscii :: Word8 -> Word8
    toLowerAscii c
        | c >= 65 && c <= 90 = c + 32
        | otherwise = c

{- | Middleware to reject requests with invalid Content-Type.

Only application/json (with optional charset) is valid for JSON APIs.
This rejects Content-Types like text/json, application/x-invalid,
application/octet-stream, text/plain, etc.

Note: We skip validation for unsupported methods so Servant can return 405.
-}
rejectInvalidContentType :: Middleware
rejectInvalidContentType app req callback
    -- Skip for unsupported methods - let Servant return 405
    | not (isSupportedMethod method) = app req callback
    -- Skip if method not allowed for this route - let Servant return 405
    | not (isMethodAllowedForRoute (pathInfo req) method) = app req callback
    -- Skip methods that don't typically have bodies
    | not needsBody = app req callback
    -- No Content-Type is handled by requireContentType
    | not hasContentType = app req callback
    -- Check if Content-Type is valid JSON
    | not isValidJsonContentType =
        callback $
            responseLBS
                status415
                [(hContentType, "application/json")]
                (errorJson "Unsupported Media Type" $ "Expected application/json, got: " <> LBS.fromStrict (fromMaybe "" contentType))
    | otherwise = app req callback
  where
    method = requestMethod req
    needsBody = method `elem` [methodPost, methodPut, methodPatch]
    headers = requestHeaders req
    contentType = lookup hContentType headers
    hasContentType = case contentType of
        Just _ct -> True
        Nothing -> False

    -- Check if Content-Type is valid for JSON APIs
    -- Valid: application/json, application/json; charset=utf-8
    -- Invalid: text/json, application/x-invalid, text/plain, etc.
    isValidJsonContentType :: Bool
    isValidJsonContentType = case contentType of
        Nothing -> True -- No Content-Type is handled separately
        Just ct ->
            let
                -- Extract media type (before any semicolon)
                mediaType = BS.takeWhile (/= 0x3B) ct -- split on ';'
                trimmed = BS.dropWhileEnd (== 0x20) $ BS.dropWhile (== 0x20) mediaType
                normalized = BS.map toLowerAscii trimmed
             in
                normalized == "application/json"

    toLowerAscii :: Word8 -> Word8
    toLowerAscii c
        | c >= 65 && c <= 90 = c + 32
        | otherwise = c

{- | Middleware to require Content-Type header for requests with bodies.

RFC 9110 recommends sending Content-Type for any message with content.
For strict API compliance, we require it for POST/PUT/PATCH with a body.

Note: We skip validation for unsupported methods so Servant can return 405.
Also skip if the method isn't allowed for the specific route.
-}
requireContentType :: Middleware
requireContentType app req callback
    -- Skip for unsupported methods - let Servant return 405
    | not (isSupportedMethod method) = app req callback
    -- Skip if method not allowed for this route - let Servant return 405
    | not (isMethodAllowedForRoute (pathInfo req) method) = app req callback
    -- Only check methods that typically have bodies
    | not needsBody = app req callback
    -- Allow empty bodies without Content-Type
    | hasNoBody = app req callback
    -- Require Content-Type for non-empty bodies
    | not hasContentType =
        callback $
            responseLBS
                status415
                [(hContentType, "application/json")]
                (errorJson "Unsupported Media Type" "Content-Type header is required")
    | otherwise = app req callback
  where
    method = requestMethod req
    needsBody = method `elem` [methodPost, methodPut, methodPatch]
    -- Only consider body absent if Content-Length is explicitly 0
    -- ChunkedBody means body may exist but size is unknown
    hasNoBody = case requestBodyLength req of
        KnownLength 0 -> True
        ChunkedBody -> False -- Chunked encoding implies body exists
        KnownLength _len -> False
    hasContentType = any (\(h, _) -> h == hContentType) (requestHeaders req)

-- | Check if a method is allowed for a specific route
isMethodAllowedForRoute :: [T.Text] -> Method -> Bool
isMethodAllowedForRoute pathSegs method =
    case findMatchingRoute pathSegs of
        Just spec -> method `elem` rsMethods spec
        Nothing -> True -- Route not found, let Servant handle 404

{- | Middleware to reject HEAD requests.

Our OpenAPI spec does not define HEAD for any endpoint. While HTTP allows
HEAD as an implicit companion to GET, strict OpenAPI compliance requires
returning 405 for methods not in the spec.

This middleware rejects all HEAD requests with 405 Method Not Allowed.
-}
rejectHeadMethod :: Middleware
rejectHeadMethod app req callback
    | requestMethod req == methodHead =
        callback $
            responseLBS
                status405
                [ (hContentType, "application/json")
                , (hAllow, "GET, POST, PUT, PATCH, DELETE")
                ]
                (errorJson "Method Not Allowed" "HEAD method is not supported")
    | otherwise = app req callback

-- | HEAD method constant
methodHead :: Method
methodHead = "HEAD"

{- | Middleware to reject unsupported HTTP methods with 405.

Methods like OPTIONS (non-CORS), TRACE, CONNECT should return 405
Method Not Allowed rather than being passed to Servant.

Note: CORS preflight OPTIONS requests are handled by enableCors middleware.
This catches non-preflight OPTIONS and other unusual methods.
-}
rejectUnsupportedMethods :: Middleware
rejectUnsupportedMethods app req callback
    | not (isSupportedMethod method) =
        callback $
            responseLBS
                status405
                [ (hContentType, "application/json")
                , (hAllow, "GET, POST, PUT, PATCH, DELETE")
                ]
                (errorJson "Method Not Allowed" $ "Method not supported: " <> LBS.fromStrict method)
    | otherwise = app req callback
  where
    method = requestMethod req

{- | Middleware to reject method/path mismatches with 405.

When a known route is accessed with an unsupported method, return 405
instead of 404. This catches cases like PATCH /project/current when
only GET is allowed.
-}
rejectMethodMismatch :: Middleware
rejectMethodMismatch app req callback
    -- Check if the path matches a known route but method doesn't match
    | Just spec <- findMatchingRoute (pathInfo req)
    , method `notElem` rsMethods spec =
        callback $
            responseLBS
                status405
                [ (hContentType, "application/json")
                , (hAllow, BS.intercalate ", " (rsMethods spec))
                ]
                (errorJson "Method Not Allowed" "This endpoint does not support the requested method")
    | otherwise = app req callback
  where
    method = requestMethod req

-- ═══════════════════════════════════════════════════════════════════════════
-- Path Security Validation
-- ═══════════════════════════════════════════════════════════════════════════

{- | Middleware to reject paths containing null bytes.

Null byte injection can trick path parsing and security checks.
This middleware rejects any path containing %00 or literal null bytes.

Note: We skip validation for unsupported methods so Servant can return 405.
-}
rejectNullBytePaths :: Middleware
rejectNullBytePaths app req callback
    -- Skip for unsupported methods - let Servant return 405
    | not (isSupportedMethod method) = app req callback
    | hasNullByte =
        callback $
            responseLBS
                status400
                [(hContentType, "application/json")]
                (errorJson "Invalid path" "Path contains null byte")
    | otherwise = app req callback
  where
    method = requestMethod req
    rawPath = rawPathInfo req
    decodedSegments = pathInfo req

    -- Check for null bytes in multiple forms:
    -- 1. Literal 0x00 in raw path
    -- 2. URL-encoded %00 in raw path (case insensitive)
    -- 3. Null byte in decoded path segments
    hasNullByte =
        0x00 `BS.elem` rawPath
            || hasEncodedNullByte rawPath
            || any (T.any (== '\x00')) decodedSegments

    -- Check for %00 (URL-encoded null byte) - case insensitive
    hasEncodedNullByte :: BS.ByteString -> Bool
    hasEncodedNullByte bs
        | BS.length bs < 3 = False
        | otherwise =
            let (first3, _) = BS.splitAt 3 bs
                lower3 = BS.map toLowerAscii first3
             in (lower3 == "%00") || hasEncodedNullByte (BS.drop 1 bs)

    toLowerAscii :: Word8 -> Word8
    toLowerAscii c
        | c >= 65 && c <= 90 = c + 32
        | otherwise = c

{- | Middleware to reject double-encoded paths.

Double URL encoding like %252e%252e%252f (which decodes to %2e%2e%2f, then to ../)
can be used for path traversal attacks that bypass single-decode checks.

This middleware detects %25XX patterns (encoded percent signs followed by hex).

Note: We skip validation for unsupported methods so Servant can return 405.
-}
rejectDoubleEncodedPaths :: Middleware
rejectDoubleEncodedPaths app req callback
    -- Skip for unsupported methods - let Servant return 405
    | not (isSupportedMethod method) = app req callback
    | hasDoubleEncoding =
        callback $
            responseLBS
                status400
                [(hContentType, "application/json")]
                (errorJson "Invalid path" "Path contains double-encoded characters")
    | otherwise = app req callback
  where
    method = requestMethod req
    rawPath = rawPathInfo req

    -- Check for %25 followed by two hex digits (double-encoded percent)
    -- %25 = encoded '%', so %252e = encoded '%2e' = encoded '.'
    hasDoubleEncoding = detectDoubleEncoding rawPath

    detectDoubleEncoding :: BS.ByteString -> Bool
    detectDoubleEncoding bs
        | BS.length bs < 6 = False
        | otherwise =
            let (first3, rest) = BS.splitAt 3 bs
             in if first3 == "%25" && BS.length rest >= 2
                    then
                        let (hex, _) = BS.splitAt 2 rest
                         in isHexPair hex || detectDoubleEncoding (BS.drop 1 bs)
                    else detectDoubleEncoding (BS.drop 1 bs)

    isHexPair :: BS.ByteString -> Bool
    isHexPair bs = case BS.unpack bs of
        [c1, c2] -> isHexDigit c1 && isHexDigit c2
        [] -> False
        [_single] -> False
        _threeOrMore -> False

    isHexDigit :: Word8 -> Bool
    isHexDigit c =
        (c >= 48 && c <= 57) -- 0-9
            || (c >= 65 && c <= 70) -- A-F
            || (c >= 97 && c <= 102) -- a-f

-- ═══════════════════════════════════════════════════════════════════════════
-- Route Specifications
-- ═══════════════════════════════════════════════════════════════════════════

{- | Specification for a route's allowed methods and parameters.

This captures the static information needed for strict request validation.
-}
data RouteSpec = RouteSpec
    { rsPattern :: [PathSegment]
    -- ^ Path pattern with wildcards for captures
    , rsMethods :: [Method]
    -- ^ HTTP methods allowed on this route
    , rsQueryParams :: Map.Map Method (Set.Set BS.ByteString)
    -- ^ Query parameters allowed per method
    }
    deriving (Show, Eq)

-- | A path segment, either literal or a capture placeholder
data PathSegment
    = Literal T.Text
    | Capture
    deriving (Show, Eq)

-- | Match a path against a pattern
matchPattern :: [PathSegment] -> [T.Text] -> Bool
matchPattern [] [] = True
matchPattern (Literal t : ps) (s : ss) = t == s && matchPattern ps ss
matchPattern (Capture : ps) (_ : ss) = matchPattern ps ss
matchPattern _ _ = False

-- | Find the route spec matching a path
findMatchingRoute :: [T.Text] -> Maybe RouteSpec
findMatchingRoute pathSegs = go routeSpecs
  where
    go [] = Nothing
    go (spec : rest)
        | matchPattern (rsPattern spec) pathSegs = Just spec
        | otherwise = go rest

{- | All route specifications for the API
Common params like 'directory' are allowed on most routes
-}
routeSpecs :: [RouteSpec]
routeSpecs =
    let dir = Set.singleton "directory"
        dirRoots = Set.fromList ["directory", "roots", "limit", "start", "search"]
        dirPath = Set.fromList ["directory", "path"]
        dirMessageId = Set.fromList ["directory", "messageID"]
        -- Find endpoints don't accept directory param (DoS prevention)
        findTextParams = Set.fromList ["query", "pattern"]
        findFileParams = Set.fromList ["query", "dirs", "type", "limit"]
        findSymbolParams = Set.singleton "query"
     in -- Global routes (no directory param)
        [ RouteSpec [Literal "global", Literal "health"] [methodGet] (Map.fromList [(methodGet, Set.empty)])
        , RouteSpec [Literal "global", Literal "config"] [methodGet, methodPatch] (Map.fromList [(methodGet, Set.empty), (methodPatch, Set.empty)])
        , RouteSpec [Literal "global", Literal "event"] [methodGet] (Map.fromList [(methodGet, Set.singleton "directory")])
        , RouteSpec [Literal "global", Literal "dispose"] [methodPost] (Map.fromList [(methodPost, Set.empty)])
        , -- Path and config
          RouteSpec [Literal "path"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "config"] [methodGet, methodPatch] (Map.fromList [(methodGet, dir), (methodPatch, dir)])
        , RouteSpec [Literal "config", Literal "providers"] [methodGet] (Map.fromList [(methodGet, dir)])
        , -- Project
          RouteSpec [Literal "project"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "project", Literal "current"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "project", Capture] [methodGet, methodPatch] (Map.fromList [(methodGet, dir), (methodPatch, dir)])
        , -- Provider
          RouteSpec [Literal "provider"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "provider", Literal "auth"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "provider", Capture] [methodGet, methodPatch] (Map.fromList [(methodGet, dir), (methodPatch, dir)])
        , RouteSpec [Literal "provider", Capture, Literal "oauth", Literal "authorize"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "provider", Capture, Literal "oauth", Literal "callback"] [methodPost] (Map.fromList [(methodPost, Set.fromList ["directory", "code", "state"])])
        , -- Auth
          RouteSpec [Literal "auth", Capture] [methodPost, methodPut, methodDelete] (Map.fromList [(methodPost, Set.empty), (methodPut, Set.empty), (methodDelete, Set.empty)])
        , -- Session
          RouteSpec [Literal "session"] [methodGet, methodPost] (Map.fromList [(methodGet, dirRoots), (methodPost, dir)])
        , RouteSpec [Literal "session", Literal "status"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "session", Capture] [methodGet, methodDelete, methodPatch] (Map.fromList [(methodGet, dir), (methodDelete, dir), (methodPatch, dir)])
        , RouteSpec [Literal "session", Capture, Literal "children"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "session", Capture, Literal "todo"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "session", Capture, Literal "init"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "session", Capture, Literal "fork"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "session", Capture, Literal "abort"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "session", Capture, Literal "share"] [methodPost, methodDelete] (Map.fromList [(methodPost, dir), (methodDelete, dir)])
        , RouteSpec [Literal "session", Capture, Literal "diff"] [methodGet] (Map.fromList [(methodGet, dirMessageId)])
        , RouteSpec [Literal "session", Capture, Literal "summarize"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "session", Capture, Literal "command"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "session", Capture, Literal "shell"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "session", Capture, Literal "revert"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "session", Capture, Literal "unrevert"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "session", Capture, Literal "permissions", Capture] [methodPut] (Map.fromList [(methodPut, dir)])
        , -- Message
          RouteSpec [Literal "session", Capture, Literal "message"] [methodGet, methodPost] (Map.fromList [(methodGet, dir), (methodPost, dir)])
        , RouteSpec [Literal "session", Capture, Literal "message", Capture] [methodGet, methodDelete] (Map.fromList [(methodGet, dir), (methodDelete, dir)])
        , RouteSpec [Literal "session", Capture, Literal "message", Capture, Literal "part", Capture] [methodDelete, methodPatch] (Map.fromList [(methodDelete, dir), (methodPatch, dir)])
        , RouteSpec [Literal "session", Capture, Literal "prompt_async"] [methodPost] (Map.fromList [(methodPost, dir)])
        , -- LSP and VCS
          RouteSpec [Literal "lsp"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "vcs"] [methodGet] (Map.fromList [(methodGet, dir)])
        , -- Permission and Question
          RouteSpec [Literal "permission"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "permission", Capture, Literal "reply"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "question"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "question", Capture, Literal "reply"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "question", Capture, Literal "reject"] [methodPost] (Map.fromList [(methodPost, dir)])
        , -- Find (no directory param to prevent DoS via searching /)
          RouteSpec [Literal "find"] [methodGet] (Map.fromList [(methodGet, findTextParams)])
        , RouteSpec [Literal "find", Literal "file"] [methodGet] (Map.fromList [(methodGet, findFileParams)])
        , RouteSpec [Literal "find", Literal "symbol"] [methodGet] (Map.fromList [(methodGet, findSymbolParams)])
        , -- File
          RouteSpec [Literal "file"] [methodGet] (Map.fromList [(methodGet, dirPath)])
        , RouteSpec [Literal "file", Literal "content"] [methodGet] (Map.fromList [(methodGet, dirPath)])
        , RouteSpec [Literal "file", Literal "status"] [methodGet] (Map.fromList [(methodGet, dir)])
        , -- PTY
          RouteSpec [Literal "pty"] [methodGet, methodPost] (Map.fromList [(methodGet, dir), (methodPost, dir)])
        , RouteSpec [Literal "pty", Capture] [methodGet, methodPut, methodDelete] (Map.fromList [(methodGet, dir), (methodPut, dir), (methodDelete, dir)])
        , RouteSpec [Literal "pty", Capture, Literal "connect"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "pty", Capture, Literal "commit"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "pty", Capture, Literal "changes"] [methodGet] (Map.fromList [(methodGet, dir)])
        , -- TUI
          RouteSpec [Literal "tui", Literal "append-prompt"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "open-help"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "open-sessions"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "open-themes"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "open-models"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "submit-prompt"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "clear-prompt"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "execute-command"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "show-toast"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "publish"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "select-session"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "control", Literal "next"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "tui", Literal "control", Literal "response"] [methodPost] (Map.fromList [(methodPost, dir)])
        , -- Lifecycle
          RouteSpec [Literal "instance", Literal "dispose"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "event"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "log"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "skill"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "formatter"] [methodGet] (Map.fromList [(methodGet, dir)])
        , -- Agent and Command
          RouteSpec [Literal "agent"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "command"] [methodGet] (Map.fromList [(methodGet, dir)])
        , -- Experimental
          RouteSpec [Literal "experimental", Literal "tool", Literal "ids"] [methodGet] (Map.fromList [(methodGet, dir)])
        , RouteSpec [Literal "experimental", Literal "tool"] [methodGet, methodPost] (Map.fromList [(methodGet, dir), (methodPost, dir)])
        , RouteSpec [Literal "experimental", Literal "worktree"] [methodGet, methodPost, methodDelete] (Map.fromList [(methodGet, dir), (methodPost, dir), (methodDelete, dir)])
        , RouteSpec [Literal "experimental", Literal "worktree", Literal "reset"] [methodPost] (Map.fromList [(methodPost, dir)])
        , RouteSpec [Literal "experimental", Literal "session"] [methodGet] (Map.fromList [(methodGet, dir)])
        , -- Chat
          RouteSpec [Literal "chat"] [methodPost] (Map.fromList [(methodPost, dir)])
        ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Helpers
-- ═══════════════════════════════════════════════════════════════════════════

-- | Build a JSON error response body
errorJson :: LBS.ByteString -> LBS.ByteString -> LBS.ByteString
errorJson name msg =
    "{\"name\":\"BadRequestError\",\"data\":{\"code\":\"" <> name <> "\",\"message\":\"" <> msg <> "\"}}"
