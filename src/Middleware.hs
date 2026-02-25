-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                 // weapon-server // middleware
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- WAI middleware for the Weapon server.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Middleware (
    supplyEmptyBody,
    requestLogger,
    rejectEmptyPathSegments,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Katip qualified
import Log (Logger, logMsg)
import Network.HTTP.Types (hContentType, methodDelete, methodPatch, methodPost, methodPut, status400)
import Network.Wai (
    Middleware,
    Request (..),
    RequestBodyLength (..),
    pathInfo,
    requestBodyLength,
    requestMethod,
    responseLBS,
    setRequestBodyChunks,
 )

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
    hasNoBody = case requestBodyLength req of
        KnownLength 0 -> True
        ChunkedBody -> False -- Can't know, assume it has body
        KnownLength _len -> False
    hasContentType = any (\(h, _) -> h == hContentType) (requestHeaders req)

{- | Middleware to reject requests with empty path segments.

Paths like @\/session\/@ or @\/pty\/@ have a trailing slash that creates an
empty path segment. These should return 400 Bad Request rather than being
routed to list endpoints.

This handles the case where a fuzzer removes a required path parameter like
@sessionID@ from @\/session\/{sessionID}@, resulting in @\/session\/@.
-}
rejectEmptyPathSegments :: Middleware
rejectEmptyPathSegments app req callback
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
