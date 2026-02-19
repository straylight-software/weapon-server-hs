-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                 // weapon-server // middleware
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- WAI middleware for the Weapon server.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Middleware
    ( supplyEmptyBody
    ) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Types (hContentType, methodDelete, methodPatch, methodPost, methodPut)
import Network.Wai
    ( Middleware
    , RequestBodyLength (..)
    , Request (..)
    , requestBodyLength
    , requestMethod
    , setRequestBodyChunks
    )

-- | Middleware to supply an empty JSON body when no body is provided.
--
-- For POST, PUT, PATCH, and DELETE requests without a body, this middleware
-- supplies an empty JSON object `{}` and sets Content-Type to application/json
-- to avoid 415 Unsupported Media Type errors from Servant.
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
            headers' = if hasContentType
                       then requestHeaders req
                       else (hContentType, "application/json") : requestHeaders req
            req' = (setRequestBodyChunks bodyChunks req) { requestHeaders = headers' }
        app req' callback
    | otherwise = app req callback
  where
    method = requestMethod req
    needsBody = method `elem` [methodPost, methodPut, methodPatch, methodDelete]
    hasNoBody = case requestBodyLength req of
        KnownLength 0 -> True
        ChunkedBody -> False  -- Can't know, assume it has body
        KnownLength _ -> False
    hasContentType = any (\(h, _) -> h == hContentType) (requestHeaders req)
