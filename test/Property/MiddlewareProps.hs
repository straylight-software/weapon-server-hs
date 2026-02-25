{-# LANGUAGE OverloadedStrings #-}

-- | Middleware property tests
module Property.MiddlewareProps where

import Control.Concurrent.MVar
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Text.Encoding qualified as TE
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Middleware (rejectEmptyPathSegments, supplyEmptyBody)
import Network.HTTP.Types
import Network.Wai (RequestBodyLength (..), Response, defaultRequest, responseLBS, responseToStream, setRequestBodyChunks, strictRequestBody)
import Network.Wai.Internal (Request (..), ResponseReceived (..))

import Test.Tasty
import Test.Tasty.Hedgehog

-- ═══════════════════════════════════════════════════════════════════════════
-- Middleware Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: POST with no body gets {} body added
prop_supplyEmptyBodyPost :: Property
prop_supplyEmptyBodyPost = property $ do
    -- Create a POST request with KnownLength 0 (no body)
    bodyRef <- evalIO $ newIORef False
    let captureApp req respond = do
            -- Read the body
            body <- strictRequestBody req
            writeIORef bodyRef True
            respond $ responseLBS status200 [] body

    -- Apply middleware and run
    result <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodPost
                    , requestBodyLength = KnownLength 0
                    , requestHeaders = []
                    }
        responseRef <- newEmptyMVar
        let respond resp = do
                body <- extractBody resp
                putMVar responseRef body
                pure ResponseReceived
        _ <- supplyEmptyBody captureApp req respond
        takeMVar responseRef

    -- The body should be "{}"
    result === "{}"

-- | Property: POST with existing body is unchanged
prop_supplyEmptyBodyWithBody :: Property
prop_supplyEmptyBodyWithBody = property $ do
    -- Generate some body content
    bodyContent <- forAll $ Gen.utf8 (Range.linear 1 100) Gen.alphaNum

    result <- evalIO $ do
        bodyReadRef <- newIORef False
        let bodyChunks = do
                wasRead <- readIORef bodyReadRef
                if wasRead
                    then pure ""
                    else do
                        writeIORef bodyReadRef True
                        pure bodyContent
        let captureApp req respond = do
                body <- strictRequestBody req
                respond $ responseLBS status200 [] body
        let reqBase =
                defaultRequest
                    { requestMethod = methodPost
                    , requestBodyLength = KnownLength (fromIntegral $ BS.length bodyContent)
                    , requestHeaders = [(hContentType, "application/json")]
                    }
        let req = setRequestBodyChunks bodyChunks reqBase

        responseRef <- newEmptyMVar
        let respond resp = do
                body <- extractBody resp
                putMVar responseRef body
                pure ResponseReceived
        _ <- supplyEmptyBody captureApp req respond
        takeMVar responseRef

    -- The body should be unchanged
    result === LBS.fromStrict bodyContent

-- | Property: GET requests are unchanged (no body added)
prop_supplyEmptyBodyGet :: Property
prop_supplyEmptyBodyGet = property $ do
    result <- evalIO $ do
        let captureApp req respond = do
                body <- strictRequestBody req
                respond $ responseLBS status200 [] body

        let req =
                defaultRequest
                    { requestMethod = methodGet
                    , requestBodyLength = KnownLength 0
                    , requestHeaders = []
                    }

        responseRef <- newEmptyMVar
        let respond resp = do
                body <- extractBody resp
                putMVar responseRef body
                pure ResponseReceived
        _ <- supplyEmptyBody captureApp req respond
        takeMVar responseRef

    -- GET with no body should remain empty (middleware only applies to POST/PUT/PATCH/DELETE)
    result === ""

-- | Property: DELETE with no body gets {} body added
prop_supplyEmptyBodyDelete :: Property
prop_supplyEmptyBodyDelete = property $ do
    result <- evalIO $ do
        let captureApp req respond = do
                body <- strictRequestBody req
                respond $ responseLBS status200 [] body

        let req =
                defaultRequest
                    { requestMethod = methodDelete
                    , requestBodyLength = KnownLength 0
                    , requestHeaders = []
                    }

        responseRef <- newEmptyMVar
        let respond resp = do
                body <- extractBody resp
                putMVar responseRef body
                pure ResponseReceived
        _ <- supplyEmptyBody captureApp req respond
        takeMVar responseRef

    result === "{}"

-- ═══════════════════════════════════════════════════════════════════════════
-- Helpers
-- ═══════════════════════════════════════════════════════════════════════════

-- | Extract body from a Response
extractBody :: Response -> IO LBS.ByteString
extractBody resp = do
    bodyRef <- newIORef ([] :: [Builder])
    let collectBuilder builder = modifyIORef' bodyRef (\builders -> builders <> [builder])
    let (_status, _headers, withBody) = responseToStream resp
    _ <- withBody $ \streamingBody -> streamingBody collectBuilder (pure ())
    builders <- readIORef bodyRef
    pure $ toLazyByteString (mconcat builders)

-- Actually, let's use a simpler approach for testing
-- The middleware modifies the request, not the response

-- ═══════════════════════════════════════════════════════════════════════════
-- Empty Path Segment Tests
-- ═══════════════════════════════════════════════════════════════════════════

{- | Helper to create a request with specific path
WAI's pathInfo includes empty segments for trailing slashes
e.g., "/session/" -> ["session", ""]
      "/session" -> ["session"]
-}
mkRequestWithPath :: BS.ByteString -> Request
mkRequestWithPath path =
    defaultRequest
        { rawPathInfo = path
        , pathInfo = map TE.decodeUtf8 $ tail' $ BS.split 0x2F path -- split on '/', drop leading empty
        }
  where
    tail' [] = []
    tail' (_ : xs) = xs -- drop leading empty from split (before first /)

-- | Test: /session/ should be rejected (missing sessionID)
prop_rejectSessionTrailingSlash :: Property
prop_rejectSessionTrailingSlash = property $ do
    statusCode <- evalIO $ do
        let req = mkRequestWithPath "/session/"
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectEmptyPathSegments passThrough req respond
        takeMVar statusRef
    statusCode === status400

-- | Test: /pty/ should be rejected (missing ptyID)
prop_rejectPtyTrailingSlash :: Property
prop_rejectPtyTrailingSlash = property $ do
    statusCode <- evalIO $ do
        let req = mkRequestWithPath "/pty/"
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectEmptyPathSegments passThrough req respond
        takeMVar statusRef
    statusCode === status400

-- | Test: /session/abc/message/ should be rejected (missing messageID)
prop_rejectMessageTrailingSlash :: Property
prop_rejectMessageTrailingSlash = property $ do
    statusCode <- evalIO $ do
        let req = mkRequestWithPath "/session/abc/message/"
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectEmptyPathSegments passThrough req respond
        takeMVar statusRef
    statusCode === status400

-- | Test: /session (no trailing slash) should pass through
prop_allowSessionNoSlash :: Property
prop_allowSessionNoSlash = property $ do
    statusCode <- evalIO $ do
        let req = mkRequestWithPath "/session"
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectEmptyPathSegments passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Test: /session/abc (with ID) should pass through
prop_allowSessionWithId :: Property
prop_allowSessionWithId = property $ do
    statusCode <- evalIO $ do
        let req = mkRequestWithPath "/session/abc"
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectEmptyPathSegments passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Middleware Property Tests"
        [ testGroup
            "supplyEmptyBody"
            [ testProperty "POST with no body gets {} added" prop_supplyEmptyBodyPost
            , testProperty "POST with existing body unchanged" prop_supplyEmptyBodyWithBody
            , testProperty "GET requests unchanged" prop_supplyEmptyBodyGet
            , testProperty "DELETE with no body gets {} added" prop_supplyEmptyBodyDelete
            ]
        , testGroup
            "rejectEmptyPathSegments"
            [ testProperty "/session/ is rejected" prop_rejectSessionTrailingSlash
            , testProperty "/pty/ is rejected" prop_rejectPtyTrailingSlash
            , testProperty "/session/abc/message/ is rejected" prop_rejectMessageTrailingSlash
            , testProperty "/session passes through" prop_allowSessionNoSlash
            , testProperty "/session/abc passes through" prop_allowSessionWithId
            ]
        ]
