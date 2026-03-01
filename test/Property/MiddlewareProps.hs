{-# LANGUAGE OverloadedStrings #-}

-- | Middleware property tests
module Property.MiddlewareProps where

import Control.Concurrent.MVar
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Text.Encoding qualified as TE
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Middleware (
    addAllowHeader,
    limitJsonDepth,
    rejectDoubleEncodedPaths,
    rejectDuplicateQueryParams,
    rejectEmptyPathSegments,
    rejectHeadMethod,
    rejectInvalidCharset,
    rejectNullBytePaths,
    rejectUnknownQueryParams,
    requireContentType,
    supplyEmptyBody,
 )
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
-- Allow Header Tests (RFC 9110)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: 405 responses get Allow header added
prop_addAllowHeaderOn405 :: Property
prop_addAllowHeaderOn405 = property $ do
    result <- evalIO $ do
        -- App that returns 405 without Allow header
        let app405 _req respond = respond $ responseLBS status405 [(hContentType, "text/plain")] "Method Not Allowed"
        let req = mkRequestWithPath "/session"

        responseRef <- newEmptyMVar
        let respond resp = do
                let headers = extractHeaders resp
                putMVar responseRef headers
                pure ResponseReceived
        _ <- addAllowHeader app405 req respond
        takeMVar responseRef

    -- Should have Allow header
    let hasAllow = any (\(h, _) -> h == hAllow) result
    hasAllow === True

-- | Property: 200 responses don't get Allow header added
prop_noAllowHeaderOn200 :: Property
prop_noAllowHeaderOn200 = property $ do
    result <- evalIO $ do
        let app200 _req respond = respond $ responseLBS status200 [] "OK"
        let req = mkRequestWithPath "/session"

        responseRef <- newEmptyMVar
        let respond resp = do
                let headers = extractHeaders resp
                putMVar responseRef headers
                pure ResponseReceived
        _ <- addAllowHeader app200 req respond
        takeMVar responseRef

    -- Should NOT have Allow header
    let hasAllow = any (\(h, _) -> h == hAllow) result
    hasAllow === False

-- | Property: Existing Allow header is preserved
prop_preserveExistingAllowHeader :: Property
prop_preserveExistingAllowHeader = property $ do
    result <- evalIO $ do
        let app405 _req respond = respond $ responseLBS status405 [(hAllow, "GET, POST")] "Method Not Allowed"
        let req = mkRequestWithPath "/session"

        responseRef <- newEmptyMVar
        let respond resp = do
                let headers = extractHeaders resp
                putMVar responseRef headers
                pure ResponseReceived
        _ <- addAllowHeader app405 req respond
        takeMVar responseRef

    -- Should have exactly one Allow header (not duplicated)
    let allowHeaders = filter (\(h, _) -> h == hAllow) result
    length allowHeaders === 1
    -- Value should be preserved
    case allowHeaders of
        [(_, v)] -> v === "GET, POST"
        [] -> failure
        _multipleHeaders -> failure

-- ═══════════════════════════════════════════════════════════════════════════
-- Duplicate Query Param Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: Duplicate query params are rejected
prop_rejectDuplicateQueryParams :: Property
prop_rejectDuplicateQueryParams = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { queryString = [("directory", Just "a"), ("directory", Just "b")]
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectDuplicateQueryParams passThrough req respond
        takeMVar statusRef
    statusCode === status400

-- | Property: Unique query params pass through
prop_allowUniqueQueryParams :: Property
prop_allowUniqueQueryParams = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { queryString = [("directory", Just "a"), ("limit", Just "10")]
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectDuplicateQueryParams passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Property: Empty query string passes through
prop_allowEmptyQueryString :: Property
prop_allowEmptyQueryString = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { queryString = []
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectDuplicateQueryParams passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- ═══════════════════════════════════════════════════════════════════════════
-- Unknown Query Param Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: Unknown query params are rejected
prop_rejectUnknownQueryParam :: Property
prop_rejectUnknownQueryParam = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { queryString = [("x_haskemathesis_unknown", Just "test")]
                    , pathInfo = ["session"]
                    , requestMethod = methodGet
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectUnknownQueryParams passThrough req respond
        takeMVar statusRef
    statusCode === status400

-- | Property: Known query params pass through (directory)
prop_allowKnownQueryParam :: Property
prop_allowKnownQueryParam = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { queryString = [("directory", Just "test")]
                    , pathInfo = ["session"]
                    , requestMethod = methodGet
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectUnknownQueryParams passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Property: Session list allows roots, limit, start, search params
prop_allowSessionListParams :: Property
prop_allowSessionListParams = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { queryString =
                        [ ("directory", Just "test")
                        , ("roots", Just "true")
                        , ("limit", Just "10")
                        , ("start", Just "0")
                        , ("search", Just "query")
                        ]
                    , pathInfo = ["session"]
                    , requestMethod = methodGet
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectUnknownQueryParams passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- ═══════════════════════════════════════════════════════════════════════════
-- JSON Depth Limit Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: Deeply nested JSON is rejected
prop_rejectDeeplyNestedJson :: Property
prop_rejectDeeplyNestedJson = property $ do
    statusCode <- evalIO $ do
        -- Create JSON nested 60 levels deep
        let deepJson = createNestedJson 60
        bodyReadRef <- newIORef False
        let bodyChunks = do
                wasRead <- readIORef bodyReadRef
                if wasRead
                    then pure ""
                    else do
                        writeIORef bodyReadRef True
                        pure $ LBS.toStrict $ A.encode deepJson
        let reqBase =
                defaultRequest
                    { requestMethod = methodPost
                    , requestBodyLength = KnownLength (fromIntegral $ LBS.length $ A.encode deepJson)
                    , requestHeaders = [(hContentType, "application/json")]
                    , pathInfo = ["session"]
                    }
        let req = setRequestBodyChunks bodyChunks reqBase

        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- limitJsonDepth passThrough req respond
        takeMVar statusRef
    statusCode === status400

-- | Property: Shallow JSON passes through
prop_allowShallowJson :: Property
prop_allowShallowJson = property $ do
    statusCode <- evalIO $ do
        -- Create JSON nested only 5 levels deep
        let shallowJson = createNestedJson 5
        bodyReadRef <- newIORef False
        let bodyChunks = do
                wasRead <- readIORef bodyReadRef
                if wasRead
                    then pure ""
                    else do
                        writeIORef bodyReadRef True
                        pure $ LBS.toStrict $ A.encode shallowJson
        let reqBase =
                defaultRequest
                    { requestMethod = methodPost
                    , requestBodyLength = KnownLength (fromIntegral $ LBS.length $ A.encode shallowJson)
                    , requestHeaders = [(hContentType, "application/json")]
                    , pathInfo = ["session"]
                    }
        let req = setRequestBodyChunks bodyChunks reqBase

        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- limitJsonDepth passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Property: GET requests skip depth check
prop_skipDepthCheckForGet :: Property
prop_skipDepthCheckForGet = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodGet
                    , requestBodyLength = KnownLength 0
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- limitJsonDepth passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Helper: Create nested JSON with given depth
createNestedJson :: Int -> A.Value
createNestedJson 0 = A.String "leaf"
createNestedJson n = A.object [("nested", createNestedJson (n - 1))]

-- ═══════════════════════════════════════════════════════════════════════════
-- Unsupported Method Passthrough Tests (TRACE, OPTIONS, etc.)
-- ═══════════════════════════════════════════════════════════════════════════

{- | Property: TRACE method skips rejectEmptyPathSegments
   Even with an invalid path like /session/, TRACE should pass through
   so Servant can return 405.
-}
prop_traceSkipsEmptyPathSegments :: Property
prop_traceSkipsEmptyPathSegments = property $ do
    statusCode <- evalIO $ do
        let req = (mkRequestWithPath "/session/"){requestMethod = methodTrace}
        statusRef <- newEmptyMVar
        -- The passthrough app returns 405 (simulating Servant)
        let passThrough _req respond = respond $ responseLBS status405 [] "Method Not Allowed"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectEmptyPathSegments passThrough req respond
        takeMVar statusRef
    -- Should get 405 from the app, not 400 from middleware
    statusCode === status405

{- | Property: TRACE method skips rejectDuplicateQueryParams
   Even with duplicate params, TRACE should pass through.
-}
prop_traceSkipsDuplicateQueryParams :: Property
prop_traceSkipsDuplicateQueryParams = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { queryString = [("directory", Just "a"), ("directory", Just "b")]
                    , pathInfo = ["session"]
                    , requestMethod = methodTrace
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status405 [] "Method Not Allowed"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectDuplicateQueryParams passThrough req respond
        takeMVar statusRef
    statusCode === status405

{- | Property: TRACE method skips rejectUnknownQueryParams
   Even with unknown params, TRACE should pass through.
-}
prop_traceSkipsUnknownQueryParams :: Property
prop_traceSkipsUnknownQueryParams = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { queryString = [("x_haskemathesis_unknown", Just "test")]
                    , pathInfo = ["session"]
                    , requestMethod = methodTrace
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status405 [] "Method Not Allowed"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectUnknownQueryParams passThrough req respond
        takeMVar statusRef
    statusCode === status405

{- | Property: TRACE method skips limitJsonDepth
   TRACE doesn't have a body, but we test that it passes through anyway.
-}
prop_traceSkipsJsonDepth :: Property
prop_traceSkipsJsonDepth = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodTrace
                    , requestBodyLength = KnownLength 0
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status405 [] "Method Not Allowed"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- limitJsonDepth passThrough req respond
        takeMVar statusRef
    statusCode === status405

-- ═══════════════════════════════════════════════════════════════════════════
-- Charset Validation Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: Non-UTF-8 charset is rejected
prop_rejectNonUtf8Charset :: Property
prop_rejectNonUtf8Charset = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodPost
                    , requestHeaders = [(hContentType, "application/json; charset=utf-16")]
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectInvalidCharset passThrough req respond
        takeMVar statusRef
    statusCode === status400

-- | Property: UTF-8 charset is accepted
prop_allowUtf8Charset :: Property
prop_allowUtf8Charset = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodPost
                    , requestHeaders = [(hContentType, "application/json; charset=utf-8")]
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectInvalidCharset passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Property: No charset parameter is accepted (defaults to UTF-8)
prop_allowNoCharset :: Property
prop_allowNoCharset = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodPost
                    , requestHeaders = [(hContentType, "application/json")]
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectInvalidCharset passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Property: TRACE skips charset validation
prop_traceSkipsCharsetValidation :: Property
prop_traceSkipsCharsetValidation = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodTrace
                    , requestHeaders = [(hContentType, "application/json; charset=utf-16")]
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status405 [] "Method Not Allowed"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectInvalidCharset passThrough req respond
        takeMVar statusRef
    statusCode === status405

-- ═══════════════════════════════════════════════════════════════════════════
-- Null Byte Path Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: Paths with null bytes are rejected
prop_rejectNullBytePath :: Property
prop_rejectNullBytePath = property $ do
    statusCode <- evalIO $ do
        -- Path with literal null byte (must use BS.concat to include 0x00)
        let pathWithNull = BS.concat ["/session/abc", BS.pack [0x00], "def"]
        let req =
                defaultRequest
                    { rawPathInfo = pathWithNull
                    , pathInfo = ["session", "abc"] -- pathInfo is after decode, null truncates
                    , requestMethod = methodGet
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectNullBytePaths passThrough req respond
        takeMVar statusRef
    statusCode === status400

-- | Property: Normal paths without null bytes pass through
prop_allowNormalPath :: Property
prop_allowNormalPath = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { rawPathInfo = "/session/abc"
                    , pathInfo = ["session", "abc"]
                    , requestMethod = methodGet
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectNullBytePaths passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Property: TRACE skips null byte validation
prop_traceSkipsNullByteValidation :: Property
prop_traceSkipsNullByteValidation = property $ do
    statusCode <- evalIO $ do
        -- Path with literal null byte
        let pathWithNull = BS.concat ["/session/abc", BS.pack [0x00], "def"]
        let req =
                defaultRequest
                    { rawPathInfo = pathWithNull
                    , pathInfo = ["session", "abc"]
                    , requestMethod = methodTrace
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status405 [] "Method Not Allowed"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectNullBytePaths passThrough req respond
        takeMVar statusRef
    statusCode === status405

-- ═══════════════════════════════════════════════════════════════════════════
-- Double Encoding Path Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: Double-encoded paths are rejected
prop_rejectDoubleEncodedPath :: Property
prop_rejectDoubleEncodedPath = property $ do
    statusCode <- evalIO $ do
        -- %252e = encoded %2e = encoded '.'
        let req =
                defaultRequest
                    { rawPathInfo = "/session/%252e%252e%252f"
                    , pathInfo = ["session", "%2e%2e%2f"] -- after single decode
                    , requestMethod = methodGet
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectDoubleEncodedPaths passThrough req respond
        takeMVar statusRef
    statusCode === status400

-- | Property: Normal encoded paths pass through
prop_allowNormalEncodedPath :: Property
prop_allowNormalEncodedPath = property $ do
    statusCode <- evalIO $ do
        -- %20 = space (single encoding is fine)
        let req =
                defaultRequest
                    { rawPathInfo = "/session/hello%20world"
                    , pathInfo = ["session", "hello world"]
                    , requestMethod = methodGet
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectDoubleEncodedPaths passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Property: TRACE skips double encoding validation
prop_traceSkipsDoubleEncodingValidation :: Property
prop_traceSkipsDoubleEncodingValidation = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { rawPathInfo = "/session/%252e%252e%252f"
                    , pathInfo = ["session", "%2e%2e%2f"]
                    , requestMethod = methodTrace
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status405 [] "Method Not Allowed"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectDoubleEncodedPaths passThrough req respond
        takeMVar statusRef
    statusCode === status405

-- ═══════════════════════════════════════════════════════════════════════════
-- Require Content-Type Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: POST with body but no Content-Type is rejected
prop_rejectMissingContentType :: Property
prop_rejectMissingContentType = property $ do
    statusCode <- evalIO $ do
        bodyReadRef <- newIORef False
        let body = "{\"test\":true}"
        let bodyChunks = do
                wasRead <- readIORef bodyReadRef
                if wasRead
                    then pure ""
                    else do
                        writeIORef bodyReadRef True
                        pure body
        let reqBase =
                defaultRequest
                    { requestMethod = methodPost
                    , requestBodyLength = KnownLength (fromIntegral $ BS.length body)
                    , requestHeaders = [] -- No Content-Type!
                    , pathInfo = ["session"]
                    }
        let req = setRequestBodyChunks bodyChunks reqBase
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- requireContentType passThrough req respond
        takeMVar statusRef
    statusCode === status415

-- | Property: POST with body and Content-Type passes through
prop_allowWithContentType :: Property
prop_allowWithContentType = property $ do
    statusCode <- evalIO $ do
        bodyReadRef <- newIORef False
        let body = "{\"test\":true}"
        let bodyChunks = do
                wasRead <- readIORef bodyReadRef
                if wasRead
                    then pure ""
                    else do
                        writeIORef bodyReadRef True
                        pure body
        let reqBase =
                defaultRequest
                    { requestMethod = methodPost
                    , requestBodyLength = KnownLength (fromIntegral $ BS.length body)
                    , requestHeaders = [(hContentType, "application/json")]
                    , pathInfo = ["session"]
                    }
        let req = setRequestBodyChunks bodyChunks reqBase
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- requireContentType passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Property: POST with empty body (no Content-Length) doesn't require Content-Type
prop_allowEmptyBodyNoContentType :: Property
prop_allowEmptyBodyNoContentType = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodPost
                    , requestBodyLength = KnownLength 0
                    , requestHeaders = [] -- No Content-Type
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- requireContentType passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Property: TRACE skips Content-Type requirement
prop_traceSkipsContentTypeRequirement :: Property
prop_traceSkipsContentTypeRequirement = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodTrace
                    , requestBodyLength = KnownLength 100 -- Has body
                    , requestHeaders = [] -- No Content-Type
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status405 [] "Method Not Allowed"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- requireContentType passThrough req respond
        takeMVar statusRef
    statusCode === status405

-- ═══════════════════════════════════════════════════════════════════════════
-- URL-Encoded Null Byte Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: URL-encoded null byte (%00) in path is rejected
prop_rejectUrlEncodedNullByte :: Property
prop_rejectUrlEncodedNullByte = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { rawPathInfo = "/session/abc%00def"
                    , pathInfo = ["session", "abc"]
                    , requestMethod = methodGet
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectNullBytePaths passThrough req respond
        takeMVar statusRef
    statusCode === status400

-- ═══════════════════════════════════════════════════════════════════════════
-- HEAD Method Rejection Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: HEAD requests are rejected with 405
prop_rejectHeadMethod :: Property
prop_rejectHeadMethod = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodHead
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectHeadMethod passThrough req respond
        takeMVar statusRef
    statusCode === status405

-- | Property: HEAD rejection includes Allow header
prop_headRejectionHasAllowHeader :: Property
prop_headRejectionHasAllowHeader = property $ do
    result <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodHead
                    , pathInfo = ["session"]
                    }
        responseRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let headers = extractHeaders resp
                putMVar responseRef headers
                pure ResponseReceived
        _ <- rejectHeadMethod passThrough req respond
        takeMVar responseRef
    -- Should have Allow header
    let hasAllow = any (\(h, _) -> h == hAllow) result
    hasAllow === True

-- | Property: GET requests pass through (not rejected as HEAD)
prop_getPassesThrough :: Property
prop_getPassesThrough = property $ do
    statusCode <- evalIO $ do
        let req =
                defaultRequest
                    { requestMethod = methodGet
                    , pathInfo = ["session"]
                    }
        statusRef <- newEmptyMVar
        let passThrough _req respond = respond $ responseLBS status200 [] "OK"
        let respond resp = do
                let (s, _, _) = responseToStream resp
                putMVar statusRef s
                pure ResponseReceived
        _ <- rejectHeadMethod passThrough req respond
        takeMVar statusRef
    statusCode === status200

-- | Helper: Extract headers from response
extractHeaders :: Response -> [Header]
extractHeaders resp =
    let (_, headers, _) = responseToStream resp
     in headers

-- | Allow header name
hAllow :: HeaderName
hAllow = "Allow"

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
        , testGroup
            "addAllowHeader (RFC 9110)"
            [ testProperty "405 responses get Allow header" prop_addAllowHeaderOn405
            , testProperty "200 responses unchanged" prop_noAllowHeaderOn200
            , testProperty "Existing Allow header preserved" prop_preserveExistingAllowHeader
            ]
        , testGroup
            "rejectDuplicateQueryParams"
            [ testProperty "Duplicate params rejected" prop_rejectDuplicateQueryParams
            , testProperty "Unique params pass through" prop_allowUniqueQueryParams
            , testProperty "Empty query string passes" prop_allowEmptyQueryString
            ]
        , testGroup
            "rejectUnknownQueryParams"
            [ testProperty "Unknown params rejected" prop_rejectUnknownQueryParam
            , testProperty "Known params pass through" prop_allowKnownQueryParam
            , testProperty "Session list params allowed" prop_allowSessionListParams
            ]
        , testGroup
            "limitJsonDepth"
            [ testProperty "Deeply nested JSON rejected" prop_rejectDeeplyNestedJson
            , testProperty "Shallow JSON passes through" prop_allowShallowJson
            , testProperty "GET requests skip check" prop_skipDepthCheckForGet
            ]
        , testGroup
            "unsupported method passthrough"
            [ testProperty "TRACE skips rejectEmptyPathSegments" prop_traceSkipsEmptyPathSegments
            , testProperty "TRACE skips rejectDuplicateQueryParams" prop_traceSkipsDuplicateQueryParams
            , testProperty "TRACE skips rejectUnknownQueryParams" prop_traceSkipsUnknownQueryParams
            , testProperty "TRACE skips limitJsonDepth" prop_traceSkipsJsonDepth
            ]
        , testGroup
            "rejectInvalidCharset"
            [ testProperty "Non-UTF-8 charset rejected" prop_rejectNonUtf8Charset
            , testProperty "UTF-8 charset accepted" prop_allowUtf8Charset
            , testProperty "No charset parameter accepted" prop_allowNoCharset
            , testProperty "TRACE skips charset validation" prop_traceSkipsCharsetValidation
            ]
        , testGroup
            "rejectNullBytePaths"
            [ testProperty "Null byte in path rejected" prop_rejectNullBytePath
            , testProperty "URL-encoded null byte rejected" prop_rejectUrlEncodedNullByte
            , testProperty "Normal paths pass through" prop_allowNormalPath
            , testProperty "TRACE skips null byte validation" prop_traceSkipsNullByteValidation
            ]
        , testGroup
            "rejectDoubleEncodedPaths"
            [ testProperty "Double-encoded paths rejected" prop_rejectDoubleEncodedPath
            , testProperty "Normal encoded paths accepted" prop_allowNormalEncodedPath
            , testProperty "TRACE skips double encoding validation" prop_traceSkipsDoubleEncodingValidation
            ]
        , testGroup
            "requireContentType"
            [ testProperty "Missing Content-Type with body rejected" prop_rejectMissingContentType
            , testProperty "With Content-Type passes through" prop_allowWithContentType
            , testProperty "Empty body without Content-Type allowed" prop_allowEmptyBodyNoContentType
            , testProperty "TRACE skips Content-Type requirement" prop_traceSkipsContentTypeRequirement
            ]
        , testGroup
            "rejectHeadMethod"
            [ testProperty "HEAD requests rejected with 405" prop_rejectHeadMethod
            , testProperty "HEAD rejection includes Allow header" prop_headRejectionHasAllowHeader
            , testProperty "GET requests pass through" prop_getPassesThrough
            ]
        ]
