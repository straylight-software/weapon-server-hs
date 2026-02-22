{-# LANGUAGE OverloadedStrings #-}

-- | Middleware property tests
module Property.MiddlewareProps where

import Control.Concurrent.MVar
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Middleware (supplyEmptyBody)
import Network.HTTP.Types
import Network.Wai (RequestBodyLength (..), Response, defaultRequest, requestBodyLength, requestHeaders, requestMethod, responseLBS, responseToStream, setRequestBodyChunks, strictRequestBody)
import Network.Wai.Internal (ResponseReceived (..))
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
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Middleware Property Tests"
        [ testProperty "POST with no body gets {} added" prop_supplyEmptyBodyPost
        , testProperty "POST with existing body unchanged" prop_supplyEmptyBodyWithBody
        , testProperty "GET requests unchanged" prop_supplyEmptyBodyGet
        , testProperty "DELETE with no body gets {} added" prop_supplyEmptyBodyDelete
        ]
