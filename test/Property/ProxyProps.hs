{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.ProxyProps
Description : Property tests for Proxy.Proxy pure functions

Property-based tests for the pure helper functions in Proxy.Proxy, including:

* URL building and parsing
* Body truncation
* Host:port parsing
* Session log filtering
* Token parsing from JSON
-}
module Property.ProxyProps where

import Data.Aeson (Value (..), object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proxy.Proxy
import Proxy.Types
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 50) Gen.alphaNum

genUTCTime :: Gen UTCTime
genUTCTime = do
    year <- Gen.integral (Range.linear 2020 2030)
    month <- Gen.integral (Range.linear 1 12)
    day <- Gen.integral (Range.linear 1 28)
    secs <- Gen.integral (Range.linear 0 86399)
    pure $ UTCTime (fromGregorian year month day) (secondsToDiffTime secs)

genHeaders :: Gen (Map.Map Text Text)
genHeaders = Map.fromList <$> Gen.list (Range.linear 0 5) ((,) <$> genNonEmptyText <*> genText)

genRequestLog :: Gen RequestLog
genRequestLog =
    RequestLog
        <$> genHeaders
        <*> Gen.maybe genText
        <*> Gen.int (Range.linear 0 1000000)

genResponseLog :: Gen ResponseLog
genResponseLog =
    ResponseLog
        <$> Gen.int (Range.linear 100 599)
        <*> genHeaders
        <*> Gen.maybe genText
        <*> Gen.int (Range.linear 0 1000000)
        <*> Gen.bool

genTokenUsage :: Gen TokenUsage
genTokenUsage =
    TokenUsage
        <$> Gen.element ["anthropic", "openai", "openrouter"]
        <*> genNonEmptyText
        <*> Gen.int (Range.linear 0 100000)
        <*> Gen.int (Range.linear 0 100000)
        <*> Gen.maybe (Gen.int (Range.linear 0 50000))
        <*> Gen.maybe (Gen.int (Range.linear 0 50000))

genLogEntry :: Gen LogEntry
genLogEntry =
    LogEntry
        <$> genUTCTime
        <*> genNonEmptyText
        <*> genNonEmptyText
        <*> Gen.element ["GET", "POST", "PUT", "DELETE"]
        <*> genNonEmptyText
        <*> genNonEmptyText
        <*> genRequestLog
        <*> Gen.maybe genResponseLog
        <*> Gen.maybe genTokenUsage
        <*> Gen.double (Range.linearFrac 0 10000)

-- | Generate a valid hostname
genHostname :: Gen String
genHostname = do
    parts <- Gen.nonEmpty (Range.linear 1 4) $ Gen.list (Range.linear 1 10) Gen.alpha
    -- Use NonEmpty's safe init/last to avoid partial functions
    let partsInit = NE.init parts
        partsLast = NE.last parts
    pure $ concat $ map (++ ".") partsInit ++ [partsLast]

-- | Generate a valid port number
genPort :: Gen Int
genPort = Gen.int (Range.linear 1 65535)

-- ============================================================================
-- parseHostPort Properties
-- ============================================================================

-- | Property: parseHostPort correctly parses valid host:port strings
prop_parseHostPortValid :: Property
prop_parseHostPortValid = property $ do
    host <- forAll genHostname
    port <- forAll genPort
    let target = host ++ ":" ++ show port
    parseHostPort target === Just (host, port)

-- | Property: parseHostPort handles leading slash
prop_parseHostPortWithSlash :: Property
prop_parseHostPortWithSlash = property $ do
    host <- forAll genHostname
    port <- forAll genPort
    let target = "/" ++ host ++ ":" ++ show port
    parseHostPort target === Just (host, port)

-- | Property: parseHostPort returns Nothing for missing port
prop_parseHostPortMissingPort :: Property
prop_parseHostPortMissingPort = property $ do
    host <- forAll genHostname
    parseHostPort host === Nothing

-- | Property: parseHostPort returns Nothing for invalid port
prop_parseHostPortInvalidPort :: Property
prop_parseHostPortInvalidPort = property $ do
    host <- forAll genHostname
    invalidPort <- forAll $ Gen.list (Range.linear 1 5) Gen.alpha
    let target = host ++ ":" ++ invalidPort
    parseHostPort target === Nothing

-- | Property: parseHostPort returns Nothing for port with trailing junk
prop_parseHostPortTrailingJunk :: Property
prop_parseHostPortTrailingJunk = property $ do
    host <- forAll genHostname
    port <- forAll genPort
    junk <- forAll $ Gen.list (Range.linear 1 5) Gen.alpha
    let target = host ++ ":" ++ show port ++ junk
    parseHostPort target === Nothing

-- ============================================================================
-- buildRequestUrl Properties
-- ============================================================================

-- | Property: buildRequestUrl returns path as-is if it starts with http
prop_buildRequestUrlHttpPrefix :: Property
prop_buildRequestUrlHttpPrefix = property $ do
    url <- forAll $ Gen.element ["http://example.com/path", "https://api.example.com/v1"]
    let path = TE.encodeUtf8 $ T.pack url
    buildRequestUrl "ignored.host" path === url

-- | Property: buildRequestUrl prepends http:// and host for relative paths
prop_buildRequestUrlRelativePath :: Property
prop_buildRequestUrlRelativePath = property $ do
    host <- forAll $ Gen.text (Range.linear 5 30) Gen.alphaNum
    pathPart <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let path = TE.encodeUtf8 $ "/" <> pathPart
    let expected = "http://" ++ T.unpack host ++ "/" ++ T.unpack pathPart
    buildRequestUrl host path === expected

-- | Property: buildRequestUrl handles empty path
prop_buildRequestUrlEmptyPath :: Property
prop_buildRequestUrlEmptyPath = property $ do
    host <- forAll $ Gen.text (Range.linear 5 30) Gen.alphaNum
    let expected = "http://" ++ T.unpack host
    buildRequestUrl host "" === expected

-- ============================================================================
-- truncateBody Properties
-- ============================================================================

-- | Property: truncateBody returns Nothing for empty body
prop_truncateBodyEmpty :: Property
prop_truncateBodyEmpty = property $ do
    maxSize <- forAll $ Gen.int (Range.linear 1 1000)
    truncateBody maxSize "" === Nothing

-- | Property: truncateBody returns full body if under max size
prop_truncateBodyUnderMax :: Property
prop_truncateBodyUnderMax = property $ do
    bodyText <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    let body = LBS.fromStrict $ TE.encodeUtf8 bodyText
        maxSize = fromIntegral (LBS.length body) + 100
    truncateBody maxSize body === Just bodyText

-- | Property: truncateBody truncates body if over max size
prop_truncateBodyOverMax :: Property
prop_truncateBodyOverMax = property $ do
    bodyText <- forAll $ Gen.text (Range.linear 20 100) Gen.alphaNum
    let body = LBS.fromStrict $ TE.encodeUtf8 bodyText
        maxSize = 10
        result = truncateBody maxSize body
    case result of
        Nothing -> failure
        Just t -> do
            -- Check byte length to avoid O(n) T.length; maxSize is in bytes
            assert $ BS.length (TE.encodeUtf8 t) <= maxSize
            -- The truncated text should be a prefix of the original
            assert $ T.isPrefixOf t bodyText

-- | Property: truncateBody result length is at most maxSize bytes
prop_truncateBodyMaxSizeBytes :: Property
prop_truncateBodyMaxSizeBytes = property $ do
    bodyText <- forAll $ Gen.text (Range.linear 1 200) Gen.alphaNum
    maxSize <- forAll $ Gen.int (Range.linear 1 100)
    let body = LBS.fromStrict $ TE.encodeUtf8 bodyText
        result = truncateBody maxSize body
    case result of
        Nothing -> success
        Just t -> assert $ BS.length (TE.encodeUtf8 t) <= maxSize

-- ============================================================================
-- isStreamingResponse Properties
-- ============================================================================

-- | Property: isStreamingResponse detects SSE content type
prop_isStreamingResponseSSE :: Property
prop_isStreamingResponseSSE = withTests 1 $ property $ do
    let headers = [("content-type", "text/event-stream")]
    assert $ isStreamingResponse headers

-- | Property: isStreamingResponse detects SSE with charset
prop_isStreamingResponseSSECharset :: Property
prop_isStreamingResponseSSECharset = withTests 1 $ property $ do
    let headers = [("content-type", "text/event-stream; charset=utf-8")]
    assert $ isStreamingResponse headers

-- | Property: isStreamingResponse returns False for JSON
prop_isStreamingResponseJSON :: Property
prop_isStreamingResponseJSON = withTests 1 $ property $ do
    let headers = [("content-type", "application/json")]
    assert $ not $ isStreamingResponse headers

-- | Property: isStreamingResponse returns False for missing content-type
prop_isStreamingResponseMissing :: Property
prop_isStreamingResponseMissing = withTests 1 $ property $ do
    let headers = [("accept", "application/json")]
    assert $ not $ isStreamingResponse headers

-- ============================================================================
-- filterSessionLogs Properties
-- ============================================================================

-- | Property: filterSessionLogs returns only matching entries
prop_filterSessionLogsMatches :: Property
prop_filterSessionLogsMatches = property $ do
    targetSession <- forAll genNonEmptyText
    entries <- forAll $ Gen.list (Range.linear 0 20) genLogEntry
    let filtered = filterSessionLogs targetSession entries
    -- All filtered entries should have the target session ID
    mapM_ (\e -> leSessionId e === targetSession) filtered

-- | Property: filterSessionLogs preserves order
prop_filterSessionLogsOrder :: Property
prop_filterSessionLogsOrder = property $ do
    targetSession <- forAll genNonEmptyText
    -- Generate entries with known session IDs
    entries <- forAll $ Gen.list (Range.linear 1 10) $ do
        e <- genLogEntry
        sessionId <- Gen.element [targetSession, "other_session"]
        pure e{leSessionId = sessionId}
    let filtered = filterSessionLogs targetSession entries
        -- Get matching entries from original list (expected result)
        expected = filter (\e -> leSessionId e == targetSession) entries
    -- Verify filtered matches expected (same elements, same order)
    filtered === expected

-- | Property: filterSessionLogs returns empty for non-matching session
prop_filterSessionLogsEmpty :: Property
prop_filterSessionLogsEmpty = property $ do
    entries <- forAll $ Gen.list (Range.linear 1 10) genLogEntry
    -- Use a session ID that definitely doesn't exist
    let nonExistentSession = "definitely_not_a_real_session_id_12345"
        filtered = filterSessionLogs nonExistentSession entries
    filtered === []

-- | Property: filterSessionLogs returns all entries if all match
prop_filterSessionLogsAll :: Property
prop_filterSessionLogsAll = property $ do
    targetSession <- forAll genNonEmptyText
    baseEntries <- forAll $ Gen.list (Range.linear 1 10) genLogEntry
    let entries = [e{leSessionId = targetSession} | e <- baseEntries]
        filtered = filterSessionLogs targetSession entries
    -- When all entries match, filtered should equal entries
    filtered === entries

-- ============================================================================
-- parseTokensFromJson Properties
-- ============================================================================

-- | Property: parseTokensFromJson parses valid Anthropic response
prop_parseTokensAnthropic :: Property
prop_parseTokensAnthropic = property $ do
    inputTokens <- forAll $ Gen.int (Range.linear 0 10000)
    outputTokens <- forAll $ Gen.int (Range.linear 0 10000)
    cacheRead <- forAll $ Gen.int (Range.linear 0 5000)
    cacheWrite <- forAll $ Gen.int (Range.linear 0 5000)
    model <- forAll $ Gen.text (Range.linear 5 30) Gen.alphaNum
    let json =
            object
                [ "model" .= model
                , "usage"
                    .= object
                        [ "input_tokens" .= inputTokens
                        , "output_tokens" .= outputTokens
                        , "cache_read_input_tokens" .= cacheRead
                        , "cache_creation_input_tokens" .= cacheWrite
                        ]
                ]
    case parseTokensFromJson "api.anthropic.com" json of
        Nothing -> failure
        Just tu -> do
            tuProvider tu === "anthropic"
            tuModel tu === model
            tuInputTokens tu === inputTokens
            tuOutputTokens tu === outputTokens
            tuCacheRead tu === Just cacheRead
            tuCacheWrite tu === Just cacheWrite

-- | Property: parseTokensFromJson parses valid OpenAI response
prop_parseTokensOpenAI :: Property
prop_parseTokensOpenAI = property $ do
    promptTokens <- forAll $ Gen.int (Range.linear 0 10000)
    completionTokens <- forAll $ Gen.int (Range.linear 0 10000)
    model <- forAll $ Gen.text (Range.linear 5 30) Gen.alphaNum
    let json =
            object
                [ "model" .= model
                , "usage"
                    .= object
                        [ "prompt_tokens" .= promptTokens
                        , "completion_tokens" .= completionTokens
                        ]
                ]
    case parseTokensFromJson "api.openai.com" json of
        Nothing -> failure
        Just tu -> do
            tuProvider tu === "openai"
            tuModel tu === model
            tuInputTokens tu === promptTokens
            tuOutputTokens tu === completionTokens
            tuCacheRead tu === Nothing
            tuCacheWrite tu === Nothing

-- | Property: parseTokensFromJson returns Nothing for unknown provider
prop_parseTokensUnknownProvider :: Property
prop_parseTokensUnknownProvider = property $ do
    let json = object ["model" .= ("test" :: Text), "usage" .= object []]
    parseTokensFromJson "api.unknown.com" json === Nothing

-- | Property: parseTokensFromJson returns Nothing for non-object JSON
prop_parseTokensNonObject :: Property
prop_parseTokensNonObject = property $ do
    host <- forAll $ Gen.element ["api.anthropic.com", "api.openai.com"]
    parseTokensFromJson host (String "not an object") === Nothing
    parseTokensFromJson host (Number 42) === Nothing
    parseTokensFromJson host (Bool True) === Nothing
    parseTokensFromJson host Null === Nothing

-- | Property: parseTokensFromJson returns Nothing for missing usage field
prop_parseTokensMissingUsage :: Property
prop_parseTokensMissingUsage = property $ do
    model <- forAll $ Gen.text (Range.linear 5 30) Gen.alphaNum
    let json = object ["model" .= model]
    parseTokensFromJson "api.anthropic.com" json === Nothing
    parseTokensFromJson "api.openai.com" json === Nothing

-- ============================================================================
-- Test Tree
-- ============================================================================

tests :: TestTree
tests =
    testGroup
        "Proxy.Proxy Property Tests"
        [ testGroup
            "parseHostPort"
            [ testProperty "parses valid host:port" prop_parseHostPortValid
            , testProperty "handles leading slash" prop_parseHostPortWithSlash
            , testProperty "returns Nothing for missing port" prop_parseHostPortMissingPort
            , testProperty "returns Nothing for invalid port" prop_parseHostPortInvalidPort
            , testProperty "returns Nothing for trailing junk" prop_parseHostPortTrailingJunk
            ]
        , testGroup
            "buildRequestUrl"
            [ testProperty "returns http URL as-is" prop_buildRequestUrlHttpPrefix
            , testProperty "prepends host for relative paths" prop_buildRequestUrlRelativePath
            , testProperty "handles empty path" prop_buildRequestUrlEmptyPath
            ]
        , testGroup
            "truncateBody"
            [ testProperty "returns Nothing for empty" prop_truncateBodyEmpty
            , testProperty "returns full body under max" prop_truncateBodyUnderMax
            , testProperty "truncates body over max" prop_truncateBodyOverMax
            , testProperty "respects max size bytes" prop_truncateBodyMaxSizeBytes
            ]
        , testGroup
            "isStreamingResponse"
            [ testProperty "detects SSE content type" prop_isStreamingResponseSSE
            , testProperty "detects SSE with charset" prop_isStreamingResponseSSECharset
            , testProperty "returns False for JSON" prop_isStreamingResponseJSON
            , testProperty "returns False for missing" prop_isStreamingResponseMissing
            ]
        , testGroup
            "filterSessionLogs"
            [ testProperty "returns only matching entries" prop_filterSessionLogsMatches
            , testProperty "preserves order" prop_filterSessionLogsOrder
            , testProperty "returns empty for non-matching" prop_filterSessionLogsEmpty
            , testProperty "returns all if all match" prop_filterSessionLogsAll
            ]
        , testGroup
            "parseTokensFromJson"
            [ testProperty "parses Anthropic format" prop_parseTokensAnthropic
            , testProperty "parses OpenAI format" prop_parseTokensOpenAI
            , testProperty "returns Nothing for unknown provider" prop_parseTokensUnknownProvider
            , testProperty "returns Nothing for non-object" prop_parseTokensNonObject
            , testProperty "returns Nothing for missing usage" prop_parseTokensMissingUsage
            ]
        ]
