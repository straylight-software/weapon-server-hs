{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.ProxyTypesProps
Description : Property tests for Proxy.Types

Property-based tests for the proxy types module, including:

* JSON serialization round-trips for all types
* Token arithmetic correctness
* Header conversion utilities
* Configuration defaults
-}
module Property.ProxyTypesProps where

import Data.Aeson (decode, encode)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
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

-- ============================================================================
-- Properties
-- ============================================================================

prop_requestLogRoundtrip :: Property
prop_requestLogRoundtrip = property $ do
    rl <- forAll genRequestLog
    let json = encode rl
    case decode json of
        Nothing -> failure
        Just rl' -> rl === rl'

prop_responseLogRoundtrip :: Property
prop_responseLogRoundtrip = property $ do
    rl <- forAll genResponseLog
    let json = encode rl
    case decode json of
        Nothing -> failure
        Just rl' -> rl === rl'

prop_tokenUsageRoundtrip :: Property
prop_tokenUsageRoundtrip = property $ do
    tu <- forAll genTokenUsage
    let json = encode tu
    case decode json of
        Nothing -> failure
        Just tu' -> tu === tu'

prop_logEntryRoundtrip :: Property
prop_logEntryRoundtrip = property $ do
    le <- forAll genLogEntry
    let json = encode le
    case decode json of
        Nothing -> failure
        Just le' -> le === le'

-- | Property: defaultProxyConfig sets correct port
prop_defaultProxyConfigPort :: Property
prop_defaultProxyConfigPort = property $ do
    logDir <- forAll $ Gen.list (Range.linear 1 20) Gen.alphaNum
    let cfg = defaultProxyConfig logDir
    pcPort cfg === 8888

-- | Property: defaultProxyConfig sets correct max body log
prop_defaultProxyConfigMaxBodyLog :: Property
prop_defaultProxyConfigMaxBodyLog = property $ do
    logDir <- forAll $ Gen.list (Range.linear 1 20) Gen.alphaNum
    let cfg = defaultProxyConfig logDir
    pcMaxBodyLog cfg === 1024 * 1024

-- | Property: defaultProxyConfig allows all hosts by default
prop_defaultProxyConfigAllowedHosts :: Property
prop_defaultProxyConfigAllowedHosts = property $ do
    logDir <- forAll $ Gen.list (Range.linear 1 20) Gen.alphaNum
    let cfg = defaultProxyConfig logDir
    pcAllowedHosts cfg === Nothing

-- | Property: ResponseLog status is valid HTTP status
prop_responseLogValidStatus :: Property
prop_responseLogValidStatus = property $ do
    rl <- forAll genResponseLog
    assert $ rsStatus rl >= 100
    assert $ rsStatus rl < 600

-- | Property: TokenUsage tokens are non-negative
prop_tokenUsageNonNegative :: Property
prop_tokenUsageNonNegative = property $ do
    tu <- forAll genTokenUsage
    assert $ tuInputTokens tu >= 0
    assert $ tuOutputTokens tu >= 0

-- | Property: LogEntry duration is non-negative
prop_logEntryDurationNonNegative :: Property
prop_logEntryDurationNonNegative = property $ do
    le <- forAll genLogEntry
    assert $ leDuration le >= 0

-- ============================================================================
-- Token Arithmetic Properties
-- ============================================================================

-- | Property: addTokens is commutative for token counts
prop_addTokensCommutative :: Property
prop_addTokensCommutative = property $ do
    t1 <- forAll genTokenUsage
    t2 <- forAll genTokenUsage
    let r1 = addTokens t1 t2
        r2 = addTokens t2 t1
    -- Token counts should be the same regardless of order
    tuInputTokens r1 === tuInputTokens r2
    tuOutputTokens r1 === tuOutputTokens r2

-- | Property: addTokens is associative for token counts
prop_addTokensAssociative :: Property
prop_addTokensAssociative = property $ do
    t1 <- forAll genTokenUsage
    t2 <- forAll genTokenUsage
    t3 <- forAll genTokenUsage
    let r1 = addTokens (addTokens t1 t2) t3
        r2 = addTokens t1 (addTokens t2 t3)
    -- Token counts should be the same regardless of grouping
    tuInputTokens r1 === tuInputTokens r2
    tuOutputTokens r1 === tuOutputTokens r2

-- | Property: addTokens correctly sums input tokens
prop_addTokensSumsInput :: Property
prop_addTokensSumsInput = property $ do
    t1 <- forAll genTokenUsage
    t2 <- forAll genTokenUsage
    let result = addTokens t1 t2
    tuInputTokens result === tuInputTokens t1 + tuInputTokens t2

-- | Property: addTokens correctly sums output tokens
prop_addTokensSumsOutput :: Property
prop_addTokensSumsOutput = property $ do
    t1 <- forAll genTokenUsage
    t2 <- forAll genTokenUsage
    let result = addTokens t1 t2
    tuOutputTokens result === tuOutputTokens t1 + tuOutputTokens t2

-- | Property: addTokens preserves provider from first argument
prop_addTokensPreservesProvider :: Property
prop_addTokensPreservesProvider = property $ do
    t1 <- forAll genTokenUsage
    t2 <- forAll genTokenUsage
    let result = addTokens t1 t2
    tuProvider result === tuProvider t1

-- | Property: addTokens preserves model from first argument
prop_addTokensPreservesModel :: Property
prop_addTokensPreservesModel = property $ do
    t1 <- forAll genTokenUsage
    t2 <- forAll genTokenUsage
    let result = addTokens t1 t2
    tuModel result === tuModel t1

-- | Property: addTokens with zero tokens is identity on counts
prop_addTokensZeroIdentity :: Property
prop_addTokensZeroIdentity = property $ do
    t <- forAll genTokenUsage
    let zero =
            TokenUsage
                { tuProvider = "test"
                , tuModel = "test"
                , tuInputTokens = 0
                , tuOutputTokens = 0
                , tuCacheRead = Nothing
                , tuCacheWrite = Nothing
                }
    let result = addTokens t zero
    tuInputTokens result === tuInputTokens t
    tuOutputTokens result === tuOutputTokens t

-- | Property: addTokens combines cache values with applicative semantics
prop_addTokensCacheCombines :: Property
prop_addTokensCacheCombines = property $ do
    cacheRead1 <- forAll $ Gen.int (Range.linear 0 1000)
    cacheRead2 <- forAll $ Gen.int (Range.linear 0 1000)
    cacheWrite1 <- forAll $ Gen.int (Range.linear 0 1000)
    cacheWrite2 <- forAll $ Gen.int (Range.linear 0 1000)
    let t1 =
            TokenUsage
                { tuProvider = "anthropic"
                , tuModel = "claude-3"
                , tuInputTokens = 100
                , tuOutputTokens = 50
                , tuCacheRead = Just cacheRead1
                , tuCacheWrite = Just cacheWrite1
                }
        t2 =
            TokenUsage
                { tuProvider = "anthropic"
                , tuModel = "claude-3"
                , tuInputTokens = 200
                , tuOutputTokens = 75
                , tuCacheRead = Just cacheRead2
                , tuCacheWrite = Just cacheWrite2
                }
        result = addTokens t1 t2
    tuCacheRead result === Just (cacheRead1 + cacheRead2)
    tuCacheWrite result === Just (cacheWrite1 + cacheWrite2)

-- | Property: addTokens returns Nothing for cache when either is Nothing
prop_addTokensCacheNothing :: Property
prop_addTokensCacheNothing = property $ do
    cacheRead <- forAll $ Gen.int (Range.linear 0 1000)
    let t1 =
            TokenUsage
                { tuProvider = "openai"
                , tuModel = "gpt-4"
                , tuInputTokens = 100
                , tuOutputTokens = 50
                , tuCacheRead = Just cacheRead
                , tuCacheWrite = Nothing
                }
        t2 =
            TokenUsage
                { tuProvider = "openai"
                , tuModel = "gpt-4"
                , tuInputTokens = 200
                , tuOutputTokens = 75
                , tuCacheRead = Nothing
                , tuCacheWrite = Nothing
                }
        result = addTokens t1 t2
    tuCacheRead result === Nothing
    tuCacheWrite result === Nothing

-- ============================================================================
-- Header Utilities Properties
-- ============================================================================

-- | Property: isHopHeader returns True for all known hop headers
prop_isHopHeaderKnownHeaders :: Property
prop_isHopHeaderKnownHeaders = withTests 1 $ property $ do
    let hopHeaders =
            [ "connection"
            , "keep-alive"
            , "proxy-authenticate"
            , "proxy-authorization"
            , "te"
            , "trailer"
            , "transfer-encoding"
            , "upgrade"
            ]
    mapM_ (assert . isHopHeader) hopHeaders

-- | Property: isHopHeader returns False for content headers
prop_isHopHeaderContentHeaders :: Property
prop_isHopHeaderContentHeaders = withTests 1 $ property $ do
    let contentHeaders =
            [ "content-type"
            , "content-length"
            , "content-encoding"
            , "accept"
            , "authorization"
            , "host"
            , "user-agent"
            ]
    mapM_ (assert . not . isHopHeader) contentHeaders

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Proxy.Types Property Tests"
        [ testGroup
            "Serialization"
            [ testProperty "RequestLog round-trip" prop_requestLogRoundtrip
            , testProperty "ResponseLog round-trip" prop_responseLogRoundtrip
            , testProperty "TokenUsage round-trip" prop_tokenUsageRoundtrip
            , testProperty "LogEntry round-trip" prop_logEntryRoundtrip
            ]
        , testGroup
            "Configuration"
            [ testProperty "defaultProxyConfig port" prop_defaultProxyConfigPort
            , testProperty "defaultProxyConfig maxBodyLog" prop_defaultProxyConfigMaxBodyLog
            , testProperty "defaultProxyConfig allowedHosts" prop_defaultProxyConfigAllowedHosts
            ]
        , testGroup
            "Validation"
            [ testProperty "ResponseLog valid status" prop_responseLogValidStatus
            , testProperty "TokenUsage non-negative" prop_tokenUsageNonNegative
            , testProperty "LogEntry duration non-negative" prop_logEntryDurationNonNegative
            ]
        , testGroup
            "Token Arithmetic"
            [ testProperty "addTokens commutative" prop_addTokensCommutative
            , testProperty "addTokens associative" prop_addTokensAssociative
            , testProperty "addTokens sums input" prop_addTokensSumsInput
            , testProperty "addTokens sums output" prop_addTokensSumsOutput
            , testProperty "addTokens preserves provider" prop_addTokensPreservesProvider
            , testProperty "addTokens preserves model" prop_addTokensPreservesModel
            , testProperty "addTokens zero identity" prop_addTokensZeroIdentity
            , testProperty "addTokens cache combines" prop_addTokensCacheCombines
            , testProperty "addTokens cache nothing" prop_addTokensCacheNothing
            ]
        , testGroup
            "Header Utilities"
            [ testProperty "isHopHeader known headers" prop_isHopHeaderKnownHeaders
            , testProperty "isHopHeader content headers" prop_isHopHeaderContentHeaders
            ]
        ]
