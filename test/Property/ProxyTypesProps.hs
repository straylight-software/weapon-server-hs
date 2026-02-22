{-# LANGUAGE OverloadedStrings #-}

-- | Proxy.Types property tests
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

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Proxy.Types Property Tests"
        [ testProperty "RequestLog round-trip" prop_requestLogRoundtrip
        , testProperty "ResponseLog round-trip" prop_responseLogRoundtrip
        , testProperty "TokenUsage round-trip" prop_tokenUsageRoundtrip
        , testProperty "LogEntry round-trip" prop_logEntryRoundtrip
        , testProperty "defaultProxyConfig port" prop_defaultProxyConfigPort
        , testProperty "defaultProxyConfig maxBodyLog" prop_defaultProxyConfigMaxBodyLog
        , testProperty "defaultProxyConfig allowedHosts" prop_defaultProxyConfigAllowedHosts
        , testProperty "ResponseLog valid status" prop_responseLogValidStatus
        , testProperty "TokenUsage non-negative" prop_tokenUsageNonNegative
        , testProperty "LogEntry duration non-negative" prop_logEntryDurationNonNegative
        ]
