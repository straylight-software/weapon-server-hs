{-# LANGUAGE OverloadedStrings #-}

{- | Property-based tests for the Health.Build module.

These tests verify the core invariants of health check construction:

* The healthy field is always True (by definition, a responding server is healthy)
* The version string is preserved exactly
* JSON serialization round-trips correctly
* Edge cases (empty strings, unicode, etc.) are handled correctly
-}
module Property.HealthProps where

import Api (Health (..))
import Data.Aeson (decode, encode)
import Data.Text (Text)
import Data.Text qualified as T
import Health.Build qualified as HealthBuild
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Helpers (genText)
import Test.Tasty
import Test.Tasty.Hedgehog

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

-- | Generate a semantic version string (e.g., "1.2.3", "0.0.1", "10.20.30")
genSemanticVersion :: Gen Text
genSemanticVersion = do
    major <- Gen.int (Range.linear 0 99)
    minor <- Gen.int (Range.linear 0 99)
    patch <- Gen.int (Range.linear 0 99)
    pure $ T.pack $ show major <> "." <> show minor <> "." <> show patch

-- | Generate version strings with optional suffixes (e.g., "1.0.0-alpha", "2.0.0-rc.1")
genVersionWithSuffix :: Gen Text
genVersionWithSuffix = do
    base <- genSemanticVersion
    suffix <- Gen.element ["", "-alpha", "-beta", "-rc.1", "-SNAPSHOT", "+build.123"]
    pure $ base <> suffix

-- | Generate edge-case version strings
genEdgeCaseVersion :: Gen Text
genEdgeCaseVersion =
    Gen.element
        [ ""
        , " "
        , "0.0.0"
        , "999.999.999"
        , "v1.0.0"
        , "1.0.0-beta+exp.sha.5114f85"
        ]

-- | Generate unicode version strings (for internationalization edge cases)
genUnicodeVersion :: Gen Text
genUnicodeVersion =
    Gen.element
        [ "версия-1.0"
        , "バージョン1.0"
        , "版本1.0"
        , "1.0.0-🚀"
        ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Core Properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: buildHealth always returns healthy=True and echoes the version
prop_buildHealth :: Property
prop_buildHealth = property $ do
    version <- forAll genText
    let Health healthy ver = HealthBuild.buildHealth version
    -- Server is always healthy when running
    healthy === True
    -- Version is echoed back
    ver === version

-- | Property: Health JSON encoding round-trips correctly
prop_healthJsonRoundtrip :: Property
prop_healthJsonRoundtrip = property $ do
    version <- forAll genText
    let health = HealthBuild.buildHealth version
    case decode (encode health) of
        Nothing -> failure
        Just health' -> health' === health

-- | Property: buildHealth uses the defaultHealthy constant
prop_buildHealthUsesDefault :: Property
prop_buildHealthUsesDefault = property $ do
    version <- forAll genText
    let Health healthy _ = HealthBuild.buildHealth version
    healthy === HealthBuild.defaultHealthy

-- | Property: defaultHealthy is always True
prop_defaultHealthyIsTrue :: Property
prop_defaultHealthyIsTrue = property $ do
    HealthBuild.defaultHealthy === True

-- ═══════════════════════════════════════════════════════════════════════════
-- Edge Case Properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: Empty version string is handled correctly
prop_emptyVersion :: Property
prop_emptyVersion = property $ do
    let Health healthy ver = HealthBuild.buildHealth ""
    healthy === True
    ver === ""

-- | Property: Semantic versions are preserved exactly
prop_semanticVersions :: Property
prop_semanticVersions = property $ do
    version <- forAll genSemanticVersion
    let Health healthy ver = HealthBuild.buildHealth version
    healthy === True
    ver === version

-- | Property: Version strings with suffixes are preserved
prop_versionWithSuffix :: Property
prop_versionWithSuffix = property $ do
    version <- forAll genVersionWithSuffix
    let Health healthy ver = HealthBuild.buildHealth version
    healthy === True
    ver === version

-- | Property: Edge case versions are handled correctly
prop_edgeCaseVersions :: Property
prop_edgeCaseVersions = property $ do
    version <- forAll genEdgeCaseVersion
    let Health healthy ver = HealthBuild.buildHealth version
    healthy === True
    ver === version

-- | Property: Unicode in version strings is preserved
prop_unicodeVersions :: Property
prop_unicodeVersions = property $ do
    version <- forAll genUnicodeVersion
    let health = HealthBuild.buildHealth version
    -- Round-trip through JSON should preserve unicode
    case decode (encode health) of
        Nothing -> failure
        Just (Health healthy' ver') -> do
            healthy' === True
            ver' === version

-- | Property: Very long version strings are handled
prop_longVersions :: Property
prop_longVersions = property $ do
    version <- forAll $ Gen.text (Range.linear 100 1000) Gen.alphaNum
    let Health healthy ver = HealthBuild.buildHealth version
    healthy === True
    ver === version

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Health Property Tests"
        [ testGroup
            "Core Properties"
            [ testProperty "build health preserves version and sets healthy=True" prop_buildHealth
            , testProperty "health JSON roundtrip" prop_healthJsonRoundtrip
            , testProperty "buildHealth uses defaultHealthy constant" prop_buildHealthUsesDefault
            , testProperty "defaultHealthy is True" prop_defaultHealthyIsTrue
            ]
        , testGroup
            "Edge Cases"
            [ testProperty "empty version string" prop_emptyVersion
            , testProperty "semantic version strings" prop_semanticVersions
            , testProperty "version strings with suffixes" prop_versionWithSuffix
            , testProperty "edge case versions" prop_edgeCaseVersions
            , testProperty "unicode version strings" prop_unicodeVersions
            , testProperty "very long version strings" prop_longVersions
            ]
        ]
