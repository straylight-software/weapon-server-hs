{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.OAuthProps
Description : Property tests for OAuth module
Stability   : experimental

Property tests for the Provider.OAuth module, including:

* URL building properties
* Parameter rendering properties
* State generation properties
-}
module Property.OAuthProps where

import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Provider.OAuth qualified as OAuth
import Test.Helpers (listLength)
import Test.Tasty
import Test.Tasty.Hedgehog

-- ═══════════════════════════════════════════════════════════════════════════
-- URL building properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: buildAuthorizeUrl includes state parameter
prop_buildAuthorizeUrlIncludesState :: Property
prop_buildAuthorizeUrlIncludesState = property $ do
    provider <- forAll genText
    state <- forAll genText
    let url = OAuth.buildAuthorizeUrl provider state Nothing []
    assert $ T.isInfixOf ("state=" <> state) url
    assert $ T.isInfixOf ("/" <> provider) url

-- | Property: buildAuthorizeUrl includes redirect_uri when provided
prop_buildAuthorizeUrlIncludesRedirect :: Property
prop_buildAuthorizeUrlIncludesRedirect = property $ do
    provider <- forAll genText
    state <- forAll genText
    redirect <- forAll genText
    let url = OAuth.buildAuthorizeUrl provider state (Just redirect) []
    assert $ T.isInfixOf ("redirect_uri=" <> redirect) url

-- | Property: buildAuthorizeUrl includes scopes when provided
prop_buildAuthorizeUrlIncludesScopes :: Property
prop_buildAuthorizeUrlIncludesScopes = property $ do
    provider <- forAll genText
    state <- forAll genText
    scopes <- forAll $ Gen.list (Range.linear 1 5) genText
    let url = OAuth.buildAuthorizeUrl provider state Nothing scopes
    assert $ T.isInfixOf "scope=" url
    -- Check that all scopes are in the URL (joined with commas)
    let scopeStr = T.intercalate "," scopes
    assert $ T.isInfixOf scopeStr url

-- | Property: buildAuthorizeUrl omits scope param when scopes empty
prop_buildAuthorizeUrlNoScopesWhenEmpty :: Property
prop_buildAuthorizeUrlNoScopesWhenEmpty = property $ do
    provider <- forAll genText
    state <- forAll genText
    let url = OAuth.buildAuthorizeUrl provider state Nothing []
    assert $ not $ T.isInfixOf "scope=" url

-- | Property: buildAuthorizeUrl starts with correct base URL
prop_buildAuthorizeUrlBaseUrl :: Property
prop_buildAuthorizeUrlBaseUrl = property $ do
    provider <- forAll genText
    state <- forAll genText
    let url = OAuth.buildAuthorizeUrl provider state Nothing []
    assert $ T.isPrefixOf "https://auth.opencode.ai/oauth/" url

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure helper properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: renderParams joins params with &
prop_renderParamsJoins :: Property
prop_renderParamsJoins = property $ do
    params <- forAll $ Gen.list (Range.linear 2 5) ((,) <$> genText <*> genText)
    let rendered = OAuth.renderParams params
    -- Should have (n-1) ampersands for n params
    let ampersands = T.count "&" rendered
    ampersands === listLength params - 1

-- | Property: renderParams format is key=value
prop_renderParamsFormat :: Property
prop_renderParamsFormat = property $ do
    key <- forAll genText
    value <- forAll genText
    let rendered = OAuth.renderParams [(key, value)]
    rendered === (key <> "=" <> value)

-- | Property: renderParams empty list gives empty string
prop_renderParamsEmpty :: Property
prop_renderParamsEmpty = property $ do
    OAuth.renderParams [] === ""

-- | Property: buildParams always includes state
prop_buildParamsIncludesState :: Property
prop_buildParamsIncludesState = property $ do
    state <- forAll genText
    redirect <- forAll $ Gen.maybe genText
    scopes <- forAll $ Gen.list (Range.linear 0 5) genText
    let params = OAuth.buildParams state redirect scopes
    assert $ ("state", state) `elem` params

-- | Property: buildParams includes redirect_uri when provided
prop_buildParamsIncludesRedirect :: Property
prop_buildParamsIncludesRedirect = property $ do
    state <- forAll genText
    redirect <- forAll genText
    scopes <- forAll $ Gen.list (Range.linear 0 5) genText
    let params = OAuth.buildParams state (Just redirect) scopes
    assert $ ("redirect_uri", redirect) `elem` params

-- | Property: buildParams includes scope when scopes non-empty
prop_buildParamsIncludesScopes :: Property
prop_buildParamsIncludesScopes = property $ do
    state <- forAll genText
    scopes <- forAll $ Gen.list (Range.linear 1 5) genText
    let params = OAuth.buildParams state Nothing scopes
    let expectedScope = T.intercalate "," scopes
    assert $ ("scope", expectedScope) `elem` params

-- | Property: buildParams omits scope when scopes empty
prop_buildParamsNoScopesWhenEmpty :: Property
prop_buildParamsNoScopesWhenEmpty = property $ do
    state <- forAll genText
    redirect <- forAll $ Gen.maybe genText
    let params = OAuth.buildParams state redirect []
    assert $ not $ any (\(k, _) -> k == "scope") params

-- ═══════════════════════════════════════════════════════════════════════════
-- State generation properties (IO)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: generateState produces non-empty strings
prop_generateStateNonEmpty :: Property
prop_generateStateNonEmpty = property $ do
    state <- evalIO OAuth.generateState
    assert $ not $ T.null state

-- | Property: generateState produces hexadecimal strings
prop_generateStateHex :: Property
prop_generateStateHex = property $ do
    state <- evalIO OAuth.generateState
    assert $ T.all isHexDigit state
  where
    isHexDigit c = c `elem` ("0123456789abcdef" :: String)

-- | Property: generateState produces different values (probabilistic)
prop_generateStateUnique :: Property
prop_generateStateUnique = property $ do
    state1 <- evalIO OAuth.generateState
    state2 <- evalIO OAuth.generateState
    -- With 64 bits of randomness, collision probability is astronomically low
    assert $ state1 /= state2

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

-- | Generator for simple alphanumeric text (URL-safe)
genText :: Gen Text
genText = Gen.text (Range.linear 1 12) Gen.alphaNum

-- ═══════════════════════════════════════════════════════════════════════════
-- Test tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "OAuth Property Tests"
        [ testGroup
            "URL Building"
            [ testProperty "includes state" prop_buildAuthorizeUrlIncludesState
            , testProperty "includes redirect" prop_buildAuthorizeUrlIncludesRedirect
            , testProperty "includes scopes" prop_buildAuthorizeUrlIncludesScopes
            , testProperty "no scopes when empty" prop_buildAuthorizeUrlNoScopesWhenEmpty
            , testProperty "correct base URL" prop_buildAuthorizeUrlBaseUrl
            ]
        , testGroup
            "renderParams"
            [ testProperty "joins with &" prop_renderParamsJoins
            , testProperty "format is key=value" prop_renderParamsFormat
            , testProperty "empty list" prop_renderParamsEmpty
            ]
        , testGroup
            "buildParams"
            [ testProperty "includes state" prop_buildParamsIncludesState
            , testProperty "includes redirect" prop_buildParamsIncludesRedirect
            , testProperty "includes scopes" prop_buildParamsIncludesScopes
            , testProperty "no scopes when empty" prop_buildParamsNoScopesWhenEmpty
            ]
        , testGroup
            "State Generation"
            [ testProperty "non-empty" prop_generateStateNonEmpty
            , testProperty "hexadecimal" prop_generateStateHex
            , testProperty "unique" prop_generateStateUnique
            ]
        ]
