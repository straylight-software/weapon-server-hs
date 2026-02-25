{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.StorageKeyProps
Description : Property tests for StorageKeys module

Property-based tests for the storage key builder functions.

These tests verify:

  * Key format correctness (namespace, segments)
  * Key/prefix consistency (keys start with their prefix)
  * Builder function behavior
-}
module Property.StorageKeyProps where

import Data.List (isPrefixOf)
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Util.StorageKeys

-- ═══════════════════════════════════════════════════════════════════════════
-- Storage Key Format Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: sessionKey produces ["session", projectId, sessionId]
prop_sessionKeyFormat :: Property
prop_sessionKeyFormat = property $ do
    projectId <- forAll genId
    sid <- forAll genId
    let key = sessionKey projectId sid
    key === ["session", projectId, sid]

-- | Property: sessionPrefix produces ["session", projectId]
prop_sessionPrefixFormat :: Property
prop_sessionPrefixFormat = property $ do
    projectId <- forAll genId
    let prefix = sessionPrefix projectId
    prefix === ["session", projectId]

-- | Property: messageKey produces ["message", sessionId, msgId]
prop_messageKeyFormat :: Property
prop_messageKeyFormat = property $ do
    sid <- forAll genId
    msgId <- forAll genId
    let key = messageKey sid msgId
    key === ["message", sid, msgId]

-- | Property: messagePrefix produces ["message", sessionId]
prop_messagePrefixFormat :: Property
prop_messagePrefixFormat = property $ do
    sid <- forAll genId
    let prefix = messagePrefix sid
    prefix === ["message", sid]

-- | Property: todoKey produces ["todo", sessionId]
prop_todoKeyFormat :: Property
prop_todoKeyFormat = property $ do
    sid <- forAll genId
    let key = todoKey sid
    key === ["todo", sid]

-- | Property: projectKey produces ["project", projectId]
prop_projectKeyFormat :: Property
prop_projectKeyFormat = property $ do
    projectId <- forAll genId
    let key = projectKey projectId
    key === ["project", projectId]

-- | Property: buildKey prepends namespace to segments
prop_buildKeyFormat :: Property
prop_buildKeyFormat = property $ do
    namespace <- forAll genId
    seg1 <- forAll genId
    seg2 <- forAll genId
    let key = buildKey namespace [seg1, seg2]
    key === [namespace, seg1, seg2]

-- | Property: buildPrefixKey produces [namespace, keyId]
prop_buildPrefixKeyFormat :: Property
prop_buildPrefixKeyFormat = property $ do
    namespace <- forAll genId
    keyId <- forAll genId
    let key = buildPrefixKey namespace keyId
    key === [namespace, keyId]

-- | Property: sessionKey and sessionPrefix are consistent
prop_sessionKeyPrefixConsistent :: Property
prop_sessionKeyPrefixConsistent = property $ do
    projectId <- forAll genId
    sid <- forAll genId
    let key = sessionKey projectId sid
    let prefix = sessionPrefix projectId
    -- The key should start with the prefix
    assert $ prefix `isPrefixOf` key

-- | Property: messageKey and messagePrefix are consistent
prop_messageKeyPrefixConsistent :: Property
prop_messageKeyPrefixConsistent = property $ do
    sid <- forAll genId
    msgId <- forAll genId
    let key = messageKey sid msgId
    let prefix = messagePrefix sid
    -- The key should start with the prefix
    assert $ prefix `isPrefixOf` key

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

genId :: Gen Text
genId = Gen.text (Range.linear 1 50) Gen.alphaNum

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Storage Key Property Tests"
        [ testGroup
            "Key Formats"
            [ testProperty "sessionKey format" prop_sessionKeyFormat
            , testProperty "sessionPrefix format" prop_sessionPrefixFormat
            , testProperty "messageKey format" prop_messageKeyFormat
            , testProperty "messagePrefix format" prop_messagePrefixFormat
            , testProperty "todoKey format" prop_todoKeyFormat
            , testProperty "projectKey format" prop_projectKeyFormat
            ]
        , testGroup
            "Helper Functions"
            [ testProperty "buildKey format" prop_buildKeyFormat
            , testProperty "buildPrefixKey format" prop_buildPrefixKeyFormat
            ]
        , testGroup
            "Consistency"
            [ testProperty "sessionKey/Prefix consistent" prop_sessionKeyPrefixConsistent
            , testProperty "messageKey/Prefix consistent" prop_messageKeyPrefixConsistent
            ]
        ]
