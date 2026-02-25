{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.SessionStatusProps
Description : Property tests for Session.Status types

Tests for session status types including JSON serialization
and convenience constructors.
-}
module Property.SessionStatusProps where

import Data.Aeson (Value (..), decode, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Session.Status (SessionStatus (..), SessionStatusType (..), busy, idle, retry)
import Test.Tasty
import Test.Tasty.Hedgehog

-- | Test that idle status serializes correctly
prop_idleStatusJson :: Property
prop_idleStatusJson = property $ do
    let status = SessionStatus StatusIdle
    case decodeValue (encode status) of
        Object obj -> do
            assert $ KM.member (Key.fromText "type") obj
            case KM.lookup (Key.fromText "type") obj of
                Just (String "idle") -> success
                _otherValue -> failure
        _otherValue -> failure
  where
    decodeValue bytes = fromMaybe Null (decode bytes)

-- | Test that retry status includes all required fields
prop_retryStatusJson :: Property
prop_retryStatusJson = property $ do
    attempt <- forAll $ Gen.int (Range.linear 1 10)
    next <- forAll $ Gen.int (Range.linear 1000 60000)
    let status = SessionStatus (StatusRetry attempt "rate limited" next)
    case decodeValue (encode status) of
        Object obj -> do
            assert $ KM.member (Key.fromText "type") obj
            assert $ KM.member (Key.fromText "attempt") obj
            assert $ KM.member (Key.fromText "message") obj
            assert $ KM.member (Key.fromText "next") obj
        _otherValue -> failure
  where
    decodeValue bytes = fromMaybe Null (decode bytes)

-- | Test that busy status serializes correctly
prop_busyStatusJson :: Property
prop_busyStatusJson = property $ do
    let status = SessionStatus StatusBusy
    case decodeValue (encode status) of
        Object obj -> do
            assert $ KM.member (Key.fromText "type") obj
            case KM.lookup (Key.fromText "type") obj of
                Just (String "busy") -> success
                _otherValue -> failure
        _otherValue -> failure
  where
    decodeValue bytes = fromMaybe Null (decode bytes)

-- | Property: idle convenience constructor creates correct status
prop_idleConstructor :: Property
prop_idleConstructor = property $ do
    let status = idle
    ssType status === StatusIdle

-- | Property: busy convenience constructor creates correct status
prop_busyConstructor :: Property
prop_busyConstructor = property $ do
    let status = busy
    ssType status === StatusBusy

-- | Property: retry convenience constructor creates correct status
prop_retryConstructor :: Property
prop_retryConstructor = property $ do
    attempt <- forAll $ Gen.int (Range.linear 1 10)
    msg <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    next <- forAll $ Gen.int (Range.linear 100 60000)
    let status = retry attempt msg next
    ssType status === StatusRetry attempt msg next

-- | Property: all status types have "type" field in JSON
prop_allStatusTypesHaveTypeField :: Property
prop_allStatusTypesHaveTypeField = property $ do
    statusType <-
        forAll $
            Gen.element
                [ StatusIdle
                , StatusBusy
                , StatusRetry 1 "test" 1000
                ]
    let status = SessionStatus statusType
    case decodeValue (encode status) of
        Object obj -> assert $ KM.member (Key.fromText "type") obj
        _otherValue -> failure
  where
    decodeValue bytes = fromMaybe Null (decode bytes)

tests :: TestTree
tests =
    testGroup
        "Session Status Property Tests"
        [ testGroup
            "JSON Serialization"
            [ testProperty "idle status JSON" prop_idleStatusJson
            , testProperty "retry status JSON" prop_retryStatusJson
            , testProperty "busy status JSON" prop_busyStatusJson
            , testProperty "all status types have type field" prop_allStatusTypesHaveTypeField
            ]
        , testGroup
            "Constructors"
            [ testProperty "idle constructor" prop_idleConstructor
            , testProperty "busy constructor" prop_busyConstructor
            , testProperty "retry constructor" prop_retryConstructor
            ]
        ]
