{-# LANGUAGE OverloadedStrings #-}

module Property.SessionStatusProps where

import Data.Aeson (Value (..), decode, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Session.Status (SessionStatus (..), SessionStatusType (..))
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

tests :: TestTree
tests =
    testGroup
        "Session Status Property Tests"
        [ testProperty "idle status JSON" prop_idleStatusJson
        , testProperty "retry status JSON" prop_retryStatusJson
        , testProperty "busy status JSON" prop_busyStatusJson
        ]
