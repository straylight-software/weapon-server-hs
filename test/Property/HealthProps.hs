{-# LANGUAGE OverloadedStrings #-}

module Property.HealthProps where

import Api (Health (..))
import Data.Aeson (decode, encode)

import Health.Build qualified as HealthBuild
import Hedgehog
import Test.Helpers (genText)
import Test.Tasty
import Test.Tasty.Hedgehog

-- | Property: buildHealth always returns healthy=True and echoes the version
prop_buildHealth :: Property
prop_buildHealth = property $ do
    version <- forAll genText
    let Health healthy ver = HealthBuild.buildHealth version
    -- Server is always healthy when running
    healthy === True
    -- Version is echoed back
    ver === version

prop_healthJsonRoundtrip :: Property
prop_healthJsonRoundtrip = property $ do
    version <- forAll genText
    let health = HealthBuild.buildHealth version
    case decode (encode health) of
        Nothing -> failure
        Just health' -> health' === health

tests :: TestTree
tests =
    testGroup
        "Health Property Tests"
        [ testProperty "build health" prop_buildHealth
        , testProperty "health JSON roundtrip" prop_healthJsonRoundtrip
        ]
