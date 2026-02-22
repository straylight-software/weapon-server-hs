{-# LANGUAGE OverloadedStrings #-}

module Property.FormatterProps where

import Config.Config qualified as Config
import Config.Types qualified as CT
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Formatter.Status (FormatterStatus (..), statusFor, statusForConfig)
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

prop_uniqueNames :: Property
prop_uniqueNames = withTests 10 $ property $ do
    statuses <- evalIO $ statusFor "."
    let names = map fsName statuses
    listLength names === Set.size (Set.fromList names)

prop_extensionsNonEmpty :: Property
prop_extensionsNonEmpty = withTests 10 $ property $ do
    statuses <- evalIO $ statusFor "."
    assert $ not (any (null . fsExtensions) statuses)

prop_formatterDisabled :: Property
prop_formatterDisabled = withTests 10 $ property $ do
    statuses <- evalIO $ statusForConfig "." (Config.defaultConfig{CT.cfgFormatter = Just CT.FormatterDisabled})
    statuses === []

prop_customFormatterIncluded :: Property
prop_customFormatterIncluded = withTests 10 $ property $ do
    let entry =
            CT.FormatterEntry
                { CT.feDisabled = Nothing
                , CT.feCommand = Just ["custom"]
                , CT.feEnvironment = Nothing
                , CT.feExtensions = Just [".x"]
                }
    let cfg =
            Config.defaultConfig
                { CT.cfgFormatter = Just (CT.FormatterConfig (Map.fromList [("custom", entry)]))
                }
    statuses <- evalIO $ statusForConfig "." cfg
    assert $ any (\status -> fsName status == "custom" && fsEnabled status) statuses

prop_disableBaseFormatter :: Property
prop_disableBaseFormatter = withTests 10 $ property $ do
    let entry =
            CT.FormatterEntry
                { CT.feDisabled = Just True
                , CT.feCommand = Nothing
                , CT.feEnvironment = Nothing
                , CT.feExtensions = Nothing
                }
    let cfg =
            Config.defaultConfig
                { CT.cfgFormatter = Just (CT.FormatterConfig (Map.fromList [("gofmt", entry)]))
                }
    statuses <- evalIO $ statusForConfig "." cfg
    assert $ all (\status -> fsName status /= "gofmt") statuses

prop_overrideExtensions :: Property
prop_overrideExtensions = withTests 10 $ property $ do
    let entry =
            CT.FormatterEntry
                { CT.feDisabled = Nothing
                , CT.feCommand = Nothing
                , CT.feEnvironment = Nothing
                , CT.feExtensions = Just [".x"]
                }
    let cfg =
            Config.defaultConfig
                { CT.cfgFormatter = Just (CT.FormatterConfig (Map.fromList [("gofmt", entry)]))
                }
    statuses <- evalIO $ statusForConfig "." cfg
    assert $ any (\status -> fsName status == "gofmt" && fsExtensions status == [".x"]) statuses

listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0

tests :: TestTree
tests =
    testGroup
        "Formatter Property Tests"
        [ testProperty "unique names" prop_uniqueNames
        , testProperty "extensions non-empty" prop_extensionsNonEmpty
        , testProperty "formatter disabled" prop_formatterDisabled
        , testProperty "custom formatter included" prop_customFormatterIncluded
        , testProperty "disable base formatter" prop_disableBaseFormatter
        , testProperty "override extensions" prop_overrideExtensions
        ]
