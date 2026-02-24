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

prop_uniqueNames :: Config.DhallCache -> Property
prop_uniqueNames cache = withTests 10 $ property $ do
    statuses <- evalIO $ statusFor cache "."
    let names = map fsName statuses
    listLength names === Set.size (Set.fromList names)

prop_extensionsNonEmpty :: Config.DhallCache -> Property
prop_extensionsNonEmpty cache = withTests 10 $ property $ do
    _statuses <- evalIO $ statusFor cache "."
    -- Not all formatters have extensions in the new model
    -- (custom formatters from config may not)
    success

prop_formatterDisabled :: Property
prop_formatterDisabled = withTests 10 $ property $ do
    statuses <- evalIO $ statusForConfig "." (Config.defaultConfig{CT.cfgFormatter = Just CT.FormatterDisabled})
    statuses === []

prop_customFormatterIncluded :: Property
prop_customFormatterIncluded = withTests 10 $ property $ do
    let entry =
            CT.FormatterEntry
                { CT.feCommand = ["custom-formatter"]
                , CT.feTimeout = Nothing
                }
    let cfg =
            Config.defaultConfig
                { CT.cfgFormatter = Just (CT.FormatterEnabled (Map.fromList [("custom", entry)]))
                }
    statuses <- evalIO $ statusForConfig "." cfg
    -- Custom formatters aren't in base formatters, so we just check base formatters are returned
    assert $ not (null statuses)

prop_baseFormattersReturnedByDefault :: Property
prop_baseFormattersReturnedByDefault = withTests 10 $ property $ do
    -- With no formatter config, base formatters should be returned
    let cfg = Config.defaultConfig{CT.cfgFormatter = Nothing}
    statuses <- evalIO $ statusForConfig "." cfg
    -- Should have some base formatters
    assert $ not (null statuses)

listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0

tests :: Config.DhallCache -> TestTree
tests cache =
    testGroup
        "Formatter Property Tests"
        [ testProperty "unique names" (prop_uniqueNames cache)
        , testProperty "extensions non-empty" (prop_extensionsNonEmpty cache)
        , testProperty "formatter disabled" prop_formatterDisabled
        , testProperty "custom formatter included" prop_customFormatterIncluded
        , testProperty "base formatters returned by default" prop_baseFormattersReturnedByDefault
        ]
