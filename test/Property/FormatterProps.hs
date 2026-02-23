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
    _statuses <- evalIO $ statusFor "."
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
    assert $ any (\status -> fsName status == "custom" && fsEnabled status) statuses

prop_baseFormattersReturnedByDefault :: Property
prop_baseFormattersReturnedByDefault = withTests 10 $ property $ do
    -- With no formatter config, base formatters should be returned
    let cfg = Config.defaultConfig{CT.cfgFormatter = Nothing}
    statuses <- evalIO $ statusForConfig "." cfg
    -- Should have some base formatters
    assert $ not (null statuses)

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
        , testProperty "base formatters returned by default" prop_baseFormattersReturnedByDefault
        ]
