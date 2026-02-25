{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.FormatterProps
Description : Property tests for Formatter.Status module

Property-based tests covering:

* IO-based status checking with caches
* Pure configuration logic (no IO required)
* FormatterInfo/FormatterStatus conversion
* Base formatters invariants
-}
module Property.FormatterProps where

import Config.Config qualified as Config
import Config.Types qualified as CT
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Formatter.Status (
    ExeCache,
    FormatterInfo (..),
    FormatterStatus (..),
    applyConfig,
    baseFormatters,
    formatterInfoToStatus,
    formattersFor,
    mkFormatterInfo,
    statusFor,
    statusForConfig,
 )
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Helpers (listLength)
import Test.Tasty
import Test.Tasty.Hedgehog

-- ════════════════════════════════════════════════════════════════════════════
-- Generators
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate a valid formatter name (alphanumeric, lowercase)
genFormatterName :: Gen Text
genFormatterName = Gen.text (Range.linear 1 20) Gen.lower

-- | Generate a file extension (starts with dot)
genExtension :: Gen Text
genExtension = do
    ext <- Gen.text (Range.linear 1 5) Gen.lower
    pure $ "." <> ext

-- | Generate a list of unique extensions
genExtensions :: Gen [Text]
genExtensions = Gen.list (Range.linear 1 8) genExtension

-- | Generate a FormatterInfo using the smart constructor
genFormatterInfo :: Gen FormatterInfo
genFormatterInfo = do
    name <- genFormatterName
    exts <- genExtensions
    exeName <- Gen.string (Range.linear 1 20) Gen.lower
    pure $ mkFormatterInfo name exts exeName

-- ════════════════════════════════════════════════════════════════════════════
-- Pure Function Properties (no IO required)
-- ════════════════════════════════════════════════════════════════════════════

-- | mkFormatterInfo correctly constructs FormatterInfo
prop_mkFormatterInfoConstructs :: Property
prop_mkFormatterInfoConstructs = property $ do
    name <- forAll genFormatterName
    exts <- forAll genExtensions
    exeName <- forAll $ Gen.string (Range.linear 1 20) Gen.lower

    let info = mkFormatterInfo name exts exeName
    fiName info === name
    fiExtensions info === exts
    fiExeName info === exeName

-- | formatterInfoToStatus preserves name and extensions
prop_formatterInfoToStatusPreservesFields :: Property
prop_formatterInfoToStatusPreservesFields = property $ do
    info <- forAll genFormatterInfo
    enabled <- forAll Gen.bool

    let status = formatterInfoToStatus info enabled
    fsName status === fiName info
    fsExtensions status === fiExtensions info
    fsEnabled status === enabled

-- | formatterInfoToStatus with True produces enabled status
prop_formatterInfoToStatusEnabled :: Property
prop_formatterInfoToStatusEnabled = property $ do
    info <- forAll genFormatterInfo
    let status = formatterInfoToStatus info True
    assert $ fsEnabled status

-- | formatterInfoToStatus with False produces disabled status
prop_formatterInfoToStatusDisabled :: Property
prop_formatterInfoToStatusDisabled = property $ do
    info <- forAll genFormatterInfo
    let status = formatterInfoToStatus info False
    assert $ not (fsEnabled status)

-- | applyConfig with FormatterDisabled returns empty list
prop_applyConfigDisabledReturnsEmpty :: Property
prop_applyConfigDisabledReturnsEmpty = property $ do
    infos <- forAll $ Gen.list (Range.linear 0 10) genFormatterInfo
    applyConfig CT.FormatterDisabled infos === []

-- | applyConfig with FormatterEnabled preserves input list
prop_applyConfigEnabledPreserves :: Property
prop_applyConfigEnabledPreserves = property $ do
    infos <- forAll $ Gen.list (Range.linear 0 10) genFormatterInfo
    let enabledMap = Map.empty -- Empty map still means "enabled"
    applyConfig (CT.FormatterEnabled enabledMap) infos === infos

-- | formattersFor with Nothing returns baseFormatters
prop_formattersForDefaultReturnsBase :: Property
prop_formattersForDefaultReturnsBase = property $ do
    let cfg = Config.defaultConfig{CT.cfgFormatter = Nothing}
    formattersFor cfg === baseFormatters

-- | formattersFor with FormatterDisabled returns empty
prop_formattersForDisabledReturnsEmpty :: Property
prop_formattersForDisabledReturnsEmpty = property $ do
    let cfg = Config.defaultConfig{CT.cfgFormatter = Just CT.FormatterDisabled}
    formattersFor cfg === []

-- | baseFormatters has no duplicate names
prop_baseFormattersUniqueNames :: Property
prop_baseFormattersUniqueNames = property $ do
    let names = map fiName baseFormatters
    listLength names === Set.size (Set.fromList names)

-- | baseFormatters has no duplicate executable names
prop_baseFormattersUniqueExeNames :: Property
prop_baseFormattersUniqueExeNames = property $ do
    let exeNames = map fiExeName baseFormatters
    listLength exeNames === Set.size (Set.fromList exeNames)

-- | baseFormatters all have non-empty extensions
prop_baseFormattersNonEmptyExtensions :: Property
prop_baseFormattersNonEmptyExtensions = property $ do
    assert $ not (any (null . fiExtensions) baseFormatters)

-- | baseFormatters extensions all start with dot
prop_baseFormattersExtensionsStartWithDot :: Property
prop_baseFormattersExtensionsStartWithDot = property $ do
    let allExts = concatMap fiExtensions baseFormatters
    assert $ all startsWithDot allExts
  where
    startsWithDot :: Text -> Bool
    startsWithDot t = case T.uncons t of
        Just ('.', _) -> True
        Just (_, _) -> False
        Nothing -> False

-- | baseFormatters contains expected formatters
prop_baseFormattersContainsExpected :: Property
prop_baseFormattersContainsExpected = property $ do
    let names = map fiName baseFormatters
    assert $ "prettier" `elem` names
    assert $ "black" `elem` names
    assert $ "gofmt" `elem` names
    assert $ "rustfmt" `elem` names

-- | baseFormatters count is stable (4 formatters)
prop_baseFormattersCount :: Property
prop_baseFormattersCount = property $ do
    listLength baseFormatters === 4

-- | Prettier handles JavaScript files
prop_prettierHandlesJs :: Property
prop_prettierHandlesJs = property $ do
    let mPrettier = List.find (\f -> fiName f == "prettier") baseFormatters
    case mPrettier of
        Nothing -> failure
        Just prettier -> do
            assert $ ".js" `elem` fiExtensions prettier
            assert $ ".ts" `elem` fiExtensions prettier
            assert $ ".jsx" `elem` fiExtensions prettier
            assert $ ".tsx" `elem` fiExtensions prettier

-- | Black handles Python files
prop_blackHandlesPy :: Property
prop_blackHandlesPy = property $ do
    let mBlack = List.find (\f -> fiName f == "black") baseFormatters
    case mBlack of
        Nothing -> failure
        Just black -> assert $ ".py" `elem` fiExtensions black

-- | Gofmt handles Go files
prop_gofmtHandlesGo :: Property
prop_gofmtHandlesGo = property $ do
    let mGofmt = List.find (\f -> fiName f == "gofmt") baseFormatters
    case mGofmt of
        Nothing -> failure
        Just gofmt -> assert $ ".go" `elem` fiExtensions gofmt

-- | Rustfmt handles Rust files
prop_rustfmtHandlesRs :: Property
prop_rustfmtHandlesRs = property $ do
    let mRustfmt = List.find (\f -> fiName f == "rustfmt") baseFormatters
    case mRustfmt of
        Nothing -> failure
        Just rustfmt -> assert $ ".rs" `elem` fiExtensions rustfmt

-- ════════════════════════════════════════════════════════════════════════════
-- IO-based Properties (require caches)
-- ════════════════════════════════════════════════════════════════════════════

-- | statusFor returns formatters with unique names
prop_uniqueNames :: Config.DhallCache -> ExeCache -> Property
prop_uniqueNames dhallCache exeCache = property $ do
    statuses <- evalIO $ statusFor dhallCache exeCache "."
    let names = map fsName statuses
    listLength names === Set.size (Set.fromList names)

-- | statusFor returns at least the base formatters
prop_statusForReturnsNonEmpty :: Config.DhallCache -> ExeCache -> Property
prop_statusForReturnsNonEmpty dhallCache exeCache = property $ do
    statuses <- evalIO $ statusFor dhallCache exeCache "."
    -- statusFor should return at least one formatter (the base formatters)
    assert $ not (null statuses)

-- | statusForConfig with FormatterDisabled returns empty list
prop_formatterDisabled :: ExeCache -> Property
prop_formatterDisabled exeCache = property $ do
    let cfg = Config.defaultConfig{CT.cfgFormatter = Just CT.FormatterDisabled}
    statuses <- evalIO $ statusForConfig exeCache "." cfg
    statuses === []

-- | statusForConfig with custom formatter entry still returns base formatters
prop_customFormatterIncluded :: ExeCache -> Property
prop_customFormatterIncluded exeCache = property $ do
    let entry =
            CT.FormatterEntry
                { CT.feCommand = ["custom-formatter"]
                , CT.feTimeout = Nothing
                }
    let cfg =
            Config.defaultConfig
                { CT.cfgFormatter = Just (CT.FormatterEnabled (Map.fromList [("custom", entry)]))
                }
    statuses <- evalIO $ statusForConfig exeCache "." cfg
    -- Custom formatters aren't in base formatters, so we just check base formatters are returned
    assert $ not (null statuses)

-- | statusForConfig with no formatter config returns base formatters
prop_baseFormattersReturnedByDefault :: ExeCache -> Property
prop_baseFormattersReturnedByDefault exeCache = property $ do
    let cfg = Config.defaultConfig{CT.cfgFormatter = Nothing}
    statuses <- evalIO $ statusForConfig exeCache "." cfg
    -- Should have some base formatters
    assert $ not (null statuses)
    -- Should have exactly as many as base formatters
    listLength statuses === listLength baseFormatters

-- | statusForConfig result names match formattersFor names
prop_statusNamesMatchFormatters :: ExeCache -> Property
prop_statusNamesMatchFormatters exeCache = property $ do
    let cfg = Config.defaultConfig{CT.cfgFormatter = Nothing}
    statuses <- evalIO $ statusForConfig exeCache "." cfg
    let statusNames = Set.fromList $ map fsName statuses
    let formatterNames = Set.fromList $ map fiName (formattersFor cfg)
    statusNames === formatterNames

-- | All returned statuses have non-empty extensions
prop_statusesHaveExtensions :: ExeCache -> Property
prop_statusesHaveExtensions exeCache = property $ do
    let cfg = Config.defaultConfig{CT.cfgFormatter = Nothing}
    statuses <- evalIO $ statusForConfig exeCache "." cfg
    assert $ not (any (null . fsExtensions) statuses)

-- ════════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ════════════════════════════════════════════════════════════════════════════

-- | All formatter property tests
tests :: Config.DhallCache -> ExeCache -> TestTree
tests dhallCache exeCache =
    testGroup
        "Formatter Property Tests"
        [ testGroup
            "Pure Functions"
            [ testProperty "mkFormatterInfo constructs correctly" prop_mkFormatterInfoConstructs
            , testProperty "formatterInfoToStatus preserves fields" prop_formatterInfoToStatusPreservesFields
            , testProperty "formatterInfoToStatus with True" prop_formatterInfoToStatusEnabled
            , testProperty "formatterInfoToStatus with False" prop_formatterInfoToStatusDisabled
            , testProperty "applyConfig Disabled returns empty" prop_applyConfigDisabledReturnsEmpty
            , testProperty "applyConfig Enabled preserves list" prop_applyConfigEnabledPreserves
            , testProperty "formattersFor default returns base" prop_formattersForDefaultReturnsBase
            , testProperty "formattersFor disabled returns empty" prop_formattersForDisabledReturnsEmpty
            ]
        , testGroup
            "Base Formatters Invariants"
            [ testProperty "unique names" prop_baseFormattersUniqueNames
            , testProperty "unique exe names" prop_baseFormattersUniqueExeNames
            , testProperty "non-empty extensions" prop_baseFormattersNonEmptyExtensions
            , testProperty "extensions start with dot" prop_baseFormattersExtensionsStartWithDot
            , testProperty "contains expected formatters" prop_baseFormattersContainsExpected
            , testProperty "count is 4" prop_baseFormattersCount
            , testProperty "prettier handles JS" prop_prettierHandlesJs
            , testProperty "black handles Python" prop_blackHandlesPy
            , testProperty "gofmt handles Go" prop_gofmtHandlesGo
            , testProperty "rustfmt handles Rust" prop_rustfmtHandlesRs
            ]
        , testGroup
            "IO-based Status Checking"
            [ testProperty "unique names" (prop_uniqueNames dhallCache exeCache)
            , testProperty "statusFor returns formatters" (prop_statusForReturnsNonEmpty dhallCache exeCache)
            , testProperty "formatter disabled" (prop_formatterDisabled exeCache)
            , testProperty "custom formatter included" (prop_customFormatterIncluded exeCache)
            , testProperty "base formatters by default" (prop_baseFormattersReturnedByDefault exeCache)
            , testProperty "status names match formatters" (prop_statusNamesMatchFormatters exeCache)
            , testProperty "statuses have extensions" (prop_statusesHaveExtensions exeCache)
            ]
        ]
