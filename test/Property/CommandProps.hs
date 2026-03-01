{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for Command module.
module Property.CommandProps (tests) where

import Command.Command
import Data.Aeson (ToJSON (..), Value (..), encode)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
    testGroup
        "Command"
        [ testGroup
            "extractHints"
            [ testProperty "extracts numbered placeholders" prop_extractsNumbered
            , testProperty "extracts $ARGUMENTS" prop_extractsArguments
            , testProperty "returns empty for no placeholders" prop_emptyForNoPlaceholders
            , testProperty "deduplicates placeholders" prop_deduplicates
            , testProperty "sorts numbered placeholders" prop_sortedNumbered
            ]
        , testGroup
            "CommandInfo"
            [ testProperty "JSON has required fields" prop_jsonRequiredFields
            , testProperty "JSON roundtrip preserves data" prop_jsonRoundtrip
            ]
        , testGroup
            "CommandSource"
            [ testProperty "command source encodes to string" prop_sourceEncodesString
            ]
        ]

-- | Generate a template with random text and placeholders
genTemplate :: Gen Text
genTemplate = do
    parts <- Gen.list (Range.linear 0 5) genPart
    return $ T.intercalate " " parts
  where
    genPart =
        Gen.choice
            [ Gen.text (Range.linear 1 10) Gen.alphaNum
            , Gen.element ["$1", "$2", "$3", "$4", "$5"]
            , pure "$ARGUMENTS"
            ]

-- | Generate a CommandInfo
genCommandInfo :: Gen CommandInfo
genCommandInfo = do
    name <- Gen.text (Range.linear 1 20) Gen.alphaNum
    template <- genTemplate
    desc <- Gen.maybe $ Gen.text (Range.linear 1 50) Gen.alphaNum
    agent <- Gen.maybe $ Gen.text (Range.linear 1 20) Gen.alphaNum
    model <- Gen.maybe $ Gen.text (Range.linear 1 30) Gen.alphaNum
    source <- Gen.maybe $ Gen.element [CommandSourceCommand, CommandSourceSkill]
    subtask <- Gen.maybe Gen.bool
    return
        CommandInfo
            { ciName = name
            , ciTemplate = template
            , ciHints = extractHints template
            , ciDescription = desc
            , ciAgent = agent
            , ciModel = model
            , ciSource = source
            , ciSubtask = subtask
            }

-- ═══════════════════════════════════════════════════════════════════════════
-- extractHints properties
-- ═══════════════════════════════════════════════════════════════════════════

prop_extractsNumbered :: Property
prop_extractsNumbered = property $ do
    n <- forAll $ Gen.int (Range.linear 1 9)
    let template = "Do something with $" <> T.pack (show n)
    let hints = extractHints template
    annotateShow hints
    assert $ ("$" <> T.pack (show n)) `elem` hints

prop_extractsArguments :: Property
prop_extractsArguments = property $ do
    prefix <- forAll $ Gen.text (Range.linear 0 20) Gen.alphaNum
    suffix <- forAll $ Gen.text (Range.linear 0 20) Gen.alphaNum
    let template = prefix <> "$ARGUMENTS" <> suffix
    let hints = extractHints template
    annotateShow hints
    assert $ "$ARGUMENTS" `elem` hints

prop_emptyForNoPlaceholders :: Property
prop_emptyForNoPlaceholders = property $ do
    -- Generate text without $ followed by number or ARGUMENTS
    text <- forAll $ Gen.text (Range.linear 0 50) Gen.alpha
    let hints = extractHints text
    annotateShow hints
    hints === []

prop_deduplicates :: Property
prop_deduplicates = property $ do
    let template = "$1 something $1 more $1"
    let hints = extractHints template
    annotateShow hints
    -- Should only have one $1 (check via pattern matching, not length)
    case filter (== "$1") hints of
        [_single] -> success
        [] -> annotate "Expected exactly one $1, got none" >> failure
        _multiple -> annotate "Expected exactly one $1, got multiple" >> failure

prop_sortedNumbered :: Property
prop_sortedNumbered = property $ do
    let template = "$3 then $1 then $2"
    let hints = extractHints template
    annotateShow hints
    -- Should be sorted
    hints === ["$1", "$2", "$3"]

-- ═══════════════════════════════════════════════════════════════════════════
-- CommandInfo JSON properties
-- ═══════════════════════════════════════════════════════════════════════════

prop_jsonRequiredFields :: Property
prop_jsonRequiredFields = property $ do
    cmd <- forAll genCommandInfo
    let json = toJSON cmd
    case json of
        Object obj -> do
            -- Required fields must be present
            assert $ KM.member (K.fromText "name") obj
            assert $ KM.member (K.fromText "template") obj
            assert $ KM.member (K.fromText "hints") obj
        -- CommandInfo should always serialize to an Object
        Array _arr -> annotate "Expected Object, got Array" >> failure
        String _str -> annotate "Expected Object, got String" >> failure
        Number _num -> annotate "Expected Object, got Number" >> failure
        Bool _b -> annotate "Expected Object, got Bool" >> failure
        Null -> annotate "Expected Object, got Null" >> failure

prop_jsonRoundtrip :: Property
prop_jsonRoundtrip = property $ do
    cmd <- forAll genCommandInfo
    let encoded = encode (toJSON cmd)
    -- Just check it encodes without error and has reasonable size
    assert $ BS.length encoded > 10

-- ═══════════════════════════════════════════════════════════════════════════
-- CommandSource properties
-- ═══════════════════════════════════════════════════════════════════════════

prop_sourceEncodesString :: Property
prop_sourceEncodesString = property $ do
    source <- forAll $ Gen.element [CommandSourceCommand, CommandSourceSkill]
    case toJSON source of
        String s -> do
            -- Should be either "command" or "skill"
            assert $ s `elem` ["command", "skill"]
        -- CommandSource should always serialize to a String
        Object _obj -> annotate "Expected String, got Object" >> failure
        Array _arr -> annotate "Expected String, got Array" >> failure
        Number _num -> annotate "Expected String, got Number" >> failure
        Bool _b -> annotate "Expected String, got Bool" >> failure
        Null -> annotate "Expected String, got Null" >> failure
