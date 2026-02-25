{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for Tool.Defs schema builder functions
module Property.ToolDefsProps where

import Data.Aeson (Value (..), decode, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog

import Tool.Defs (
    boolProp,
    mkObjectSchema,
    numberProp,
    stringProp,
 )

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

genKey :: Gen Text
genKey = Gen.text (Range.linear 1 20) Gen.alphaNum

genDescription :: Gen Text
genDescription = Gen.text (Range.linear 1 100) Gen.alphaNum

-- ═══════════════════════════════════════════════════════════════════════════
-- Schema Builder Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: stringProp creates a property with type "string"
prop_stringPropType :: Property
prop_stringPropType = property $ do
    key <- forAll genKey
    desc <- forAll genDescription
    let (_, propValue) = stringProp (Key.fromText key) desc
    case propValue of
        Object obj -> do
            KM.lookup "type" obj === Just (String "string")
            KM.lookup "description" obj === Just (String desc)
        Array _ -> failure
        String _ -> failure
        Number _ -> failure
        Bool _ -> failure
        Null -> failure

-- | Property: numberProp creates a property with type "number"
prop_numberPropType :: Property
prop_numberPropType = property $ do
    key <- forAll genKey
    desc <- forAll genDescription
    let (_, propValue) = numberProp (Key.fromText key) desc
    case propValue of
        Object obj -> do
            KM.lookup "type" obj === Just (String "number")
            KM.lookup "description" obj === Just (String desc)
        Array _ -> failure
        String _ -> failure
        Number _ -> failure
        Bool _ -> failure
        Null -> failure

-- | Property: boolProp creates a property with type "boolean"
prop_boolPropType :: Property
prop_boolPropType = property $ do
    key <- forAll genKey
    desc <- forAll genDescription
    let (_, propValue) = boolProp (Key.fromText key) desc
    case propValue of
        Object obj -> do
            KM.lookup "type" obj === Just (String "boolean")
            KM.lookup "description" obj === Just (String desc)
        Array _ -> failure
        String _ -> failure
        Number _ -> failure
        Bool _ -> failure
        Null -> failure

-- | Property: mkObjectSchema creates valid JSON Schema object
prop_mkObjectSchemaStructure :: Property
prop_mkObjectSchemaStructure = property $ do
    numProps <- forAll $ Gen.int (Range.linear 1 5)
    props <-
        forAll $
            Gen.list (Range.singleton numProps) $
                stringProp . Key.fromText <$> genKey <*> genDescription
    numRequired <- forAll $ Gen.int (Range.linear 0 numProps)
    let required = take numRequired [T.pack $ "req" <> show i | i <- [1 :: Int ..]]
    let schema = mkObjectSchema props required
    case schema of
        Object obj -> do
            -- Has type "object"
            KM.lookup "type" obj === Just (String "object")
            -- Has properties key
            assert $ KM.member "properties" obj
            -- Has required key
            assert $ KM.member "required" obj
        Array _ -> failure
        String _ -> failure
        Number _ -> failure
        Bool _ -> failure
        Null -> failure

-- | Property: mkObjectSchema required field contains correct values
prop_mkObjectSchemaRequired :: Property
prop_mkObjectSchemaRequired = property $ do
    let props = [stringProp "foo" "desc", stringProp "bar" "desc"]
    let required = ["foo" :: Text]
    let schema = mkObjectSchema props required
    case schema of
        Object obj ->
            case KM.lookup "required" obj of
                Just (Array arr) -> do
                    let reqList = [t | String t <- toList arr]
                    reqList === required
                Just (Object _) -> failure
                Just (String _) -> failure
                Just (Number _) -> failure
                Just (Bool _) -> failure
                Just Null -> failure
                Nothing -> failure
        Array _ -> failure
        String _ -> failure
        Number _ -> failure
        Bool _ -> failure
        Null -> failure

-- | Property: schema builders produce valid JSON (roundtrips)
prop_schemaRoundtrip :: Property
prop_schemaRoundtrip = property $ do
    numProps <- forAll $ Gen.int (Range.linear 1 3)
    props <- forAll $ Gen.list (Range.singleton numProps) $ do
        k <- genKey
        d <- genDescription
        Gen.element
            [ stringProp (Key.fromText k) d
            , numberProp (Key.fromText k) d
            , boolProp (Key.fromText k) d
            ]
    let schema = mkObjectSchema props []
    let encoded = encode schema
    case decode encoded of
        Nothing -> failure
        Just (_ :: Value) -> success

-- | Property: properties key returns correct key-value
prop_propKeyCorrect :: Property
prop_propKeyCorrect = property $ do
    key <- forAll genKey
    desc <- forAll genDescription
    let (k, _) = stringProp (Key.fromText key) desc
    Key.toText k === key

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Tool.Defs Schema Builder Tests"
        [ testProperty "stringProp type" prop_stringPropType
        , testProperty "numberProp type" prop_numberPropType
        , testProperty "boolProp type" prop_boolPropType
        , testProperty "mkObjectSchema structure" prop_mkObjectSchemaStructure
        , testProperty "mkObjectSchema required" prop_mkObjectSchemaRequired
        , testProperty "schema roundtrip" prop_schemaRoundtrip
        , testProperty "prop key correct" prop_propKeyCorrect
        ]
