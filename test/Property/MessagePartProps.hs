{-# LANGUAGE OverloadedStrings #-}

module Property.MessagePartProps where

import Control.Monad (when)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Message.Parts qualified as Parts
import Test.Tasty
import Test.Tasty.Hedgehog

prop_updatePart :: Property
prop_updatePart = property $ do
    pid <- forAll genNonEmptyText
    part <- forAll (genPart pid)
    patch <- forAll genPatch
    parts <- forAll $ Gen.list (Range.linear 0 5) genPartAny
    let allParts = part : parts
    case Parts.updatePart pid patch allParts of
        Nothing -> failure
        Just updated -> do
            let mpart = Parts.findPart pid updated
            case mpart of
                Nothing -> failure
                Just value -> do
                    value === mergeExpected part patch

prop_deletePart :: Property
prop_deletePart = property $ do
    pid <- forAll genNonEmptyText
    part <- forAll (genPart pid)
    parts <- forAll $ Gen.list (Range.linear 0 5) genPartAny
    let allParts = part : parts
    case Parts.deletePart pid allParts of
        Nothing -> failure
        Just updated -> do
            Parts.findPart pid updated === Nothing

prop_findPart :: Property
prop_findPart = property $ do
    pid <- forAll genNonEmptyText
    part <- forAll (genPart pid)
    parts <- forAll $ Gen.list (Range.linear 0 5) genPartAny
    let allParts = part : parts
    Parts.findPart pid allParts === Just part

prop_updateMissingPart :: Property
prop_updateMissingPart = property $ do
    pid <- forAll genNonEmptyText
    otherPid <- forAll genNonEmptyText
    when (pid == otherPid) discard
    part <- forAll (genPart otherPid)
    patch <- forAll genPatch
    Parts.updatePart pid patch [part] === Nothing

prop_deleteMissingPart :: Property
prop_deleteMissingPart = property $ do
    pid <- forAll genNonEmptyText
    otherPid <- forAll genNonEmptyText
    when (pid == otherPid) discard
    part <- forAll (genPart otherPid)
    Parts.deletePart pid [part] === Nothing

prop_updatePreservesOtherParts :: Property
prop_updatePreservesOtherParts = property $ do
    pid <- forAll genNonEmptyText
    otherPid <- forAll (Gen.filter (/= pid) genNonEmptyText)
    part <- forAll (genPart pid)
    other <- forAll (genPart otherPid)
    patch <- forAll genPatch
    case Parts.updatePart pid patch [part, other] of
        Nothing -> failure
        Just updated -> do
            Parts.findPart otherPid updated === Just other

-- ═══════════════════════════════════════════════════════════════════════════
-- Additional Edge Case Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: updating a part with same content is idempotent
prop_updatePartIdempotent :: Property
prop_updatePartIdempotent = property $ do
    pid <- forAll genNonEmptyText
    part <- forAll (genPart pid)
    -- Update with the same content
    let patch = part -- Use the same value as patch
    case Parts.updatePart pid patch [part] of
        Nothing -> failure
        Just updated1 -> do
            case Parts.updatePart pid patch updated1 of
                Nothing -> failure
                Just updated2 -> updated1 === updated2

-- | Property: deleting same part twice - second returns Nothing
prop_deletePartTwice :: Property
prop_deletePartTwice = property $ do
    pid <- forAll genNonEmptyText
    part <- forAll (genPart pid)
    case Parts.deletePart pid [part] of
        Nothing -> failure
        Just afterFirst -> do
            -- Second delete should return Nothing (part no longer exists)
            Parts.deletePart pid afterFirst === Nothing

-- | Property: findPart in nested structure with multiple levels
prop_findPartInNestedStructure :: Property
prop_findPartInNestedStructure = property $ do
    pid <- forAll genNonEmptyText
    content <- forAll genText
    -- Create a deeply nested part structure
    let nestedPart =
            object
                [ "id" .= pid
                , "type" .= ("nested" :: Text)
                , "data"
                    .= object
                        [ "inner" .= object ["value" .= content]
                        ]
                ]
    otherParts <- forAll $ Gen.list (Range.linear 0 3) genPartAny
    let allParts = nestedPart : otherParts
    -- findPart should still locate by top-level id
    case Parts.findPart pid allParts of
        Nothing -> failure
        Just found -> found === nestedPart

-- | Property: empty parts list operations
prop_emptyPartsOperations :: Property
prop_emptyPartsOperations = property $ do
    pid <- forAll genNonEmptyText
    patch <- forAll genPatch
    -- All operations on empty list should return Nothing
    Parts.findPart pid [] === Nothing
    Parts.updatePart pid patch [] === Nothing
    Parts.deletePart pid [] === Nothing

-- ═══════════════════════════════════════════════════════════════════════════
-- partId Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: partId extracts ID from "id" field
prop_partIdFromId :: Property
prop_partIdFromId = property $ do
    pid <- forAll genNonEmptyText
    let part = object ["id" .= pid, "type" .= ("text" :: Text)]
    Parts.partId part === Just pid

-- | Property: partId extracts ID from "partID" field as fallback
prop_partIdFromPartID :: Property
prop_partIdFromPartID = property $ do
    pid <- forAll genNonEmptyText
    let part = object ["partID" .= pid, "type" .= ("text" :: Text)]
    Parts.partId part === Just pid

-- | Property: partId prefers "id" over "partID"
prop_partIdPrefersId :: Property
prop_partIdPrefersId = property $ do
    pid1 <- forAll genNonEmptyText
    pid2 <- forAll genNonEmptyText
    let part = object ["id" .= pid1, "partID" .= pid2, "type" .= ("text" :: Text)]
    Parts.partId part === Just pid1

-- | Property: partId returns Nothing for non-object values
prop_partIdNonObject :: Property
prop_partIdNonObject = property $ do
    Parts.partId Null === Nothing
    Parts.partId (String "test") === Nothing
    Parts.partId (Number 42) === Nothing
    Parts.partId (Bool True) === Nothing

-- | Property: partId returns Nothing for object without id fields
prop_partIdNoIdField :: Property
prop_partIdNoIdField = property $ do
    let part = object ["type" .= ("text" :: Text), "text" .= ("hello" :: Text)]
    Parts.partId part === Nothing

-- ═══════════════════════════════════════════════════════════════════════════
-- partExists Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: partExists returns True when part exists
prop_partExistsTrue :: Property
prop_partExistsTrue = property $ do
    pid <- forAll genNonEmptyText
    part <- forAll (genPart pid)
    others <- forAll $ Gen.list (Range.linear 0 5) genPartAny
    assert $ Parts.partExists pid (part : others)

-- | Property: partExists returns False when part doesn't exist
prop_partExistsFalse :: Property
prop_partExistsFalse = property $ do
    pid <- forAll genNonEmptyText
    otherPid <- forAll (Gen.filter (/= pid) genNonEmptyText)
    part <- forAll (genPart otherPid)
    assert $ not $ Parts.partExists pid [part]

-- | Property: partExists returns False for empty list
prop_partExistsEmpty :: Property
prop_partExistsEmpty = property $ do
    pid <- forAll genNonEmptyText
    assert $ not $ Parts.partExists pid []

-- ═══════════════════════════════════════════════════════════════════════════
-- mergePart Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: mergePart merges objects correctly
prop_mergePartObjects :: Property
prop_mergePartObjects = property $ do
    key1 <- forAll genNonEmptyText
    val1 <- forAll genNonEmptyText
    key2 <- forAll genNonEmptyText
    val2 <- forAll genNonEmptyText
    let old = object [Key.fromText key1 .= val1]
    let new = object [Key.fromText key2 .= val2]
    let merged = Parts.mergePart old new
    case merged of
        Object obj -> do
            -- Both keys should be present
            KM.size obj === if key1 == key2 then 1 else 2
        Null -> failure
        String _ -> failure
        Number _ -> failure
        Bool _ -> failure
        Array _ -> failure

-- | Property: mergePart patch overrides old values
prop_mergePartOverride :: Property
prop_mergePartOverride = property $ do
    key <- forAll genNonEmptyText
    oldVal <- forAll genNonEmptyText
    newVal <- forAll genNonEmptyText
    let old = object [Key.fromText key .= oldVal]
    let new = object [Key.fromText key .= newVal]
    Parts.mergePart old new === new

-- | Property: mergePart with non-object returns patch
prop_mergePartNonObject :: Property
prop_mergePartNonObject = property $ do
    patch <- forAll genPatch
    Parts.mergePart Null patch === patch
    Parts.mergePart (String "old") patch === patch

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 50) Gen.alphaNum

genPart :: Text -> Gen Value
genPart pid = do
    content <- genText
    pure $ object ["id" .= pid, "type" .= ("text" :: Text), "text" .= content]

genPartAny :: Gen Value
genPartAny = do
    pid <- genNonEmptyText
    genPart pid

genPatch :: Gen Value
genPatch = do
    content <- genNonEmptyText
    pure $ object ["text" .= content]

mergeExpected :: Value -> Value -> Value
mergeExpected (Object old) (Object new) = Object (KM.union new old)
mergeExpected _ new = new

tests :: TestTree
tests =
    testGroup
        "Message Part Property Tests"
        [ testGroup
            "findPart"
            [ testProperty "locates part" prop_findPart
            , testProperty "in nested structure" prop_findPartInNestedStructure
            ]
        , testGroup
            "updatePart"
            [ testProperty "merges patch" prop_updatePart
            , testProperty "missing part returns Nothing" prop_updateMissingPart
            , testProperty "preserves other parts" prop_updatePreservesOtherParts
            , testProperty "is idempotent" prop_updatePartIdempotent
            ]
        , testGroup
            "deletePart"
            [ testProperty "removes part" prop_deletePart
            , testProperty "missing part returns Nothing" prop_deleteMissingPart
            , testProperty "delete twice" prop_deletePartTwice
            ]
        , testGroup
            "partId"
            [ testProperty "extracts from id field" prop_partIdFromId
            , testProperty "extracts from partID field" prop_partIdFromPartID
            , testProperty "prefers id over partID" prop_partIdPrefersId
            , testProperty "returns Nothing for non-object" prop_partIdNonObject
            , testProperty "returns Nothing for no id field" prop_partIdNoIdField
            ]
        , testGroup
            "partExists"
            [ testProperty "returns True when exists" prop_partExistsTrue
            , testProperty "returns False when missing" prop_partExistsFalse
            , testProperty "returns False for empty list" prop_partExistsEmpty
            ]
        , testGroup
            "mergePart"
            [ testProperty "merges objects correctly" prop_mergePartObjects
            , testProperty "patch overrides old values" prop_mergePartOverride
            , testProperty "non-object returns patch" prop_mergePartNonObject
            ]
        , testProperty "empty parts operations" prop_emptyPartsOperations
        ]
