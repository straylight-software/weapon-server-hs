{-# LANGUAGE OverloadedStrings #-}

module Property.MessagePartProps where

import Control.Monad (when)
import Data.Aeson (Value (..), object, (.=))
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
    let patch = part  -- Use the same value as patch
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
    let nestedPart = object 
            [ "id" .= pid
            , "type" .= ("nested" :: Text)
            , "data" .= object 
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
        [ testProperty "updatePart merges patch" prop_updatePart
        , testProperty "deletePart removes part" prop_deletePart
        , testProperty "findPart locates part" prop_findPart
        , testProperty "update missing part" prop_updateMissingPart
        , testProperty "delete missing part" prop_deleteMissingPart
        , testProperty "update preserves other parts" prop_updatePreservesOtherParts
        -- Additional edge cases
        , testProperty "update part is idempotent" prop_updatePartIdempotent
        , testProperty "delete part twice" prop_deletePartTwice
        , testProperty "find part in nested structure" prop_findPartInNestedStructure
        , testProperty "empty parts operations" prop_emptyPartsOperations
        ]
