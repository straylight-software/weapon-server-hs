{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for Message.Types module
module Property.MessageTypesProps where

import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Message.Types qualified as Types
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

-- | Generate a valid role text
genValidRoleText :: Gen Text
genValidRoleText = Gen.element ["user", "assistant"]

-- | Generate an invalid role text
genInvalidRoleText :: Gen Text
genInvalidRoleText =
    Gen.filter (`notElem` ["user", "assistant"]) $
        Gen.text (Range.linear 1 20) Gen.alphaNum

-- | Generate a MessageRole
genMessageRole :: Gen Types.MessageRole
genMessageRole = Gen.element [Types.User, Types.Assistant]

-- ============================================================================
-- roleToText / textToRole Tests
-- ============================================================================

-- | Property: roleToText produces expected values
prop_roleToTextUser :: Property
prop_roleToTextUser = property $ do
    Types.roleToText Types.User === "user"

-- | Property: roleToText produces expected values for Assistant
prop_roleToTextAssistant :: Property
prop_roleToTextAssistant = property $ do
    Types.roleToText Types.Assistant === "assistant"

-- | Property: textToRole parses valid role texts
prop_textToRoleValid :: Property
prop_textToRoleValid = property $ do
    Types.textToRole "user" === Just Types.User
    Types.textToRole "assistant" === Just Types.Assistant

-- | Property: textToRole rejects invalid role texts
prop_textToRoleInvalid :: Property
prop_textToRoleInvalid = property $ do
    invalidText <- forAll genInvalidRoleText
    Types.textToRole invalidText === Nothing

-- | Property: roleToText and textToRole are inverses
prop_roleRoundtrip :: Property
prop_roleRoundtrip = property $ do
    role <- forAll genMessageRole
    Types.textToRole (Types.roleToText role) === Just role

-- | Property: textToRole and roleToText are inverses for valid texts
prop_textRoundtrip :: Property
prop_textRoundtrip = property $ do
    text <- forAll genValidRoleText
    case Types.textToRole text of
        Nothing -> failure
        Just role -> Types.roleToText role === text

-- ============================================================================
-- Test Tree
-- ============================================================================

tests :: TestTree
tests =
    testGroup
        "Message.Types Property Tests"
        [ testGroup
            "roleToText"
            [ testProperty "User -> user" prop_roleToTextUser
            , testProperty "Assistant -> assistant" prop_roleToTextAssistant
            ]
        , testGroup
            "textToRole"
            [ testProperty "parses valid texts" prop_textToRoleValid
            , testProperty "rejects invalid texts" prop_textToRoleInvalid
            ]
        , testGroup
            "roundtrip"
            [ testProperty "role -> text -> role" prop_roleRoundtrip
            , testProperty "text -> role -> text" prop_textRoundtrip
            ]
        ]
