{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for Message.Todo module
module Property.TodoProps where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Message.Todo qualified as Todo
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

-- | Generate a todo item value
genItem :: Gen Value
genItem = do
    text <- Gen.text (Range.linear 1 30) Gen.alphaNum
    done <- Gen.bool
    pure $ object ["text" .= text, "done" .= done]

-- | Generate a non-todo part
genNonTodoPart :: Gen Value
genNonTodoPart = do
    text <- Gen.text (Range.linear 1 30) Gen.alphaNum
    Gen.element
        [ object ["type" .= ("text" :: Text), "text" .= text]
        , object ["type" .= ("code" :: Text), "text" .= text]
        , object ["type" .= ("file" :: Text), "url" .= text]
        ]

-- | Generate a todo part with items
genTodoPart :: [Value] -> Value
genTodoPart items = object ["type" .= ("todo" :: Text), "items" .= items]

-- | Generate an empty todo part (no items field)
genEmptyTodoPart :: Value
genEmptyTodoPart = object ["type" .= ("todo" :: Text)]

-- | Generate a todo part with non-array items
genInvalidTodoPart :: Gen Value
genInvalidTodoPart = do
    text <- Gen.text (Range.linear 1 30) Gen.alphaNum
    pure $ object ["type" .= ("todo" :: Text), "items" .= text]

-- ============================================================================
-- Property Tests: extractTodos
-- ============================================================================

-- | Property: extractTodos extracts all items from todo parts
prop_extractTodos :: Property
prop_extractTodos = property $ do
    items <- forAll $ Gen.list (Range.linear 0 10) genItem
    parts <- forAll $ Gen.list (Range.linear 0 5) genNonTodoPart
    let todo = genTodoPart items
    let allParts = todo : parts
    let extracted = Todo.extractTodos allParts
    extracted === items

-- | Property: extractTodos returns empty for non-todo parts only
prop_extractTodosEmptyForNonTodos :: Property
prop_extractTodosEmptyForNonTodos = property $ do
    parts <- forAll $ Gen.list (Range.linear 0 10) genNonTodoPart
    Todo.extractTodos parts === []

-- | Property: extractTodos handles multiple todo parts
prop_extractTodosMultiple :: Property
prop_extractTodosMultiple = property $ do
    items1 <- forAll $ Gen.list (Range.linear 1 5) genItem
    items2 <- forAll $ Gen.list (Range.linear 1 5) genItem
    let todo1 = genTodoPart items1
    let todo2 = genTodoPart items2
    let extracted = Todo.extractTodos [todo1, todo2]
    extracted === items1 ++ items2

-- | Property: extractTodos handles empty list
prop_extractTodosEmptyList :: Property
prop_extractTodosEmptyList = property $ do
    Todo.extractTodos [] === []

-- | Property: extractTodos handles todo without items field
prop_extractTodosNoItemsField :: Property
prop_extractTodosNoItemsField = property $ do
    Todo.extractTodos [genEmptyTodoPart] === []

-- | Property: extractTodos handles todo with non-array items
prop_extractTodosInvalidItems :: Property
prop_extractTodosInvalidItems = property $ do
    invalidPart <- forAll genInvalidTodoPart
    Todo.extractTodos [invalidPart] === []

-- ============================================================================
-- Property Tests: extractTodosFromPart
-- ============================================================================

-- | Property: extractTodosFromPart extracts items from a single todo
prop_extractTodosFromPartSingle :: Property
prop_extractTodosFromPartSingle = property $ do
    items <- forAll $ Gen.list (Range.linear 0 10) genItem
    let todo = genTodoPart items
    Todo.extractTodosFromPart todo === items

-- | Property: extractTodosFromPart returns empty for non-todo
prop_extractTodosFromPartNonTodo :: Property
prop_extractTodosFromPartNonTodo = property $ do
    part <- forAll genNonTodoPart
    Todo.extractTodosFromPart part === []

-- | Property: extractTodosFromPart handles non-object values
prop_extractTodosFromPartNonObject :: Property
prop_extractTodosFromPartNonObject = property $ do
    Todo.extractTodosFromPart Null === []
    Todo.extractTodosFromPart (String "test") === []
    Todo.extractTodosFromPart (Number 42) === []
    Todo.extractTodosFromPart (Bool True) === []

-- ============================================================================
-- Property Tests: isTodoPart
-- ============================================================================

-- | Property: isTodoPart returns True for todo parts
prop_isTodoPartTrue :: Property
prop_isTodoPartTrue = property $ do
    items <- forAll $ Gen.list (Range.linear 0 5) genItem
    let todo = genTodoPart items
    assert $ Todo.isTodoPart todo

-- | Property: isTodoPart returns True even for empty todo
prop_isTodoPartEmpty :: Property
prop_isTodoPartEmpty = property $ do
    assert $ Todo.isTodoPart genEmptyTodoPart

-- | Property: isTodoPart returns False for non-todo parts
prop_isTodoPartFalse :: Property
prop_isTodoPartFalse = property $ do
    part <- forAll genNonTodoPart
    assert $ not $ Todo.isTodoPart part

-- | Property: isTodoPart returns False for non-object values
prop_isTodoPartNonObject :: Property
prop_isTodoPartNonObject = property $ do
    assert $ not $ Todo.isTodoPart Null
    assert $ not $ Todo.isTodoPart (String "todo")
    assert $ not $ Todo.isTodoPart (Number 0)

-- ============================================================================
-- Test Tree
-- ============================================================================

tests :: TestTree
tests =
    testGroup
        "Todo Property Tests"
        [ testGroup
            "extractTodos"
            [ testProperty "extracts items from todo parts" prop_extractTodos
            , testProperty "returns empty for non-todo parts" prop_extractTodosEmptyForNonTodos
            , testProperty "handles multiple todo parts" prop_extractTodosMultiple
            , testProperty "handles empty list" prop_extractTodosEmptyList
            , testProperty "handles todo without items field" prop_extractTodosNoItemsField
            , testProperty "handles todo with non-array items" prop_extractTodosInvalidItems
            ]
        , testGroup
            "extractTodosFromPart"
            [ testProperty "extracts items from single todo" prop_extractTodosFromPartSingle
            , testProperty "returns empty for non-todo" prop_extractTodosFromPartNonTodo
            , testProperty "handles non-object values" prop_extractTodosFromPartNonObject
            ]
        , testGroup
            "isTodoPart"
            [ testProperty "returns True for todo parts" prop_isTodoPartTrue
            , testProperty "returns True for empty todo" prop_isTodoPartEmpty
            , testProperty "returns False for non-todo parts" prop_isTodoPartFalse
            , testProperty "returns False for non-object values" prop_isTodoPartNonObject
            ]
        ]
