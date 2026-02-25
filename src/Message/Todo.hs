{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Message.Todo
Description : Todo item extraction from message parts

This module provides pure functions for extracting todo items from
message parts. Todo parts are identified by a "type" field with value
"todo" and contain an "items" array.

= Example

@
let todoPart = object
      [ "type" .= "todo"
      , "items" .= [object ["text" .= "Buy milk"], object ["text" .= "Call mom"]]
      ]
extractTodos [todoPart]
-- Returns: [{"text": "Buy milk"}, {"text": "Call mom"}]
@
-}
module Message.Todo (
    -- * Todo extraction
    extractTodos,
    extractTodosFromPart,

    -- * Predicates
    isTodoPart,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Text (Text)

{- | Extract all todo items from a list of message parts.

Finds all parts with type "todo" and extracts their "items" arrays,
flattening the results into a single list.

>>> let todo = object ["type" .= "todo", "items" .= [object ["text" .= "task1"]]]
>>> let text = object ["type" .= "text", "text" .= "hello"]
>>> extractTodos [todo, text]
[Object (fromList [("text","task1")])]
-}
extractTodos :: [Value] -> [Value]
extractTodos = concatMap extractTodosFromPart

{- | Extract todo items from a single message part.

Returns the items array contents if the part is a todo part,
otherwise returns an empty list.

>>> let todo = object ["type" .= "todo", "items" .= [object ["done" .= False]]]
>>> extractTodosFromPart todo
[Object (fromList [("done",Bool False)])]
>>> extractTodosFromPart (String "not an object")
[]
-}
extractTodosFromPart :: Value -> [Value]
extractTodosFromPart (Object obj)
    | isTodoType obj = extractItems obj
    | otherwise = []
extractTodosFromPart (Array _) = []
extractTodosFromPart (String _) = []
extractTodosFromPart (Number _) = []
extractTodosFromPart (Bool _) = []
extractTodosFromPart Null = []

{- | Check if a message part is a todo part.

A part is considered a todo part if it's an object with type "todo".

>>> isTodoPart (object ["type" .= "todo"])
True
>>> isTodoPart (object ["type" .= "text"])
False
>>> isTodoPart Null
False
-}
isTodoPart :: Value -> Bool
isTodoPart (Object obj) = isTodoType obj
isTodoPart (Array _) = False
isTodoPart (String _) = False
isTodoPart (Number _) = False
isTodoPart (Bool _) = False
isTodoPart Null = False

-- | Check if an object has type "todo".
isTodoType :: KM.KeyMap Value -> Bool
isTodoType obj = getStringField "type" obj == Just "todo"

-- | Extract items from a todo object.
extractItems :: KM.KeyMap Value -> [Value]
extractItems obj = case KM.lookup "items" obj of
    Just (Array xs) -> toList xs
    Just (Object _) -> []
    Just (String _) -> []
    Just (Number _) -> []
    Just (Bool _) -> []
    Just Null -> []
    Nothing -> []

-- | Get a string field value from a JSON object.
getStringField :: Text -> KM.KeyMap Value -> Maybe Text
getStringField key obj = case KM.lookup (Key.fromText key) obj of
    Just (String t) -> Just t
    Just (Object _) -> Nothing
    Just (Array _) -> Nothing
    Just (Number _) -> Nothing
    Just (Bool _) -> Nothing
    Just Null -> Nothing
    Nothing -> Nothing
