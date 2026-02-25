{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Message.Parts
Description : Operations on message parts

This module provides pure functions for manipulating message parts.
Parts are represented as generic JSON 'Value's and identified by their
"id" or "partID" field.

All functions in this module are pure and can be easily tested.
-}
module Message.Parts (
    -- * Part operations
    findPart,
    updatePart,
    deletePart,

    -- * Part utilities
    partId,
    mergePart,
    partExists,
) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.List (find)
import Data.Text (Text)

{- | Find a part by its ID in a list of parts.

Searches for a part with a matching "id" or "partID" field.

>>> let part = object ["id" .= "abc", "text" .= "hello"]
>>> findPart "abc" [part]
Just (Object ...)
>>> findPart "xyz" [part]
Nothing
-}
findPart :: Text -> [Value] -> Maybe Value
findPart pid = find (hasId pid)

{- | Check if a part has the given ID.

A pure predicate used by other functions.
-}
hasId :: Text -> Value -> Bool
hasId pid part = partId part == Just pid

{- | Check if a part with the given ID exists in the list.

>>> let part = object ["id" .= "abc"]
>>> partExists "abc" [part]
True
>>> partExists "xyz" [part]
False
-}
partExists :: Text -> [Value] -> Bool
partExists pid = any (hasId pid)

{- | Update a part by its ID, applying a patch.

If the part exists, merges the patch into it and returns the updated list.
Returns 'Nothing' if no part with the given ID exists.

The patch is merged using JSON object union (patch values override old values).

>>> let part = object ["id" .= "abc", "text" .= "hello"]
>>> let patch = object ["text" .= "world"]
>>> updatePart "abc" patch [part]
Just [Object (fromList [("id","abc"),("text","world")])]
-}
updatePart :: Text -> Value -> [Value] -> Maybe [Value]
updatePart pid patch parts
    | partExists pid parts = Just (map applyIfMatch parts)
    | otherwise = Nothing
  where
    applyIfMatch part
        | hasId pid part = mergePart part patch
        | otherwise = part

{- | Delete a part by its ID.

Returns the list with the part removed, or 'Nothing' if no part
with the given ID exists.

>>> let part = object ["id" .= "abc"]
>>> deletePart "abc" [part]
Just []
>>> deletePart "xyz" [part]
Nothing
-}
deletePart :: Text -> [Value] -> Maybe [Value]
deletePart pid parts
    | partExists pid parts = Just (filter (not . hasId pid) parts)
    | otherwise = Nothing

{- | Extract the ID from a part value.

Looks for the ID in the "id" field first, then falls back to "partID".
Returns 'Nothing' if neither field contains a string value.

>>> partId (object ["id" .= "abc"])
Just "abc"
>>> partId (object ["partID" .= "xyz"])
Just "xyz"
>>> partId (object ["other" .= "value"])
Nothing
>>> partId Null
Nothing
-}
partId :: Value -> Maybe Text
partId (Object obj) = extractStringField "id" obj <|> extractStringField "partID" obj
partId (Array _) = Nothing
partId (String _) = Nothing
partId (Number _) = Nothing
partId (Bool _) = Nothing
partId Null = Nothing

{- | Try to extract a string value from a field in a JSON object.

Returns 'Nothing' if the field doesn't exist or isn't a string.
-}
extractStringField :: KM.Key -> KM.KeyMap Value -> Maybe Text
extractStringField key obj = case KM.lookup key obj of
    Just (String t) -> Just t
    Just (Object _) -> Nothing
    Just (Array _) -> Nothing
    Just (Number _) -> Nothing
    Just (Bool _) -> Nothing
    Just Null -> Nothing
    Nothing -> Nothing

-- | Alternative operator for Maybe (avoids importing Control.Applicative)
infixl 3 <|>

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _ = Just x
Nothing <|> y = y

{- | Merge two JSON values, preferring values from the second (patch).

For objects, performs a shallow merge where patch keys override old keys.
For non-objects, the patch value replaces the old value entirely.

>>> mergePart (object ["a" .= 1, "b" .= 2]) (object ["b" .= 3])
Object (fromList [("a",1),("b",3)])
-}
mergePart :: Value -> Value -> Value
mergePart (Object old) (Object new) = Object (KM.union new old)
mergePart _ new = new
