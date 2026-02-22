{-# LANGUAGE OverloadedStrings #-}

module Message.Parts (
    findPart,
    updatePart,
    deletePart,
) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.List (find)
import Data.Text (Text)

findPart :: Text -> [Value] -> Maybe Value
findPart pid = find (\part -> partId part == Just pid)

updatePart :: Text -> Value -> [Value] -> Maybe [Value]
updatePart pid patch parts =
    if any (\part -> partId part == Just pid) parts
        then Just (map apply parts)
        else Nothing
  where
    apply part = case partId part of
        Just pid' | pid' == pid -> mergePart part patch
        _otherPartId -> part

deletePart :: Text -> [Value] -> Maybe [Value]
deletePart pid parts =
    if any (\part -> partId part == Just pid) parts
        then Just (filter (\part -> partId part /= Just pid) parts)
        else Nothing

partId :: Value -> Maybe Text
partId (Object obj) = case KM.lookup "id" obj of
    Just (String t) -> Just t
    Just _otherValue -> case KM.lookup "partID" obj of
        Just (String t) -> Just t
        Just _otherId -> Nothing
        Nothing -> Nothing
    Nothing -> case KM.lookup "partID" obj of
        Just (String t) -> Just t
        Just _otherId -> Nothing
        Nothing -> Nothing
partId _otherValue = Nothing

mergePart :: Value -> Value -> Value
mergePart (Object old) (Object new) = Object (KM.union new old)
mergePart _ new = new
