{-# LANGUAGE OverloadedStrings #-}

module Message.Todo (
    extractTodos,
) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)

extractTodos :: [Value] -> [Value]
extractTodos = concatMap extract
  where
    extract (Object obj) = case KM.lookup "type" obj of
        Just (String "todo") -> case KM.lookup "items" obj of
            Just (Array xs) -> toList xs
            Just _otherValue -> []
            Nothing -> []
        Just _otherValue -> []
        Nothing -> []
    extract _otherValue = []
