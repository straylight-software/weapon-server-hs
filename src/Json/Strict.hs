{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Json.Strict
Description : Strict JSON parsing utilities

This module provides utilities for strict JSON parsing that rejects
unknown properties. This is required for OpenAPI compliance where
additionalProperties should be rejected.

== Usage

Instead of using 'withObject' directly, use 'withStrictObject':

@
instance FromJSON MyType where
    parseJSON = withStrictObject "MyType" ["field1", "field2"] $ \\v ->
        MyType
            \<$\> v .:? "field1"
            \<*\> v .:? "field2"
@

This will reject any JSON object that contains keys not in the allowed list.

== Strict Optional Fields

Use '(.:!?)' instead of '(.:?)' when you want to allow missing fields but
reject explicit @null@ values. This is required for OpenAPI compliance where
a field is optional (can be omitted) but not nullable (cannot be @null@).

@
instance FromJSON MyType where
    parseJSON = withStrictObject "MyType" ["name"] $ \\v ->
        MyType \<$\> v .:!? "name"  -- Rejects {"name": null}
@
-}
module Json.Strict (
    withStrictObject,
    checkUnknownKeys,
    (.:!?),
) where

import Data.Aeson (FromJSON, Object, Value (..))
import Data.Aeson qualified as A
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

{- | Parse a JSON object while rejecting unknown keys.

This is a strict version of 'withObject' that validates that all keys
in the JSON object are in the allowed set. If unknown keys are found,
parsing fails with an error message.

@
instance FromJSON Person where
    parseJSON = withStrictObject "Person" ["name", "age"] $ \\v ->
        Person \<$\> v .: "name" \<*\> v .: "age"
@
-}
withStrictObject ::
    -- | Type name for error messages
    String ->
    -- | List of allowed keys
    [Text] ->
    -- | Parser function
    (Object -> Parser a) ->
    -- | JSON value to parse
    Value ->
    Parser a
withStrictObject name allowedKeys f = A.withObject name $ \obj -> do
    checkUnknownKeys name allowedKeys obj
    f obj

{- | Check for unknown keys in a JSON object.

Fails the parser if any keys in the object are not in the allowed set.
-}
checkUnknownKeys ::
    -- | Type name for error messages
    String ->
    -- | List of allowed keys
    [Text] ->
    -- | JSON object to check
    Object ->
    Parser ()
checkUnknownKeys typeName allowedKeys obj = do
    let allowedSet = Set.fromList allowedKeys
        actualKeys = map Key.toText (KM.keys obj)
        unknownKeys = filter (`Set.notMember` allowedSet) actualKeys
    case unknownKeys of
        [] -> pure ()
        (k : _) ->
            fail $
                typeName
                    <> ": unknown property '"
                    <> T.unpack k
                    <> "'. Allowed properties: "
                    <> show allowedKeys

{- | Strict optional field parser that rejects explicit @null@.

Like '(.:?)' but fails if the field is present with value @null@.
This is required for OpenAPI compliance where a field is optional
(can be omitted) but not nullable (cannot be @null@).

* Missing field → @Nothing@
* Present with valid value → @Just value@
* Present with @null@ → Parse failure

@
data Person = Person { name :: Maybe Text }

instance FromJSON Person where
    parseJSON = withStrictObject "Person" ["name"] $ \\v ->
        Person \<$\> v .:!? "name"
@
-}
(.:!?) :: (FromJSON a) => Object -> Key -> Parser (Maybe a)
obj .:!? key = case KM.lookup key obj of
    Nothing -> pure Nothing
    Just Null -> fail $ "Unexpected null for field '" <> T.unpack (Key.toText key) <> "'"
    Just v -> Just <$> A.parseJSON v
