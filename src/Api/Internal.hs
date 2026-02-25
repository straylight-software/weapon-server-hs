{-# LANGUAGE OverloadedStrings #-}

{- | ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                             // weapon-server // api/internal
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Internal utilities for API type JSON serialization.
This module provides shared helpers for consistent JSON encoding/decoding
across all API types, reducing boilerplate and ensuring uniform behavior.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-}
module Api.Internal (
    -- * Optional Field Encoding
    -- $optionalFields
    optField,
    optFields,

    -- * JSON Object Building
    -- $objectBuilding
    buildObject,
) where

import Data.Aeson (KeyValue, ToJSON, (.=))
import Data.Aeson.Key (Key)

{- $optionalFields
Helper functions for encoding optional fields in JSON.
These eliminate the common @maybe [] (\\x -> [field .= x])@ pattern.
-}

{- | Encode an optional field, returning an empty list if 'Nothing'.

Used in 'ToJSON' instances to conditionally include optional fields:

@
toJSON foo = object $
    [ "required" .= requiredField foo ]
    ++ optField "optional" (optionalField foo)
@
-}
optField :: (KeyValue e kv, ToJSON v) => Key -> Maybe v -> [kv]
optField key = maybe [] (\v -> [key .= v])
{-# INLINE optField #-}

{- | Encode multiple optional fields at once.

@
toJSON foo = object $
    [ "required" .= requiredField foo ]
    ++ optFields
        [ ("opt1", toJSON \<$\> optField1 foo)
        , ("opt2", toJSON \<$\> optField2 foo)
        ]
@
-}
optFields :: (KeyValue e kv) => [(Key, Maybe a)] -> [kv]
optFields [] = []
optFields ((_, Nothing) : rest) = optFields rest
optFields ((_, Just _) : _) = error "optFields: use optField for Maybe values with toJSON"
{-# INLINE optFields #-}

{- $objectBuilding
Utilities for building JSON objects with mixed required and optional fields.
-}

{- | Build a JSON object from required fields and a list of optional field generators.

This helper enables a cleaner pattern for complex ToJSON instances:

@
toJSON info = buildObject
    [ "id" .= infoId info
    , "name" .= infoName info
    ]
    [ optField "description" (infoDescription info)
    , optField "metadata" (infoMetadata info)
    ]
@
-}
buildObject :: [a] -> [[a]] -> [a]
buildObject required optionals = required ++ concat optionals
{-# INLINE buildObject #-}
