{- |
Module      : Pty.Parse
Description : JSON parsing utilities for PTY input

This module provides utilities for parsing PTY creation input from JSON values.
It handles malformed input gracefully by returning default values.
-}
module Pty.Parse (
    -- * Parsing
    parseInput,

    -- * Default Values
    defaultCreatePtyInput,
) where

import Data.Aeson (Result (..), Value, fromJSON)
import Pty.Types

{- | Parse a JSON 'Value' into a 'CreatePtyInput'.

If the JSON is malformed or does not match the expected structure,
returns 'defaultCreatePtyInput' with all fields set to 'Nothing'.

==== __Examples__

>>> import Data.Aeson (object, (.=))
>>> parseInput (object ["cwd" .= "/tmp"])
CreatePtyInput {cpiCommand = Nothing, ..., cpiCwd = Just "/tmp", ...}

>>> parseInput (object [])
CreatePtyInput {cpiCommand = Nothing, cpiArgs = Nothing, ...}

@since 0.1.0
-}
parseInput :: Value -> CreatePtyInput
parseInput input = case fromJSON input of
    Success value -> value
    Error _err -> defaultCreatePtyInput

{- | Default 'CreatePtyInput' with all fields set to 'Nothing'.

This represents a PTY creation request with no specific configuration,
which will use system defaults (e.g., the user's shell).

@since 0.1.0
-}
defaultCreatePtyInput :: CreatePtyInput
defaultCreatePtyInput =
    CreatePtyInput
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
