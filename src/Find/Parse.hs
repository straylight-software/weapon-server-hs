{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Find.Parse
Description : Pure parsing functions for CLI tool output

This module provides pure parsing functions for extracting structured data
from command-line tool output. It handles output from:

* @ripgrep@ (@rg@) - for text/symbol search results
* @fd@ - for file path search results

All functions in this module are pure and can be easily tested.
-}
module Find.Parse (
    -- * Ripgrep Output Parsing
    parseRgLine,

    -- * Fd Output Parsing
    parseFdLine,

    -- * Internal Helpers (exported for testing)
    breakOnColon,
    parseLineNumber,
) where

import Data.Text (Text)
import Data.Text qualified as T

{- | Parse a single line of ripgrep output in the format @path:linenum:text@.

Ripgrep outputs matches in the format:

@
filepath:linenumber:matched text content
@

This function extracts the three components and returns them as a tuple.

==== Examples

>>> parseRgLine "src/Main.hs:42:main = putStrLn \"hello\""
Just ("src/Main.hs", 42, "main = putStrLn \"hello\"")

>>> parseRgLine "invalid line without colons"
Nothing

>>> parseRgLine "file.txt:notanumber:text"
Nothing
-}
parseRgLine :: Text -> Maybe (Text, Int, Text)
parseRgLine line = do
    (path, rest) <- breakOnColon line
    (lineTxt, text) <- breakOnColon rest
    lineNum <- parseLineNumber lineTxt
    Just (path, lineNum, text)

{- | Break text on the first colon, returning both parts.

Returns 'Nothing' if there is no colon or if either part would be empty
(except the text after the second colon which may be empty).
-}
breakOnColon :: Text -> Maybe (Text, Text)
breakOnColon t =
    let (before, after) = T.breakOn ":" t
     in if T.null before || T.null after
            then Nothing
            else Just (before, T.drop 1 after)

{- | Parse a line number from text.

Returns 'Nothing' if the text is not a valid integer.
-}
parseLineNumber :: Text -> Maybe Int
parseLineNumber t =
    case reads (T.unpack t) of
        [(n, "")] -> Just n
        _otherReads -> Nothing

{- | Parse a single line of fd output.

Fd outputs one file path per line. This function strips whitespace
and returns 'Nothing' for empty lines.

==== Examples

>>> parseFdLine "src/Main.hs"
Just "src/Main.hs"

>>> parseFdLine "   "
Nothing

>>> parseFdLine ""
Nothing
-}
parseFdLine :: Text -> Maybe Text
parseFdLine line =
    case T.strip line of
        "" -> Nothing
        trimmed -> Just trimmed
