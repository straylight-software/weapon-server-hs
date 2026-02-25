{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Vcs.Internal
Description : Shared internal utilities for VCS operations

This module provides shared internal utilities used by the VCS modules.
It is not intended for direct use outside the Vcs hierarchy.
-}
module Vcs.Internal (
    -- * Line parsing utilities
    splitNonEmptyLines,
    splitTabFields,

    -- * List utilities
    listLength,
    sumInts,
) where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T

-- ═══════════════════════════════════════════════════════════════════════════
-- Line parsing utilities
-- ═══════════════════════════════════════════════════════════════════════════

{- | Split input text into non-empty lines.

Filters out empty lines that result from trailing newlines or
multiple consecutive newlines.

==== __Examples__

>>> splitNonEmptyLines "a\nb\nc"
["a", "b", "c"]

>>> splitNonEmptyLines "a\n\nb\n"
["a", "b"]
-}
splitNonEmptyLines :: Text -> [Text]
splitNonEmptyLines = filter (not . T.null) . T.lines

{- | Split a line into tab-separated fields.

==== __Examples__

>>> splitTabFields "10\t5\tfile.hs"
["10", "5", "file.hs"]
-}
splitTabFields :: Text -> [Text]
splitTabFields = T.splitOn "\t"

-- ═══════════════════════════════════════════════════════════════════════════
-- List utilities
-- ═══════════════════════════════════════════════════════════════════════════

{- | O(n) strict list length.

This avoids the lazy spine issue with 'length' that can cause
space leaks with large lists.
-}
listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0

{- | Sum a list of integers strictly.

Uses a strict left fold to avoid space leaks.
-}
sumInts :: [Int] -> Int
sumInts = List.foldl' (+) 0
