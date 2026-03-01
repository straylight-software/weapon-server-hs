{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.FindParseProps
Description : Property tests for Find.Parse and Find.Search modules

This module contains property-based tests for the parsing and search
functionality in the Find modules. Tests cover:

* Ripgrep output parsing
* Fd output parsing
* Pure transformation functions
* Search options handling
-}
module Property.FindParseProps where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Find.Parse qualified as Parse
import Find.Search qualified as Search
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Helpers (listLength)
import Test.Tasty
import Test.Tasty.Hedgehog

--------------------------------------------------------------------------------
-- Ripgrep Parsing Properties
--------------------------------------------------------------------------------

{- | Property: Valid rg lines are parsed correctly.

A valid rg line has the format: path:linenum:text
-}
prop_parseRgLine :: Property
prop_parseRgLine = property $ do
    path <- forAll genPathWithoutColon
    lineNum <- forAll $ Gen.int (Range.linear 1 10000)
    text <- forAll genText
    let line = path <> ":" <> T.pack (show lineNum) <> ":" <> text
    case Parse.parseRgLine line of
        Nothing -> failure
        Just (p, n, t) -> do
            p === path
            n === lineNum
            t === text

-- | Property: Invalid line numbers are rejected.
prop_parseRgLineInvalid :: Property
prop_parseRgLineInvalid = property $ do
    path <- forAll genPathWithoutColon
    let line = path <> ":" <> "not-a-number" <> ":" <> "text"
    Parse.parseRgLine line === Nothing

-- | Property: Empty text after colon is valid.
prop_parseRgEmptyText :: Property
prop_parseRgEmptyText = property $ do
    path <- forAll genPathWithoutColon
    lineNum <- forAll $ Gen.int (Range.linear 1 10000)
    let line = path <> ":" <> T.pack (show lineNum) <> ":"
    Parse.parseRgLine line === Just (path, lineNum, "")

-- | Property: Lines without colons are rejected.
prop_parseRgLineNoColons :: Property
prop_parseRgLineNoColons = property $ do
    text <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    Parse.parseRgLine text === Nothing

-- | Property: Lines with only one colon are rejected.
prop_parseRgLineOneColon :: Property
prop_parseRgLineOneColon = property $ do
    part1 <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    part2 <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let line = part1 <> ":" <> part2
    -- Should fail because there's no second colon for the line number
    Parse.parseRgLine line === Nothing

-- | Property: Negative line numbers are parsed (rg doesn't produce them, but parser accepts).
prop_parseRgNegativeLineNum :: Property
prop_parseRgNegativeLineNum = property $ do
    path <- forAll genPathWithoutColon
    lineNum <- forAll $ Gen.int (Range.linear (-1000) (-1))
    text <- forAll genText
    let line = path <> ":" <> T.pack (show lineNum) <> ":" <> text
    Parse.parseRgLine line === Just (path, lineNum, text)

--------------------------------------------------------------------------------
-- breakOnColon Helper Properties
--------------------------------------------------------------------------------

-- | Property: breakOnColon correctly splits on first colon.
prop_breakOnColonValid :: Property
prop_breakOnColonValid = property $ do
    beforePart <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    afterPart <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let input = beforePart <> ":" <> afterPart
    Parse.breakOnColon input === Just (beforePart, afterPart)

-- | Property: breakOnColon returns Nothing for empty before.
prop_breakOnColonEmptyBefore :: Property
prop_breakOnColonEmptyBefore = property $ do
    afterPart <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    Parse.breakOnColon (":" <> afterPart) === Nothing

-- | Property: breakOnColon returns Nothing for no colon.
prop_breakOnColonNoColon :: Property
prop_breakOnColonNoColon = property $ do
    text <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    Parse.breakOnColon text === Nothing

-- | Property: breakOnColon handles multiple colons (takes first).
prop_breakOnColonMultiple :: Property
prop_breakOnColonMultiple = property $ do
    part1 <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    part2 <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    part3 <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    let input = part1 <> ":" <> part2 <> ":" <> part3
    Parse.breakOnColon input === Just (part1, part2 <> ":" <> part3)

--------------------------------------------------------------------------------
-- parseLineNumber Helper Properties
--------------------------------------------------------------------------------

-- | Property: parseLineNumber parses valid positive integers.
prop_parseLineNumberValid :: Property
prop_parseLineNumberValid = property $ do
    n <- forAll $ Gen.int (Range.linear 0 1000000)
    Parse.parseLineNumber (T.pack (show n)) === Just n

-- | Property: parseLineNumber parses negative integers.
prop_parseLineNumberNegative :: Property
prop_parseLineNumberNegative = property $ do
    n <- forAll $ Gen.int (Range.linear (-1000000) (-1))
    Parse.parseLineNumber (T.pack (show n)) === Just n

-- | Property: parseLineNumber rejects non-numeric text.
prop_parseLineNumberInvalid :: Property
prop_parseLineNumberInvalid = property $ do
    text <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha
    Parse.parseLineNumber text === Nothing

-- | Property: parseLineNumber rejects mixed text.
prop_parseLineNumberMixed :: Property
prop_parseLineNumberMixed = property $ do
    n <- forAll $ Gen.int (Range.linear 1 1000)
    suffix <- forAll $ Gen.text (Range.linear 1 5) Gen.alpha
    Parse.parseLineNumber (T.pack (show n) <> suffix) === Nothing

-- | Property: parseLineNumber rejects empty string.
prop_parseLineNumberEmpty :: Property
prop_parseLineNumberEmpty = property $ do
    Parse.parseLineNumber "" === Nothing

--------------------------------------------------------------------------------
-- Fd Parsing Properties
--------------------------------------------------------------------------------

-- | Property: Non-empty paths are parsed correctly.
prop_parseFdLine :: Property
prop_parseFdLine = property $ do
    path <- forAll genPath
    Parse.parseFdLine path === Just path

-- | Property: Whitespace-only lines return Nothing.
prop_parseFdLineEmpty :: Property
prop_parseFdLineEmpty = property $ do
    Parse.parseFdLine "   " === Nothing

-- | Property: Empty string returns Nothing.
prop_parseFdLineEmptyString :: Property
prop_parseFdLineEmptyString = property $ do
    Parse.parseFdLine "" === Nothing

-- | Property: Leading/trailing whitespace is stripped.
prop_parseFdLineStripsWhitespace :: Property
prop_parseFdLineStripsWhitespace = property $ do
    path <- forAll genPath
    spaces <- forAll $ Gen.text (Range.linear 1 5) (pure ' ')
    Parse.parseFdLine (spaces <> path <> spaces) === Just path

--------------------------------------------------------------------------------
-- Pure Transformation Properties
--------------------------------------------------------------------------------

-- | Property: rgMatchesToJson produces correct JSON structure.
prop_rgMatchesToJsonStructure :: Property
prop_rgMatchesToJsonStructure = property $ do
    matches <- forAll $ Gen.list (Range.linear 0 20) genRgMatch
    let results = Search.rgMatchesToJson matches
    listLength results === listLength matches

-- | Property: rgMatchesToJson preserves data.
prop_rgMatchesToJsonPreservesData :: Property
prop_rgMatchesToJsonPreservesData = property $ do
    path <- forAll genPathWithoutColon
    lineNum <- forAll $ Gen.int (Range.linear 1 10000)
    text <- forAll genText
    let matches = [(path, lineNum, text)]
    let expected = [object ["path" .= path, "line" .= lineNum, "text" .= text]]
    Search.rgMatchesToJson matches === expected

-- | Property: fdResultsToJson produces correct JSON structure.
prop_fdResultsToJsonStructure :: Property
prop_fdResultsToJsonStructure = property $ do
    paths <- forAll $ Gen.list (Range.linear 0 20) genPath
    let results = Search.fdResultsToJson paths
    listLength results === listLength paths

-- | Property: fdResultsToJson preserves paths as JSON strings.
prop_fdResultsToJsonPreservesPaths :: Property
prop_fdResultsToJsonPreservesPaths = property $ do
    path <- forAll genPath
    let expected = [toJSON path]
    Search.fdResultsToJson [path] === expected

-- | Property: applyResultLimit with Nothing returns all results.
prop_applyResultLimitNothing :: Property
prop_applyResultLimitNothing = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 1000))
    Search.applyResultLimit Nothing xs === xs

-- | Property: applyResultLimit with Just n returns at most n results.
prop_applyResultLimitJust :: Property
prop_applyResultLimitJust = property $ do
    n <- forAll $ Gen.int (Range.linear 0 50)
    xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 1000))
    let result = Search.applyResultLimit (Just n) xs
    listLength result === min n (listLength xs)

-- | Property: applyResultLimit preserves order.
prop_applyResultLimitPreservesOrder :: Property
prop_applyResultLimitPreservesOrder = property $ do
    n <- forAll $ Gen.int (Range.linear 1 20)
    xs <- forAll $ Gen.list (Range.linear n 50) (Gen.int (Range.linear 0 1000))
    let result = Search.applyResultLimit (Just n) xs
    result === take n xs

--------------------------------------------------------------------------------
-- buildFdTypeArgs Properties
--------------------------------------------------------------------------------

-- | Property: File type filter produces correct args.
prop_buildFdTypeArgsFile :: Property
prop_buildFdTypeArgsFile = property $ do
    includeDirs <- forAll Gen.bool
    limit <- forAll $ Gen.maybe $ Gen.int (Range.linear 1 100)
    let opts = Search.FindFileOptions includeDirs (Just "file") limit
    Search.buildFdTypeArgs opts === ["--type", "f"]

-- | Property: Directory type filter produces correct args.
prop_buildFdTypeArgsDirectory :: Property
prop_buildFdTypeArgsDirectory = property $ do
    includeDirs <- forAll Gen.bool
    limit <- forAll $ Gen.maybe $ Gen.int (Range.linear 1 100)
    let opts = Search.FindFileOptions includeDirs (Just "directory") limit
    Search.buildFdTypeArgs opts === ["--type", "d"]

-- | Property: Unknown type defaults to file.
prop_buildFdTypeArgsUnknown :: Property
prop_buildFdTypeArgsUnknown = property $ do
    unknownType <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
    -- Ensure it's not a known type
    case unknownType of
        "file" -> discard
        "directory" -> discard
        _ -> pure ()
    limit <- forAll $ Gen.maybe $ Gen.int (Range.linear 1 100)
    let opts = Search.FindFileOptions False (Just unknownType) limit
    Search.buildFdTypeArgs opts === ["--type", "f"]

-- | Property: No type with includeDirs=True returns empty args.
prop_buildFdTypeArgsIncludeDirs :: Property
prop_buildFdTypeArgsIncludeDirs = property $ do
    limit <- forAll $ Gen.maybe $ Gen.int (Range.linear 1 100)
    let opts = Search.FindFileOptions True Nothing limit
    Search.buildFdTypeArgs opts === []

-- | Property: No type with includeDirs=False defaults to files.
prop_buildFdTypeArgsDefaultFiles :: Property
prop_buildFdTypeArgsDefaultFiles = property $ do
    limit <- forAll $ Gen.maybe $ Gen.int (Range.linear 1 100)
    let opts = Search.FindFileOptions False Nothing limit
    Search.buildFdTypeArgs opts === ["--type", "f"]

--------------------------------------------------------------------------------
-- FindFileOptions Properties
--------------------------------------------------------------------------------

-- | Property: FindFileOptions has sensible defaults.
prop_findFileOptionsDefaults :: Property
prop_findFileOptionsDefaults = property $ do
    let opts = Search.defaultFindFileOptions
    Search.ffoIncludeDirs opts === False
    Search.ffoFileType opts === Nothing
    Search.ffoLimit opts === Nothing

-- | Property: FindFileOptions can be constructed with all options.
prop_findFileOptionsConstruction :: Property
prop_findFileOptionsConstruction = property $ do
    includeDirs <- forAll Gen.bool
    fileType <- forAll $ Gen.maybe $ Gen.element ["file", "directory"]
    limit <- forAll $ Gen.maybe $ Gen.int (Range.linear 1 200)
    let opts = Search.FindFileOptions includeDirs fileType limit
    Search.ffoIncludeDirs opts === includeDirs
    Search.ffoFileType opts === fileType
    Search.ffoLimit opts === limit

-- | Property: FindFileOptions has Eq instance that works correctly.
prop_findFileOptionsEq :: Property
prop_findFileOptionsEq = property $ do
    includeDirs <- forAll Gen.bool
    fileType <- forAll $ Gen.maybe $ Gen.element ["file", "directory"]
    limit <- forAll $ Gen.maybe $ Gen.int (Range.linear 1 200)
    let opts1 = Search.FindFileOptions includeDirs fileType limit
    let opts2 = Search.FindFileOptions includeDirs fileType limit
    opts1 === opts2

--------------------------------------------------------------------------------
-- Roundtrip Properties
--------------------------------------------------------------------------------

-- | Property: Parsing then converting rg output gives same result as direct conversion.
prop_rgRoundtrip :: Property
prop_rgRoundtrip = property $ do
    matches <- forAll $ Gen.list (Range.linear 0 10) genRgMatch
    let lines_ = map formatRgLine matches
    let rawText = T.unlines lines_
    let parsed = mapMaybe Parse.parseRgLine (T.lines rawText)
    let result = Search.rgMatchesToJson parsed
    let expected = Search.rgMatchesToJson matches
    result === expected
  where
    formatRgLine (path, lineNum, text) =
        path <> ":" <> T.pack (show lineNum) <> ":" <> text

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate a file path (may contain colons on Windows, but we avoid them for testing).
genPath :: Gen Text
genPath = do
    name <- Gen.text (Range.linear 1 12) Gen.alphaNum
    ext <- Gen.text (Range.linear 1 3) Gen.alphaNum
    pure (name <> "." <> ext)

-- | Generate a file path without colons (safe for rg parsing tests).
genPathWithoutColon :: Gen Text
genPathWithoutColon = do
    dir <- Gen.text (Range.linear 0 10) Gen.alphaNum
    name <- Gen.text (Range.linear 1 12) Gen.alphaNum
    ext <- Gen.text (Range.linear 1 3) Gen.alphaNum
    pure $ if T.null dir then name <> "." <> ext else dir <> "/" <> name <> "." <> ext

-- | Generate arbitrary text content.
genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

-- | Generate a ripgrep match tuple.
genRgMatch :: Gen (Text, Int, Text)
genRgMatch = do
    path <- genPathWithoutColon
    lineNum <- Gen.int (Range.linear 1 10000)
    text <- genText
    pure (path, lineNum, text)

--------------------------------------------------------------------------------
-- Test Tree
--------------------------------------------------------------------------------

tests :: TestTree
tests =
    testGroup
        "Find Module Property Tests"
        [ testGroup
            "Ripgrep Parsing"
            [ testProperty "parse rg line" prop_parseRgLine
            , testProperty "parse rg invalid line number" prop_parseRgLineInvalid
            , testProperty "parse rg empty text" prop_parseRgEmptyText
            , testProperty "parse rg no colons" prop_parseRgLineNoColons
            , testProperty "parse rg one colon" prop_parseRgLineOneColon
            , testProperty "parse rg negative line number" prop_parseRgNegativeLineNum
            ]
        , testGroup
            "Parse Helpers"
            [ testProperty "breakOnColon valid" prop_breakOnColonValid
            , testProperty "breakOnColon empty before" prop_breakOnColonEmptyBefore
            , testProperty "breakOnColon no colon" prop_breakOnColonNoColon
            , testProperty "breakOnColon multiple colons" prop_breakOnColonMultiple
            , testProperty "parseLineNumber valid" prop_parseLineNumberValid
            , testProperty "parseLineNumber negative" prop_parseLineNumberNegative
            , testProperty "parseLineNumber invalid" prop_parseLineNumberInvalid
            , testProperty "parseLineNumber mixed" prop_parseLineNumberMixed
            , testProperty "parseLineNumber empty" prop_parseLineNumberEmpty
            ]
        , testGroup
            "Fd Parsing"
            [ testProperty "parse fd line" prop_parseFdLine
            , testProperty "parse fd whitespace" prop_parseFdLineEmpty
            , testProperty "parse fd empty string" prop_parseFdLineEmptyString
            , testProperty "parse fd strips whitespace" prop_parseFdLineStripsWhitespace
            ]
        , testGroup
            "JSON Transformations"
            [ testProperty "rgMatchesToJson structure" prop_rgMatchesToJsonStructure
            , testProperty "rgMatchesToJson preserves data" prop_rgMatchesToJsonPreservesData
            , testProperty "fdResultsToJson structure" prop_fdResultsToJsonStructure
            , testProperty "fdResultsToJson preserves paths" prop_fdResultsToJsonPreservesPaths
            ]
        , testGroup
            "Result Limiting"
            [ testProperty "applyResultLimit Nothing" prop_applyResultLimitNothing
            , testProperty "applyResultLimit Just n" prop_applyResultLimitJust
            , testProperty "applyResultLimit preserves order" prop_applyResultLimitPreservesOrder
            ]
        , testGroup
            "Fd Type Args"
            [ testProperty "buildFdTypeArgs file" prop_buildFdTypeArgsFile
            , testProperty "buildFdTypeArgs directory" prop_buildFdTypeArgsDirectory
            , testProperty "buildFdTypeArgs unknown type" prop_buildFdTypeArgsUnknown
            , testProperty "buildFdTypeArgs include dirs" prop_buildFdTypeArgsIncludeDirs
            , testProperty "buildFdTypeArgs default files" prop_buildFdTypeArgsDefaultFiles
            ]
        , testGroup
            "FindFileOptions"
            [ testProperty "defaults" prop_findFileOptionsDefaults
            , testProperty "construction" prop_findFileOptionsConstruction
            , testProperty "equality" prop_findFileOptionsEq
            ]
        , testGroup
            "Roundtrips"
            [ testProperty "rg parse and convert roundtrip" prop_rgRoundtrip
            ]
        ]
