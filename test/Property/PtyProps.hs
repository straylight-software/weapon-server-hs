{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.PtyProps
Description : Property tests for Pty.Parse module

Property tests for PTY input parsing, including field preservation,
default values, and error handling.
-}
module Property.PtyProps where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pty.Parse qualified as PtyParse
import Pty.Types qualified as PtyT
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

-- | Generate alphanumeric text
genAlphaNumText :: Gen Text
genAlphaNumText = Gen.text (Range.linear 1 50) Gen.alphaNum

-- | Generate a valid file path
genFilePath :: Gen Text
genFilePath = do
    segments <- Gen.list (Range.linear 1 4) (Gen.text (Range.linear 1 10) Gen.alphaNum)
    case segments of
        [] -> pure "/tmp" -- Should not happen due to Range.linear 1 4
        [x] -> pure $ "/" <> x
        xs -> pure $ "/" <> mconcat (map (<> "/") (initSafe xs)) <> lastSafe xs
  where
    initSafe [] = []
    initSafe [_] = []
    initSafe (x : xs') = x : initSafe xs'
    lastSafe [] = "tmp"
    lastSafe [x] = x
    lastSafe (_ : xs') = lastSafe xs'

-- | Generate environment variable pairs
genEnvPairs :: Gen [(Text, Text)]
genEnvPairs =
    Gen.list
        (Range.linear 0 5)
        ((,) <$> genAlphaNumText <*> genAlphaNumText)

-- | Generate mount specifications
genMounts :: Gen [(Text, Text, Bool)]
genMounts =
    Gen.list
        (Range.linear 0 3)
        ((,,) <$> genFilePath <*> genFilePath <*> Gen.bool)

-- ============================================================================
-- Parse Input Properties
-- ============================================================================

-- | Property: parseInput preserves all specified fields
prop_parseInputPreserves :: Property
prop_parseInputPreserves = property $ do
    cwd <- forAll genFilePath
    title <- forAll genAlphaNumText
    sandbox <- forAll Gen.bool
    let input =
            object
                [ "cwd" .= cwd
                , "title" .= title
                , "sandbox" .= sandbox
                ]
    let parsed = PtyParse.parseInput input
    PtyT.cpiCwd parsed === Just cwd
    PtyT.cpiTitle parsed === Just title
    PtyT.cpiSandbox parsed === Just sandbox

-- | Property: parseInput with all fields preserves everything
prop_parseInputAllFields :: Property
prop_parseInputAllFields = property $ do
    cmd <- forAll genAlphaNumText
    args <- forAll $ Gen.list (Range.linear 0 5) genAlphaNumText
    cwd <- forAll genFilePath
    title <- forAll genAlphaNumText
    env <- forAll genEnvPairs
    sandbox <- forAll Gen.bool
    network <- forAll Gen.bool
    sessionId <- forAll genAlphaNumText
    let input =
            object
                [ "command" .= cmd
                , "args" .= args
                , "cwd" .= cwd
                , "title" .= title
                , "env" .= env
                , "sandbox" .= sandbox
                , "network" .= network
                , "sessionId" .= sessionId
                ]
    let parsed = PtyParse.parseInput input
    PtyT.cpiCommand parsed === Just cmd
    PtyT.cpiArgs parsed === Just args
    PtyT.cpiCwd parsed === Just cwd
    PtyT.cpiTitle parsed === Just title
    PtyT.cpiEnv parsed === Just env
    PtyT.cpiSandbox parsed === Just sandbox
    PtyT.cpiNetwork parsed === Just network
    PtyT.cpiSessionId parsed === Just sessionId

-- | Property: parseInput returns all Nothing for empty object
prop_parseInputDefaults :: Property
prop_parseInputDefaults = property $ do
    let parsed = PtyParse.parseInput (object [])
    PtyT.cpiCommand parsed === Nothing
    PtyT.cpiArgs parsed === Nothing
    PtyT.cpiCwd parsed === Nothing
    PtyT.cpiTitle parsed === Nothing
    PtyT.cpiEnv parsed === Nothing
    PtyT.cpiSandbox parsed === Nothing
    PtyT.cpiNetwork parsed === Nothing
    PtyT.cpiMounts parsed === Nothing
    PtyT.cpiSessionId parsed === Nothing

-- | Property: parseInput handles null values as defaults
prop_parseInputNullValues :: Property
prop_parseInputNullValues = property $ do
    let input =
            object
                [ "command" .= Null
                , "cwd" .= Null
                ]
    let parsed = PtyParse.parseInput input
    -- Null values should be treated as Nothing
    PtyT.cpiCommand parsed === Nothing
    PtyT.cpiCwd parsed === Nothing

-- | Property: parseInput is idempotent via defaultCreatePtyInput
prop_defaultCreatePtyInputAllNothing :: Property
prop_defaultCreatePtyInputAllNothing = property $ do
    let def = PtyParse.defaultCreatePtyInput
    PtyT.cpiCommand def === Nothing
    PtyT.cpiArgs def === Nothing
    PtyT.cpiCwd def === Nothing
    PtyT.cpiTitle def === Nothing
    PtyT.cpiEnv def === Nothing
    PtyT.cpiSandbox def === Nothing
    PtyT.cpiNetwork def === Nothing
    PtyT.cpiMounts def === Nothing
    PtyT.cpiSessionId def === Nothing

-- | Property: parseInput handles invalid JSON gracefully
prop_parseInputInvalidJson :: Property
prop_parseInputInvalidJson = property $ do
    -- Non-object values should return defaults
    let invalidInputs =
            [ String "not an object"
            , Number 42
            , Bool True
            , Null
            ]
    -- All should parse to defaults
    let results = map PtyParse.parseInput invalidInputs
    mapM_ (\parsed -> PtyT.cpiCommand parsed === Nothing) results

-- | Property: parseInput partially specified fields
prop_parseInputPartial :: Property
prop_parseInputPartial = property $ do
    cwd <- forAll genFilePath
    let input = object ["cwd" .= cwd]
    let parsed = PtyParse.parseInput input
    PtyT.cpiCwd parsed === Just cwd
    -- Other fields should be Nothing
    PtyT.cpiCommand parsed === Nothing
    PtyT.cpiTitle parsed === Nothing
    PtyT.cpiSandbox parsed === Nothing

-- ============================================================================
-- Test Tree
-- ============================================================================

tests :: TestTree
tests =
    testGroup
        "Pty.Parse Property Tests"
        [ testGroup
            "Field Preservation"
            [ testProperty "preserves specified fields" prop_parseInputPreserves
            , testProperty "preserves all fields" prop_parseInputAllFields
            , testProperty "partial fields" prop_parseInputPartial
            ]
        , testGroup
            "Default Values"
            [ testProperty "empty object returns defaults" prop_parseInputDefaults
            , testProperty "null values as defaults" prop_parseInputNullValues
            , testProperty "defaultCreatePtyInput all Nothing" prop_defaultCreatePtyInputAllNothing
            ]
        , testGroup
            "Error Handling"
            [ testProperty "invalid JSON gracefully handled" prop_parseInputInvalidJson
            ]
        ]
