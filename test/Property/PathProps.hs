{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.PathProps
Description : Property tests for Path.Build module

Property-based tests verifying the correctness of path building
functions, JSON serialization, and path component handling.
-}
module Property.PathProps where

import Api (PathInfo (..))
import Data.Aeson (decode, encode)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Path.Build (PathComponents (..))
import Path.Build qualified as PathBuild
import Test.Tasty
import Test.Tasty.Hedgehog

-- * Generators

{- | Generate arbitrary text suitable for path components.
Uses alphanumeric characters to avoid issues with special path characters.
-}
genText :: Gen Text
genText = Gen.text (Range.linear 1 20) Gen.alphaNum

-- | Generate a path-like text string with slashes.
genPathText :: Gen Text
genPathText = do
    segments <- Gen.list (Range.linear 1 5) $ Gen.text (Range.linear 1 10) Gen.alphaNum
    pure $ "/" <> T.intercalate "/" segments

-- | Generate a 'PathComponents' record with arbitrary values.
genPathComponents :: Gen PathComponents
genPathComponents =
    PathComponents
        <$> genPathText
        <*> genPathText
        <*> genPathText
        <*> genPathText
        <*> genPathText

-- * Properties for buildPath

-- | Property: buildPath correctly assigns all fields.
prop_buildPath :: Property
prop_buildPath = property $ do
    home <- forAll genText
    state <- forAll genText
    config <- forAll genText
    worktree <- forAll genText
    directory <- forAll genText
    let PathInfo h s c w d = PathBuild.buildPath home state config worktree directory
    h === home
    s === state
    c === config
    w === worktree
    d === directory

-- | Property: PathInfo survives JSON round-trip encoding.
prop_pathJsonRoundtrip :: Property
prop_pathJsonRoundtrip = property $ do
    home <- forAll genText
    state <- forAll genText
    config <- forAll genText
    worktree <- forAll genText
    directory <- forAll genText
    let info = PathBuild.buildPath home state config worktree directory
    case decode (encode info) of
        Nothing -> failure
        Just info' -> info' === info

-- * Properties for buildPathFromComponents

-- | Property: buildPathFromComponents produces same result as buildPath.
prop_buildPathFromComponents_equivalence :: Property
prop_buildPathFromComponents_equivalence = property $ do
    pc <- forAll genPathComponents
    let fromComponents = PathBuild.buildPathFromComponents pc
    let direct =
            PathBuild.buildPath
                (pcHome pc)
                (pcState pc)
                (pcConfig pc)
                (pcWorktree pc)
                (pcDirectory pc)
    fromComponents === direct

-- | Property: buildPathFromComponents correctly extracts all fields.
prop_buildPathFromComponents_fields :: Property
prop_buildPathFromComponents_fields = property $ do
    pc <- forAll genPathComponents
    let PathInfo h s c w d = PathBuild.buildPathFromComponents pc
    h === pcHome pc
    s === pcState pc
    c === pcConfig pc
    w === pcWorktree pc
    d === pcDirectory pc

-- * Properties for computeStateDir

-- | Property: computeStateDir always ends with ".opencode/state".
prop_computeStateDir_suffix :: Property
prop_computeStateDir_suffix = property $ do
    worktree <- forAll genPathText
    let stateDir = PathBuild.computeStateDir worktree
    assert $ T.isSuffixOf "/.opencode/state" stateDir

-- | Property: computeStateDir produces consistent output regardless of trailing slash.
prop_computeStateDir_trailing_slash_invariant :: Property
prop_computeStateDir_trailing_slash_invariant = property $ do
    worktree <- forAll genPathText
    let withSlash = PathBuild.computeStateDir (worktree <> "/")
    let withoutSlash = PathBuild.computeStateDir worktree
    withSlash === withoutSlash

-- | Property: computeStateDir never produces double slashes before .opencode.
prop_computeStateDir_no_double_slash :: Property
prop_computeStateDir_no_double_slash = property $ do
    worktree <- forAll genPathText
    let stateDir = PathBuild.computeStateDir worktree
    assert $ not $ T.isInfixOf "//.opencode" stateDir

-- | Property: computeStateDir preserves the worktree prefix.
prop_computeStateDir_preserves_prefix :: Property
prop_computeStateDir_preserves_prefix = property $ do
    worktree <- forAll genPathText
    let normalizedWorktree = T.dropWhileEnd (== '/') worktree
    let stateDir = PathBuild.computeStateDir worktree
    assert $ T.isPrefixOf normalizedWorktree stateDir

-- * Test Tree

tests :: TestTree
tests =
    testGroup
        "Path Property Tests"
        [ testGroup
            "buildPath"
            [ testProperty "correctly assigns all fields" prop_buildPath
            , testProperty "JSON round-trip" prop_pathJsonRoundtrip
            ]
        , testGroup
            "buildPathFromComponents"
            [ testProperty "equivalent to buildPath" prop_buildPathFromComponents_equivalence
            , testProperty "correctly extracts all fields" prop_buildPathFromComponents_fields
            ]
        , testGroup
            "computeStateDir"
            [ testProperty "always ends with .opencode/state" prop_computeStateDir_suffix
            , testProperty "trailing slash invariant" prop_computeStateDir_trailing_slash_invariant
            , testProperty "no double slashes" prop_computeStateDir_no_double_slash
            , testProperty "preserves worktree prefix" prop_computeStateDir_preserves_prefix
            ]
        ]
