{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.ProjectProps
Description : Property tests for Project.Build module
Stability   : stable

This module contains Hedgehog property tests for the project construction
functions in "Project.Build". Tests cover both the main 'projectFromDir'
function and the pure helper functions 'makeProjectId' and 'makeProjectName'.
-}
module Property.ProjectProps where

import Api (Project (..), ProjectTime (..), id)
import Data.Char (isAsciiLower, isDigit)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Project.Build qualified as ProjectBuild
import Test.Tasty
import Test.Tasty.Hedgehog
import Prelude hiding (id)

-- | Generate a timestamp for testing
genTimestamp :: Gen Double
genTimestamp = Gen.double (Range.linearFrac 1000000000 2000000000)

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Helper Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: makeProjectId with non-empty basename returns "proj_" prefix
prop_makeProjectIdNonEmpty :: Property
prop_makeProjectIdNonEmpty = property $ do
    basename <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let pid = ProjectBuild.makeProjectId basename
    T.isPrefixOf "proj_" pid === True
    T.stripPrefix "proj_" pid === Just basename

-- | Property: makeProjectId with empty basename returns default
prop_makeProjectIdEmpty :: Property
prop_makeProjectIdEmpty = property $ do
    ProjectBuild.makeProjectId "" === "proj_default"

-- | Property: makeProjectName with non-empty basename returns Just basename
prop_makeProjectNameNonEmpty :: Property
prop_makeProjectNameNonEmpty = property $ do
    basename <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    ProjectBuild.makeProjectName basename === Just basename

-- | Property: makeProjectName with empty basename returns Nothing
prop_makeProjectNameEmpty :: Property
prop_makeProjectNameEmpty = property $ do
    ProjectBuild.makeProjectName "" === Nothing

-- | Property: makeProjectId is deterministic
prop_makeProjectIdDeterministic :: Property
prop_makeProjectIdDeterministic = property $ do
    basename <- forAll $ Gen.text (Range.linear 0 20) Gen.alphaNum
    let pid1 = ProjectBuild.makeProjectId basename
    let pid2 = ProjectBuild.makeProjectId basename
    pid1 === pid2

-- | Property: makeProjectName is deterministic
prop_makeProjectNameDeterministic :: Property
prop_makeProjectNameDeterministic = property $ do
    basename <- forAll $ Gen.text (Range.linear 0 20) Gen.alphaNum
    let name1 = ProjectBuild.makeProjectName basename
    let name2 = ProjectBuild.makeProjectName basename
    name1 === name2

-- ═══════════════════════════════════════════════════════════════════════════
-- projectFromDir Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: projectFromDir extracts basename correctly
prop_projectFromDirUsesBase :: Property
prop_projectFromDirUsesBase = property $ do
    base <- forAll $ Gen.text (Range.linear 1 12) Gen.alphaNum
    now <- forAll genTimestamp
    let dir = "/tmp/" <> T.unpack base
    let project = ProjectBuild.projectFromDir now dir
    id project === "proj_" <> base
    worktree project === T.pack dir
    name project === Just base

-- | Property: projectFromDir handles root directory
prop_projectFromDirDefault :: Property
prop_projectFromDirDefault = property $ do
    now <- forAll genTimestamp
    let dir = "/"
    let project = ProjectBuild.projectFromDir now dir
    id project === "proj_default"
    name project === Nothing

-- | Property: project id is deterministic based on directory
prop_projectIdDeterministic :: Property
prop_projectIdDeterministic = property $ do
    base <- forAll $ Gen.text (Range.linear 1 12) Gen.alphaNum
    now <- forAll genTimestamp
    let dir = "/tmp/" <> T.unpack base
    let project1 = ProjectBuild.projectFromDir now dir
    let project2 = ProjectBuild.projectFromDir now dir
    id project1 === id project2

-- | Property: project id format is proj_{basename}
prop_projectIdFormat :: Property
prop_projectIdFormat = property $ do
    base <- forAll $ Gen.text (Range.linear 1 12) Gen.alphaNum
    now <- forAll genTimestamp
    let dir = "/some/path/" <> T.unpack base
    let project = ProjectBuild.projectFromDir now dir
    T.isPrefixOf "proj_" (id project) === True

-- | Property: project worktree path is absolute
prop_projectWorktreeAbsolute :: Property
prop_projectWorktreeAbsolute = property $ do
    dir <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    now <- forAll genTimestamp
    let project = ProjectBuild.projectFromDir now ("/home/user/" <> T.unpack dir)
    T.isPrefixOf "/" (worktree project) === True

-- | Property: project name can be extracted from path
prop_projectNameFromPath :: Property
prop_projectNameFromPath = property $ do
    nm <- forAll $ Gen.text (Range.linear 1 15) Gen.alphaNum
    now <- forAll genTimestamp
    let path = "/home/user/projects/" <> T.unpack nm
    let project = ProjectBuild.projectFromDir now path
    name project === Just nm

-- | Property: project id contains only valid characters
prop_projectIdValidChars :: Property
prop_projectIdValidChars = property $ do
    dir <- forAll $ Gen.text (Range.linear 1 20) Gen.lower
    now <- forAll genTimestamp
    let project = ProjectBuild.projectFromDir now ("/tmp/" <> T.unpack dir)
    let pid = id project
    assert $ T.all (\c -> c == '_' || c == '-' || isAsciiLower c || isDigit c) pid

-- | Property: same directory and timestamp produces same project
prop_projectSameDirSameProject :: Property
prop_projectSameDirSameProject = property $ do
    dir <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    now <- forAll genTimestamp
    let path = "/tmp/" <> T.unpack dir
    let p1 = ProjectBuild.projectFromDir now path
    let p2 = ProjectBuild.projectFromDir now path
    p1 === p2

-- | Property: different directories produce different project ids
prop_projectDifferentDirDifferentProject :: Property
prop_projectDifferentDirDifferentProject = property $ do
    dir1 <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    dir2 <- forAll $ Gen.text (Range.linear 11 20) Gen.alphaNum
    now <- forAll genTimestamp
    if dir1 == dir2
        then success
        else do
            let p1 = ProjectBuild.projectFromDir now ("/tmp/" <> T.unpack dir1)
            let p2 = ProjectBuild.projectFromDir now ("/tmp/" <> T.unpack dir2)
            assert $ id p1 /= id p2

-- | Property: worktree preserves the full path
prop_projectWorktreePreservesPath :: Property
prop_projectWorktreePreservesPath = property $ do
    segments <-
        forAll $
            Gen.list (Range.linear 1 5) $
                Gen.text (Range.linear 1 10) Gen.alphaNum
    now <- forAll genTimestamp
    let path = "/" <> T.unpack (T.intercalate "/" segments)
    let project = ProjectBuild.projectFromDir now path
    worktree project === T.pack path

-- | Property: projectFromDir sets empty sandboxes
prop_projectFromDirEmptySandboxes :: Property
prop_projectFromDirEmptySandboxes = property $ do
    base <- forAll $ Gen.text (Range.linear 1 12) Gen.alphaNum
    now <- forAll genTimestamp
    let dir = "/tmp/" <> T.unpack base
    let project = ProjectBuild.projectFromDir now dir
    sandboxes project === []

-- | Property: projectFromDir uses provided timestamp
prop_projectFromDirUsesTimestamp :: Property
prop_projectFromDirUsesTimestamp = property $ do
    base <- forAll $ Gen.text (Range.linear 1 12) Gen.alphaNum
    now <- forAll genTimestamp
    let dir = "/tmp/" <> T.unpack base
    let project = ProjectBuild.projectFromDir now dir
    let pt = time project
    created pt === now
    updated pt === now
    initialized pt === Nothing

tests :: TestTree
tests =
    testGroup
        "Project Property Tests"
        [ testGroup
            "Pure Helpers"
            [ testProperty "makeProjectId non-empty" prop_makeProjectIdNonEmpty
            , testProperty "makeProjectId empty" prop_makeProjectIdEmpty
            , testProperty "makeProjectName non-empty" prop_makeProjectNameNonEmpty
            , testProperty "makeProjectName empty" prop_makeProjectNameEmpty
            , testProperty "makeProjectId deterministic" prop_makeProjectIdDeterministic
            , testProperty "makeProjectName deterministic" prop_makeProjectNameDeterministic
            ]
        , testGroup
            "projectFromDir"
            [ testProperty "uses base name" prop_projectFromDirUsesBase
            , testProperty "default for root" prop_projectFromDirDefault
            , testProperty "id deterministic" prop_projectIdDeterministic
            , testProperty "id format" prop_projectIdFormat
            , testProperty "worktree absolute" prop_projectWorktreeAbsolute
            , testProperty "name from path" prop_projectNameFromPath
            , testProperty "id valid chars" prop_projectIdValidChars
            , testProperty "same dir same project" prop_projectSameDirSameProject
            , testProperty "different dir different project" prop_projectDifferentDirDifferentProject
            , testProperty "worktree preserves path" prop_projectWorktreePreservesPath
            , testProperty "empty sandboxes" prop_projectFromDirEmptySandboxes
            , testProperty "uses timestamp" prop_projectFromDirUsesTimestamp
            ]
        ]
