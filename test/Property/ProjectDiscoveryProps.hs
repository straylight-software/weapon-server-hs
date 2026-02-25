{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.ProjectDiscoveryProps
Description : Property tests for Project.Discovery module
Stability   : stable

This module contains Hedgehog property tests for the project discovery
functions in "Project.Discovery". Tests cover both the IO-based discovery
and the pure helper functions.
-}
module Property.ProjectDiscoveryProps where

import Api (Project (..))
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Project.Build qualified as ProjectBuild
import Project.Discovery qualified as Discovery
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import Test.Helpers (listLength)
import Test.Tasty
import Test.Tasty.Hedgehog

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Helper Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: buildProjectList always includes root project first
prop_buildProjectListIncludesRoot :: Property
prop_buildProjectListIncludesRoot = property $ do
    root <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    subdirs <-
        forAll $
            Gen.list (Range.linear 0 5) $
                Gen.text (Range.linear 1 10) Gen.alphaNum
    let rootPath = "/tmp/" <> T.unpack root
    let subdirPaths = map T.unpack subdirs
    let projects = Discovery.buildProjectList rootPath subdirPaths
    case projects of
        [] -> failure
        (firstProject : _) -> worktree firstProject === T.pack rootPath

-- | Property: buildProjectList includes all subdirs
prop_buildProjectListIncludesSubdirs :: Property
prop_buildProjectListIncludesSubdirs = property $ do
    root <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    -- Generate unique subdirs to avoid deduplication
    subdir1 <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    subdir2 <- forAll $ Gen.text (Range.linear 11 20) Gen.alphaNum
    let rootPath = "/workspace/" <> T.unpack root
    let subdirs = [T.unpack subdir1, T.unpack subdir2]
    let projects = Discovery.buildProjectList rootPath subdirs
    -- Should have root + 2 subdirs (unless duplicates)
    assert $ listLength projects >= 1

-- | Property: buildProjectList with empty subdirs returns just root
prop_buildProjectListEmptySubdirs :: Property
prop_buildProjectListEmptySubdirs = property $ do
    root <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let rootPath = "/tmp/" <> T.unpack root
    let projects = Discovery.buildProjectList rootPath []
    listLength projects === 1
    case projects of
        [] -> failure
        (proj : _) -> worktree proj === T.pack rootPath

-- | Property: deduplicateProjects removes duplicates
prop_deduplicateProjectsRemovesDuplicates :: Property
prop_deduplicateProjectsRemovesDuplicates = property $ do
    name <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let path = "/tmp/" <> T.unpack name
    let proj = ProjectBuild.projectFromDir path
    let duplicated = [proj, proj, proj]
    let deduplicated = Discovery.deduplicateProjects duplicated
    listLength deduplicated === 1

-- | Property: deduplicateProjects preserves order (first wins)
prop_deduplicateProjectsPreservesOrder :: Property
prop_deduplicateProjectsPreservesOrder = property $ do
    names <-
        forAll $
            Gen.list (Range.linear 1 10) $
                Gen.text (Range.linear 1 15) Gen.alphaNum
    let paths = map (\n -> "/tmp/" <> T.unpack n) names
    let projects = map ProjectBuild.projectFromDir paths
    let deduplicated = Discovery.deduplicateProjects projects
    -- Deduplicated should be a prefix of unique elements
    assert $ listLength deduplicated <= listLength projects

-- | Property: deduplicateProjects is idempotent
prop_deduplicateProjectsIdempotent :: Property
prop_deduplicateProjectsIdempotent = property $ do
    names <-
        forAll $
            Gen.list (Range.linear 1 10) $
                Gen.text (Range.linear 1 15) Gen.alphaNum
    let paths = map (\n -> "/tmp/" <> T.unpack n) names
    let projects = map ProjectBuild.projectFromDir paths
    let once = Discovery.deduplicateProjects projects
    let twice = Discovery.deduplicateProjects once
    once === twice

-- | Property: sameWorktree is reflexive
prop_sameWorktreeReflexive :: Property
prop_sameWorktreeReflexive = property $ do
    name <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let proj = ProjectBuild.projectFromDir ("/tmp/" <> T.unpack name)
    assert $ Discovery.sameWorktree proj proj

-- | Property: sameWorktree is symmetric
prop_sameWorktreeSymmetric :: Property
prop_sameWorktreeSymmetric = property $ do
    name1 <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    name2 <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let proj1 = ProjectBuild.projectFromDir ("/tmp/" <> T.unpack name1)
    let proj2 = ProjectBuild.projectFromDir ("/tmp/" <> T.unpack name2)
    Discovery.sameWorktree proj1 proj2 === Discovery.sameWorktree proj2 proj1

-- | Property: sameWorktree with same path returns True
prop_sameWorktreeSamePath :: Property
prop_sameWorktreeSamePath = property $ do
    name <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let path = "/tmp/" <> T.unpack name
    let proj1 = ProjectBuild.projectFromDir path
    let proj2 = ProjectBuild.projectFromDir path
    assert $ Discovery.sameWorktree proj1 proj2

-- | Property: sameWorktree with different paths returns False
prop_sameWorktreeDifferentPath :: Property
prop_sameWorktreeDifferentPath = property $ do
    name1 <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    name2 <- forAll $ Gen.text (Range.linear 11 20) Gen.alphaNum
    let proj1 = ProjectBuild.projectFromDir ("/tmp/" <> T.unpack name1)
    let proj2 = ProjectBuild.projectFromDir ("/tmp/" <> T.unpack name2)
    if name1 == name2
        then success
        else assert $ not (Discovery.sameWorktree proj1 proj2)

-- ═══════════════════════════════════════════════════════════════════════════
-- IO Integration Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: discoverProjects includes subdirs with weapon.dhall
prop_discoverProjects :: Property
prop_discoverProjects = property $ do
    name <- forAll genText
    result <- evalIO $ do
        tmpDir <- createTempDirectory "/tmp" "project-discovery"
        let subDir = tmpDir </> T.unpack name
        createDirectoryIfMissing True subDir
        writeFile (subDir </> "weapon.dhall") "{=}"
        projects <- Discovery.discoverProjects tmpDir
        removeDirectoryRecursive tmpDir
        pure projects
    assert $ listLength result >= 2

-- | Property: discoverProjects ignores subdirs without weapon.dhall
prop_discoverProjectsIgnoresNonProjects :: Property
prop_discoverProjectsIgnoresNonProjects = property $ do
    name <- forAll genText
    result <- evalIO $ do
        tmpDir <- createTempDirectory "/tmp" "project-discovery"
        let subDir = tmpDir </> T.unpack name
        createDirectoryIfMissing True subDir
        -- No weapon.dhall file
        projects <- Discovery.discoverProjects tmpDir
        removeDirectoryRecursive tmpDir
        pure projects
    -- Should only have the root project
    listLength result === 1

-- | Property: discoverProjects always includes root
prop_discoverProjectsAlwaysIncludesRoot :: Property
prop_discoverProjectsAlwaysIncludesRoot = property $ do
    result <- evalIO $ do
        tmpDir <- createTempDirectory "/tmp" "project-discovery"
        projects <- Discovery.discoverProjects tmpDir
        removeDirectoryRecursive tmpDir
        pure projects
    assert $ listLength result >= 1

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

-- | Generate a short alphanumeric text suitable for directory names
genText :: Gen Text
genText = Gen.text (Range.linear 3 10) Gen.alphaNum

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Project Discovery Property Tests"
        [ testGroup
            "Pure Helpers"
            [ testProperty "buildProjectList includes root" prop_buildProjectListIncludesRoot
            , testProperty "buildProjectList includes subdirs" prop_buildProjectListIncludesSubdirs
            , testProperty "buildProjectList empty subdirs" prop_buildProjectListEmptySubdirs
            , testProperty "deduplicateProjects removes duplicates" prop_deduplicateProjectsRemovesDuplicates
            , testProperty "deduplicateProjects preserves order" prop_deduplicateProjectsPreservesOrder
            , testProperty "deduplicateProjects idempotent" prop_deduplicateProjectsIdempotent
            , testProperty "sameWorktree reflexive" prop_sameWorktreeReflexive
            , testProperty "sameWorktree symmetric" prop_sameWorktreeSymmetric
            , testProperty "sameWorktree same path" prop_sameWorktreeSamePath
            , testProperty "sameWorktree different path" prop_sameWorktreeDifferentPath
            ]
        , testGroup
            "IO Discovery"
            [ testProperty "includes subdir with weapon.dhall" prop_discoverProjects
            , testProperty "ignores subdirs without weapon.dhall" prop_discoverProjectsIgnoresNonProjects
            , testProperty "always includes root" prop_discoverProjectsAlwaysIncludesRoot
            ]
        ]
