{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Project.Discovery
Description : Automatic project discovery from filesystem
Stability   : stable

This module provides functionality to automatically discover projects
within a directory tree. A project is identified by the presence of a
@weapon.dhall@ configuration file.

= Discovery Algorithm

1. The root directory itself is always included as a project
2. Immediate subdirectories containing @weapon.dhall@ are discovered
3. Duplicate projects (by worktree path) are removed

= Example

@
projects <- discoverProjects "/home/user/workspace"
-- Returns projects for /home/user/workspace and any subdirs with weapon.dhall
@
-}
module Project.Discovery (
    -- * Project Discovery (IO)
    discoverProjects,

    -- * Pure Helpers (for testing)
    buildProjectList,
    deduplicateProjects,
    sameWorktree,
) where

import Api (Project (..))
import Control.Monad (filterM)
import Data.List (nubBy)
import Project.Build qualified as ProjectBuild
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

{- | Discover all projects within a root directory.

This function scans the root directory and its immediate subdirectories
for project configurations. The root itself is always included as a project.
Subdirectories containing a @weapon.dhall@ file are also included.

Duplicate projects (identified by matching 'worktree' paths) are removed,
keeping the first occurrence.

==== __Examples__

Given a directory structure:

@
/workspace/
  weapon.dhall
  projectA/
    weapon.dhall
  projectB/
    weapon.dhall
  docs/           -- no weapon.dhall, not included
@

>>> discoverProjects "/workspace"
[Project "proj_workspace" "/workspace" (Just "workspace"),
 Project "proj_projectA" "/workspace/projectA" (Just "projectA"),
 Project "proj_projectB" "/workspace/projectB" (Just "projectB")]
-}
discoverProjects :: FilePath -> IO [Project]
discoverProjects root = do
    entries <- listDirectory root
    subdirsWithConfig <- filterSubdirsWithConfig root entries
    pure $ buildProjectList root subdirsWithConfig

{- | Filter subdirectories that contain a @weapon.dhall@ configuration file.

This is the IO portion of discovery - checking filesystem for config files.
-}
filterSubdirsWithConfig :: FilePath -> [FilePath] -> IO [FilePath]
filterSubdirsWithConfig root = filterM hasConfig
  where
    hasConfig :: FilePath -> IO Bool
    hasConfig dir = doesFileExist (root </> dir </> "weapon.dhall")

{- | Build the project list from root and discovered subdirectories.

This is a pure function that constructs projects from paths and deduplicates
them. The root directory is always included as the first project.

==== __Examples__

>>> buildProjectList "/workspace" ["projectA", "projectB"]
[Project "proj_workspace" "/workspace" (Just "workspace"),
 Project "proj_projectA" "/workspace/projectA" (Just "projectA"),
 Project "proj_projectB" "/workspace/projectB" (Just "projectB")]
-}
buildProjectList :: FilePath -> [FilePath] -> [Project]
buildProjectList root subdirs =
    deduplicateProjects $ rootProject : subProjects
  where
    rootProject = ProjectBuild.projectFromDir root
    subProjects = map (ProjectBuild.projectFromDir . (root </>)) subdirs

{- | Remove duplicate projects by worktree path, keeping the first occurrence.

Two projects are considered duplicates if they have the same 'worktree' path.

==== __Examples__

>>> deduplicateProjects [proj1, proj1duplicate, proj2]
[proj1, proj2]
-}
deduplicateProjects :: [Project] -> [Project]
deduplicateProjects = nubBy sameWorktree

{- | Check if two projects refer to the same worktree directory.

This is used for deduplication during discovery.
-}
sameWorktree :: Project -> Project -> Bool
sameWorktree a b = worktree a == worktree b
