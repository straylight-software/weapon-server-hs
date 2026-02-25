{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Project.Build
Description : Project construction from filesystem paths
Stability   : stable

This module provides functions to construct 'Api.Project' values from
filesystem directory paths. The project ID is derived from the directory
basename, and the worktree is set to the full path.

= Example

@
let project = projectFromDir "/home/user/myproject"
-- project.id == "proj_myproject"
-- project.worktree == "/home/user/myproject"
-- project.name == Just "myproject"
@
-}
module Project.Build (
    -- * Project Construction
    projectFromDir,

    -- * Pure Helpers (for testing)
    makeProjectId,
    makeProjectName,
) where

import Api qualified
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath (takeFileName)
import Prelude hiding (id)

{- | Construct a 'Project' from a filesystem directory path.

The project is built as follows:

  * The 'id' is @proj_\<basename\>@ where basename is the last path component,
    or @proj_default@ if the basename is empty (e.g., for root directory @\/@).
  * The 'worktree' is the full directory path as-is.
  * The 'name' is 'Just' the basename, or 'Nothing' if the basename is empty.

==== __Examples__

>>> projectFromDir "/home/user/myproject"
Project {id = "proj_myproject", worktree = "/home/user/myproject", name = Just "myproject"}

>>> projectFromDir "/"
Project {id = "proj_default", worktree = "/", name = Nothing}
-}
projectFromDir :: FilePath -> Api.Project
projectFromDir dir =
    let basename = extractBasename dir
        pid = makeProjectId basename
        title = makeProjectName basename
     in Api.Project pid (T.pack dir) title

{- | Extract the basename from a filepath as 'Text'.

This is a pure helper that wraps 'takeFileName' and converts to 'Text'.
-}
extractBasename :: FilePath -> Text
extractBasename = T.pack . takeFileName

{- | Generate a project ID from a basename.

Returns @\"proj_default\"@ if the basename is empty, otherwise returns
@\"proj_\" <> basename@.

==== __Examples__

>>> makeProjectId "myproject"
"proj_myproject"

>>> makeProjectId ""
"proj_default"
-}
makeProjectId :: Text -> Text
makeProjectId basename
    | T.null basename = "proj_default"
    | otherwise = "proj_" <> basename

{- | Generate a project name from a basename.

Returns 'Nothing' if the basename is empty, otherwise 'Just' the basename.

==== __Examples__

>>> makeProjectName "myproject"
Just "myproject"

>>> makeProjectName ""
Nothing
-}
makeProjectName :: Text -> Maybe Text
makeProjectName basename
    | T.null basename = Nothing
    | otherwise = Just basename
