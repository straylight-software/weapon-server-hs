{- |
Module      : Path.Build
Description : Path information construction and utilities
Stability   : stable

This module provides functions for constructing 'PathInfo' records that describe
the various directory paths used by the application. It separates pure path
construction logic from IO operations, enabling easier testing and composition.

= Usage

The primary function 'buildPath' constructs a 'PathInfo' from individual path
components. For convenience, 'buildPathFromPaths' accepts a 'PathComponents'
record.

@
let paths = buildPath "/home/user" "/home/user/.opencode/state"
                       "/home/user/.config/weapon/weapon.dhall"
                       "/project" "/project"
@

= Design

All functions in this module are pure. IO operations for discovering actual
paths should be performed in the calling code (typically handlers), with
results passed to these pure constructors.
-}
module Path.Build (
    -- * Path Construction
    buildPath,
    buildPathFromComponents,

    -- * Path Components
    PathComponents (..),

    -- * State Directory Utilities
    computeStateDir,
) where

import Api (PathInfo (..))
import Data.Text (Text)
import Data.Text qualified as T

{- | Components needed to build a 'PathInfo'.

This record groups all the raw path components that are needed
to construct a 'PathInfo'. Using this record can make code clearer
when multiple paths are being passed around.
-}
data PathComponents = PathComponents
    { pcHome :: !Text
    -- ^ User's home directory (e.g., @\/home\/user@)
    , pcState :: !Text
    -- ^ Application state directory (e.g., @\/project\/.opencode\/state@)
    , pcConfig :: !Text
    -- ^ Global configuration file path
    , pcWorktree :: !Text
    -- ^ Project worktree directory
    , pcDirectory :: !Text
    -- ^ Current working directory
    }
    deriving (Eq, Show)

{- | Construct a 'PathInfo' from individual path components.

This is the primary constructor for 'PathInfo', taking each path
component as a separate argument.

==== __Examples__

>>> buildPath "/home/user" "/state" "/config" "/worktree" "/cwd"
PathInfo {home = "/home/user", state = "/state", config = "/config", worktree = "/worktree", directory = "/cwd"}
-}
buildPath ::
    -- | Home directory path
    Text ->
    -- | State directory path
    Text ->
    -- | Config file path
    Text ->
    -- | Worktree directory path
    Text ->
    -- | Current directory path
    Text ->
    PathInfo
buildPath home state config worktree directory =
    PathInfo
        { home = home
        , state = state
        , config = config
        , worktree = worktree
        , directory = directory
        }

{- | Construct a 'PathInfo' from a 'PathComponents' record.

This is a convenience function when paths are already grouped
in a 'PathComponents' record.

==== __Examples__

>>> let comps = PathComponents "/home" "/state" "/config" "/worktree" "/cwd"
>>> buildPathFromComponents comps
PathInfo {home = "/home", state = "/state", config = "/config", worktree = "/worktree", directory = "/cwd"}
-}
buildPathFromComponents :: PathComponents -> PathInfo
buildPathFromComponents pc =
    buildPath
        (pcHome pc)
        (pcState pc)
        (pcConfig pc)
        (pcWorktree pc)
        (pcDirectory pc)

{- | Compute the state directory path from a worktree path.

The state directory is located at @\<worktree\>\/.opencode\/state@.
This is a pure function that computes the expected path without
performing any IO.

==== __Examples__

>>> computeStateDir "/home/user/project"
"/home/user/project/.opencode/state"

>>> computeStateDir "/home/user/project/"
"/home/user/project/.opencode/state"
-}
computeStateDir :: Text -> Text
computeStateDir worktree =
    normalizedWorktree <> "/.opencode/state"
  where
    -- Remove trailing slash if present for consistent output
    normalizedWorktree = T.dropWhileEnd (== '/') worktree
