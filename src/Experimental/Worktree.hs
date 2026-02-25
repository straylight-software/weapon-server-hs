{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Experimental.Worktree
Description : Experimental worktree state management
Stability   : experimental

This module provides experimental worktree management functionality.
A worktree represents a separate working directory that can be used
for isolated development or testing.

The worktree state is persisted to JSON storage and includes metadata
such as the root directory and status flags (ready, reset).

= Usage

@
import qualified Experimental.Worktree as Worktree
import qualified Storage.Storage as Storage

example :: IO ()
example = Storage.withStorage "/tmp/store" $ \store -> do
    info <- Worktree.getInfo store "/project/root"
    print info
@
-}
module Experimental.Worktree (
    -- * Core Operations
    getInfo,
    setInfo,
    resetInfo,
    remove,

    -- * Pure Helpers (for testing)
    defaultWorktreeInfo,
    resetWorktreeInfo,
    worktreeKey,
) where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Storage.Storage qualified as Storage

{- | The storage key path for worktree information.

All worktree state is stored under this key in the storage backend.
-}
worktreeKey :: [Text]
worktreeKey = ["experimental", "worktree"]

-- * Pure Functions

{- | Create a default worktree info object for a given root path.

The default state has @ready = True@, indicating the worktree
is ready for use.

==== __Examples__

>>> defaultWorktreeInfo "/my/project"
Object (fromList [("ready",Bool True),("root",String "/my/project")])
-}
defaultWorktreeInfo :: Text -> Value
defaultWorktreeInfo root = object ["root" .= root, "ready" .= True]

{- | Create a reset worktree info object for a given root path.

The reset state has @reset = True@, indicating the worktree
should be reset/reinitialized.

==== __Examples__

>>> resetWorktreeInfo "/my/project"
Object (fromList [("reset",Bool True),("root",String "/my/project")])
-}
resetWorktreeInfo :: Text -> Value
resetWorktreeInfo root = object ["root" .= root, "reset" .= True]

-- * IO Operations

{- | Retrieve worktree information from storage.

If no worktree info exists in storage, returns a default info object
with the provided root path and @ready = True@.

The @root@ parameter is only used to construct the default value;
if info exists in storage, it is returned unchanged regardless of
the provided root.
-}
getInfo :: Storage.StorageConfig -> Text -> IO Value
getInfo storage root = do
    result <- Storage.readMaybe storage worktreeKey
    pure $ fromMaybe (defaultWorktreeInfo root) result

{- | Store worktree information.

Writes the provided value to storage and returns it unchanged.
This can be used to persist arbitrary worktree state.
-}
setInfo :: Storage.StorageConfig -> Value -> IO Value
setInfo = writeAndReturn

{- | Reset worktree information to the reset state.

Creates a new info object with @reset = True@ and the provided
root path, persists it to storage, and returns it.

This is typically called when a worktree needs to be reinitialized.
-}
resetInfo :: Storage.StorageConfig -> Text -> IO Value
resetInfo storage root = writeAndReturn storage (resetWorktreeInfo root)

{- | Remove worktree information from storage.

Deletes the stored worktree info. After removal, subsequent calls
to 'getInfo' will return a default info object.

This operation is idempotent - removing non-existent info succeeds.

__Note:__ The @root@ and @mDir@ parameters are currently unused
but reserved for future functionality (e.g., removing actual
worktree directories from disk).
-}
remove :: Storage.StorageConfig -> Text -> Maybe Text -> IO (Either Text ())
remove storage _root _mDir = do
    Storage.remove storage worktreeKey
    pure (Right ())

-- * Internal Helpers

{- | Write a value to storage and return it.

This is a common pattern for setInfo and resetInfo - they both
persist a value and return it for convenience.
-}
writeAndReturn :: Storage.StorageConfig -> Value -> IO Value
writeAndReturn storage value = do
    Storage.write storage worktreeKey value
    pure value
