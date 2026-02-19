{-# LANGUAGE OverloadedStrings #-}

module Experimental.Worktree (
    getInfo,
    setInfo,
    resetInfo,
    remove,
)
where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Storage.Storage qualified as Storage

worktreeKey :: [Text]
worktreeKey = ["experimental", "worktree"]

getInfo :: Storage.StorageConfig -> Text -> IO Value
getInfo storage root = do
    result <- Storage.readMaybe storage worktreeKey
    pure $ fromMaybe (object ["root" .= root, "ready" .= True]) result

setInfo :: Storage.StorageConfig -> Value -> IO Value
setInfo storage value = do
    Storage.write storage worktreeKey value
    pure value

resetInfo :: Storage.StorageConfig -> Text -> IO Value
resetInfo storage root = do
    let value = object ["root" .= root, "reset" .= True]
    Storage.write storage worktreeKey value
    pure value

-- | Remove a worktree
remove :: Storage.StorageConfig -> Text -> Maybe Text -> IO (Either Text ())
remove storage _root _mDir = do
    -- Remove worktree info from storage (remove already ignores missing files)
    Storage.remove storage worktreeKey
    pure (Right ())
