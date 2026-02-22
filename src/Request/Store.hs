{-# LANGUAGE OverloadedStrings #-}

module Request.Store (
    writeRequest,
    listRequests,
    generateId,
) where

import Data.Aeson (Value)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Numeric (showHex)
import Storage.Storage qualified as Storage
import System.Random (randomIO)

writeRequest :: Storage.StorageConfig -> Text -> Text -> Value -> IO ()
writeRequest storage kind req = Storage.write storage [kind, req]

listRequests :: Storage.StorageConfig -> Text -> IO [Value]
listRequests storage kind = do
    keys <- Storage.list storage [kind]
    values <- mapM (Storage.readMaybe storage) keys
    pure (catMaybes values)

generateId :: IO Text
generateId = do
    n <- randomIO :: IO Word64
    pure $ "req_" <> T.pack (showHex n "")
