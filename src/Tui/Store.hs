{-# LANGUAGE OverloadedStrings #-}

module Tui.Store (
    getPrompt,
    appendPrompt,
    clearPrompt,
    submitPrompt,
    setLast,
    getLast,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Storage.Storage qualified as Storage

promptKey :: [Text]
promptKey = ["tui", "prompt"]

lastKey :: [Text]
lastKey = ["tui", "last"]

-- | Retry an IO action with a delay between attempts
retryWithDelay :: Int -> Int -> a -> IO (Either e a) -> IO a
retryWithDelay 0 _ defaultVal _ = pure defaultVal
retryWithDelay attempts delayUs defaultVal action = do
    result <- action
    case result of
        Right v -> pure v
        Left _ -> do
            threadDelay delayUs
            retryWithDelay (attempts - 1) delayUs defaultVal action

getPrompt :: Storage.StorageConfig -> IO Text
getPrompt storage = retryWithDelay 3 1000 "" $ do
    result <- try @SomeException (Storage.read storage promptKey)
    pure $ case result of
        Right (String t) -> Right t
        Right _ -> Right ""
        Left e -> Left e

appendPrompt :: Storage.StorageConfig -> Text -> IO Text
appendPrompt storage text = do
    current <- getPrompt storage
    let next = current <> text
    Storage.write storage promptKey (String next)
    pure next

clearPrompt :: Storage.StorageConfig -> IO ()
clearPrompt storage = Storage.write storage promptKey (String "")

submitPrompt :: Storage.StorageConfig -> IO Text
submitPrompt storage = do
    current <- getPrompt storage
    Storage.write storage promptKey (String "")
    Storage.write storage ["tui", "submitted"] (object ["prompt" .= current])
    pure current

setLast :: Storage.StorageConfig -> Value -> IO ()
setLast storage = Storage.write storage lastKey

getLast :: Storage.StorageConfig -> IO (Maybe Value)
getLast storage =
    retryWithDelay 3 1000 Nothing $
        try @SomeException (Just <$> Storage.read storage lastKey)
