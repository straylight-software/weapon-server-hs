{- | Shared test helpers to eliminate duplication across test modules

This module provides common utilities for:
- STM-based waiting with timeouts (no threadDelay)
- JSON value inspection
- Common generators
- List utilities
-}
module Test.Helpers (
    -- * STM wait helpers (deterministic, no threadDelay)
    waitVar,
    waitForTVar,
    waitForLength,
    waitForCount,

    -- * JSON inspection helpers
    lookupText,
    lookupBool,
    lookupArray,
    hasKey,
    isObject,
    valueToText,

    -- * Common generators
    genName,
    genSessionId,
    genMessageId,
    genProviderId,
    genText,
    genNonEmptyText,

    -- * List utilities
    listLength,

    -- * Handler utilities
    runHandlerIO,
) where

import Control.Concurrent.STM
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Servant.Server (Handler, ServerError, runHandler)

-- ═══════════════════════════════════════════════════════════════════════════
-- STM wait helpers (no threadDelay anywhere - fully deterministic)
-- ═══════════════════════════════════════════════════════════════════════════

{- | Wait for a TMVar to be filled, with timeout (microseconds).
Returns Nothing on timeout, Just value if filled.
-}
waitVar :: Int -> TMVar a -> IO (Maybe a)
waitVar timeoutUs var = do
    gate <- registerDelay timeoutUs
    atomically $
        (Just <$> takeTMVar var)
            `orElse` do
                done <- readTVar gate
                if done then pure Nothing else retry

{- | Wait for a TVar to satisfy a predicate, with timeout (microseconds).
Returns the final value regardless of whether the predicate was met.
-}
waitForTVar :: Int -> TVar a -> (a -> Bool) -> IO a
waitForTVar timeoutUs var predicate = do
    gate <- registerDelay timeoutUs
    atomically $ do
        val <- readTVar var
        if predicate val
            then pure val
            else do
                done <- readTVar gate
                if done then pure val else retry

-- | Wait for a TVar list to reach at least the given length, with timeout.
waitForLength :: Int -> TVar [a] -> Int -> IO [a]
waitForLength timeoutUs var n = waitForTVar timeoutUs var (\xs -> listLength xs >= n)

-- | Wait for a TVar Int to reach at least the given value, with timeout.
waitForCount :: Int -> TVar Int -> Int -> IO Int
waitForCount timeoutUs var n = waitForTVar timeoutUs var (>= n)

-- ═══════════════════════════════════════════════════════════════════════════
-- JSON inspection helpers
-- ═══════════════════════════════════════════════════════════════════════════

-- | Extract a Text value from a JSON object by key
lookupText :: Text -> Value -> Maybe Text
lookupText key (Object obj) = case KM.lookup (K.fromText key) obj of
    Just (String t) -> Just t
    _other -> Nothing
lookupText _ _ = Nothing

-- | Extract a Bool value from a JSON object by key
lookupBool :: Text -> Value -> Maybe Bool
lookupBool key (Object obj) = case KM.lookup (K.fromText key) obj of
    Just (Bool b) -> Just b
    _other -> Nothing
lookupBool _ _ = Nothing

-- | Extract an Array value from a JSON object by key
lookupArray :: Text -> Value -> Maybe [Value]
lookupArray key (Object obj) = case KM.lookup (K.fromText key) obj of
    Just (Array arr) -> Just (foldr (:) [] arr)
    _other -> Nothing
lookupArray _ _ = Nothing

-- | Check if a key exists in a JSON object
hasKey :: Text -> Value -> Bool
hasKey key (Object obj) = KM.member (K.fromText key) obj
hasKey _ _ = False

-- | Check if a Value is an Object
isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False

-- | Extract Text from a JSON String value
valueToText :: Value -> Maybe Text
valueToText (String t) = Just t
valueToText _ = Nothing

-- ═══════════════════════════════════════════════════════════════════════════
-- Common generators
-- ═══════════════════════════════════════════════════════════════════════════

-- | Generate a short alphanumeric name (1-12 chars)
genName :: Gen Text
genName = Gen.text (Range.linear 1 12) Gen.alphaNum

-- | Generate a valid session ID (must start with "ses")
genSessionId :: Gen Text
genSessionId = do
    suffix <- Gen.text (Range.linear 1 12) Gen.alphaNum
    pure $ "ses" <> suffix

-- | Generate a valid message ID (must start with "msg")
genMessageId :: Gen Text
genMessageId = do
    suffix <- Gen.text (Range.linear 1 12) Gen.alphaNum
    pure $ "msg" <> suffix

-- | Generate a valid provider ID (lowercase alphanumeric and hyphens only, must start with letter/digit)
genProviderId :: Gen Text
genProviderId = do
    -- First character must be letter or digit
    firstChar <- Gen.element (['a' .. 'z'] ++ ['0' .. '9'])
    -- Remaining characters can include hyphens
    rest <- Gen.text (Range.linear 0 11) (Gen.element (['a' .. 'z'] ++ ['0' .. '9'] ++ ['-']))
    pure $ T.cons firstChar rest

-- | Generate alphanumeric text (1-64 chars)
genText :: Gen Text
genText = Gen.text (Range.linear 1 64) Gen.alphaNum

-- | Generate non-empty alphanumeric text (1-100 chars)
genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 100) Gen.alphaNum

-- ═══════════════════════════════════════════════════════════════════════════
-- List utilities
-- ═══════════════════════════════════════════════════════════════════════════

-- | O(n) list length that avoids the lazy spine issue with 'length'
listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0

-- ═══════════════════════════════════════════════════════════════════════════
-- Handler utilities
-- ═══════════════════════════════════════════════════════════════════════════

-- | Run a Servant Handler in IO, returning Either ServerError a
runHandlerIO :: Handler a -> IO (Either ServerError a)
runHandlerIO = runHandler
