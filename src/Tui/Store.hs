{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Tui.Store
Description : TUI state persistence layer
Stability   : experimental

This module provides a persistence layer for TUI state, including the current
prompt text and the last submitted prompt. It uses the Storage module for
JSON file-based persistence with automatic retry on transient failures.

= Architecture

The TUI store manages two main pieces of state:

* __Prompt__: The current prompt text being edited by the user
* __Last__: The most recently submitted prompt (for history/recall)

All operations are resilient to transient storage failures through an
automatic retry mechanism. When a logger is provided, retry attempts and
exhaustion are logged at appropriate levels.

= Example Usage

@
import qualified Storage.Storage as Storage
import qualified Tui.Store as TuiStore

example :: IO ()
example = Storage.withStorage ".tui-state" $ \\store -> do
    -- Build up a prompt (without logging)
    _ <- TuiStore.appendPrompt Nothing store "Hello, "
    _ <- TuiStore.appendPrompt Nothing store "world!"

    -- Submit and clear
    submitted <- TuiStore.submitPrompt Nothing store
    print submitted  -- "Hello, world!"
@
-}
module Tui.Store (
    -- * Prompt Operations

    -- | Functions for managing the current prompt text
    getPrompt,
    appendPrompt,
    clearPrompt,
    submitPrompt,

    -- * Last Prompt Operations

    -- | Functions for managing the last submitted prompt
    setLast,
    getLast,

    -- * Storage Keys

    -- | Storage key constants (exported for testing)
    promptKey,
    lastKey,
    submittedKey,

    -- * Pure Helpers (exported for testing)
    combinePromptText,
    extractTextFromValue,
    mkSubmittedPayload,

    -- * Retry Configuration
    RetryConfig (..),
    defaultRetryConfig,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Log qualified
import Storage.Storage qualified as Storage

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for retry behavior on transient storage failures.
data RetryConfig = RetryConfig
    { retryAttempts :: !Int
    -- ^ Maximum number of attempts (including the initial try)
    , retryDelayMicros :: !Int
    -- ^ Delay between retries in microseconds
    }
    deriving (Show, Eq)

{- | Default retry configuration: 3 attempts with 1ms delay.

This is tuned for local file system operations where transient failures
(e.g., brief file locks) typically resolve within milliseconds.
-}
defaultRetryConfig :: RetryConfig
defaultRetryConfig =
    RetryConfig
        { retryAttempts = 3
        , retryDelayMicros = 1000
        }

--------------------------------------------------------------------------------
-- Storage Keys
--------------------------------------------------------------------------------

-- | Storage key for the current prompt text.
promptKey :: [Text]
promptKey = ["tui", "prompt"]

-- | Storage key for the last submitted payload.
lastKey :: [Text]
lastKey = ["tui", "last"]

-- | Storage key for submitted prompt records.
submittedKey :: [Text]
submittedKey = ["tui", "submitted"]

--------------------------------------------------------------------------------
-- Pure Helpers
--------------------------------------------------------------------------------

{- | Combine existing prompt text with new text.

This is the pure core of 'appendPrompt', separated for testability.

>>> combinePromptText "Hello, " "world!"
"Hello, world!"
>>> combinePromptText "" "start"
"start"
-}
combinePromptText :: Text -> Text -> Text
combinePromptText current new = current <> new

{- | Extract text from a JSON Value, returning empty string for non-strings.

This handles the case where storage might contain unexpected value types.

>>> extractTextFromValue (String "hello")
"hello"
>>> extractTextFromValue (Number 42)
""
>>> extractTextFromValue Null
""
-}
extractTextFromValue :: Value -> Text
extractTextFromValue (String t) = t
extractTextFromValue _ = ""

{- | Create the payload for a submitted prompt.

>>> mkSubmittedPayload "my prompt"
Object (fromList [("prompt",String "my prompt")])
-}
mkSubmittedPayload :: Text -> Value
mkSubmittedPayload promptText = object ["prompt" .= promptText]

--------------------------------------------------------------------------------
-- Retry Mechanism
--------------------------------------------------------------------------------

{- | Retry an IO action with configurable delay between attempts.

On failure (Left), waits for the specified delay and retries.
Returns the default value if all attempts are exhausted.

This is useful for handling transient storage failures that may resolve
on their own (e.g., brief file locks, network blips for remote storage).

When a logger is provided:
- DEBUG: logs each failed retry attempt with remaining count
- WARN: logs when all retries are exhausted and default is returned
-}
retryWithDelay :: (Show e) => Maybe Log.Logger -> RetryConfig -> a -> IO (Either e a) -> IO a
retryWithDelay mLogger cfg defaultVal action = go (retryAttempts cfg)
  where
    go 0 = do
        case mLogger of
            Just lg -> Log.logWarn lg "All retry attempts exhausted, returning default" ()
            Nothing -> pure ()
        pure defaultVal
    go n = do
        result <- action
        case result of
            Right v -> pure v
            Left err -> do
                let remaining = n - 1
                case mLogger of
                    Just lg ->
                        Log.logDebug lg
                            ("Retry attempt failed (" <> T.pack (show remaining) <> " remaining): " <> T.pack (show err))
                            ()
                    Nothing -> pure ()
                threadDelay (retryDelayMicros cfg)
                go remaining

{- | Execute a storage read with default retry configuration.

Wraps the storage read in exception handling and extracts the result
using the provided extraction function.
-}
withRetryingRead ::
    -- | Optional logger for retry diagnostics
    Maybe Log.Logger ->
    -- | Default value on failure
    a ->
    -- | How to extract the result from a JSON Value
    (Value -> a) ->
    Storage.StorageConfig ->
    -- | Storage key
    [Text] ->
    IO a
withRetryingRead mLogger defaultVal extract storage key =
    retryWithDelay mLogger defaultRetryConfig defaultVal $ do
        result <- try @SomeException (Storage.read storage key)
        pure $ case result of
            Right val -> Right (extract val)
            Left e -> Left e

--------------------------------------------------------------------------------
-- Prompt Operations
--------------------------------------------------------------------------------

{- | Get the current prompt text from storage.

Returns an empty string if the prompt doesn't exist or on read failure
(after retries are exhausted).

This operation is resilient to transient storage failures through
automatic retry with the default configuration.
-}
getPrompt :: Maybe Log.Logger -> Storage.StorageConfig -> IO Text
getPrompt mLogger storage = withRetryingRead mLogger "" extractTextFromValue storage promptKey

{- | Append text to the current prompt.

Reads the current prompt, appends the new text, writes back, and returns
the combined result.

>>> -- Assuming empty initial state
>>> appendPrompt Nothing store "Hello"
"Hello"
>>> appendPrompt Nothing store ", world!"
"Hello, world!"
-}
appendPrompt :: Maybe Log.Logger -> Storage.StorageConfig -> Text -> IO Text
appendPrompt mLogger storage text = do
    current <- getPrompt mLogger storage
    let next = combinePromptText current text
    writePrompt storage next
    pure next

{- | Clear the current prompt (set to empty string).

This is typically called after submitting a prompt or when the user
wants to start fresh.
-}
clearPrompt :: Storage.StorageConfig -> IO ()
clearPrompt storage = writePrompt storage ""

{- | Submit the current prompt and clear it.

This operation:

1. Reads the current prompt
2. Clears the prompt storage
3. Writes the submitted prompt to a separate 'submitted' key
4. Returns the submitted text

The submitted record can be used for history or to signal other
components that a prompt was submitted.
-}
submitPrompt :: Maybe Log.Logger -> Storage.StorageConfig -> IO Text
submitPrompt mLogger storage = do
    current <- getPrompt mLogger storage
    writePrompt storage ""
    Storage.write storage submittedKey (mkSubmittedPayload current)
    pure current

--------------------------------------------------------------------------------
-- Last Prompt Operations
--------------------------------------------------------------------------------

{- | Set the last submitted payload.

This stores arbitrary JSON data as the "last" value, which can be used
for history recall or state restoration.
-}
setLast :: Storage.StorageConfig -> Value -> IO ()
setLast storage = Storage.write storage lastKey

{- | Get the last submitted payload.

Returns 'Nothing' if no last value exists or on read failure
(after retries are exhausted).
-}
getLast :: Maybe Log.Logger -> Storage.StorageConfig -> IO (Maybe Value)
getLast mLogger storage = withRetryingRead mLogger Nothing Just storage lastKey

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Write prompt text to storage (internal helper).
writePrompt :: Storage.StorageConfig -> Text -> IO ()
writePrompt storage text = Storage.write storage promptKey (String text)
