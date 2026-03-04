{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Util.Thread
Description : Supervised thread spawning utilities

This module provides utilities for spawning threads with proper error handling
and supervision. Use these functions instead of raw 'forkIO' for any thread
that should not die silently.

== Problem

Haskell's 'forkIO' spawns threads that can die silently when they throw
exceptions. The parent thread has no idea the child died, leading to
silent degradation of the system.

== Solution

This module provides two supervised thread spawning functions:

* 'forkLogged' - Catches exceptions, logs them at ERROR level, then re-throws.
  The thread still dies, but we have visibility into what happened.

* 'forkLinked' - Links the child thread to the parent. If the child dies with
  an exception, the exception propagates to the parent thread.

== Usage Example

@
import Util.Thread (forkLogged, forkLinked)
import qualified Log

-- Thread that logs errors before dying
forkLogged logger "heartbeat" $ forever $ do
    threadDelay 1000000
    sendHeartbeat

-- Thread that kills parent if it dies
forkLinked $ runCriticalService
@

@since 0.1.0
-}
module Util.Thread (
    -- * Supervised Thread Spawning
    forkLogged,
    forkLinked,
) where

import Control.Concurrent (ThreadId, forkIO, myThreadId, throwTo)
import Control.Exception (SomeException, catch, throwIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Log

{- | Fork a thread that logs exceptions before dying.

Use this instead of raw 'forkIO' for any thread that should not die silently.
When the thread throws an exception:

1. The exception is caught
2. An ERROR log message is emitted with the thread name and exception details
3. The exception is re-thrown (so the thread still dies)

This provides visibility into thread failures without changing the behavior
of the thread itself.

@
forkLogged logger "subscriber" $ forever $ do
    event <- readEvent
    processEvent event
@
-}
forkLogged :: Log.Logger -> Text -> IO () -> IO ThreadId
forkLogged logger threadName action =
    forkIO $
        action `catch` \(e :: SomeException) -> do
            Log.logError
                logger
                ("Thread '" <> threadName <> "' died with exception: " <> T.pack (show e))
                ()
            throwIO e

{- | Fork a thread and link it to the current thread.

When the forked thread dies with an exception, that exception is re-thrown
in the parent thread. Use this when the parent MUST know if the child dies
and should handle the failure.

This is useful for critical infrastructure threads where the parent needs
to take action (restart the child, shut down, etc.) if the child fails.

Note: This uses asynchronous exception delivery via 'throwTo'. The parent
thread will receive the exception at the next safe point.

@
parentTid <- myThreadId
forkLinked $ runCriticalWorker
-- If runCriticalWorker throws, this thread will receive the exception
@
-}
forkLinked :: IO () -> IO ThreadId
forkLinked action = do
    parentTid <- myThreadId
    forkIO $
        action `catch` \(e :: SomeException) -> do
            throwTo parentTid e
            throwIO e
