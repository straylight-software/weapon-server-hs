{- |
Module      : Util.ExeCache
Description : Shared executable cache for avoiding repeated PATH lookups

This module provides a thread-safe cache for executable lookups. Instead of
repeatedly searching the PATH for executables (which can be expensive on
systems with many PATH entries), results are cached after the first lookup.

= Usage Example

@
cache <- 'newExeCache'
mGit <- 'findExecutableCached' cache "git"  -- Searches PATH
mGit' <- 'findExecutableCached' cache "git" -- Returns cached result
@

= Thread Safety

The cache uses 'atomicModifyIORef'' for thread-safe updates, making it safe
to share across multiple threads.
-}
module Util.ExeCache (
    -- * Types
    ExeCache,

    -- * Construction
    newExeCache,

    -- * Lookup
    findExecutableCached,

    -- * Pure helpers (for testing)
    lookupCache,
    insertCache,
) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Directory (findExecutable)

{- | Cache for executable lookups (avoids repeated PATH searches).

The cache maps executable names to their resolved paths. A 'Nothing' value
indicates that the executable was not found on PATH (this is also cached
to avoid repeated failed lookups).
-}
newtype ExeCache = ExeCache (IORef (Map String (Maybe FilePath)))

{- | Create a new empty executable cache.

The cache starts empty and is populated lazily as executables are looked up.
-}
newExeCache :: IO ExeCache
newExeCache = ExeCache <$> newIORef Map.empty

{- | Look up an executable, using the cache if available.

On the first lookup for a given name, searches the system PATH using
'System.Directory.findExecutable'. Subsequent lookups return the cached
result immediately.

Both successful lookups ('Just path') and failed lookups ('Nothing') are
cached. This means that if an executable is installed after a failed lookup,
the cache will continue to return 'Nothing' until a new cache is created.
-}
findExecutableCached :: ExeCache -> String -> IO (Maybe FilePath)
findExecutableCached (ExeCache ref) name = do
    cache <- readIORef ref
    case lookupCache name cache of
        Just result -> pure result
        Nothing -> do
            result <- findExecutable name
            atomicModifyIORef' ref $ \m -> (insertCache name result m, result)

{- | Pure cache lookup (for testing).

Returns 'Just (Just path)' if the executable is cached and found,
'Just Nothing' if the executable is cached but not found,
or 'Nothing' if the executable has not been looked up yet.
-}
lookupCache :: String -> Map String (Maybe FilePath) -> Maybe (Maybe FilePath)
lookupCache = Map.lookup

{- | Pure cache insertion (for testing).

Inserts an executable lookup result into the cache.
-}
insertCache :: String -> Maybe FilePath -> Map String (Maybe FilePath) -> Map String (Maybe FilePath)
insertCache = Map.insert
