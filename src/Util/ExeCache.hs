-- | Shared executable cache for avoiding repeated PATH lookups
module Util.ExeCache (
    ExeCache,
    newExeCache,
    findExecutableCached,
) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Directory (findExecutable)

-- | Cache for executable lookups (avoids repeated PATH searches)
newtype ExeCache = ExeCache (IORef (Map String (Maybe FilePath)))

-- | Create a new empty executable cache
newExeCache :: IO ExeCache
newExeCache = ExeCache <$> newIORef Map.empty

-- | Lookup or compute executable path, caching the result
findExecutableCached :: ExeCache -> String -> IO (Maybe FilePath)
findExecutableCached (ExeCache ref) name = do
    cache <- readIORef ref
    case Map.lookup name cache of
        Just result -> pure result
        Nothing -> do
            result <- findExecutable name
            atomicModifyIORef' ref $ \m -> (Map.insert name result m, result)
