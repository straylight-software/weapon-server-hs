{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Buffer pool for zero-allocation steady-state operation.
--
-- Pre-allocates a fixed number of buffers that get recycled.
-- Each connection gets a recv buffer from the pool and returns it on close.
module Evring.Wai.Pool
  ( BufferPool,
    Buffer (..),
    newBufferPool,
    acquireBuffer,
    releaseBuffer,
  )
where

import Data.IORef
import Data.Primitive (MutablePrimArray, mutablePrimArrayContents, newPinnedPrimArray)
import Data.Word (Word8)
import Foreign (Ptr)
import GHC.Exts (RealWorld)

-- | A pooled buffer
data Buffer = Buffer
  { bufArray :: !(MutablePrimArray RealWorld Word8),
    bufPtr :: !(Ptr Word8),
    bufSize :: !Int
  }

-- | Pool of reusable buffers
data BufferPool = BufferPool
  { poolBuffers :: ![Buffer], -- all buffers
    poolFree :: !(IORef [Buffer]), -- available buffers
    poolBufSize :: !Int
  }

-- | Create a new buffer pool
newBufferPool :: Int -> Int -> IO BufferPool
newBufferPool count bufSize = do
  buffers <- mapM (const $ allocBuffer bufSize) [1 .. count]
  freeRef <- newIORef buffers
  pure $
    BufferPool
      { poolBuffers = buffers,
        poolFree = freeRef,
        poolBufSize = bufSize
      }
  where
    allocBuffer size = do
      arr <- newPinnedPrimArray size
      let ptr = mutablePrimArrayContents arr
      pure $ Buffer arr ptr size

-- | Acquire a buffer from the pool (or allocate if empty)
{-# INLINE acquireBuffer #-}
acquireBuffer :: BufferPool -> IO Buffer
acquireBuffer BufferPool {..} = do
  free <- readIORef poolFree
  case free of
    (b : bs) -> do
      writeIORef poolFree bs
      pure b
    [] -> do
      -- Pool exhausted, allocate new (will be returned to pool later)
      arr <- newPinnedPrimArray poolBufSize
      let ptr = mutablePrimArrayContents arr
      pure $ Buffer arr ptr poolBufSize

-- | Return a buffer to the pool
{-# INLINE releaseBuffer #-}
releaseBuffer :: BufferPool -> Buffer -> IO ()
releaseBuffer BufferPool {..} buf = do
  modifyIORef' poolFree (buf :)
