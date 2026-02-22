{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Generational handles for resource management.
--
-- A Handle is an index + generation pair that uniquely identifies a resource.
-- The generation prevents use-after-free: when a resource is closed and its
-- slot reused, the old handle becomes invalid because the generation changed.
--
-- This is the same pattern as used in the C++ evring library.
module Evring.Handle
  ( Handle(..)
  , invalidHandle
  , isValid
  , makeHandle
  , packHandle
  , unpackHandle
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

-- | A generational handle: index + generation packed into 64 bits.
--
-- Layout: [index: 32 bits][generation: 32 bits]
--
-- The generation prevents ABA problems: when a resource slot is reused,
-- the generation is incremented, invalidating any old handles to that slot.
data Handle = Handle
  { handleIndex      :: !Word32
    -- ^ Index into the resource table
  , handleGeneration :: !Word32
    -- ^ Generation counter (incremented on reuse)
  } deriving stock (Eq, Ord, Show, Generic)

-- | The invalid handle sentinel value.
--
-- Index = maxBound, Generation = 0 is reserved for "no resource".
invalidHandle :: Handle
invalidHandle = Handle
  { handleIndex      = maxBound
  , handleGeneration = 0
  }

-- | Check if a handle is valid (not the invalid sentinel).
isValid :: Handle -> Bool
isValid h = h /= invalidHandle

-- | Create a handle from index and generation.
makeHandle :: Word32 -> Word32 -> Handle
makeHandle = Handle

-- | Pack a handle into a single Word64 for efficient storage/transmission.
packHandle :: Handle -> Word64
packHandle (Handle idx gen) =
  (fromIntegral idx `shiftL` 32) .|. fromIntegral gen

-- | Unpack a Word64 into a handle.
unpackHandle :: Word64 -> Handle
unpackHandle w = Handle
  { handleIndex      = fromIntegral (w `shiftR` 32)
  , handleGeneration = fromIntegral (w .&. 0xFFFFFFFF)
  }
