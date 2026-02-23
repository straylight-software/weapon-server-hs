{-# LANGUAGE OverloadedStrings #-}

{- | Identifier generation compatible with opencode TUI
IDs are lexicographically sortable with a time-based prefix
Format: 12 hex chars (timestamp) + 14 base62 random chars = 26 chars total

The module exposes both pure functions for testing and IO wrappers for production use.
-}
module Util.Identifier (
    -- * IO API (production use)
    ascending,
    descending,
    ascendingWithPrefix,
    descendingWithPrefix,

    -- * State management
    IdGenState,
    newIdGenState,
    withIdGenState,

    -- * Pure API (for testing)
    createPure,
    encodeTimeBytes,
    base62Chars,
    base62Vector,

    -- * Types
    IdParams (..),
) where

import Control.Concurrent.MVar
import Data.Bits (complement, shiftR, (.&.))
import Data.Char (chr)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import System.Random (randomIO)

-- | Parameters for pure ID generation (for testing)
data IdParams = IdParams
    { ipTimestamp :: !Word64
    -- ^ Timestamp in milliseconds
    , ipCounter :: !Word64
    -- ^ Counter for same-millisecond ordering
    , ipRandomSuffix :: !String
    -- ^ Random suffix (should be 14 base62 chars)
    , ipDescending :: !Bool
    -- ^ Whether to generate descending (time-inverted) ID
    }
    deriving (Show, Eq)

-- | State for ID generation, encapsulating mutable refs
data IdGenState = IdGenState
    { igsLastTimestamp :: !(IORef Word64)
    , igsCounter :: !(IORef Word64)
    , igsLock :: !(MVar ())
    }

-- | Create a new ID generation state
newIdGenState :: IO IdGenState
newIdGenState = do
    lastTs <- newIORef 0
    counter <- newIORef 0
    lock <- newMVar ()
    pure $ IdGenState lastTs counter lock

{- | Global state initialized on first use
This is a controlled use of global state for ID generation
The state is thread-safe via MVar locking
-}
globalState :: IO IdGenState
globalState = do
    lastTs <- newIORef 0
    counter <- newIORef 0
    lock <- newMVar ()
    pure $ IdGenState lastTs counter lock

-- | Bracket for using ID generation state
withIdGenState :: (IdGenState -> IO a) -> IO a
withIdGenState action = do
    state <- globalState
    action state

-- | Base62 character set (same as TypeScript)
base62Chars :: String
base62Chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

-- | Base62 character set as a Vector for safe indexing
base62Vector :: V.Vector Char
base62Vector = V.fromList base62Chars

-- | Safely index into base62 characters using Vector's safe indexing
safeBase62Char :: Int -> Char
safeBase62Char n
    | n >= 0 && n < 62 = fromMaybe '0' (base62Vector V.!? n)
    | otherwise = '0' -- Fallback (should never happen with mod 62)

-- | Generate random base62 characters
randomBase62 :: Int -> IO String
randomBase62 len = do
    bytes <- mapM (\_ -> randomIO :: IO Word8) [1 .. len]
    pure $ map (\b -> safeBase62Char (fromIntegral b `mod` 62)) bytes

-- | Create an ascending ID (lexicographically increasing with time)
ascending :: IO Text
ascending = withIdGenState $ \state -> createWithState state False

-- | Create an ascending ID with a prefix (e.g., "message_xxx")
ascendingWithPrefix :: Text -> IO Text
ascendingWithPrefix prefix = do
    id' <- ascending
    pure $ prefix <> "_" <> id'

-- | Create a descending ID (lexicographically decreasing with time)
descending :: IO Text
descending = withIdGenState $ \state -> createWithState state True

-- | Create a descending ID with a prefix
descendingWithPrefix :: Text -> IO Text
descendingWithPrefix prefix = do
    id' <- descending
    pure $ prefix <> "_" <> id'

-- | Core ID creation function using explicit state
createWithState :: IdGenState -> Bool -> IO Text
createWithState state isDescending = do
    -- Use lock for thread-safety
    () <- takeMVar (igsLock state)

    -- Get current timestamp in milliseconds
    posix <- getPOSIXTime
    let currentTimestamp = floor (posix * 1000) :: Word64

    -- Update counter (reset if timestamp changed, increment otherwise)
    lastTs <- readIORef (igsLastTimestamp state)
    counter <-
        if currentTimestamp /= lastTs
            then do
                writeIORef (igsLastTimestamp state) currentTimestamp
                writeIORef (igsCounter state) 1
                pure 1
            else do
                c <- readIORef (igsCounter state)
                let newCounter = c + 1
                writeIORef (igsCounter state) newCounter
                pure newCounter

    putMVar (igsLock state) ()

    -- Generate random suffix
    randomPart <- randomBase62 14

    -- Use pure function for the actual encoding
    let params =
            IdParams
                { ipTimestamp = currentTimestamp
                , ipCounter = counter
                , ipRandomSuffix = randomPart
                , ipDescending = isDescending
                }

    pure $ createPure params

{- | Pure ID creation function (for testing)
Given deterministic inputs, produces deterministic output
-}
createPure :: IdParams -> Text
createPure params =
    let
        -- Compute the time value: timestamp * 0x1000 + counter
        -- This gives us millisecond precision with sub-millisecond ordering
        timeValue :: Word64 = ipTimestamp params * 0x1000 + ipCounter params

        -- For descending IDs, complement the bits (flip all bits)
        finalValue = if ipDescending params then complement timeValue else timeValue

        -- Encode to hex
        hexPart = encodeTimeBytes finalValue

        -- Take exactly 14 chars from random suffix (pad or truncate)
        randomPart = take 14 (ipRandomSuffix params ++ repeat '0')
     in
        T.pack $ hexPart <> randomPart

{- | Encode a 64-bit value to 12 hex characters (6 bytes)
Takes the most significant 48 bits
-}
encodeTimeBytes :: Word64 -> String
encodeTimeBytes value =
    let bytes =
            [ fromIntegral ((value `shiftR` 40) .&. 0xFF) :: Word8
            , fromIntegral ((value `shiftR` 32) .&. 0xFF)
            , fromIntegral ((value `shiftR` 24) .&. 0xFF)
            , fromIntegral ((value `shiftR` 16) .&. 0xFF)
            , fromIntegral ((value `shiftR` 8) .&. 0xFF)
            , fromIntegral (value .&. 0xFF)
            ]
     in concatMap toHex bytes
  where
    toHex :: Word8 -> String
    toHex b =
        let hi = b `div` 16
            lo = b `mod` 16
         in [hexChar hi, hexChar lo]

    -- Safe hex character conversion using chr instead of toEnum
    hexChar :: Word8 -> Char
    hexChar n
        | n < 10 = chr (fromIntegral n + 48) -- '0' = 48
        | n < 16 = chr (fromIntegral n - 10 + 97) -- 'a' = 97
        | otherwise = '0' -- Fallback (should never happen)
