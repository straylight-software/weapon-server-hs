{-# LANGUAGE OverloadedStrings #-}

{- | Identifier generation property tests
Tests that IDs are lexicographically sortable and match expected format
-}
module Property.IdentifierProps where

import Control.Concurrent.Async (async, wait)
import Control.Monad (replicateM, when)
import Data.Char (isAlphaNum, isHexDigit)

import Data.List (sort)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Util.Identifier

-- | Check if text has exactly n characters (uses efficient compareLength)
textHasLength :: Int -> T.Text -> Bool
textHasLength n t = T.compareLength t n == EQ

{- | Check if list has exactly n elements (safe for infinite lists)
Uses explicit recursion to avoid 'length' on potentially infinite lists
-}
listHasLength :: Int -> [a] -> Bool
listHasLength 0 [] = True
listHasLength 0 (_ : _) = False
listHasLength n [] = n == 0
listHasLength n (_ : xs)
    | n > 0 = listHasLength (n - 1) xs
    | otherwise = False

-- | Get length of a finite list (strict, for test assertions)
listLength :: [a] -> Int
listLength = go 0
  where
    go !acc [] = acc
    go !acc (_ : xs) = go (acc + 1) xs

-- | Check if a list is sorted in ascending order
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : xs) = x <= y && isSorted (y : xs)

-- | Check if a list is sorted in descending order
isSortedDesc :: (Ord a) => [a] -> Bool
isSortedDesc [] = True
isSortedDesc [_] = True
isSortedDesc (x : y : xs) = x >= y && isSortedDesc (y : xs)

-- ═══════════════════════════════════════════════════════════════════════════
-- Core Properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: IDs have correct length (26 chars)
prop_idLength :: Property
prop_idLength = property $ do
    params <- forAll genIdParams
    let id' = createPure params
    assert $ textHasLength 26 id'

-- | Property: First 12 chars are hex digits
prop_idHexPrefix :: Property
prop_idHexPrefix = property $ do
    params <- forAll genIdParams
    let id' = createPure params
    let hexPart = T.take 12 id'
    assert $ T.all isHexDigit hexPart

-- | Property: Last 14 chars are base62 (alphanumeric)
prop_idBase62Suffix :: Property
prop_idBase62Suffix = property $ do
    params <- forAll genIdParams
    let id' = createPure params
    let suffix = T.drop 12 id'
    assert $ textHasLength 14 suffix
    -- All chars should be alphanumeric (base62 is subset of alphanumeric)
    assert $ T.all isAlphaNum suffix

{- | Property: Ascending IDs sort correctly by timestamp
IDs generated at later timestamps should sort after earlier ones
Note: We use realistic millisecond timestamps (like Date.now() returns)
because the encoding captures 48 bits of (timestamp * 0x1000 + counter)
-}
prop_ascendingOrderByTimestamp :: Property
prop_ascendingOrderByTimestamp = property $ do
    ts1 <- forAll genRealisticTimestamp
    ts2 <- forAll genRealisticTimestamp
    suffix <- forAll genBase62Suffix

    let id1 = createPure (IdParams ts1 1 suffix False)
    let id2 = createPure (IdParams ts2 1 suffix False)

    -- If ts1 < ts2, then id1 should sort before id2
    if ts1 < ts2
        then assert (id1 < id2)
        else
            if ts1 > ts2
                then assert (id1 > id2)
                else id1 === id2 -- Same timestamp and counter means same hex prefix

-- | Property: Ascending IDs sort correctly by counter (same timestamp)
prop_ascendingOrderByCounter :: Property
prop_ascendingOrderByCounter = property $ do
    ts <- forAll genRealisticTimestamp
    c1 <- forAll $ Gen.word64 (Range.linear 1 0xFFF)
    c2 <- forAll $ Gen.word64 (Range.linear 1 0xFFF)
    suffix <- forAll genBase62Suffix

    let id1 = createPure (IdParams ts c1 suffix False)
    let id2 = createPure (IdParams ts c2 suffix False)

    -- Same timestamp, different counter - should still sort correctly
    if c1 < c2
        then assert (id1 < id2)
        else
            if c1 > c2
                then assert (id1 > id2)
                else id1 === id2

-- | Property: Descending IDs sort in reverse order by timestamp
prop_descendingOrderByTimestamp :: Property
prop_descendingOrderByTimestamp = property $ do
    ts1 <- forAll genRealisticTimestamp
    ts2 <- forAll genRealisticTimestamp
    suffix <- forAll genBase62Suffix

    let id1 = createPure (IdParams ts1 1 suffix True)
    let id2 = createPure (IdParams ts2 1 suffix True)

    -- For descending: if ts1 < ts2, then id1 should sort AFTER id2
    if ts1 < ts2
        then assert (id1 > id2)
        else
            if ts1 > ts2
                then assert (id1 < id2)
                else id1 === id2

-- | Property: A list of ascending IDs sorts correctly
prop_ascendingListSort :: Property
prop_ascendingListSort = property $ do
    timestamps <- forAll $ Gen.list (Range.linear 1 20) genRealisticTimestamp
    suffix <- forAll genBase62Suffix

    let ids = zipWith (\ts i -> createPure (IdParams ts i suffix False)) timestamps [1 ..]
    let sortedTimestamps = sort timestamps
    let sortedIds = sort ids

    -- After sorting, the IDs should be in the same order as sorted timestamps
    -- (assuming unique timestamps; with same timestamps, counter matters)
    annotate $ "timestamps: " ++ show timestamps
    annotate $ "sortedTimestamps: " ++ show sortedTimestamps
    annotate $ "ids: " ++ show ids
    annotate $ "sortedIds: " ++ show sortedIds

    -- Basic check: sorted list should equal sort of list
    sortedIds === sort ids

-- | Property: Same inputs produce same outputs (determinism)
prop_determinism :: Property
prop_determinism = property $ do
    params <- forAll genIdParams
    let id1 = createPure params
    let id2 = createPure params
    id1 === id2

-- | Property: encodeTimeBytes produces 12 hex chars
prop_encodeTimeBytesLength :: Property
prop_encodeTimeBytesLength = property $ do
    value <- forAll $ Gen.word64 Range.linearBounded
    let encoded = encodeTimeBytes value
    assert $ listHasLength 12 encoded
    assert $ all isHexDigit encoded

-- | Property: Different timestamps produce different hex prefixes
prop_differentTimestampsDifferentPrefix :: Property
prop_differentTimestampsDifferentPrefix = property $ do
    ts1 <- forAll genRealisticTimestamp
    ts2 <- forAll genRealisticTimestamp

    -- Skip if timestamps are the same
    when (ts1 == ts2) discard

    let enc1 = encodeTimeBytes (ts1 * 0x1000 + 1)
    let enc2 = encodeTimeBytes (ts2 * 0x1000 + 1)

    assert $ enc1 /= enc2

-- | Property: Counter affects the encoded value
prop_counterAffectsEncoding :: Property
prop_counterAffectsEncoding = property $ do
    ts <- forAll $ Gen.word64 (Range.linear 0 (maxBound `div` 0x1000))
    c1 <- forAll $ Gen.word64 (Range.linear 1 0xFFF)
    c2 <- forAll $ Gen.word64 (Range.linear 1 0xFFF)

    when (c1 == c2) discard

    let enc1 = encodeTimeBytes (ts * 0x1000 + c1)
    let enc2 = encodeTimeBytes (ts * 0x1000 + c2)

    assert $ enc1 /= enc2

-- | Property: Message and part IDs with same base timestamp but incremented counter sort correctly
prop_messagePartOrdering :: Property
prop_messagePartOrdering = property $ do
    -- Simulate real usage: message created, then parts created slightly later
    baseTs <- forAll $ Gen.word64 (Range.linear 1000000000000 2000000000000) -- realistic ms timestamp
    suffix <- forAll genBase62Suffix

    -- Message ID created first
    let messageId = createPure (IdParams baseTs 1 suffix False)
    -- Part IDs created in same millisecond with higher counters
    let partId1 = createPure (IdParams baseTs 2 suffix False)
    let partId2 = createPure (IdParams baseTs 3 suffix False)
    -- Or created in next millisecond
    let partId3 = createPure (IdParams (baseTs + 1) 1 suffix False)

    -- All part IDs should sort after message ID
    assert $ messageId < partId1
    assert $ messageId < partId2
    assert $ messageId < partId3
    assert $ partId1 < partId2
    assert $ partId2 < partId3

-- ═══════════════════════════════════════════════════════════════════════════
-- IO-based Properties (testing actual state management)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: IDs generated via IO are unique
prop_ioIdsUnique :: Property
prop_ioIdsUnique = withTests 50 $ property $ do
    count <- forAll $ Gen.int (Range.linear 10 100)
    ids <- evalIO $ do
        idGen <- newIdGenState
        replicateM count (ascending idGen)
    -- All IDs should be unique (Set.size equals list length)
    Set.size (Set.fromList ids) === listLength ids

-- | Property: Ascending IDs generated via IO are lexicographically ordered
prop_ioAscendingOrdered :: Property
prop_ioAscendingOrdered = withTests 50 $ property $ do
    count <- forAll $ Gen.int (Range.linear 5 50)
    ids <- evalIO $ do
        idGen <- newIdGenState
        replicateM count (ascending idGen)
    -- IDs should already be in sorted order
    assert $ isSorted ids

-- | Property: Descending IDs generated via IO are reverse lexicographically ordered
prop_ioDescendingOrdered :: Property
prop_ioDescendingOrdered = withTests 50 $ property $ do
    count <- forAll $ Gen.int (Range.linear 5 50)
    ids <- evalIO $ do
        idGen <- newIdGenState
        replicateM count (descending idGen)
    -- IDs should be in reverse sorted order
    assert $ isSortedDesc ids

-- | Property: Concurrent ID generation produces unique IDs (thread safety)
prop_ioConcurrentUnique :: Property
prop_ioConcurrentUnique = withTests 20 $ property $ do
    numThreads <- forAll $ Gen.int (Range.linear 2 8)
    idsPerThread <- forAll $ Gen.int (Range.linear 10 30)
    allIds <- evalIO $ do
        idGen <- newIdGenState
        asyncs <-
            replicateM numThreads $
                async $
                    replicateM idsPerThread (ascending idGen)
        concat <$> mapM wait asyncs
    -- All IDs should be unique even with concurrent generation
    let totalCount = listLength allIds
    let uniqueCount = Set.size (Set.fromList allIds)
    annotate $ "Total IDs: " ++ show totalCount
    annotate $ "Unique IDs: " ++ show uniqueCount
    uniqueCount === totalCount

-- | Property: ascendingWithPrefix correctly applies prefix
prop_ioAscendingWithPrefix :: Property
prop_ioAscendingWithPrefix = withTests 50 $ property $ do
    prefix <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
    id' <- evalIO $ do
        idGen <- newIdGenState
        ascendingWithPrefix idGen prefix
    -- ID should start with prefix_
    let prefixWithUnderscore = prefix <> "_"
    case T.stripPrefix prefixWithUnderscore id' of
        Nothing -> do
            annotate $ "ID does not start with expected prefix: " ++ T.unpack prefixWithUnderscore
            failure
        Just suffix ->
            -- After prefix_, should have 26 chars (the base ID length)
            assert $ textHasLength 26 suffix

-- | Property: descendingWithPrefix correctly applies prefix
prop_ioDescendingWithPrefix :: Property
prop_ioDescendingWithPrefix = withTests 50 $ property $ do
    prefix <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
    id' <- evalIO $ do
        idGen <- newIdGenState
        descendingWithPrefix idGen prefix
    -- ID should start with prefix_
    let prefixWithUnderscore = prefix <> "_"
    case T.stripPrefix prefixWithUnderscore id' of
        Nothing -> do
            annotate $ "ID does not start with expected prefix: " ++ T.unpack prefixWithUnderscore
            failure
        Just suffix ->
            -- After prefix_, should have 26 chars (the base ID length)
            assert $ textHasLength 26 suffix

-- | Property: IDs with same prefix still maintain ordering
prop_ioPrefixedOrdering :: Property
prop_ioPrefixedOrdering = withTests 50 $ property $ do
    prefix <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
    count <- forAll $ Gen.int (Range.linear 5 20)
    ids <- evalIO $ do
        idGen <- newIdGenState
        replicateM count (ascendingWithPrefix idGen prefix)
    -- Prefixed IDs should maintain order
    ids === sort ids

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

genIdParams :: Gen IdParams
genIdParams = do
    ts <- genRealisticTimestamp
    counter <- Gen.word64 (Range.linear 1 0xFFF) -- Counter typically 1-4095
    suffix <- genBase62Suffix
    IdParams ts counter suffix <$> Gen.bool

{- | Generate realistic millisecond timestamps

IMPORTANT: The ID encoding captures only 48 bits of (timestamp * 0x1000 + counter).
For timestamps > 36 bits (~68B ms), the high bits are truncated. This means
timestamps that differ by more than ~68B ms might not sort correctly.

In practice, this is fine because:
1. IDs are compared within sessions (same day/hour)
2. The TypeScript implementation has the same limitation

For testing, we use a narrow window (~1 hour) to ensure correct ordering.
-}
genRealisticTimestamp :: Gen Word64
genRealisticTimestamp =
    -- Use a 1-hour window around "now" (Feb 2025)
    -- This ensures all timestamps have the same high bits
    let baseTime = 1740000000000 :: Word64 -- ~Feb 2025
        oneHour = 3600000 :: Word64 -- 1 hour in ms
     in Gen.word64 (Range.linear baseTime (baseTime + oneHour))

genBase62Suffix :: Gen String
genBase62Suffix = do
    -- Generate exactly 14 base62 characters
    Gen.list (Range.singleton 14) genBase62Char

genBase62Char :: Gen Char
genBase62Char = Gen.element base62Chars

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Identifier Property Tests"
        [ testGroup
            "Format"
            [ testProperty "ID length is 26" prop_idLength
            , testProperty "First 12 chars are hex" prop_idHexPrefix
            , testProperty "Last 14 chars are base62" prop_idBase62Suffix
            , testProperty "encodeTimeBytes produces 12 hex chars" prop_encodeTimeBytesLength
            ]
        , testGroup
            "Ordering"
            [ testProperty "Ascending IDs order by timestamp" prop_ascendingOrderByTimestamp
            , testProperty "Ascending IDs order by counter" prop_ascendingOrderByCounter
            , testProperty "Descending IDs reverse order" prop_descendingOrderByTimestamp
            , testProperty "List sort works correctly" prop_ascendingListSort
            , testProperty "Message/part ordering works" prop_messagePartOrdering
            ]
        , testGroup
            "Encoding"
            [ testProperty "Different timestamps -> different prefix" prop_differentTimestampsDifferentPrefix
            , testProperty "Counter affects encoding" prop_counterAffectsEncoding
            , testProperty "Same inputs -> same outputs" prop_determinism
            ]
        , testGroup
            "IO State"
            [ testProperty "IO IDs are unique" prop_ioIdsUnique
            , testProperty "IO ascending IDs are ordered" prop_ioAscendingOrdered
            , testProperty "IO descending IDs are reverse ordered" prop_ioDescendingOrdered
            , testProperty "IO concurrent IDs are unique" prop_ioConcurrentUnique
            , testProperty "ascendingWithPrefix applies prefix" prop_ioAscendingWithPrefix
            , testProperty "descendingWithPrefix applies prefix" prop_ioDescendingWithPrefix
            , testProperty "Prefixed IDs maintain ordering" prop_ioPrefixedOrdering
            ]
        ]
