{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for System.IoUring pure functions

These tests verify the pure logic that can be tested without
actually interacting with io_uring (which requires Linux and root).
-}
module Property.IoUringProps where

import Foreign.C.Error (Errno (..))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.IoUring (
    IoResult (..),
    computeRingSize,
    interpretResult,
 )
import System.IoUring.URing (
    IOCompletion (..),
    IOOpId (..),
    IOResult (..),
    parseCqe,
 )
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- IoUring.interpretResult Properties
-- ============================================================================

-- | Negative results are interpreted as errors
prop_interpretResultNegativeIsError :: Property
prop_interpretResultNegativeIsError = property $ do
    -- Generate negative values (errors in io_uring)
    rawResult <- forAll $ Gen.int64 (Range.linear (-4096) (-1))
    let result = interpretResult rawResult
    case result of
        IoErrno (Errno e) -> do
            -- Error code should be the negation
            fromIntegral e === negate rawResult
        Complete _ -> do
            annotate "Expected IoErrno for negative result"
            failure
        Eof -> do
            annotate "Expected IoErrno for negative result"
            failure

-- | Zero is interpreted as success with 0 bytes
prop_interpretResultZeroIsSuccess :: Property
prop_interpretResultZeroIsSuccess = property $ do
    let result = interpretResult 0
    result === Complete 0

-- | Positive results are interpreted as success with byte count
prop_interpretResultPositiveIsSuccess :: Property
prop_interpretResultPositiveIsSuccess = property $ do
    rawResult <- forAll $ Gen.int64 (Range.linear 1 (2 ^ (30 :: Int)))
    let result = interpretResult rawResult
    case result of
        Complete n -> fromIntegral n === rawResult
        IoErrno _ -> do
            annotate "Expected Complete for positive result"
            failure
        Eof -> do
            annotate "Expected Complete for positive result"
            failure

-- | interpretResult is deterministic
prop_interpretResultDeterministic :: Property
prop_interpretResultDeterministic = property $ do
    rawResult <- forAll $ Gen.int64 Range.linearBounded
    let r1 = interpretResult rawResult
    let r2 = interpretResult rawResult
    r1 === r2

-- ============================================================================
-- IoUring.computeRingSize Properties
-- ============================================================================

-- | Ring size is at least 32
prop_ringSizeMinimum :: Property
prop_ringSizeMinimum = property $ do
    batchSize <- forAll $ Gen.int (Range.linear 0 1000)
    let ringSize = computeRingSize batchSize
    assert $ ringSize >= 32

-- | Ring size is at least the batch size
prop_ringSizeAtLeastBatchSize :: Property
prop_ringSizeAtLeastBatchSize = property $ do
    batchSize <- forAll $ Gen.int (Range.linear 0 1000)
    let ringSize = computeRingSize batchSize
    assert $ ringSize >= batchSize

-- | Ring size equals batch size when batch size >= 32
prop_ringSizeEqualsBatchWhenLarge :: Property
prop_ringSizeEqualsBatchWhenLarge = property $ do
    batchSize <- forAll $ Gen.int (Range.linear 32 1000)
    let ringSize = computeRingSize batchSize
    ringSize === batchSize

-- | computeRingSize is deterministic
prop_ringSizeDeterministic :: Property
prop_ringSizeDeterministic = property $ do
    batchSize <- forAll $ Gen.int (Range.linear 0 1000)
    let r1 = computeRingSize batchSize
    let r2 = computeRingSize batchSize
    r1 === r2

-- ============================================================================
-- URing.parseCqe Properties
-- ============================================================================

-- | parseCqe preserves user data
prop_parseCqePreservesUserData :: Property
prop_parseCqePreservesUserData = property $ do
    userData <- forAll $ Gen.word64 Range.linearBounded
    res32 <- forAll $ Gen.int32 Range.linearBounded
    let IOCompletion (IOOpId extractedUserData) _ = parseCqe userData res32
    extractedUserData === userData

-- | parseCqe preserves result value
prop_parseCqePreservesResult :: Property
prop_parseCqePreservesResult = property $ do
    userData <- forAll $ Gen.word64 Range.linearBounded
    res32 <- forAll $ Gen.int32 Range.linearBounded
    let IOCompletion _ (IOResult extractedResult) = parseCqe userData res32
    extractedResult === fromIntegral res32

-- | parseCqe is deterministic
prop_parseCqeDeterministic :: Property
prop_parseCqeDeterministic = property $ do
    userData <- forAll $ Gen.word64 Range.linearBounded
    res32 <- forAll $ Gen.int32 Range.linearBounded
    let c1 = parseCqe userData res32
    let c2 = parseCqe userData res32
    c1 === c2

-- | parseCqe correctly converts negative results
prop_parseCqeNegativeResult :: Property
prop_parseCqeNegativeResult = property $ do
    userData <- forAll $ Gen.word64 Range.linearBounded
    -- Generate negative result (error case)
    res32 <- forAll $ Gen.int32 (Range.linear (-4096) (-1))
    let IOCompletion _ (IOResult extractedResult) = parseCqe userData res32
    -- Should preserve the sign
    assert $ extractedResult < 0
    extractedResult === fromIntegral res32

-- ============================================================================
-- Cross-module Consistency Properties
-- ============================================================================

-- | interpretResult and parseCqe should be consistent for the result part
prop_interpretResultConsistentWithParseCqe :: Property
prop_interpretResultConsistentWithParseCqe = property $ do
    res32 <- forAll $ Gen.int32 Range.linearBounded
    let ioResult = interpretResult (fromIntegral res32)
    let IOCompletion _ (IOResult parsedResult) = parseCqe 0 res32

    -- For positive values, interpretResult returns Complete with byte count
    -- parseCqe returns IOResult with the raw value
    -- They should be consistent
    case ioResult of
        Complete n -> do
            annotate $ "Complete " ++ show n ++ " vs IOResult " ++ show parsedResult
            assert $ parsedResult >= 0
            fromIntegral n === parsedResult
        IoErrno (Errno e) -> do
            annotate $ "IoErrno " ++ show e ++ " vs IOResult " ++ show parsedResult
            assert $ parsedResult < 0
            fromIntegral (negate e) === parsedResult
        Eof -> do
            annotate "Unexpected Eof"
            failure

-- ============================================================================
-- Test Tree
-- ============================================================================

tests :: TestTree
tests =
    testGroup
        "IoUring Property Tests"
        [ testGroup
            "interpretResult"
            [ testProperty "Negative results are errors" prop_interpretResultNegativeIsError
            , testProperty "Zero is success with 0 bytes" prop_interpretResultZeroIsSuccess
            , testProperty "Positive results are success" prop_interpretResultPositiveIsSuccess
            , testProperty "Is deterministic" prop_interpretResultDeterministic
            ]
        , testGroup
            "computeRingSize"
            [ testProperty "Ring size >= 32" prop_ringSizeMinimum
            , testProperty "Ring size >= batch size" prop_ringSizeAtLeastBatchSize
            , testProperty "Ring size = batch size when >= 32" prop_ringSizeEqualsBatchWhenLarge
            , testProperty "Is deterministic" prop_ringSizeDeterministic
            ]
        , testGroup
            "parseCqe"
            [ testProperty "Preserves user data" prop_parseCqePreservesUserData
            , testProperty "Preserves result value" prop_parseCqePreservesResult
            , testProperty "Is deterministic" prop_parseCqeDeterministic
            , testProperty "Correctly handles negative results" prop_parseCqeNegativeResult
            ]
        , testGroup
            "Cross-module Consistency"
            [ testProperty "interpretResult consistent with parseCqe" prop_interpretResultConsistentWithParseCqe
            ]
        ]
