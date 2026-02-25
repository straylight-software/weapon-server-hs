{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.EvringProps
Description : Property tests for Evring modules

Comprehensive property tests for:

* Handle generation and packing
* HTTP parsing utilities
* Sigil wire format decoding
* Machine replay determinism
-}
module Property.EvringProps (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Evring.Handle (
    invalidHandle,
    isValid,
    makeHandle,
    packHandle,
    unpackHandle,
 )
import Evring.Sigil (
    ParseMode (..),
    SigilState (..),
    decodeVarint,
    initSigilState,
    isControlByte,
    isExtendedByte,
    isHotByte,
    resetSigilState,
 )
import Evring.Wai.Internal (
    byteSwap16,
    checkKeepAliveHeaders,
    splitHeaderBody,
    splitPathQuery,
    stripCR,
 )

-- ════════════════════════════════════════════════════════════════════════════
-- Handle Tests
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: pack/unpack roundtrips correctly
prop_handle_pack_unpack_roundtrip :: Property
prop_handle_pack_unpack_roundtrip = property $ do
    idx <- forAll $ Gen.word32 Range.linearBounded
    gen <- forAll $ Gen.word32 Range.linearBounded
    let h = makeHandle idx gen
    unpackHandle (packHandle h) === h

-- | Property: invalidHandle is not valid
prop_invalid_handle_not_valid :: Property
prop_invalid_handle_not_valid = property $ do
    isValid invalidHandle === False

-- | Property: any non-maxBound index handle is valid
prop_normal_handle_is_valid :: Property
prop_normal_handle_is_valid = property $ do
    idx <- forAll $ Gen.word32 (Range.linear 0 (maxBound - 1))
    gen <- forAll $ Gen.word32 Range.linearBounded
    let h = makeHandle idx gen
    isValid h === True

-- | Property: handle equality is correct
prop_handle_equality :: Property
prop_handle_equality = property $ do
    idx1 <- forAll $ Gen.word32 Range.linearBounded
    gen1 <- forAll $ Gen.word32 Range.linearBounded
    idx2 <- forAll $ Gen.word32 Range.linearBounded
    gen2 <- forAll $ Gen.word32 Range.linearBounded
    let h1 = makeHandle idx1 gen1
        h2 = makeHandle idx2 gen2
    (h1 == h2) === (idx1 == idx2 && gen1 == gen2)

-- ════════════════════════════════════════════════════════════════════════════
-- HTTP Parsing Tests
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: stripCR removes trailing CR
prop_stripCR_removes_trailing_cr :: Property
prop_stripCR_removes_trailing_cr = property $ do
    content <- forAll $ Gen.bytes (Range.linear 0 100)
    let withCR = content <> "\r"
    stripCR withCR === content

-- | Property: stripCR is idempotent on clean input
prop_stripCR_idempotent :: Property
prop_stripCR_idempotent = property $ do
    content <- forAll $ Gen.bytes (Range.linear 0 100)
    -- Filter out trailing CRs for clean input
    let clean
            | BS.null content = content
            | BS.last content == 13 = BS.init content
            | otherwise = content
    stripCR clean === clean

-- | Property: splitHeaderBody finds CRLFCRLF boundary
prop_splitHeaderBody_finds_boundary :: Property
prop_splitHeaderBody_finds_boundary = property $ do
    headers <- forAll genHttpHeaders
    body <- forAll $ Gen.bytes (Range.linear 0 100)
    let combined = headers <> "\r\n\r\n" <> body
        (h, b) = splitHeaderBody combined
    h === headers
    b === body

-- | Property: splitHeaderBody handles no body
prop_splitHeaderBody_no_body :: Property
prop_splitHeaderBody_no_body = property $ do
    headers <- forAll genHttpHeaders
    let combined = headers <> "\r\n\r\n"
        (h, b) = splitHeaderBody combined
    h === headers
    BS.null b === True

-- | Property: splitPathQuery splits at question mark
prop_splitPathQuery_splits_at_question :: Property
prop_splitPathQuery_splits_at_question = property $ do
    path <- forAll genUrlPath
    query <- forAll genQueryString
    let combined = path <> "?" <> query
        (p, q) = splitPathQuery combined
    p === path
    q === ("?" <> query)

-- | Property: splitPathQuery handles no query
prop_splitPathQuery_no_query :: Property
prop_splitPathQuery_no_query = property $ do
    path <- forAll genUrlPath
    let (p, q) = splitPathQuery path
    p === path
    BS.null q === True

-- | Property: checkKeepAliveHeaders respects Connection: close
prop_keepAlive_close_header :: Property
prop_keepAlive_close_header = property $ do
    isHttp11 <- forAll Gen.bool
    checkKeepAliveHeaders (Just "close") isHttp11 === False

-- | Property: checkKeepAliveHeaders respects Connection: keep-alive
prop_keepAlive_keepalive_header :: Property
prop_keepAlive_keepalive_header = property $ do
    isHttp11 <- forAll Gen.bool
    checkKeepAliveHeaders (Just "keep-alive") isHttp11 === True

-- | Property: HTTP/1.1 defaults to keep-alive without header
prop_keepAlive_http11_default :: Property
prop_keepAlive_http11_default = property $ do
    checkKeepAliveHeaders Nothing True === True

-- | Property: HTTP/1.0 defaults to close without header
prop_keepAlive_http10_default :: Property
prop_keepAlive_http10_default = property $ do
    checkKeepAliveHeaders Nothing False === False

-- | Property: byteSwap16 is its own inverse
prop_byteSwap16_involution :: Property
prop_byteSwap16_involution = property $ do
    w <- forAll $ Gen.word16 Range.linearBounded
    byteSwap16 (byteSwap16 w) === w

-- ════════════════════════════════════════════════════════════════════════════
-- Sigil Tests
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: initSigilState is the ground state
prop_sigil_init_is_ground :: Property
prop_sigil_init_is_ground = property $ do
    let s = initSigilState
    sigilParseMode s === ModeText
    null (sigilBuffer s) === True
    BS.null (sigilLeftover s) === True
    sigilDone s === False

-- | Property: reset always returns to ground state
prop_sigil_reset_returns_ground :: Property
prop_sigil_reset_returns_ground = property $ do
    mode <- forAll genParseMode
    let s = initSigilState{sigilParseMode = mode}
    resetSigilState s === initSigilState

-- | Property: reset is idempotent
prop_sigil_reset_idempotent :: Property
prop_sigil_reset_idempotent = property $ do
    mode <- forAll genParseMode
    let s = initSigilState{sigilParseMode = mode}
    resetSigilState (resetSigilState s) === resetSigilState s

-- | Property: isHotByte identifies 0x00-0x7E correctly
prop_sigil_isHotByte :: Property
prop_sigil_isHotByte = property $ do
    b <- forAll $ Gen.word8 Range.linearBounded
    isHotByte b === (b < 0x7F)

-- | Property: isExtendedByte identifies 0x80-0xBF correctly
prop_sigil_isExtendedByte :: Property
prop_sigil_isExtendedByte = property $ do
    b <- forAll $ Gen.word8 Range.linearBounded
    isExtendedByte b === (b >= 0x80 && b < 0xC0)

-- | Property: isControlByte identifies 0xC0-0xCF and 0xF0
prop_sigil_isControlByte :: Property
prop_sigil_isControlByte = property $ do
    b <- forAll $ Gen.word8 Range.linearBounded
    let expected = (b >= 0xC0 && b < 0xD0) || b == 0xF0
    isControlByte b === expected

-- | Property: decodeVarint decodes single-byte values
prop_sigil_varint_single_byte :: Property
prop_sigil_varint_single_byte = property $ do
    val <- forAll $ Gen.word8 (Range.linear 0 127)
    decodeVarint (BS.singleton val) === Just (fromIntegral val, 1)

-- | Property: decodeVarint decodes multi-byte values
prop_sigil_varint_multi_byte :: Property
prop_sigil_varint_multi_byte = property $ do
    -- Encode a known two-byte value: 300 = 0x012C = [0xAC, 0x02]
    let encoded = BS.pack [0xAC, 0x02]
    decodeVarint encoded === Just (300, 2)

-- | Property: decodeVarint returns Nothing for incomplete input
prop_sigil_varint_incomplete :: Property
prop_sigil_varint_incomplete = property $ do
    -- High bit set means continuation expected
    let incomplete = BS.pack [0x80]
    decodeVarint incomplete === Nothing

-- ════════════════════════════════════════════════════════════════════════════
-- Generators
-- ════════════════════════════════════════════════════════════════════════════

genHttpHeaders :: Gen ByteString
genHttpHeaders = do
    method <- Gen.element ["GET", "POST", "PUT", "DELETE"]
    path <- genUrlPath
    pure $ method <> " " <> path <> " HTTP/1.1\r\nHost: localhost"

genUrlPath :: Gen ByteString
genUrlPath = do
    segments <- Gen.list (Range.linear 1 5) genPathSegment
    pure $ "/" <> BS.intercalate "/" segments

-- | Generate a path segment without special characters like '?'
genPathSegment :: Gen ByteString
genPathSegment = Gen.utf8 (Range.linear 1 20) Gen.alphaNum

-- | Generate a query string without '?' character
genQueryString :: Gen ByteString
genQueryString = do
    key <- Gen.utf8 (Range.linear 1 10) Gen.alphaNum
    val <- Gen.utf8 (Range.linear 1 20) Gen.alphaNum
    pure $ key <> "=" <> val

genParseMode :: Gen ParseMode
genParseMode = Gen.element [ModeText, ModeThink, ModeToolCall, ModeCodeBlock]

-- ════════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ════════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Evring Property Tests"
        [ testGroup
            "Handle"
            [ testProperty "pack/unpack roundtrip" prop_handle_pack_unpack_roundtrip
            , testProperty "invalidHandle is not valid" prop_invalid_handle_not_valid
            , testProperty "normal handle is valid" prop_normal_handle_is_valid
            , testProperty "equality is correct" prop_handle_equality
            ]
        , testGroup
            "HTTP Parsing"
            [ testProperty "stripCR removes trailing CR" prop_stripCR_removes_trailing_cr
            , testProperty "stripCR idempotent on clean input" prop_stripCR_idempotent
            , testProperty "splitHeaderBody finds boundary" prop_splitHeaderBody_finds_boundary
            , testProperty "splitHeaderBody handles no body" prop_splitHeaderBody_no_body
            , testProperty "splitPathQuery splits at ?" prop_splitPathQuery_splits_at_question
            , testProperty "splitPathQuery handles no query" prop_splitPathQuery_no_query
            , testProperty "keep-alive: close header" prop_keepAlive_close_header
            , testProperty "keep-alive: keep-alive header" prop_keepAlive_keepalive_header
            , testProperty "keep-alive: HTTP/1.1 default" prop_keepAlive_http11_default
            , testProperty "keep-alive: HTTP/1.0 default" prop_keepAlive_http10_default
            , testProperty "byteSwap16 is involution" prop_byteSwap16_involution
            ]
        , testGroup
            "Sigil"
            [ testProperty "init is ground state" prop_sigil_init_is_ground
            , testProperty "reset returns to ground" prop_sigil_reset_returns_ground
            , testProperty "reset is idempotent" prop_sigil_reset_idempotent
            , testProperty "isHotByte classification" prop_sigil_isHotByte
            , testProperty "isExtendedByte classification" prop_sigil_isExtendedByte
            , testProperty "isControlByte classification" prop_sigil_isControlByte
            , testProperty "varint single byte" prop_sigil_varint_single_byte
            , testProperty "varint multi byte" prop_sigil_varint_multi_byte
            , testProperty "varint incomplete" prop_sigil_varint_incomplete
            ]
        ]
