{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Evring.Wai.Internal
Description : Shared utilities for evring-wai server modules

This module provides shared utilities for the evring-wai server:

* Socket address parsing from raw sockaddr structures
* Byte order conversion (network to host)
* IPv6 address construction helpers
* HTTP header parsing utilities

These utilities are used across 'Evring.Wai', 'Evring.Wai.Server',
'Evring.Wai.MultiCore', and 'Evring.Wai.Conn'.
-}
module Evring.Wai.Internal (
    -- * Socket Address Parsing
    parseSockAddr,

    -- * Byte Order Conversion
    byteSwap16,

    -- * IPv6 Helpers
    ipv6AnyAddress,
    ipv6AddressFromWords,

    -- * HTTP Parsing (Pure)
    parseHeaders,
    getContentLength,
    stripCR,
    splitHeaderBody,
    splitPathQuery,

    -- * HTTP Header Building
    formatHeader,

    -- * Keep-Alive Detection
    checkKeepAliveHeaders,
) where

import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BC
import Data.CaseInsensitive qualified as CI
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word32)
import Foreign (Ptr, peekByteOff)
import Network.HTTP.Types (RequestHeaders)
import Network.Socket (HostAddress6, PortNumber, SockAddr (..))
import Text.Read (readMaybe)

-- ════════════════════════════════════════════════════════════════════════════
-- Socket Address Parsing
-- ════════════════════════════════════════════════════════════════════════════

{- | Parse a sockaddr structure from an accept buffer.

Handles AF_INET (IPv4) family code 2 and AF_INET6 (IPv6) family code 10.
Returns a placeholder address for unknown family codes.

The buffer layout for AF_INET is:

@
struct sockaddr_in {
    sa_family_t    sin_family;   // 2 bytes at offset 0
    in_port_t      sin_port;     // 2 bytes at offset 2 (network byte order)
    struct in_addr sin_addr;     // 4 bytes at offset 4
    char           sin_zero[8];  // padding
};
@
-}
parseSockAddr :: Ptr () -> IO SockAddr
parseSockAddr addrBuf = do
    family <- peekByteOff addrBuf 0 :: IO Word16
    case family of
        2 -> parseIPv4 addrBuf
        10 -> parseIPv6 addrBuf
        _unknownFamily -> pure $ SockAddrInet 0 0

-- | Parse IPv4 sockaddr_in structure.
parseIPv4 :: Ptr () -> IO SockAddr
parseIPv4 addrBuf = do
    port <- peekByteOff addrBuf 2 :: IO Word16
    addr <- peekByteOff addrBuf 4 :: IO Word32
    let portNum = fromIntegral (byteSwap16 port) :: PortNumber
    pure $ SockAddrInet portNum addr

{- | Parse IPv6 sockaddr_in6 structure.

Note: Currently returns a placeholder IPv6 address. Full IPv6 parsing
would read the 16-byte address at offset 8.
-}
parseIPv6 :: Ptr () -> IO SockAddr
parseIPv6 addrBuf = do
    port <- peekByteOff addrBuf 2 :: IO Word16
    let portNum = fromIntegral (byteSwap16 port) :: PortNumber
    pure $ SockAddrInet6 portNum 0 ipv6AnyAddress 0

-- ════════════════════════════════════════════════════════════════════════════
-- Byte Order Conversion
-- ════════════════════════════════════════════════════════════════════════════

{- | Byte swap for 16-bit values (network to host order on little-endian).

Network byte order is big-endian. On little-endian hosts, we need to swap.
This function swaps unconditionally, which is correct for x86/x86_64.

>>> byteSwap16 0x1234
0x3412
-}
byteSwap16 :: Word16 -> Word16
byteSwap16 w = (w `shiftR` 8) .|. (w `shiftL` 8)
{-# INLINE byteSwap16 #-}

-- ════════════════════════════════════════════════════════════════════════════
-- IPv6 Helpers
-- ════════════════════════════════════════════════════════════════════════════

{- | IPv6 any address (::) as HostAddress6.

Used as a placeholder when full IPv6 parsing isn't needed.
Wrapped to avoid stan's "big tuple" warning.
-}
ipv6AnyAddress :: HostAddress6
ipv6AnyAddress = ipv6AddressFromWords 0 0 0 0

{- | Construct HostAddress6 from four Word32 components.

HostAddress6 is defined as @(Word32, Word32, Word32, Word32)@ in the
network library. This wrapper avoids tuple syntax in calling code.
-}
ipv6AddressFromWords :: Word32 -> Word32 -> Word32 -> Word32 -> HostAddress6
ipv6AddressFromWords !a !b !c !d = (a, b, c, d)
{-# INLINE ipv6AddressFromWords #-}

-- ════════════════════════════════════════════════════════════════════════════
-- HTTP Header Parsing (Pure)
-- ════════════════════════════════════════════════════════════════════════════

{- | Parse header lines into RequestHeaders.

Each line should be in the format @Name: Value@ (with optional trailing CR).
Malformed headers (missing colon) are skipped.

>>> parseHeaders ["Content-Type: text/plain\r", "Content-Length: 42"]
[("Content-Type","text/plain"),("Content-Length","42")]
-}
parseHeaders :: [ByteString] -> RequestHeaders
parseHeaders = foldr parseHeader []
  where
    parseHeader line acc =
        case BC.break (== ':') (stripCR line) of
            (name, rest)
                | BS.null rest -> acc -- Skip malformed headers
                | otherwise ->
                    let value = BS.dropWhile (== 32) (BS.drop 1 rest) -- Drop ':' and leading spaces
                     in (CI.mk name, value) : acc

{- | Get Content-Length from headers, defaulting to 0.

>>> getContentLength [("Content-Length", "42")]
42
>>> getContentLength []
0
-}
getContentLength :: RequestHeaders -> Int
getContentLength headers =
    case lookup "Content-Length" headers of
        Just val -> fromMaybe 0 (readMaybe (BC.unpack val))
        Nothing -> 0
{-# INLINE getContentLength #-}

{- | Strip trailing carriage return from a line.

HTTP headers use CRLF line endings. After splitting on LF, lines may
have a trailing CR that needs to be removed.

>>> stripCR "Hello\r"
"Hello"
>>> stripCR "Hello"
"Hello"
-}
stripCR :: ByteString -> ByteString
stripCR bs
    | BS.null bs = bs
    | BS.last bs == 13 = BS.init bs -- 13 = '\r'
    | otherwise = bs
{-# INLINE stripCR #-}

{- | Split raw data into header section and body at the CRLFCRLF boundary.

Returns @(headers, body)@ where body is everything after @\\r\\n\\r\\n@.

>>> splitHeaderBody "GET / HTTP/1.1\r\n\r\nbody"
("GET / HTTP/1.1","body")
-}
splitHeaderBody :: ByteString -> (ByteString, ByteString)
splitHeaderBody bs =
    case BS.breakSubstring "\r\n\r\n" bs of
        (headers, rest)
            | BS.null rest -> (headers, BS.empty)
            | otherwise -> (headers, BS.drop 4 rest) -- Drop the \r\n\r\n

{- | Split path from query string at '?'.

The query string (if present) includes the leading '?'.

>>> splitPathQuery "/api/v1?foo=bar"
("/api/v1","?foo=bar")
>>> splitPathQuery "/api/v1"
("/api/v1","")
-}
splitPathQuery :: ByteString -> (ByteString, ByteString)
splitPathQuery bs =
    case BC.break (== '?') bs of
        (path, query)
            | BS.null query -> (path, BS.empty)
            | otherwise -> (path, query) -- Keep the '?' in query string

-- ════════════════════════════════════════════════════════════════════════════
-- HTTP Header Building
-- ════════════════════════════════════════════════════════════════════════════

{- | Format a single header as @Name: Value\\r\\n@.

Used when building HTTP response headers.
-}
formatHeader :: (CI.CI ByteString, ByteString) -> Builder.Builder
formatHeader (name, value) =
    mconcat
        [ Builder.byteString (CI.original name)
        , Builder.byteString ": "
        , Builder.byteString value
        , Builder.byteString "\r\n"
        ]
{-# INLINE formatHeader #-}

-- ════════════════════════════════════════════════════════════════════════════
-- Keep-Alive Detection
-- ════════════════════════════════════════════════════════════════════════════

{- | Check if connection should be kept alive based on headers.

HTTP/1.1 defaults to keep-alive unless @Connection: close@ is present.
HTTP/1.0 closes unless @Connection: keep-alive@ is present.

This is a pure function that takes the Connection header value and
HTTP version as a boolean (True for HTTP/1.1+).

>>> checkKeepAliveHeaders Nothing True  -- HTTP/1.1, no Connection header
True
>>> checkKeepAliveHeaders (Just "close") True
False
>>> checkKeepAliveHeaders (Just "keep-alive") False  -- HTTP/1.0
True
-}
checkKeepAliveHeaders ::
    -- | Connection header value
    Maybe ByteString ->
    -- | Is HTTP/1.1 or later
    Bool ->
    Bool
checkKeepAliveHeaders connHeader isHttp11 =
    case connHeader of
        Just val
            | CI.mk val == "close" -> False
            | CI.mk val == "keep-alive" -> True
            | otherwise -> isHttp11 -- Unknown value, default based on version
        Nothing -> isHttp11 -- HTTP/1.1 defaults to keep-alive
