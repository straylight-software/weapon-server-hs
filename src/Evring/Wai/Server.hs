{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | CPS-based WAI server using io_uring.
--
-- Single-threaded, no synchronization, continuation-driven.
-- Optimized with buffer pools for zero-allocation steady state.
module Evring.Wai.Server
  ( runServer,
    ServerSettings (..),
    defaultServerSettings,
  )
where

import Data.Bits (shiftL, shiftR, (.|.))
import Data.Word (Word16, Word32)
import Evring.Wai.Conn
import Evring.Wai.Loop (CompletionResult (..), Cont (..), Loop, ioAccept, runLoop, withLoop)
import Foreign (Ptr, castPtr, mallocBytes, peekByteOff, poke)
import Network.Socket
import Network.Wai (Application)
import System.Posix.Types (Fd (..))

-- | Server settings
data ServerSettings = ServerSettings
  { serverPort :: !Int,
    serverBacklog :: !Int,
    serverRingSize :: !Int,
    serverMaxConns :: !Int
  }

defaultServerSettings :: ServerSettings
defaultServerSettings =
  ServerSettings
    { serverPort = 8080,
      serverBacklog = 4096,
      serverRingSize = 4096,
      serverMaxConns = 10000
    }

-- | Run the server
runServer :: ServerSettings -> Application -> IO ()
runServer ServerSettings {..} app = do
  putStrLn $ "evring-wai (CPS): Starting on port " ++ show serverPort

  -- Create connection context with buffer pools
  ctx <- newConnContext serverMaxConns

  -- Create listen socket
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral serverPort) 0)
  listen sock serverBacklog

  withFdSocket sock $ \listenFd -> do
    withLoop serverRingSize $ \loop -> do
      -- Allocate accept buffers (reused)
      addrBuf <- mallocBytes 128
      addrLenBuf <- mallocBytes 4
      poke (castPtr addrLenBuf :: Ptr Word32) 128

      -- Start accept loop
      ioAccept
        loop
        (Fd listenFd)
        addrBuf
        addrLenBuf
        (acceptCont ctx loop (Fd listenFd) addrBuf addrLenBuf app)

      -- Run event loop
      runLoop loop

  close sock

-- | Accept continuation - handles new connections
acceptCont :: ConnContext -> Loop -> Fd -> Ptr () -> Ptr () -> Application -> Cont
acceptCont ctx loop listenFd addrBuf addrLenBuf app = Cont $ \case
  Failure _errno -> do
    -- Accept failed, try again
    ioAccept
      loop
      listenFd
      addrBuf
      addrLenBuf
      (acceptCont ctx loop listenFd addrBuf addrLenBuf app)
    pure Nothing
  Success clientFdInt -> do
    let clientFd = Fd (fromIntegral clientFdInt)

    -- Parse client address
    clientAddr <- parseSockAddr addrBuf

    -- Reset addrlen for next accept
    poke (castPtr addrLenBuf :: Ptr Word32) 128

    -- Start connection handler (spawns its own continuation chain)
    startConnection ctx loop clientFd clientAddr app

    -- Immediately submit next accept (this is the key - we don't block!)
    ioAccept
      loop
      listenFd
      addrBuf
      addrLenBuf
      (acceptCont ctx loop listenFd addrBuf addrLenBuf app)

    -- This continuation is done (accept cont chain continues independently)
    pure Nothing

-- | Parse sockaddr from accept buffer
parseSockAddr :: Ptr () -> IO SockAddr
parseSockAddr addrBuf = do
  family <- peekByteOff addrBuf 0 :: IO Word16
  case family of
    2 -> do
      -- AF_INET
      port <- peekByteOff addrBuf 2 :: IO Word16
      addr <- peekByteOff addrBuf 4 :: IO Word32
      let portNum = fromIntegral (byteSwap16 port)
      pure $ SockAddrInet portNum addr
    10 -> do
      -- AF_INET6
      port <- peekByteOff addrBuf 2 :: IO Word16
      let portNum = fromIntegral (byteSwap16 port)
      pure $ SockAddrInet6 portNum 0 (0, 0, 0, 0) 0
    _ -> pure $ SockAddrInet 0 0

byteSwap16 :: Word16 -> Word16
byteSwap16 w = (w `shiftR` 8) .|. (w `shiftL` 8)
