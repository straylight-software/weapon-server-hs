{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Multi-core evring-wai: one io_uring ring per core.
--
-- Each core runs its own event loop with its own ring.
-- SO_REUSEPORT lets kernel load-balance accepts across all listeners.
-- No cross-core communication, no locks, linear scaling.
module Evring.Wai.MultiCore
  ( runServerMultiCore,
    ServerSettings (..),
    defaultServerSettings,
  )
where

import Control.Concurrent (forkOn, getNumCapabilities, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Word (Word16, Word32)
import Evring.Wai.Conn
import Evring.Wai.Loop
import Foreign (Ptr, castPtr, mallocBytes, peekByteOff, poke)
import GHC.Conc (setNumCapabilities)
import Network.Socket
import Network.Wai (Application)
import System.Posix.Types (Fd (..))

-- | Server settings
data ServerSettings = ServerSettings
  { serverPort :: !Int,
    serverBacklog :: !Int,
    serverRingSize :: !Int,
    serverMaxConns :: !Int, -- per core
    serverCores :: !(Maybe Int) -- Nothing = use all capabilities
  }

defaultServerSettings :: ServerSettings
defaultServerSettings =
  ServerSettings
    { serverPort = 8080,
      serverBacklog = 4096,
      serverRingSize = 4096,
      serverMaxConns = 4096,
      serverCores = Nothing
    }

-- | Run server with one event loop per core
runServerMultiCore :: ServerSettings -> Application -> IO ()
runServerMultiCore settings@ServerSettings {..} app = do
  -- Determine core count
  numCores <- case serverCores of
    Just n -> setNumCapabilities n >> pure n
    Nothing -> getNumCapabilities

  putStrLn $ "evring-wai (multi-core): Starting on port " ++ show serverPort
  putStrLn $ "  Cores: " ++ show numCores
  putStrLn $ "  Ring size: " ++ show serverRingSize
  putStrLn ""

  -- Create one socket per core with SO_REUSEPORT
  -- Kernel will load-balance incoming connections
  sockets <- replicateM numCores (createListenSocket settings)

  -- Barrier for all workers
  dones <- replicateM numCores newEmptyMVar

  -- Fork a worker on each capability
  forM_ (zip3 [0 ..] sockets dones) $ \(coreId, sock, done) -> do
    forkOn coreId $ do
      runWorker settings app coreId sock
      putMVar done ()

  -- Wait for all workers (they run forever unless killed)
  mapM_ takeMVar dones

  -- Cleanup
  mapM_ close sockets

-- | Create a listening socket with SO_REUSEPORT
createListenSocket :: ServerSettings -> IO Socket
createListenSocket ServerSettings {..} = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  setSocketOption sock ReusePort 1 -- Key: allows multiple listeners on same port
  bind sock (SockAddrInet (fromIntegral serverPort) 0)
  listen sock serverBacklog
  pure sock

-- | Run a single worker on a specific core
runWorker :: ServerSettings -> Application -> Int -> Socket -> IO ()
runWorker ServerSettings {..} app _coreId sock = do
  -- Each worker gets its own buffer pools
  ctx <- newConnContext serverMaxConns

  withFdSocket sock $ \listenFd -> do
    withLoop serverRingSize $ \loop -> do
      -- Allocate accept buffers (reused within this worker)
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

      -- Run this core's event loop (forever)
      runLoop loop

-- | Accept continuation
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

    -- Start connection handler
    startConnection ctx loop clientFd clientAddr app

    -- Immediately submit next accept
    ioAccept
      loop
      listenFd
      addrBuf
      addrLenBuf
      (acceptCont ctx loop listenFd addrBuf addrLenBuf app)

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
      pure $ SockAddrInet (fromIntegral (byteSwap16 port)) addr
    10 -> do
      -- AF_INET6
      port <- peekByteOff addrBuf 2 :: IO Word16
      pure $ SockAddrInet6 (fromIntegral (byteSwap16 port)) 0 (0, 0, 0, 0) 0
    _ -> pure $ SockAddrInet 0 0

byteSwap16 :: Word16 -> Word16
byteSwap16 w = (w `shiftR` 8) .|. (w `shiftL` 8)
