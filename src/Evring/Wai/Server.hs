{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Evring.Wai.Server
Description : Single-threaded CPS-based WAI server using io_uring
Stability   : experimental

This module provides a single-threaded, continuation-driven HTTP server.
It uses io_uring for all network I/O and buffer pools for zero-allocation
in steady state.

= Architecture

* Single io_uring ring (see 'Evring.Wai.MultiCore' for multi-core)
* Continuation-passing style for request handling
* Buffer pooling for reduced allocation pressure
* No synchronization needed (single-threaded)

= Usage

@
import Evring.Wai.Server (runServer, defaultServerSettings)

main :: IO ()
main = runServer defaultServerSettings myApp
@
-}
module Evring.Wai.Server (
    -- * Running the server
    runServer,

    -- * Configuration
    ServerSettings (..),
    defaultServerSettings,
)
where

import Data.Text qualified as T
import Data.Word (Word32)
import Evring.Wai.Conn
import Evring.Wai.Internal (parseSockAddr)
import Evring.Wai.Loop (CompletionResult (..), Cont (..), Loop, ioAccept, runLoop, withLoop)
import Foreign (Ptr, castPtr, mallocBytes, poke)
import Log qualified
import Network.Socket
import Network.Wai (Application)
import System.Posix.Types (Fd (..))

-- | Server settings
data ServerSettings = ServerSettings
    { serverPort :: !Int
    , serverBacklog :: !Int
    , serverRingSize :: !Int
    , serverMaxConns :: !Int
    , serverLogger :: !Log.Logger
    }

defaultServerSettings :: Log.Logger -> ServerSettings
defaultServerSettings logger =
    ServerSettings
        { serverPort = 8080
        , serverBacklog = 4096
        , serverRingSize = 4096
        , serverMaxConns = 10000
        , serverLogger = logger
        }

-- | Run the server
runServer :: ServerSettings -> Application -> IO ()
runServer ServerSettings{..} app = do
    let lg = Log.withNS serverLogger "evring-cps"
    Log.logInfo lg ("Starting on port " <> T.pack (show serverPort)) ()

    -- Create connection context with buffer pools
    ctx <- newConnContext serverLogger serverMaxConns

    -- Create listen socket
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral serverPort) 0)
    listen sock serverBacklog

    withFdSocket sock $ \listenFd -> do
        withLoop serverLogger serverRingSize $ \loop -> do
            -- Allocate accept buffers (reused)
            addrBuf <- mallocBytes 128
            addrLenBuf <- mallocBytes 4
            poke (castPtr addrLenBuf :: Ptr Word32) 128

            -- Start accept loop
            !_ <- ioAccept
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
        !_ <- ioAccept
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
        !_ <- ioAccept
            loop
            listenFd
            addrBuf
            addrLenBuf
            (acceptCont ctx loop listenFd addrBuf addrLenBuf app)

        -- This continuation is done (accept cont chain continues independently)
        pure Nothing

-- Note: parseSockAddr, byteSwap16, ipv6AnyAddress, ipv6AddressFromWords
-- are imported from Evring.Wai.Internal
