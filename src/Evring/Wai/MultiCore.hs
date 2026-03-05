{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Evring.Wai.MultiCore
Description : Multi-core evring-wai server with per-core io_uring rings
Stability   : experimental

This module provides a multi-core HTTP server where each core runs its
own io_uring event loop. The kernel load-balances incoming connections
across all listeners via @SO_REUSEPORT@.

= Architecture

* One io_uring ring per core
* Each core has its own accept loop and buffer pools
* No cross-core communication or locks needed
* Linear scaling with core count

= Usage

@
import Evring.Wai.MultiCore (runServerMultiCore, defaultServerSettings)

main :: IO ()
main = runServerMultiCore defaultServerSettings myApp
@
-}
module Evring.Wai.MultiCore (
    -- * Running the server
    runServerMultiCore,
    runServerMultiCoreWithCleanup,

    -- * Configuration
    ServerSettings (..),
    defaultServerSettings,
)
where

import Control.Concurrent (forkOn, getNumCapabilities, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad (forM_, replicateM, unless)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Word (Word32)
import Evring.Wai.Conn
import Evring.Wai.Internal (parseSockAddr)
import Evring.Wai.Loop
import Foreign (Ptr, castPtr, mallocBytes, poke)
import GHC.Conc (setNumCapabilities)
import Log qualified
import Network.Socket
import Network.Wai (Application)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import System.Posix.Types (Fd (..))

-- | Server settings
data ServerSettings = ServerSettings
    { serverPort :: !Int
    , serverPortRetry :: !Int
    -- ^ Number of additional ports to try if serverPort is busy (0 = no retry)
    , serverBacklog :: !Int
    , serverRingSize :: !Int
    , serverMaxConns :: !Int -- per core
    , serverCores :: !(Maybe Int) -- Nothing = use all capabilities
    , serverLogger :: !Log.Logger
    }

defaultServerSettings :: Log.Logger -> ServerSettings
defaultServerSettings logger =
    ServerSettings
        { serverPort = 8080
        , serverPortRetry = 10
        , serverBacklog = 4096
        , serverRingSize = 4096
        , serverMaxConns = 4096
        , serverCores = Nothing
        , serverLogger = logger
        }

{- | Run server with one event loop per core

If the requested port is busy, will retry on subsequent ports up to
@serverPortRetry@ times.
-}
runServerMultiCore :: ServerSettings -> Application -> IO ()
runServerMultiCore settings app = runServerMultiCoreWithCleanup settings app (pure ())

{- | Run server with one event loop per core, with cleanup action on shutdown.

The cleanup action is guaranteed to run when SIGTERM or SIGINT is received,
before the process exits. This is useful for flushing telemetry, closing
database connections, etc.
-}
runServerMultiCoreWithCleanup :: ServerSettings -> Application -> IO () -> IO ()
runServerMultiCoreWithCleanup settings@ServerSettings{..} app cleanup = do
    let lg = Log.withNS serverLogger "evring"

    -- Determine core count
    numCores <- case serverCores of
        Just n -> setNumCapabilities n >> pure n
        Nothing -> getNumCapabilities

    -- Try to bind to port with retry
    (actualPort, sockets) <- bindWithRetry lg settings numCores serverPort serverPortRetry

    Log.logInfo lg ("Starting on port " <> T.pack (show actualPort)) ()
    Log.logDebug lg ("Cores: " <> T.pack (show numCores) <> ", Ring size: " <> T.pack (show serverRingSize)) ()

    -- Shutdown flag (also prevents multiple signal handlers from running)
    shutdownRef <- newIORef False

    -- Install signal handlers for graceful shutdown
    let shutdownHandler = do
            alreadyShuttingDown <- atomicModifyIORef' shutdownRef (True,)
            unless alreadyShuttingDown $ do
                Log.logInfo lg "Received shutdown signal, running cleanup..." ()
                cleanup
                Log.logInfo lg "Cleanup complete, exiting" ()
                -- Close sockets to unblock workers
                mapM_ close sockets

    _ <- installHandler sigTERM (Catch shutdownHandler) Nothing
    _ <- installHandler sigINT (Catch shutdownHandler) Nothing

    -- Barrier for all workers
    dones <- replicateM numCores newEmptyMVar

    -- Fork a worker on each capability
    forM_ (zip3 [0 ..] sockets dones) $ \(coreId, sock, done) -> do
        forkOn coreId $ do
            result <- try $ runWorker settings app coreId sock
            case result of
                Right () -> pure ()
                Left (e :: SomeException) ->
                    Log.logError lg ("Worker " <> T.pack (show coreId) <> " failed: " <> T.pack (show e)) ()
            putMVar done ()

    -- Wait for all workers (they run forever unless killed)
    mapM_ takeMVar dones

    -- Cleanup sockets (may already be closed by signal handler)
    alreadyShutdown <- readIORef shutdownRef
    unless alreadyShutdown $ mapM_ close sockets

-- | Try to bind to a port, retrying on subsequent ports if busy
bindWithRetry :: Log.Logger -> ServerSettings -> Int -> Int -> Int -> IO (Int, [Socket])
bindWithRetry lg settings numCores port retriesLeft = do
    -- First check if something is already listening on this port
    inUse <- isPortInUse port
    if inUse
        then
            if retriesLeft > 0
                then do
                    Log.logInfo lg ("Port " <> T.pack (show port) <> " is busy, trying " <> T.pack (show (port + 1))) ()
                    bindWithRetry lg settings numCores (port + 1) (retriesLeft - 1)
                else ioError $ userError $ "Port " ++ show port ++ " is in use (use --port-retry N to try additional ports)"
        else do
            -- Port is free, try to bind
            result <- tryBindPort settings numCores port
            case result of
                Right sockets -> pure (port, sockets)
                Left err
                    | retriesLeft > 0 -> do
                        Log.logInfo lg ("Port " <> T.pack (show port) <> " bind failed, trying " <> T.pack (show (port + 1))) ()
                        bindWithRetry lg settings numCores (port + 1) (retriesLeft - 1)
                    | otherwise -> ioError err

{- | Check if a port is already in use by attempting a connection
Returns True if something is listening on the port
-}
isPortInUse :: Int -> IO Bool
isPortInUse port =
    catch
        ( do
            sock <- socket AF_INET Stream 0
            -- Try to connect to localhost:port
            connect sock (SockAddrInet (fromIntegral port) 0x0100007f) -- 127.0.0.1
            close sock
            pure True -- Connection succeeded, something is listening
        )
        (\(_ :: IOException) -> pure False) -- Connection failed, port is free

{- | Try to create all listening sockets for a given port
Returns Left on bind failure, Right with sockets on success

Uses a difference list to avoid 'reverse' on the accumulated sockets.
-}
tryBindPort :: ServerSettings -> Int -> Int -> IO (Either IOException [Socket])
tryBindPort settings numCores port = do
    let settingsWithPort = settings{serverPort = port}
    -- Use difference list to build in order without reverse
    result <- go numCores settingsWithPort id
    pure $ fmap (\dl -> dl []) result
  where
    go 0 _ acc = pure (Right acc)
    go n s acc =
        catch
            ( do
                sock <- createListenSocket s
                go (n - 1) s (acc . (sock :))
            )
            ( \e -> do
                -- Close any sockets we already opened
                mapM_ close (acc [])
                pure (Left (e :: IOException))
            )

-- | Create a listening socket with SO_REUSEPORT
createListenSocket :: ServerSettings -> IO Socket
createListenSocket ServerSettings{..} = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1 -- Key: allows multiple listeners on same port
    bind sock (SockAddrInet (fromIntegral serverPort) 0)
    listen sock serverBacklog
    pure sock

-- | Run a single worker on a specific core
runWorker :: ServerSettings -> Application -> Int -> Socket -> IO ()
runWorker ServerSettings{..} app _coreId sock = do
    -- Each worker gets its own buffer pools
    ctx <- newConnContext serverLogger serverMaxConns

    withFdSocket sock $ \listenFd -> do
        withLoop serverLogger serverRingSize $ \loop -> do
            -- Allocate accept buffers (reused within this worker)
            addrBuf <- mallocBytes 128
            addrLenBuf <- mallocBytes 4
            poke (castPtr addrLenBuf :: Ptr Word32) 128

            -- Start accept loop
            !_ <-
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
        !_ <-
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
        !_ <-
            ioAccept
                loop
                listenFd
                addrBuf
                addrLenBuf
                (acceptCont ctx loop listenFd addrBuf addrLenBuf app)

        pure Nothing

-- Note: parseSockAddr, byteSwap16, ipv6AnyAddress, ipv6AddressFromWords
-- are imported from Evring.Wai.Internal
