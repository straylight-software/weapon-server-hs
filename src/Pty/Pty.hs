{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | PTY management for sandboxed shell sessions

Each PTY session runs inside a bwrap sandbox with:
- Isolated namespaces (user, pid, mount, net, ipc)
- Copy-on-write filesystem via overlayfs
- Resource limits via cgroups (when available)
- seccomp-bpf syscall filtering

Uses posix-pty for proper terminal emulation with resize support.
-}
module Pty.Pty (
    -- * PTY Manager
    PtyManager,
    newManager,

    -- * PTY Operations
    create,
    get,
    list,
    update,
    remove,
    write,
    resize,

    -- * Connection
    connect,
    PtyConnection (..),

    -- * Sandbox commit
    commitChanges,
    getChangedFiles,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.Posix.Pty (Pty, closePty, readPty, resizePty, spawnWithPty, writePty)
import System.Posix.Signals qualified as Sig
import System.Process (
    ProcessHandle,
    createProcess,
    getPid,
    getProcessExitCode,
    proc,
    terminateProcess,
    waitForProcess,
 )

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Pty.Types
import Sandbox.Sandbox qualified as Sandbox
import Sandbox.Types

-- | PTY Manager - holds all active PTY sessions
data PtyManager = PtyManager
    { pmSessions :: TVar (Map Text RealPtySession)
    , pmCounter :: IORef Word64
    , pmDirectory :: FilePath -- Default working directory
    }

-- | Real PTY session with posix-pty
data RealPtySession = RealPtySession
    { rpsInfo :: PtyInfo
    , rpsPty :: Pty
    , rpsProcess :: ProcessHandle
    , rpsBuffer :: TVar PtyBuffer
    , rpsOverlayDir :: Maybe FilePath
    , rpsSandboxCfg :: Maybe SandboxConfig
    , rpsNetworkProcess :: Maybe ProcessHandle
    }

-- | Create a new PTY manager
newManager :: FilePath -> IO PtyManager
newManager directory = do
    sessions <- newTVarIO Map.empty
    counter <- newIORef 0
    pure
        PtyManager
            { pmSessions = sessions
            , pmCounter = counter
            , pmDirectory = directory
            }

-- | Generate a new PTY ID
nextId :: PtyManager -> IO Text
nextId PtyManager{..} = do
    n <- atomicModifyIORef' pmCounter (\x -> (x + 1, x))
    pure $ "pty_" <> T.pack (show n)

-- | Create a new PTY session
create :: PtyManager -> CreatePtyInput -> IO (Either Text PtyInfo)
create mgr@PtyManager{..} input = do
    ptyId <- nextId mgr

    let sandbox = fromMaybe True (cpiSandbox input)
        cwd = T.unpack $ fromMaybe (T.pack pmDirectory) (cpiCwd input)
        title = fromMaybe ("Terminal " <> T.takeEnd 4 ptyId) (cpiTitle input)
        sessionId = fromMaybe ptyId (cpiSessionId input)
        -- Inject OPENCODE_SESSION_ID for proxy correlation
        baseEnv = fromMaybe [] (cpiEnv input)
        env = ("OPENCODE_SESSION_ID", sessionId) : baseEnv
        network = fromMaybe False (cpiNetwork input)

    if sandbox
        then do
            -- Check if bwrap is available
            bwrapPath <- findExecutable "bwrap"
            case bwrapPath of
                Nothing -> createUnsandboxed mgr ptyId cwd title env input
                Just _ -> do
                    result <- createSandboxed mgr ptyId cwd title env network input
                    case result of
                        Left _ -> createUnsandboxed mgr ptyId cwd title env input
                        Right info -> pure $ Right info
        else createUnsandboxed mgr ptyId cwd title env input

-- | Create a sandboxed PTY using bwrap with real PTY
createSandboxed :: PtyManager -> Text -> FilePath -> Text -> [(Text, Text)] -> Bool -> CreatePtyInput -> IO (Either Text PtyInfo)
createSandboxed PtyManager{..} ptyId cwd title env network input = do
    -- Build sandbox config
    let config =
            (defaultConfig cwd)
                { scNetwork = if network then NetworkSlirp else NetworkNone
                , scEnv = env
                , scMounts = maybe [] (map toMountSpec) (cpiMounts input)
                }
    slirpPath <- if scNetwork config == NetworkSlirp then findExecutable "slirp4netns" else pure (Just "")
    case (scNetwork config, slirpPath) of
        (NetworkSlirp, Nothing) -> pure $ Left "slirp4netns not found"
        _ -> do
            -- Create sandbox directories
            result <- Sandbox.create ptyId config

            case result of
                Left err -> pure $ Left err
                Right (overlayDir, _) -> do
                    -- Build the full bwrap command
                    let bwrapArgs = Sandbox.buildBwrapArgs config
                        _envList = map (bimap T.unpack T.unpack) env ++ defaultEnvList

                    -- Spawn with PTY
                    ptyResult <-
                        try @SomeException $
                            spawnWithPty Nothing True "bwrap" bwrapArgs (80, 24)

                    case ptyResult of
                        Left e -> do
                            void $ try @SomeException $ Sandbox.destroyDir overlayDir
                            pure $ Left $ "Failed to spawn sandbox PTY: " <> T.pack (show e)
                        Right (pty, ph) -> do
                            pid <- getPid ph
                            bufferVar <- newTVarIO emptyBuffer
                            netProcess <- case (scNetwork config, pid) of
                                (NetworkSlirp, Just procPid) -> do
                                    let args = ["--configure", show procPid, "tap0"]
                                    (_, _, _, slirpPh) <- createProcess (proc "slirp4netns" args)
                                    pure (Just slirpPh)
                                _ -> pure Nothing

                            let info =
                                    PtyInfo
                                        { piId = ptyId
                                        , piTitle = title
                                        , piCommand = "bwrap"
                                        , piArgs = map T.pack bwrapArgs
                                        , piCwd = T.pack cwd
                                        , piStatus = PtyRunning
                                        , piPid = maybe 0 fromIntegral pid
                                        , piSandbox = True
                                        }

                            let session =
                                    RealPtySession
                                        { rpsInfo = info
                                        , rpsPty = pty
                                        , rpsProcess = ph
                                        , rpsBuffer = bufferVar
                                        , rpsOverlayDir = Just overlayDir
                                        , rpsSandboxCfg = Just config
                                        , rpsNetworkProcess = netProcess
                                        }

                            atomically $ modifyTVar' pmSessions (Map.insert ptyId session)

                            -- Start reader thread
                            void $ forkIO $ ptyReaderThread session

                            -- Monitor for exit
                            void $ forkIO $ exitMonitor pmSessions ptyId ph (Just overlayDir)

                            pure $ Right info

-- | Create an unsandboxed PTY
createUnsandboxed :: PtyManager -> Text -> FilePath -> Text -> [(Text, Text)] -> CreatePtyInput -> IO (Either Text PtyInfo)
createUnsandboxed PtyManager{..} ptyId cwd title env input = do
    let cmd = T.unpack $ fromMaybe "/bin/sh" (cpiCommand input)
        args = map T.unpack $ fromMaybe ["-l"] (cpiArgs input)
        envList = Just $ map (bimap T.unpack T.unpack) env ++ defaultEnvList

    ptyResult <-
        try @SomeException $
            spawnWithPty envList True cmd args (80, 24)

    case ptyResult of
        Left e -> pure $ Left $ "Failed to spawn PTY: " <> T.pack (show e)
        Right (pty, ph) -> do
            pid <- getPid ph
            bufferVar <- newTVarIO emptyBuffer

            let info =
                    PtyInfo
                        { piId = ptyId
                        , piTitle = title
                        , piCommand = T.pack cmd
                        , piArgs = map T.pack args
                        , piCwd = T.pack cwd
                        , piStatus = PtyRunning
                        , piPid = maybe 0 fromIntegral pid
                        , piSandbox = False
                        }

            let session =
                    RealPtySession
                        { rpsInfo = info
                        , rpsPty = pty
                        , rpsProcess = ph
                        , rpsBuffer = bufferVar
                        , rpsOverlayDir = Nothing
                        , rpsSandboxCfg = Nothing
                        , rpsNetworkProcess = Nothing
                        }

            atomically $ modifyTVar' pmSessions (Map.insert ptyId session)
            void $ forkIO $ ptyReaderThread session
            void $ forkIO $ exitMonitor pmSessions ptyId ph Nothing

            pure $ Right info

-- | Default environment variables
defaultEnvList :: [(String, String)]
defaultEnvList =
    [ ("HOME", "/root")
    , ("USER", "root")
    , ("SHELL", "/bin/sh")
    , -- NixOS-compatible PATH
      ("PATH", "/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/bin")
    , ("TERM", "xterm-256color")
    , ("LANG", "C.UTF-8")
    , ("LC_ALL", "C.UTF-8")
    , -- Route all HTTP traffic through MITM proxy
      ("HTTP_PROXY", "http://127.0.0.1:8888")
    , ("HTTPS_PROXY", "http://127.0.0.1:8888")
    , ("http_proxy", "http://127.0.0.1:8888")
    , ("https_proxy", "http://127.0.0.1:8888")
    ]

-- | Convert mount tuple to MountSpec
toMountSpec :: (Text, Text, Bool) -> MountSpec
toMountSpec (src, dest, ro) = MountSpec (T.unpack src) (T.unpack dest) ro

-- | Reader thread - reads from PTY and fills buffer
ptyReaderThread :: RealPtySession -> IO ()
ptyReaderThread RealPtySession{..} = loop
  where
    loop = do
        result <- try @SomeException $ readPty rpsPty
        case result of
            Left _ -> pure () -- PTY closed
            Right bs | BS.null bs -> do
                threadDelay 10000 -- 10ms
                loop
            Right bs -> do
                atomically $ modifyTVar' rpsBuffer $ \buf ->
                    let newCursor = pbCursor buf + fromIntegral (BS.length bs)
                        newData = BS.take bufferLimit (pbData buf <> bs)
                        excess = max 0 (BS.length newData - bufferLimit)
                        newBufferCursor = pbBufferCursor buf + fromIntegral excess
                     in buf
                            { pbData = newData
                            , pbCursor = newCursor
                            , pbBufferCursor = newBufferCursor
                            }
                loop

-- | Exit monitor thread
exitMonitor :: TVar (Map Text RealPtySession) -> Text -> ProcessHandle -> Maybe FilePath -> IO ()
exitMonitor sessions ptyId ph mOverlayDir = do
    code <- waitForProcess ph
    let status = case code of
            ExitSuccess -> PtyExited 0
            ExitFailure n -> PtyExited n

    atomically $
        modifyTVar' sessions $
            Map.adjust
                (\s -> s{rpsInfo = (rpsInfo s){piStatus = status}})
                ptyId

    mSession <- readTVarIO sessions
    case Map.lookup ptyId mSession of
        Nothing -> pure ()
        Just session -> case rpsNetworkProcess session of
            Nothing -> pure ()
            Just netPh -> do
                terminateProcess netPh
                void $ try @SomeException $ waitForProcess netPh

    -- Cleanup overlay after delay
    case mOverlayDir of
        Nothing -> pure ()
        Just dir -> void $ forkIO $ do
            threadDelay 5000000 -- 5 seconds
            void $ try @SomeException $ Sandbox.destroyDir dir

-- | Helper to run an action on a PTY session, returning a default if not found
withSession :: PtyManager -> Text -> a -> (RealPtySession -> IO a) -> IO a
withSession PtyManager{..} ptyId defaultVal action = do
    sessions <- readTVarIO pmSessions
    case Map.lookup ptyId sessions of
        Nothing -> pure defaultVal
        Just session -> action session

-- | Get a PTY session by ID
get :: PtyManager -> Text -> IO (Maybe PtyInfo)
get mgr ptyId = withSession mgr ptyId Nothing (pure . Just . rpsInfo)

-- | List all PTY sessions
list :: PtyManager -> IO [PtyInfo]
list PtyManager{..} = do
    sessions <- readTVarIO pmSessions
    pure $ map rpsInfo (Map.elems sessions)

-- | Update a PTY session
update :: PtyManager -> Text -> UpdatePtyInput -> IO (Maybe PtyInfo)
update mgr@PtyManager{..} ptyId UpdatePtyInput{..} = do
    -- Handle resize if requested
    case upiSize of
        Just (ResizeInput rows cols) -> void $ resize mgr ptyId cols rows
        Nothing -> pure ()

    -- Update title
    atomically $ do
        sessions <- readTVar pmSessions
        case Map.lookup ptyId sessions of
            Nothing -> pure Nothing
            Just session -> do
                let info' =
                        (rpsInfo session)
                            { piTitle = fromMaybe (piTitle (rpsInfo session)) upiTitle
                            }
                let session' = session{rpsInfo = info'}
                writeTVar pmSessions (Map.insert ptyId session' sessions)
                pure $ Just info'

-- | Remove a PTY session
remove :: PtyManager -> Text -> IO Bool
remove PtyManager{..} ptyId = do
    mSession <- atomically $ do
        sessions <- readTVar pmSessions
        case Map.lookup ptyId sessions of
            Nothing -> pure Nothing
            Just s -> do
                writeTVar pmSessions (Map.delete ptyId sessions)
                pure (Just s)

    case mSession of
        Nothing -> pure False
        Just session -> do
            -- Close the PTY first to signal EOF to the process
            void $ try @SomeException $ closePty (rpsPty session)
            -- Terminate the process
            terminateProcess (rpsProcess session)
            case rpsNetworkProcess session of
                Nothing -> pure ()
                Just netPh -> do
                    terminateProcess netPh
                    void $ try @SomeException $ waitForProcess netPh
            -- Wait briefly for process to exit (poll a few times)
            waitForExit 5 (rpsProcess session)

            case rpsOverlayDir session of
                Nothing -> pure ()
                Just dir -> void $ try @SomeException $ Sandbox.destroyDir dir

            pure True
  where
    -- Poll for process exit with limited attempts, then SIGKILL
    waitForExit :: Int -> ProcessHandle -> IO ()
    waitForExit 0 ph = do
        -- Process didn't exit with SIGTERM, send SIGKILL
        mpid <- getPid ph
        case mpid of
            Nothing -> pure ()
            Just pid -> void $ try @SomeException $ Sig.signalProcess Sig.sigKILL pid
    waitForExit n ph = do
        code <- getProcessExitCode ph
        case code of
            Just _ -> pure ()
            Nothing -> do
                threadDelay 10000 -- 10ms
                waitForExit (n - 1) ph

-- | Write data to a PTY
write :: PtyManager -> Text -> ByteString -> IO Bool
write mgr ptyId bs = withSession mgr ptyId False $ \session -> do
    result <- try @SomeException $ writePty (rpsPty session) bs
    pure $ either (const False) (const True) result

-- | Resize a PTY (sends SIGWINCH via ioctl TIOCSWINSZ)
resize :: PtyManager -> Text -> Int -> Int -> IO Bool
resize mgr ptyId cols rows = withSession mgr ptyId False $ \session -> do
    result <- try @SomeException $ resizePty (rpsPty session) (cols, rows)
    pure $ either (const False) (const True) result

-- | PTY connection for WebSocket bridging
data PtyConnection = PtyConnection
    { pcSend :: ByteString -> IO ()
    , pcOnData :: (ByteString -> IO ()) -> IO ()
    , pcClose :: IO ()
    }

-- | Connect to a PTY session (for WebSocket bridging)
connect :: PtyManager -> Text -> Maybe Word64 -> IO (Maybe PtyConnection)
connect mgr ptyId cursor = withSession mgr ptyId Nothing $ \session -> do
    buf <- readTVarIO (rpsBuffer session)

    let replayFrom = fromMaybe 0 cursor
        replayData =
            if replayFrom >= pbCursor buf
                then BS.empty
                else
                    let offset = max 0 (fromIntegral $ replayFrom - pbBufferCursor buf)
                     in BS.drop offset (pbData buf)

    lastCursorRef <- newIORef (pbCursor buf)
    runningRef <- newIORef True

    pure $
        Just
            PtyConnection
                { pcSend = void . try @SomeException . writePty (rpsPty session)
                , pcOnData = \handler -> do
                    unless (BS.null replayData) $ handler replayData

                    void $ forkIO $ do
                        let pollLoop = do
                                running <- readIORef runningRef
                                when running $ do
                                    currentBuf <- readTVarIO (rpsBuffer session)
                                    lastCursor <- readIORef lastCursorRef

                                    when (pbCursor currentBuf > lastCursor) $ do
                                        let start = pbBufferCursor currentBuf
                                            offset = max 0 (fromIntegral $ lastCursor - start)
                                            newData = BS.drop offset (pbData currentBuf)
                                        unless (BS.null newData) $ handler newData
                                        writeIORef lastCursorRef (pbCursor currentBuf)

                                    threadDelay 10000
                                    pollLoop
                        pollLoop
                , pcClose = writeIORef runningRef False
                }

{- | Commit sandbox changes to real filesystem
Copies modified files from sandbox overlay to the workdir
-}
commitChanges :: PtyManager -> Text -> IO (Either Text ())
commitChanges mgr ptyId = withSession mgr ptyId (Left "PTY not found") $ \session ->
    case (rpsOverlayDir session, rpsSandboxCfg session) of
        (Nothing, _) -> pure $ Left "PTY is not sandboxed"
        (_, Nothing) -> pure $ Left "PTY has no sandbox config"
        (Just overlayDir, Just cfg) -> do
            let workdir = scWorkdir cfg
            Sandbox.commit overlayDir workdir

-- | Get list of changed files in sandbox
getChangedFiles :: PtyManager -> Text -> IO (Either Text [FilePath])
getChangedFiles mgr ptyId = withSession mgr ptyId (Left "PTY not found") $ \session ->
    case rpsOverlayDir session of
        Nothing -> pure $ Left "PTY is not sandboxed"
        Just overlayDir -> do
            files <- Sandbox.getChanges overlayDir
            pure $ Right files
