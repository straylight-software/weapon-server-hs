{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Pty.Pty
Description : PTY management for sandboxed shell sessions

This module provides comprehensive PTY (pseudo-terminal) management for
sandboxed shell sessions. Each PTY session can optionally run inside a
bwrap sandbox with:

* Isolated namespaces (user, pid, mount, net, ipc)
* Copy-on-write filesystem via overlayfs
* Resource limits via cgroups (when available)
* seccomp-bpf syscall filtering

Uses posix-pty for proper terminal emulation with resize support.

== Architecture

The module is organized around a 'PtyManager' that maintains a collection
of active PTY sessions. Each session can be either:

* __Sandboxed__: Running inside a bwrap container with filesystem isolation
* __Unsandboxed__: Running directly on the host system

== Thread Model

Each PTY session spawns two background threads:

1. __Reader thread__: Continuously reads PTY output into a circular buffer
2. __Exit monitor__: Waits for process termination and updates status

@since 0.1.0
-}
module Pty.Pty (
    -- * PTY Manager
    PtyManager,
    newManager,

    -- * Session Lifecycle
    create,
    remove,

    -- * Session Access
    get,
    list,
    update,

    -- * PTY I/O
    write,
    resize,

    -- * Connection
    connect,
    PtyConnection (..),

    -- * Sandbox Operations
    commitChanges,
    getChangedFiles,
) where

import Control.Concurrent (threadDelay)
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
import System.Environment (lookupEnv)

import Log qualified
import Util.Thread (forkLogged)

import System.Posix.Pty (Pty, closePty, readPty, resizePty, spawnWithPty, writePty)
import System.Posix.Signals qualified as Sig
import System.Posix.Types (CPid)
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

import Pty.Internal (CreateParams (..), exitCodeToStatus, resolveCreateParams, toMountSpec)
import Pty.Types
import Sandbox.Sandbox qualified as Sandbox
import Sandbox.Types

-- ============================================================================
-- PTY Manager
-- ============================================================================

{- | PTY Manager - holds all active PTY sessions.

The manager maintains a thread-safe map of sessions and provides
a counter for generating unique session IDs.

@since 0.1.0
-}
data PtyManager = PtyManager
    { pmSessions :: TVar (Map Text RealPtySession)
    -- ^ Active sessions indexed by ID
    , pmCounter :: IORef Word64
    -- ^ Counter for generating unique IDs
    , pmDirectory :: FilePath
    -- ^ Default working directory for new sessions
    , pmLogger :: Log.Logger
    -- ^ Logger for supervised thread spawning
    }

{- | Internal representation of a PTY session.

This contains the actual POSIX PTY handle, process handle,
output buffer, and optional sandbox configuration.
-}
data RealPtySession = RealPtySession
    { rpsInfo :: PtyInfo
    -- ^ Public session information
    , rpsPty :: Pty
    -- ^ POSIX PTY handle
    , rpsProcess :: ProcessHandle
    -- ^ Process handle for the shell
    , rpsBuffer :: TVar PtyBuffer
    -- ^ Circular output buffer
    , rpsOverlayDir :: Maybe FilePath
    -- ^ Overlay directory for sandboxed sessions
    , rpsSandboxCfg :: Maybe SandboxConfig
    -- ^ Sandbox configuration (if sandboxed)
    , rpsNetworkProcess :: Maybe ProcessHandle
    -- ^ slirp4netns process for network support
    }

-- ============================================================================
-- Manager Operations
-- ============================================================================

{- | Create a new PTY manager.

The manager will use the given directory as the default working
directory for new PTY sessions.

@since 0.1.0
-}
newManager :: Log.Logger -> FilePath -> IO PtyManager
newManager logger directory = do
    sessions <- newTVarIO Map.empty
    counter <- newIORef 0
    pure
        PtyManager
            { pmSessions = sessions
            , pmCounter = counter
            , pmDirectory = directory
            , pmLogger = Log.withNS logger "pty"
            }

{- | Generate a new unique PTY ID.

IDs are of the form @pty_N@ where N is a monotonically increasing number.
-}
nextId :: PtyManager -> IO Text
nextId PtyManager{..} = do
    n <- atomicModifyIORef' pmCounter (\x -> (x + 1, x))
    pure $ "pty_" <> T.pack (show n)

-- ============================================================================
-- Session Creation
-- ============================================================================

{- | Create a new PTY session.

Attempts to create a sandboxed session if:

1. The @sandbox@ field in input is 'True' (default)
2. @bwrap@ is available on the system

Falls back to an unsandboxed session if sandboxing fails or is disabled.

Returns 'Left' with an error message on failure, or 'Right' with the
session info on success.

@since 0.1.0
-}
create :: PtyManager -> CreatePtyInput -> IO (Either Text PtyInfo)
create mgr@PtyManager{..} input = do
    ptyId <- nextId mgr
    let params = resolveCreateParams pmDirectory ptyId input

    if cpSandbox params
        then do
            bwrapPath <- findExecutable "bwrap"
            case bwrapPath of
                Nothing -> createUnsandboxed mgr ptyId params input
                Just _bwrapExe -> do
                    result <- createSandboxed mgr ptyId params input
                    case result of
                        Left _err -> createUnsandboxed mgr ptyId params input
                        Right info -> pure $ Right info
        else createUnsandboxed mgr ptyId params input

-- ============================================================================
-- Sandboxed Session Creation
-- ============================================================================

{- | Create a sandboxed PTY using bwrap with a real PTY.

Sets up:

* Overlay filesystem for copy-on-write
* Network namespace (optionally with slirp4netns)
* Environment variables including proxy settings
* Reader and exit monitor threads
-}
createSandboxed ::
    PtyManager ->
    Text ->
    CreateParams ->
    CreatePtyInput ->
    IO (Either Text PtyInfo)
createSandboxed PtyManager{..} ptyId params input = do
    -- Get user environment for sandbox
    preferredShell <- getPreferredShell
    home <- fromMaybe "/tmp" <$> lookupEnv "HOME"
    user <- fromMaybe "nobody" <$> lookupEnv "USER"

    -- Build sandbox config with actual user values
    let config = buildSandboxConfig params input preferredShell home user

    -- Check slirp4netns availability if needed
    slirpPath <-
        if scNetwork config == NetworkSlirp
            then findExecutable "slirp4netns"
            else pure (Just "")

    case (scNetwork config, slirpPath) of
        (NetworkSlirp, Nothing) -> pure $ Left "slirp4netns not found"
        _otherNetwork -> do
            -- Create sandbox directories
            result <- Sandbox.create ptyId config

            case result of
                Left err -> pure $ Left err
                Right (overlayDir, _) ->
                    spawnSandboxedPty pmLogger pmSessions ptyId params config overlayDir

{- | Build sandbox configuration from resolved parameters.

This is a pure function that constructs the 'SandboxConfig' from
the input parameters and environment values.
-}
buildSandboxConfig ::
    CreateParams ->
    CreatePtyInput ->
    FilePath ->
    FilePath ->
    String ->
    SandboxConfig
buildSandboxConfig CreateParams{..} input shell home user =
    (defaultConfig cpCwd)
        { scNetwork = if cpNetwork then NetworkSlirp else NetworkNone
        , scEnv = cpEnv
        , scMounts = maybe [] (map toMountSpec) (cpiMounts input)
        , scShell = shell
        , scHome = home
        , scUser = T.pack user
        }

{- | Spawn a PTY inside a sandbox.

Creates the bwrap process, sets up the slirp4netns network if needed,
and registers the session with the manager.
-}
spawnSandboxedPty ::
    Log.Logger ->
    TVar (Map Text RealPtySession) ->
    Text ->
    CreateParams ->
    SandboxConfig ->
    FilePath ->
    IO (Either Text PtyInfo)
spawnSandboxedPty logger sessions ptyId params config overlayDir = do
    let bwrapArgs = Sandbox.buildBwrapArgs config

    ptyResult <-
        try @SomeException $
            spawnWithPty Nothing True "bwrap" bwrapArgs (80, 24)

    case ptyResult of
        Left e -> do
            void $ tryIO $ Sandbox.destroyDir overlayDir
            pure $ Left $ "Failed to spawn sandbox PTY: " <> T.pack (show e)
        Right (pty, ph) -> do
            pid <- getPid ph
            bufferVar <- newTVarIO emptyBuffer

            netProcess <- case (scNetwork config, pid) of
                (NetworkSlirp, Just procPid) -> do
                    let args = ["--configure", show procPid, "tap0"]
                    (_, _, _, slirpPh) <- createProcess (proc "slirp4netns" args)
                    pure (Just slirpPh)
                _otherNetwork -> pure Nothing

            let info = buildPtyInfo ptyId params "bwrap" (map T.pack bwrapArgs) pid True

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

            atomically $ modifyTVar' sessions (Map.insert ptyId session)

            -- Start reader thread (supervised)
            void $ forkLogged logger ("pty-reader-" <> ptyId) $ ptyReaderThread session

            -- Monitor for exit (supervised)
            void $ forkLogged logger ("pty-exit-monitor-" <> ptyId) $ exitMonitor logger sessions ptyId ph (Just overlayDir)

            pure $ Right info

-- ============================================================================
-- Unsandboxed Session Creation
-- ============================================================================

{- | Create an unsandboxed PTY running directly on the host.

This is used when sandboxing is disabled or unavailable.
-}
createUnsandboxed ::
    PtyManager ->
    Text ->
    CreateParams ->
    CreatePtyInput ->
    IO (Either Text PtyInfo)
createUnsandboxed PtyManager{..} ptyId params input = do
    -- Get preferred shell from $SHELL or fall back
    preferredShell <- getPreferredShell
    let cmd = T.unpack $ fromMaybe (T.pack preferredShell) (cpiCommand input)
        args = map T.unpack $ fromMaybe ["-l"] (cpiArgs input)

    -- Build environment with actual user values
    defaultEnv <- getDefaultEnvList
    let envList = Just $ map (bimap T.unpack T.unpack) (cpEnv params) ++ defaultEnv

    ptyResult <-
        try @SomeException $
            spawnWithPty envList True cmd args (80, 24)

    case ptyResult of
        Left e -> pure $ Left $ "Failed to spawn PTY: " <> T.pack (show e)
        Right (pty, ph) -> do
            pid <- getPid ph
            bufferVar <- newTVarIO emptyBuffer

            let info = buildPtyInfo ptyId params (T.pack cmd) (map T.pack args) pid False

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

            -- Start reader thread (supervised)
            void $ forkLogged pmLogger ("pty-reader-" <> ptyId) $ ptyReaderThread session

            -- Monitor for exit (supervised)
            void $ forkLogged pmLogger ("pty-exit-monitor-" <> ptyId) $ exitMonitor pmLogger pmSessions ptyId ph Nothing

            pure $ Right info

-- ============================================================================
-- Pure Helpers for PtyInfo Construction
-- ============================================================================

{- | Build a 'PtyInfo' record from session parameters.

This is a pure function that constructs the public session info.
-}
buildPtyInfo :: Text -> CreateParams -> Text -> [Text] -> Maybe CPid -> Bool -> PtyInfo
buildPtyInfo ptyId CreateParams{..} cmd args pid sandbox =
    PtyInfo
        { piId = ptyId
        , piTitle = cpTitle
        , piCommand = cmd
        , piArgs = args
        , piCwd = T.pack cpCwd
        , piStatus = PtyRunning
        , piPid = maybe 0 fromIntegral pid
        , piSandbox = sandbox
        }

-- ============================================================================
-- Environment Helpers
-- ============================================================================

{- | Get the preferred shell from @$SHELL@ or fall back to bash or @/bin/sh@.

Checks in order:

1. @$SHELL@ environment variable (if non-empty)
2. @bash@ in PATH
3. @/bin/sh@

@since 0.1.0
-}
getPreferredShell :: IO String
getPreferredShell = do
    mShell <- lookupEnv "SHELL"
    case mShell of
        Just shell | not (null shell) -> pure shell
        Just _emptyShell -> findBashOrSh
        Nothing -> findBashOrSh
  where
    findBashOrSh = do
        mBash <- findExecutable "bash"
        pure $ fromMaybe "/bin/sh" mBash

{- | Get default environment variables with actual user values.

Includes:

* User info (HOME, USER, SHELL)
* Terminal settings (TERM, LANG, LC_ALL)
* PATH (inherited or NixOS-compatible default)
* HTTP proxy settings for MITM proxy

@since 0.1.0
-}
getDefaultEnvList :: IO [(String, String)]
getDefaultEnvList = do
    home <- fromMaybe "/tmp" <$> lookupEnv "HOME"
    user <- fromMaybe "nobody" <$> lookupEnv "USER"
    shell <- getPreferredShell
    path <- fromMaybe defaultPath <$> lookupEnv "PATH"
    lang <- fromMaybe "C.UTF-8" <$> lookupEnv "LANG"
    lcAll <- fromMaybe lang <$> lookupEnv "LC_ALL"
    pure
        [ ("HOME", home)
        , ("USER", user)
        , ("SHELL", shell)
        , ("PATH", path)
        , ("TERM", "xterm-256color")
        , ("LANG", lang)
        , ("LC_ALL", lcAll)
        , ("HTTP_PROXY", proxyUrl)
        , ("HTTPS_PROXY", proxyUrl)
        , ("http_proxy", proxyUrl)
        , ("https_proxy", proxyUrl)
        ]
  where
    defaultPath = "/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/bin"
    proxyUrl = "http://127.0.0.1:8888"

-- ============================================================================
-- Background Threads
-- ============================================================================

{- | Reader thread that continuously reads from the PTY and fills the buffer.

Runs until the PTY is closed or an error occurs.
-}
ptyReaderThread :: RealPtySession -> IO ()
ptyReaderThread RealPtySession{..} = loop
  where
    loop = do
        result <- tryIO $ readPty rpsPty
        case result of
            Left _err -> pure () -- PTY closed
            Right bs | BS.null bs -> do
                threadDelay 10000 -- 10ms
                loop
            Right bs -> do
                atomically $ modifyTVar' rpsBuffer (appendToBuffer bs)
                loop

{- | Exit monitor thread that watches for process termination.

Updates the session status when the process exits and cleans up
resources after a delay.
-}
exitMonitor ::
    Log.Logger ->
    TVar (Map Text RealPtySession) ->
    Text ->
    ProcessHandle ->
    Maybe FilePath ->
    IO ()
exitMonitor logger sessions ptyId ph mOverlayDir = do
    code <- waitForProcess ph
    let status = exitCodeToStatus code

    atomically $
        modifyTVar' sessions $
            Map.adjust
                (\s -> s{rpsInfo = (rpsInfo s){piStatus = status}})
                ptyId

    -- Clean up network process if present
    mSession <- readTVarIO sessions
    case Map.lookup ptyId mSession of
        Nothing -> pure ()
        Just session -> case rpsNetworkProcess session of
            Nothing -> pure ()
            Just netPh -> do
                terminateProcess netPh
                void $ tryIO $ waitForProcess netPh

    -- Cleanup overlay after delay (supervised - failures should be visible)
    case mOverlayDir of
        Nothing -> pure ()
        Just dir -> void $ forkLogged logger ("pty-cleanup-" <> ptyId) $ do
            threadDelay 5000000 -- 5 seconds
            void $ tryIO $ Sandbox.destroyDir dir

-- ============================================================================
-- Session Access
-- ============================================================================

{- | Helper to run an action on a PTY session.

Returns the default value if the session is not found.
-}
withSession :: PtyManager -> Text -> a -> (RealPtySession -> IO a) -> IO a
withSession PtyManager{..} ptyId defaultVal action = do
    sessions <- readTVarIO pmSessions
    case Map.lookup ptyId sessions of
        Nothing -> pure defaultVal
        Just session -> action session

{- | Get a PTY session by ID.

Returns 'Nothing' if the session does not exist.

@since 0.1.0
-}
get :: PtyManager -> Text -> IO (Maybe PtyInfo)
get mgr ptyId = withSession mgr ptyId Nothing (pure . Just . rpsInfo)

{- | List all active PTY sessions.

@since 0.1.0
-}
list :: PtyManager -> IO [PtyInfo]
list PtyManager{..} = do
    sessions <- readTVarIO pmSessions
    pure $ map rpsInfo (Map.elems sessions)

{- | Update a PTY session.

Supports updating the title and/or resizing the terminal.
Returns the updated info, or 'Nothing' if the session doesn't exist.

@since 0.1.0
-}
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

-- ============================================================================
-- Session Removal
-- ============================================================================

{- | Remove a PTY session.

Terminates the process, closes the PTY, and cleans up resources.
Returns 'True' if the session was found and removed, 'False' otherwise.

@since 0.1.0
-}
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
            cleanupSession session
            pure True

{- | Clean up a session's resources.

Closes the PTY, terminates processes, and removes the overlay directory.
-}
cleanupSession :: RealPtySession -> IO ()
cleanupSession session = do
    -- Close the PTY first to signal EOF to the process
    void $ tryIO $ closePty (rpsPty session)

    -- Terminate the process
    terminateProcess (rpsProcess session)

    -- Terminate network process if present
    case rpsNetworkProcess session of
        Nothing -> pure ()
        Just netPh -> do
            terminateProcess netPh
            void $ tryIO $ waitForProcess netPh

    -- Wait briefly for process to exit (poll a few times)
    waitForExit 5 (rpsProcess session)

    -- Clean up overlay directory
    case rpsOverlayDir session of
        Nothing -> pure ()
        Just dir -> void $ tryIO $ Sandbox.destroyDir dir

{- | Poll for process exit with limited attempts, then SIGKILL.

Attempts to wait for graceful exit, escalating to SIGKILL if necessary.
-}
waitForExit :: Int -> ProcessHandle -> IO ()
waitForExit 0 ph = do
    -- Process didn't exit with SIGTERM, send SIGKILL
    mpid <- getPid ph
    case mpid of
        Nothing -> pure ()
        Just pid -> void $ tryIO $ Sig.signalProcess Sig.sigKILL pid
waitForExit n ph = do
    code <- getProcessExitCode ph
    case code of
        Just _exitCode -> pure ()
        Nothing -> do
            threadDelay 10000 -- 10ms
            waitForExit (n - 1) ph

-- ============================================================================
-- PTY I/O
-- ============================================================================

{- | Write data to a PTY.

Sends the given bytes to the PTY input. Returns 'True' on success,
'False' if the session doesn't exist or writing fails.

@since 0.1.0
-}
write :: PtyManager -> Text -> ByteString -> IO Bool
write mgr ptyId bs = withSession mgr ptyId False $ \session -> do
    result <- tryIO $ writePty (rpsPty session) bs
    pure $ either (const False) (const True) result

{- | Resize a PTY terminal.

Sends SIGWINCH via ioctl TIOCSWINSZ to notify the process of the new
terminal size. Returns 'True' on success, 'False' otherwise.

@since 0.1.0
-}
resize :: PtyManager -> Text -> Int -> Int -> IO Bool
resize mgr ptyId cols rows = withSession mgr ptyId False $ \session -> do
    result <- tryIO $ resizePty (rpsPty session) (cols, rows)
    pure $ either (const False) (const True) result

-- ============================================================================
-- WebSocket Connection
-- ============================================================================

{- | PTY connection for WebSocket bridging.

Provides callbacks for bidirectional communication with a PTY session.

@since 0.1.0
-}
data PtyConnection = PtyConnection
    { pcSend :: ByteString -> IO ()
    -- ^ Send data to the PTY
    , pcOnData :: (ByteString -> IO ()) -> IO ()
    -- ^ Register a handler for PTY output
    , pcClose :: IO ()
    -- ^ Close the connection
    }

{- | Connect to a PTY session for WebSocket bridging.

Returns a 'PtyConnection' that can be used to send and receive data.
The optional cursor parameter allows replaying missed output.

Returns 'Nothing' if the session doesn't exist.

@since 0.1.0
-}
connect :: PtyManager -> Text -> Maybe Word64 -> IO (Maybe PtyConnection)
connect mgr@PtyManager{pmLogger} ptyId cursor = withSession mgr ptyId Nothing $ \session -> do
    buf <- readTVarIO (rpsBuffer session)

    let replayFrom = fromMaybe 0 cursor
        replayData = calculateReplayData replayFrom buf

    lastCursorRef <- newIORef (pbCursor buf)
    runningRef <- newIORef True

    pure $
        Just
            PtyConnection
                { pcSend = void . tryIO . writePty (rpsPty session)
                , pcOnData = \handler -> do
                    unless (BS.null replayData) $ handler replayData
                    void $ forkLogged pmLogger ("pty-ws-poll-" <> ptyId) $ pollLoop session lastCursorRef runningRef handler
                , pcClose = writeIORef runningRef False
                }

{- | Poll loop for sending PTY output to the WebSocket handler.

Continuously checks for new data in the buffer and sends it to the handler.
-}
pollLoop ::
    RealPtySession ->
    IORef Word64 ->
    IORef Bool ->
    (ByteString -> IO ()) ->
    IO ()
pollLoop session lastCursorRef runningRef handler = do
    running <- readIORef runningRef
    when running $ do
        currentBuf <- readTVarIO (rpsBuffer session)
        lastCursor <- readIORef lastCursorRef

        when (pbCursor currentBuf > lastCursor) $ do
            let newData = calculateReplayData lastCursor currentBuf
            unless (BS.null newData) $ handler newData
            writeIORef lastCursorRef (pbCursor currentBuf)

        threadDelay 10000
        pollLoop session lastCursorRef runningRef handler

-- ============================================================================
-- Sandbox Operations
-- ============================================================================

{- | Commit sandbox changes to real filesystem.

Copies modified files from the sandbox overlay to the workdir.
Only works for sandboxed PTY sessions.

Returns 'Left' with an error message if:

* The PTY doesn't exist
* The PTY is not sandboxed
* The sandbox config is missing

@since 0.1.0
-}
commitChanges :: PtyManager -> Text -> IO (Either Text ())
commitChanges mgr ptyId = withSession mgr ptyId (Left "PTY not found") $ \session ->
    case (rpsOverlayDir session, rpsSandboxCfg session) of
        (Nothing, _) -> pure $ Left "PTY is not sandboxed"
        (_, Nothing) -> pure $ Left "PTY has no sandbox config"
        (Just overlayDir, Just cfg) -> do
            let workdir = scWorkdir cfg
            Sandbox.commit overlayDir workdir

{- | Get list of changed files in sandbox.

Returns the paths of files that have been modified in the sandbox.
Only works for sandboxed PTY sessions.

@since 0.1.0
-}
getChangedFiles :: PtyManager -> Text -> IO (Either Text [FilePath])
getChangedFiles mgr ptyId = withSession mgr ptyId (Left "PTY not found") $ \session ->
    case rpsOverlayDir session of
        Nothing -> pure $ Left "PTY is not sandboxed"
        Just overlayDir -> do
            files <- Sandbox.getChanges overlayDir
            pure $ Right files

-- ============================================================================
-- Utility Functions
-- ============================================================================

{- | Try an IO action, catching all exceptions.

This is a convenient wrapper around 'try' for 'SomeException'.
-}
tryIO :: IO a -> IO (Either SomeException a)
tryIO = try @SomeException
