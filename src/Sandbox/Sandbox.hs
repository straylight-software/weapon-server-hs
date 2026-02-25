{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Sandbox.Sandbox
Description : Sandbox execution via bwrap (bubblewrap)

Sandbox execution using Linux namespaces via bubblewrap (bwrap).

== Architecture

@
Host                          Sandbox (namespaced)
────                          ────────────────────
weapon-server
     │
     │ fork+exec bwrap
     └──────────────────────▶ bwrap
                                 │
                                 │ unshare(CLONE_NEWUSER|NEWPID|NEWNS|...)
                                 │ pivot_root to overlayfs
                                 │ seccomp-bpf
                                 │
                                 └──▶ \/bin\/sh (or specified shell)
                                           │
                                           │ PTY master\/slave
                                           ▼
                                      user shell session
@

== Filesystem Layout (overlayfs)

@
\/tmp\/opencode-sandbox-{id}\/
├── upper\/     ← tmpfs, COW writes go here
├── work\/      ← overlayfs workdir
└── merged\/    ← union mount (lower=\/, upper=upper, workdir=work)
@

This gives us:

- __Zero-cost COW__: reads go to host @\/@, writes go to tmpfs
- __Instant cleanup__: @rm -rf@ the sandbox dir
- __No persistent state__ unless explicitly mounted
-}
module Sandbox.Sandbox (
    -- * Sandbox Lifecycle
    create,
    destroy,
    destroyDir,

    -- * Commit / Changes
    commit,
    getChanges,

    -- * Pure Functions (for testing)
    -- $pure-functions
    buildBwrapArgs,
    SandboxDirPaths (..),
    sandboxDirPaths,
    buildDefaultEnv,
    buildNetworkArgs,
    buildNamespaceArgs,
    buildFilesystemArgs,
    buildSecurityArgs,
    mountToArgs,
    envToArgs,
    upperDirPath,
    buildRsyncArgs,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Text (Text)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process

import Data.Text qualified as T

import Sandbox.Types
import Util.FileSystem (listDirectoryRecursive)

{- $pure-functions

These pure functions are exposed for testing. They allow property-based
testing of the argument building logic without any IO.
-}

-- ============================================================================
-- Sandbox Lifecycle
-- ============================================================================

{- | Create sandbox directories and return the bwrap arguments.

This function:

1. Creates the overlay directory structure
2. Builds the bwrap command line arguments
3. Returns the base directory and arguments for process spawning

The actual process spawning is handled by the Pty module.

Returns @Left error@ if directory creation fails, with automatic cleanup.
-}
create :: Text -> SandboxConfig -> IO (Either Text (FilePath, [String]))
create sandboxId config = do
    let SandboxDirPaths{sdpBaseDir, sdpUpperDir, sdpWorkDir, sdpMergedDir} = sandboxDirPaths sandboxId

    result <- tryIO $ do
        createDirectoryIfMissing True sdpUpperDir
        createDirectoryIfMissing True sdpWorkDir
        createDirectoryIfMissing True sdpMergedDir
        let args = buildBwrapArgs config
        pure (sdpBaseDir, args)

    case result of
        Left e -> do
            cleanupDirectory sdpBaseDir
            pure $ Left $ "Failed to create sandbox: " <> T.pack (show e)
        Right r -> pure $ Right r

{- | Destroy a sandbox by terminating the process and cleaning up directories.

This function:

1. Sends SIGTERM to the process
2. Waits 100ms for graceful shutdown
3. Sends SIGTERM again (in case it was caught)
4. Removes the overlay directory

All errors are silently ignored to ensure cleanup completes.
-}
destroy :: FilePath -> ProcessHandle -> IO ()
destroy overlayDir ph = do
    terminateProcess ph
    threadDelay 100000 -- 100ms grace period
    void $ tryIO $ terminateProcess ph
    cleanupDirectory overlayDir

{- | Destroy just the sandbox directory when the process has already terminated.

Use this when you know the sandbox process has exited and only cleanup
is needed. All errors are silently ignored.
-}
destroyDir :: FilePath -> IO ()
destroyDir = cleanupDirectory

-- ============================================================================
-- Commit / Changes
-- ============================================================================

{- | Commit sandbox changes to the real filesystem.

Copies files from the overlay upper directory to the original workdir
using @rsync -a@ to preserve permissions and timestamps.

Returns @Left error@ if:

- The overlay upper directory doesn't exist
- @rsync@ fails or exits with non-zero code

__Warning__: This makes sandbox changes permanent. There is no undo.
-}
commit :: FilePath -> FilePath -> IO (Either Text ())
commit overlayDir workdir = do
    let upperDir = upperDirPath overlayDir
    exists <- doesDirectoryExist upperDir
    if not exists
        then pure $ Left "Overlay upper dir not found"
        else runRsync (buildRsyncArgs upperDir workdir)

{- | Get list of changed files in the sandbox.

Returns the paths of all files that were created or modified in the
sandbox overlay. These are files that would be committed by 'commit'.

Returns an empty list if the overlay directory doesn't exist.
-}
getChanges :: FilePath -> IO [FilePath]
getChanges overlayDir = do
    let upperDir = upperDirPath overlayDir
    exists <- doesDirectoryExist upperDir
    if not exists
        then pure []
        else listDirectoryRecursive upperDir

-- ============================================================================
-- Pure Functions: Directory Paths
-- ============================================================================

-- | Directory paths for a sandbox overlay filesystem.
data SandboxDirPaths = SandboxDirPaths
    { sdpBaseDir :: FilePath
    -- ^ Root directory for the sandbox
    , sdpUpperDir :: FilePath
    -- ^ Upper layer for copy-on-write changes
    , sdpWorkDir :: FilePath
    -- ^ Working directory for overlayfs
    , sdpMergedDir :: FilePath
    -- ^ Merged view of the filesystem
    }
    deriving (Show, Eq)

{- | Calculate sandbox directory paths from a sandbox ID.

Returns a 'SandboxDirPaths' record with all overlay directories.

This is a pure function for testability.
-}
sandboxDirPaths :: Text -> SandboxDirPaths
sandboxDirPaths sandboxId =
    let baseDir = "/tmp/opencode-sandbox-" <> T.unpack sandboxId
        upperDir = baseDir </> "upper"
        workDir = baseDir </> "work"
        mergedDir = baseDir </> "merged"
     in SandboxDirPaths baseDir upperDir workDir mergedDir

{- | Get the upper directory path from an overlay directory.

The upper directory contains all writes made inside the sandbox.
-}
upperDirPath :: FilePath -> FilePath
upperDirPath overlayDir = overlayDir </> "upper"

{- | Build rsync arguments for committing changes.

Arguments:

- @-a@: Archive mode (preserves permissions, ownership, timestamps)
- Source ends with @\/@ to copy contents, not directory itself
-}
buildRsyncArgs :: FilePath -> FilePath -> [String]
buildRsyncArgs upperDir workdir = ["-a", upperDir <> "/", workdir <> "/"]

-- ============================================================================
-- Pure Functions: Bwrap Argument Building
-- ============================================================================

{- | Build the complete bwrap command line arguments from a config.

This is a pure function that assembles all the namespace, filesystem,
environment, and security arguments for bubblewrap.

The resulting arguments can be passed directly to @bwrap@.
-}
buildBwrapArgs :: SandboxConfig -> [String]
buildBwrapArgs config@SandboxConfig{..} =
    concat
        [ buildNamespaceArgs scNetwork
        , buildFilesystemArgs scWorkdir scMounts
        , buildEnvironmentArgs config
        , buildSecurityArgs scSeccomp scLimits
        , buildCommandArgs scShell
        ]

{- | Build namespace isolation arguments.

Always unshares: user, PID, UTS, IPC namespaces.
Network namespace is conditional on 'NetworkMode'.
-}
buildNamespaceArgs :: NetworkMode -> [String]
buildNamespaceArgs networkMode =
    concat
        [ ["--unshare-user"] -- Required for unprivileged operation
        , ["--unshare-pid"] -- Process isolation
        , ["--unshare-uts"] -- Hostname isolation
        , ["--unshare-ipc"] -- System V IPC isolation
        , buildNetworkArgs networkMode
        , ["--die-with-parent"] -- Clean up if parent dies
        ]

{- | Build network namespace arguments based on 'NetworkMode'.

- 'NetworkNone': Unshare network (loopback only)
- 'NetworkHost': Share host network (no arguments)
- 'NetworkSlirp': Unshare network (for slirp4netns)
-}
buildNetworkArgs :: NetworkMode -> [String]
buildNetworkArgs NetworkNone = ["--unshare-net"]
buildNetworkArgs NetworkHost = []
buildNetworkArgs NetworkSlirp = ["--unshare-net"]

{- | Build filesystem mount arguments.

Sets up:

- @\/dev@ and @\/proc@ pseudo-filesystems
- Read-only bind mount of host @\/@
- Tmpfs for @\/tmp@ and @\/var\/tmp@
- Read-write bind mount of workdir
- User-specified additional mounts
-}
buildFilesystemArgs :: FilePath -> [MountSpec] -> [String]
buildFilesystemArgs workdir mounts =
    concat
        [ ["--dev", "/dev"]
        , ["--proc", "/proc"]
        , ["--ro-bind", "/", "/"] -- Host root read-only
        , ["--tmpfs", "/tmp"] -- Writable /tmp
        , ["--tmpfs", "/var/tmp"] -- Writable /var/tmp
        , ["--bind", workdir, workdir] -- Project dir read-write
        , concatMap mountToArgs mounts
        , ["--chdir", workdir]
        ]

{- | Build environment variable arguments.

Clears the environment and sets up a minimal, secure environment
including proxy settings for network surveillance.
-}
buildEnvironmentArgs :: SandboxConfig -> [String]
buildEnvironmentArgs config =
    concat
        [ ["--clearenv"]
        , concatMap envToArgs (scEnv config)
        , buildDefaultEnv config
        ]

{- | Build the default environment variables for the sandbox.

Includes:

- User environment: @HOME@, @USER@, @SHELL@
- Locale: @LANG@, @LC_ALL@ (C.UTF-8)
- Terminal: @TERM@ (xterm-256color)
- Sandbox indicator: @OPENCODE_SANDBOX=1@
- Proxy settings for HTTP/HTTPS traffic surveillance
- NixOS-compatible @PATH@
-}
buildDefaultEnv :: SandboxConfig -> [String]
buildDefaultEnv SandboxConfig{..} =
    concat
        [ setEnv "HOME" scHome
        , setEnv "USER" (T.unpack scUser)
        , setEnv "SHELL" scShell
        , setEnv "PATH" nixCompatiblePath
        , setEnv "TERM" "xterm-256color"
        , setEnv "OPENCODE_SANDBOX" "1"
        , setEnv "LANG" "C.UTF-8"
        , setEnv "LC_ALL" "C.UTF-8"
        , proxyEnvVars
        ]
  where
    -- NixOS-compatible PATH (includes /run/current-system/sw/bin)
    nixCompatiblePath :: String
    nixCompatiblePath =
        "/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/bin"

    -- Proxy environment variables for traffic surveillance
    proxyEnvVars :: [String]
    proxyEnvVars =
        concat
            [ setEnv "HTTP_PROXY" proxyUrl
            , setEnv "HTTPS_PROXY" proxyUrl
            , setEnv "http_proxy" proxyUrl
            , setEnv "https_proxy" proxyUrl
            ]

    proxyUrl :: String
    proxyUrl = "http://127.0.0.1:8888"

{- | Build security-related arguments.

Handles:

- Session creation (when seccomp is enabled)
- Capability dropping (when NoNewPrivs is set)
-}
buildSecurityArgs :: Bool -> ResourceLimits -> [String]
buildSecurityArgs seccompEnabled limits =
    ["--new-session" | seccompEnabled]
        ++ (if rlNoNewPrivs limits then ["--cap-drop", "ALL"] else [])

{- | Build the command to run inside the sandbox.

Runs the specified shell in login mode (@-l@).
-}
buildCommandArgs :: FilePath -> [String]
buildCommandArgs shellPath = ["--", shellPath, "-l"]

-- ============================================================================
-- Pure Functions: Argument Converters
-- ============================================================================

{- | Convert a 'MountSpec' to bwrap arguments.

Read-only mounts use @--ro-bind@, read-write mounts use @--bind@.
-}
mountToArgs :: MountSpec -> [String]
mountToArgs MountSpec{..} =
    if msReadOnly
        then ["--ro-bind", msSource, msDest]
        else ["--bind", msSource, msDest]

{- | Convert an environment variable pair to bwrap arguments.

Produces @[\"--setenv\", key, value]@.
-}
envToArgs :: (Text, Text) -> [String]
envToArgs (k, v) = ["--setenv", T.unpack k, T.unpack v]

-- ============================================================================
-- Internal Helpers
-- ============================================================================

-- | Helper to build --setenv arguments
setEnv :: String -> String -> [String]
setEnv key val = ["--setenv", key, val]

-- | Try an IO action, catching all exceptions
tryIO :: IO a -> IO (Either SomeException a)
tryIO = try @SomeException

-- | Cleanup a directory, ignoring all errors
cleanupDirectory :: FilePath -> IO ()
cleanupDirectory dir = void $ tryIO $ removeDirectoryRecursive dir

-- | Run rsync with the given arguments and return the result
runRsync :: [String] -> IO (Either Text ())
runRsync rsyncArgs = do
    result <- tryIO $ do
        (_, _, _, ph) <- createProcess (proc "rsync" rsyncArgs)
        waitForProcess ph
    case result of
        Left e -> pure $ Left $ "rsync failed: " <> T.pack (show e)
        Right ExitSuccess -> pure $ Right ()
        Right (ExitFailure n) -> pure $ Left $ "rsync exited with code " <> T.pack (show n)
