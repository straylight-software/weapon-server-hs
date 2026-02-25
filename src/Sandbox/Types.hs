{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Sandbox.Types
Description : Types for isolated shell execution in Linux namespaces

Sandbox types for isolated shell execution using Linux namespaces.

This module provides the core types for sandbox configuration and state
management. The sandbox uses Linux namespaces (via bwrap/unshare) for:

- __User namespace__: Unprivileged isolation (no root required)
- __PID namespace__: Process isolation (sandbox sees only its own processes)
- __Mount namespace__: Filesystem isolation with overlayfs COW
- __Network namespace__: Optional network isolation
- __IPC namespace__: System V IPC isolation

__Filesystem Architecture__:

The sandbox uses overlayfs for zero-cost copy-on-write semantics:

@
\/tmp\/opencode-sandbox-{id}\/
├── upper\/     ← tmpfs, COW writes go here
├── work\/      ← overlayfs workdir
└── merged\/    ← union mount (lower=\/, upper=upper, workdir=work)
@

This gives us instant cleanup (just rm -rf the sandbox dir) and no
persistent state unless explicitly mounted.
-}
module Sandbox.Types (
    -- * Configuration
    SandboxConfig (..),
    NetworkMode (..),
    MountSpec (..),
    defaultConfig,

    -- * Sandbox State
    SandboxState (..),
    SandboxStatus (..),
    SandboxInfo (..),

    -- * Resource Limits
    ResourceLimits (..),
    defaultLimits,

    -- * Coeffects (for DischargeProof integration)
    Coeffects (..),
    pureCoeffects,
) where

import Data.Aeson
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Posix.Types (ProcessID)

{- | Network isolation mode for the sandbox.

Controls how the sandbox interacts with the network:

- 'NetworkNone': Full isolation, only loopback available. Safest option.
- 'NetworkHost': Share host network namespace. Required for network access.
- 'NetworkSlirp': User-mode networking via slirp4netns. Isolated but functional.
-}
data NetworkMode
    = -- | No network access (loopback only). Safest, default option.
      NetworkNone
    | -- | Share host network namespace. Full network access.
      NetworkHost
    | -- | User-mode networking via slirp4netns. Isolated but functional.
      NetworkSlirp
    deriving (Eq, Show, Generic)

instance ToJSON NetworkMode where
    toJSON NetworkNone = "none"
    toJSON NetworkHost = "host"
    toJSON NetworkSlirp = "slirp"

instance FromJSON NetworkMode where
    parseJSON = withText "NetworkMode" $ \case
        "none" -> pure NetworkNone
        "host" -> pure NetworkHost
        "slirp" -> pure NetworkSlirp
        _otherMode -> fail "Invalid NetworkMode"

{- | Mount specification for bind mounts into the sandbox.

Bind mounts allow specific host directories to be visible inside the sandbox.
Use read-only mounts for security unless writes are required.

Example:

@
MountSpec
    { msSource = \"\/home\/user\/project\"
    , msDest = \"\/workspace\"
    , msReadOnly = False  -- Allow writes to project
    }
@
-}
data MountSpec = MountSpec
    { msSource :: FilePath
    -- ^ Host filesystem path to mount from
    , msDest :: FilePath
    -- ^ Path inside the sandbox where the mount will appear
    , msReadOnly :: Bool
    -- ^ If 'True', mount is read-only (recommended for security)
    }
    deriving (Eq, Show, Generic)

instance ToJSON MountSpec
instance FromJSON MountSpec

{- | Resource limits enforced via cgroup v2.

These limits prevent resource exhaustion attacks from sandboxed processes.
All limits are optional except 'rlNoNewPrivs' which should always be 'True'.

__Security considerations__:

- Memory limits prevent OOM attacks against the host
- PID limits prevent fork bomb attacks
- CPU limits prevent CPU starvation attacks
- NoNewPrivs prevents privilege escalation

See 'defaultLimits' for recommended security defaults.
-}
data ResourceLimits = ResourceLimits
    { rlMemoryMax :: Maybe Word64
    -- ^ Memory limit in bytes. 'Nothing' means unlimited.
    , rlCpuMax :: Maybe Word64
    -- ^ CPU quota in microseconds per period. 'Nothing' means unlimited.
    , rlCpuPeriod :: Word64
    -- ^ CPU accounting period in microseconds (default: 100000 = 100ms)
    , rlPidsMax :: Maybe Word64
    -- ^ Maximum number of processes. Prevents fork bombs.
    , rlNoNewPrivs :: Bool
    {- ^ If 'True', sets PR_SET_NO_NEW_PRIVS to prevent privilege escalation.
    Should always be 'True' for security.
    -}
    }
    deriving (Eq, Show, Generic)

instance ToJSON ResourceLimits
instance FromJSON ResourceLimits

{- | Default resource limits with conservative security settings.

These defaults balance usability with security:

- __2GB memory__: Enough for most tasks, prevents memory DoS
- __No CPU limit__: Processes can use available CPU
- __1000 processes__: Prevents fork bombs while allowing normal operation
- __NoNewPrivs enabled__: Prevents privilege escalation (always set)

These values are part of the sandbox security contract and should not
be changed without explicit security review.
-}
defaultLimits :: ResourceLimits
defaultLimits =
    ResourceLimits
        { rlMemoryMax = Just (2 * 1024 * 1024 * 1024) -- 2GB
        , rlCpuMax = Nothing -- No CPU limit
        , rlCpuPeriod = 100000 -- 100ms
        , rlPidsMax = Just 1000 -- 1000 processes
        , rlNoNewPrivs = True -- Always set
        }

{- | Coeffects - what resources the sandbox requires from the environment.

Coeffects declare the external resources a sandbox needs to function.
This enables static analysis of sandbox requirements before execution.

Maps to sensenet's Resource.dhall for capability-based authorization.

Example of a sandbox that needs network and auth:

@
Coeffects
    { cfNetwork = True
    , cfAuth = [\"github\", \"npm\"]
    , cfFilesystem = [\"\/home\/user\/.ssh\"]
    }
@

See 'pureCoeffects' for a sandbox that needs no external resources.
-}
data Coeffects = Coeffects
    { cfNetwork :: Bool
    -- ^ Whether the sandbox needs network access
    , cfAuth :: [Text]
    -- ^ List of authentication provider names needed (e.g., \"github\", \"npm\")
    , cfFilesystem :: [FilePath]
    -- ^ Filesystem paths needed beyond the sandbox workdir
    }
    deriving (Eq, Show, Generic)

instance ToJSON Coeffects
instance FromJSON Coeffects

{- | Pure coeffects - a sandbox that needs no external resources.

This is the safest and most isolated sandbox configuration.
Use this as a starting point and add only the capabilities you need.

Properties (verified by tests):

- No network access ('cfNetwork' = 'False')
- No authentication tokens ('cfAuth' = [])
- No filesystem mounts ('cfFilesystem' = [])
-}
pureCoeffects :: Coeffects
pureCoeffects =
    Coeffects
        { cfNetwork = False
        , cfAuth = []
        , cfFilesystem = []
        }

{- | Complete sandbox configuration.

This is the primary configuration type for creating sandboxes.
Use 'defaultConfig' as a starting point and modify as needed.

Example:

@
let config = (defaultConfig \"\/home\/user\/project\")
      { scNetwork = NetworkHost  -- Enable network access
      , scMounts = [MountSpec \"\/etc\/ssl\" \"\/etc\/ssl\" True]
      }
@
-}
data SandboxConfig = SandboxConfig
    { scRootfs :: Maybe FilePath
    -- ^ Custom rootfs path. If 'Nothing', uses overlay on host @\/@.
    , scWorkdir :: FilePath
    -- ^ Working directory inside sandbox. This path is bind-mounted read-write.
    , scNetwork :: NetworkMode
    -- ^ Network isolation mode. See 'NetworkMode' for options.
    , scMounts :: [MountSpec]
    -- ^ Additional bind mounts beyond the workdir.
    , scEnv :: [(Text, Text)]
    -- ^ Additional environment variables to set in the sandbox.
    , scLimits :: ResourceLimits
    -- ^ Resource limits for the sandbox. See 'defaultLimits'.
    , scCoeffects :: Coeffects
    -- ^ Declared coeffects (capabilities the sandbox requires).
    , scSeccomp :: Bool
    -- ^ If 'True', enable seccomp-bpf syscall filtering.
    , scTmpfsSize :: Word64
    -- ^ Size of tmpfs overlay in bytes. Limits temporary file space.
    , scShell :: FilePath
    -- ^ Shell to run inside sandbox (typically from @$SHELL@ or fallback).
    , scHome :: FilePath
    -- ^ User's home directory path (set as @$HOME@ in sandbox).
    , scUser :: Text
    -- ^ Username (set as @$USER@ in sandbox).
    }
    deriving (Eq, Show, Generic)

instance ToJSON SandboxConfig
instance FromJSON SandboxConfig

{- | Default sandbox configuration with maximum security.

Creates a configuration with:

- __No network access__ ('NetworkNone')
- __No additional mounts__ (only workdir is writable)
- __Seccomp enabled__ (syscall filtering)
- __512MB tmpfs__ (limits temporary file space)
- __Conservative resource limits__ ('defaultLimits')

The @scShell@, @scHome@, and @scUser@ fields are set to placeholders
and should be configured by the caller (typically from environment).

@
config <- do
    shell <- fromMaybe \"\/bin\/sh\" \<$\> lookupEnv \"SHELL\"
    home <- getHomeDirectory
    user <- getLoginName
    pure $ (defaultConfig \"\/path\/to\/project\")
        { scShell = shell
        , scHome = home
        , scUser = T.pack user
        }
@
-}
defaultConfig :: FilePath -> SandboxConfig
defaultConfig workdir =
    SandboxConfig
        { scRootfs = Nothing
        , scWorkdir = workdir
        , scNetwork = NetworkNone
        , scMounts = []
        , scEnv = []
        , scLimits = defaultLimits
        , scCoeffects = pureCoeffects
        , scSeccomp = True
        , scTmpfsSize = 512 * 1024 * 1024 -- 512MB tmpfs
        , scShell = "/bin/sh" -- Placeholder, should be set by caller
        , scHome = "/tmp" -- Placeholder, should be set by caller
        , scUser = "nobody" -- Placeholder, should be set by caller
        }

{- | Runtime status of a sandbox.

Tracks the lifecycle state of a sandbox process:

- 'SandboxRunning': Process is currently executing
- 'SandboxExited': Process terminated normally with an exit code
- 'SandboxKilled': Process was killed by a signal
-}
data SandboxStatus
    = -- | Process is currently running
      SandboxRunning
    | -- | Process exited normally with the given exit code (0 = success)
      SandboxExited Int
    | -- | Process was killed by the given signal number
      SandboxKilled Int
    deriving (Eq, Show, Generic)

instance ToJSON SandboxStatus where
    toJSON SandboxRunning = object ["status" .= ("running" :: Text)]
    toJSON (SandboxExited c) = object ["status" .= ("exited" :: Text), "code" .= c]
    toJSON (SandboxKilled s) = object ["status" .= ("killed" :: Text), "signal" .= s]

{- | Runtime state of a sandbox.

Contains all information needed to manage a running sandbox,
including the process ID and overlay directory for cleanup.

This is an internal type - use 'SandboxInfo' for API responses.
-}
data SandboxState = SandboxState
    { ssConfig :: SandboxConfig
    -- ^ The configuration used to create this sandbox
    , ssPid :: ProcessID
    -- ^ PID of the sandbox init process (bwrap)
    , ssOverlayDir :: FilePath
    -- ^ Path to overlay directory (for cleanup on destroy)
    , ssStatus :: SandboxStatus
    -- ^ Current status of the sandbox process
    }
    deriving (Eq, Show)

{- | Public information about a sandbox for API responses.

This is the external representation of sandbox state, safe to expose
via the API. It includes only the information clients need.
-}
data SandboxInfo = SandboxInfo
    { siId :: Text
    -- ^ Unique identifier for this sandbox
    , siPid :: Int
    -- ^ Process ID of the sandbox (for monitoring)
    , siStatus :: SandboxStatus
    -- ^ Current status of the sandbox
    , siWorkdir :: FilePath
    -- ^ Working directory inside the sandbox
    , siNetwork :: NetworkMode
    -- ^ Network isolation mode in use
    , siCoeffects :: Coeffects
    -- ^ Declared coeffects (capabilities)
    }
    deriving (Eq, Show, Generic)

instance ToJSON SandboxInfo
