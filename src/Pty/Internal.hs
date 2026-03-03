{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Pty.Internal
Description : Internal utilities for PTY management (exported for testing)

This module exports internal pure functions from the PTY module for
testing purposes. These functions are not part of the public API and
may change without notice.

__WARNING__: This module is intended for testing only. Do not use
in production code outside of the Pty module.
-}
module Pty.Internal (
    -- * Parameter Resolution
    CreateParams (..),
    resolveCreateParams,

    -- * Exit Code Conversion
    exitCodeToStatus,

    -- * Mount Specification
    toMountSpec,
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Exit (ExitCode (..))

import Data.Text qualified as T

import Pty.Types (CreatePtyInput (..), PtyStatus (..))
import Sandbox.Types (MountSpec (..))

-- ============================================================================
-- Pure Parameter Resolution
-- ============================================================================

{- | Resolved parameters for PTY creation.

This record holds the computed values after applying defaults
to the optional fields in 'CreatePtyInput'.

@since 0.1.0
-}
data CreateParams = CreateParams
    { cpCwd :: FilePath
    -- ^ Working directory
    , cpTitle :: Text
    -- ^ Display title
    , cpSessionId :: Text
    -- ^ Session ID for proxy correlation
    , cpEnv :: [(Text, Text)]
    -- ^ Environment variables
    , cpNetwork :: Bool
    -- ^ Enable network in sandbox
    , cpSandbox :: Bool
    -- ^ Enable sandboxing
    }
    deriving (Eq, Show)

{- | Resolve optional input parameters to concrete values.

This is a pure function that computes defaults for all optional fields.
The @OPENCODE_SESSION_ID@ environment variable is automatically injected.

==== __Examples__

>>> let input = CreatePtyInput Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
>>> let params = resolveCreateParams "/home/user" "pty_1" input
>>> cpCwd params
"/home/user"
>>> cpSandbox params
True

@since 0.1.0
-}
resolveCreateParams :: FilePath -> Text -> CreatePtyInput -> CreateParams
resolveCreateParams defaultDir ptyId CreatePtyInput{..} =
    let sandbox = fromMaybe True cpiSandbox
        cwd = T.unpack $ fromMaybe (T.pack defaultDir) cpiCwd
        title = fromMaybe ("Terminal " <> T.takeEnd 4 ptyId) cpiTitle
        sessionId = fromMaybe ptyId cpiSessionId
        baseEnv = maybe [] Map.toList cpiEnv
        env = ("OPENCODE_SESSION_ID", sessionId) : baseEnv
        network = fromMaybe False cpiNetwork
     in CreateParams
            { cpCwd = cwd
            , cpTitle = title
            , cpSessionId = sessionId
            , cpEnv = env
            , cpNetwork = network
            , cpSandbox = sandbox
            }

-- ============================================================================
-- Exit Code Conversion
-- ============================================================================

{- | Convert an 'ExitCode' to a 'PtyStatus'.

This is a pure function for converting process exit codes.

==== __Examples__

>>> exitCodeToStatus ExitSuccess
PtyExited 0

>>> exitCodeToStatus (ExitFailure 1)
PtyExited 1

@since 0.1.0
-}
exitCodeToStatus :: ExitCode -> PtyStatus
exitCodeToStatus ExitSuccess = PtyExited 0
exitCodeToStatus (ExitFailure n) = PtyExited n

-- ============================================================================
-- Mount Specification Conversion
-- ============================================================================

{- | Convert mount tuple to 'MountSpec'.

This is a pure conversion function.

==== __Examples__

>>> toMountSpec ("/src", "/dest", True)
MountSpec {msSource = "/src", msDest = "/dest", msReadOnly = True}

@since 0.1.0
-}
toMountSpec :: (Text, Text, Bool) -> MountSpec
toMountSpec (src, dest, ro) = MountSpec (T.unpack src) (T.unpack dest) ro
