-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                   // weapon-server // api/pty
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- Pseudo-terminal (PTY) API endpoints. Manages sandboxed terminal sessions
-- for safe command execution with filesystem isolation.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Pty
    ( -- * PTY API Endpoints
      PtyListAPI
    , PtyCreateAPI
    , PtyGetAPI
    , PtyUpdateAPI
    , PtyDeleteAPI
    , PtyConnectAPI
    , PtyCommitAPI
    , PtyChangesAPI
    ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Servant


-- ═══════════════════════════════════════════════════════════════════════════
-- // api type definitions //
-- ═══════════════════════════════════════════════════════════════════════════

-- list all pty sessions
type PtyListAPI = "pty" :> Get '[JSON] [Value]

-- create a new pty session
type PtyCreateAPI = "pty" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- get a specific pty session
type PtyGetAPI = "pty" :> Capture "ptyID" Text :> Get '[JSON] Value

-- update a pty session
type PtyUpdateAPI = "pty" :> Capture "ptyID" Text :> ReqBody '[JSON] Value :> Put '[JSON] Value

-- delete a pty session
type PtyDeleteAPI = "pty" :> Capture "ptyID" Text :> Delete '[JSON] Bool

-- websocket connection for pty
type PtyConnectAPI = "pty" :> Capture "ptyID" Text :> "connect" :> Raw

-- commit sandbox changes to real filesystem
type PtyCommitAPI = "pty" :> Capture "ptyID" Text :> "commit" :> Post '[JSON] Value

-- get list of changed files in sandbox
type PtyChangesAPI = "pty" :> Capture "ptyID" Text :> "changes" :> Get '[JSON] Value
