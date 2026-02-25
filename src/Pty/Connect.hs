{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Pty.Connect
Description : WebSocket connection handler for PTY sessions

This module provides the HTTP handler for PTY WebSocket connections.
Clients connect to this endpoint to establish a bidirectional terminal
session with an existing PTY.

The handler validates that the PTY exists and requires a WebSocket
upgrade to proceed with the connection.
-}
module Pty.Connect (
    -- * Handlers
    ptyConnectHandler,
) where

import Data.Text (Text)
import Network.HTTP.Types (status400)
import Network.Wai (Application, responseLBS)
import Pty.Pty qualified as Pty
import Servant (Handler, Tagged (..))
import State (AppState (..))

{- | HTTP handler for PTY WebSocket connections.

This handler validates that the requested PTY session exists. If the PTY
is found, it returns a 400 response indicating that a WebSocket upgrade
is required. The actual WebSocket upgrade is handled by middleware.

If the PTY does not exist, returns a 400 error with \"PTY not found\".

==== __Usage__

This handler is typically mounted at @\/pty\/:id\/connect@ in the API.

@since 0.1.0
-}
ptyConnectHandler :: AppState -> Text -> Tagged Handler Application
ptyConnectHandler st ptyId = Tagged $ \_req respond' -> do
    mInfo <- Pty.get (stPtyManager st) ptyId
    case mInfo of
        Nothing ->
            respond' $
                responseLBS
                    status400
                    [("Content-Type", "text/plain")]
                    "PTY not found"
        Just _info -> do
            respond' $
                responseLBS
                    status400
                    [("Content-Type", "text/plain")]
                    "WebSocket upgrade required"
