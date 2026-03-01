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
import Network.HTTP.Types (status404, status405)
import Network.Wai (Application, responseLBS)
import Pty.Pty qualified as Pty
import Servant (Handler, Tagged (..))
import State (AppState (..))

{- | HTTP handler for PTY WebSocket connections.

This handler validates that the requested PTY session exists. If the PTY
is found but the request is not a WebSocket upgrade, it returns 405 Method
Not Allowed (RFC 9110). The actual WebSocket upgrade is handled by middleware.

If the PTY does not exist, returns 404 Not Found.

==== __Usage__

This handler is typically mounted at @\/pty\/:id\/connect@ in the API.

@since 0.1.0
-}
ptyConnectHandler :: AppState -> Text -> Tagged Handler Application
ptyConnectHandler st ptyId = Tagged $ \_req respond' -> do
    mInfo <- Pty.get (stPtyManager st) ptyId
    case mInfo of
        Nothing ->
            -- PTY not found - 404 Not Found
            respond' $
                responseLBS
                    status404
                    [("Content-Type", "application/json")]
                    "{\"name\":\"NotFoundError\",\"data\":{\"message\":\"PTY not found\"}}"
        Just _info -> do
            -- PTY exists but request is not a WebSocket upgrade
            -- Return 405 Method Not Allowed with Allow header (RFC 9110)
            respond' $
                responseLBS
                    status405
                    [ ("Content-Type", "application/json")
                    , ("Allow", "GET")
                    ]
                    "{\"name\":\"MethodNotAllowedError\",\"data\":{\"message\":\"WebSocket upgrade required\"}}"
