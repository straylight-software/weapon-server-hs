{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Provider.OAuth
Description : OAuth utilities for provider authentication
Stability   : experimental

This module provides OAuth-related utilities for authenticating with
AI providers that support OAuth flows.

== Usage

@
state <- generateState
let url = buildAuthorizeUrl "anthropic" state (Just "http://localhost:3000/callback") ["read", "write"]
-- Redirect user to url...
@
-}
module Provider.OAuth (
    -- * State generation
    generateState,

    -- * URL building
    buildAuthorizeUrl,

    -- * Pure helpers (exported for testing)
    renderParams,
    buildParams,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Numeric (showHex)
import System.Random (randomIO)

{- | Generate a cryptographically random state parameter for OAuth flows.

The state parameter is used to prevent CSRF attacks by ensuring the
OAuth callback matches a request initiated by the user.

Returns a hexadecimal string representation of a random 64-bit number.
-}
generateState :: IO Text
generateState = do
    n <- randomIO :: IO Word64
    pure $ T.pack (showHex n "")

{- | Build an OAuth authorization URL for a provider.

Constructs the full authorization URL including all query parameters.

==== Parameters

* @providerId@ - The provider identifier (e.g., @"anthropic"@)
* @state@ - The CSRF protection state parameter
* @redirect@ - Optional redirect URI for the callback
* @scopes@ - List of OAuth scopes to request

==== Example

@
let url = buildAuthorizeUrl "openai" "abc123" (Just "http://localhost:3000") ["read"]
-- Returns: "https://auth.opencode.ai/oauth/openai?state=abc123&redirect_uri=http://localhost:3000&scope=read"
@
-}
buildAuthorizeUrl :: Text -> Text -> Maybe Text -> [Text] -> Text
buildAuthorizeUrl provId state redirect scopes =
    let base = "https://auth.opencode.ai/oauth/" <> provId
        params = buildParams state redirect scopes
     in base <> "?" <> renderParams params

{- | Build the query parameters for an OAuth authorization URL.

This is a pure function that constructs the parameter list without
any IO, making it easy to test.

==== Parameters

* @state@ - The CSRF protection state parameter (always included)
* @redirect@ - Optional redirect URI
* @scopes@ - List of OAuth scopes (joined with commas if non-empty)
-}
buildParams :: Text -> Maybe Text -> [Text] -> [(Text, Text)]
buildParams state redirect scopes =
    [("state", state)]
        <> maybe [] (\r -> [("redirect_uri", r)]) redirect
        <> scopeParams scopes
  where
    scopeParams [] = []
    scopeParams xs = [("scope", T.intercalate "," xs)]

{- | Render query parameters as a URL-encoded query string.

Joins key-value pairs with @=@ and separates pairs with @&@.

Note: This does not perform URL encoding of special characters.
For production use with untrusted input, use a proper URL encoding library.

==== Example

@
renderParams [("a", "1"), ("b", "2")] == "a=1&b=2"
@
-}
renderParams :: [(Text, Text)] -> Text
renderParams params =
    T.intercalate "&" (map (\(k, v) -> k <> "=" <> v) params)
