{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Provider.Provider
Description : AI provider management operations
Stability   : experimental

This module provides operations for managing AI providers, including
listing providers, retrieving API keys, managing authentication,
and querying provider/model information.

== Overview

The module supports both static (Anthropic, OpenAI) and dynamic (OpenRouter)
providers. Dynamic providers fetch their model lists from external APIs.

== Authentication

API keys can be provided via:

1. Environment variables (e.g., @ANTHROPIC_API_KEY@)
2. Stored authentication (via 'setAuth')

Environment variables take precedence over stored authentication.

== Usage

@
import qualified Provider.Provider as Provider
import qualified Storage.Storage as Storage

main = Storage.withStorage ".opencode" $ \storage -> do
    -- List all providers
    providers <- Provider.list

    -- Get API key for a provider
    mKey <- Provider.getApiKey storage "anthropic"

    -- Check auth status
    auths <- Provider.authStatus storage
@
-}
module Provider.Provider (
    -- * Types (re-exported from Provider.Types)
    Provider.Types.Provider (..),
    Provider.Types.Model (..),
    Provider.Types.ModelCost (..),
    Provider.Types.ModelLimit (..),
    Provider.Types.ModelInterleaved (..),
    Provider.Types.ModelModalities (..),
    Provider.Types.ProviderAuth (..),

    -- * Provider queries
    list,
    listWithModels,
    get,
    getModel,

    -- * Authentication operations
    getApiKey,
    authStatus,
    listConnected,
    setAuth,
    removeAuth,

    -- * Built-in providers
    builtinProviders,

    -- * Pure helpers (exported for testing)
    extractTextField,
    findProvider,
    findModel,
    updateProviderModels,
    determineAuthMethod,
) where

import Control.Exception qualified
import Control.Monad (filterM)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.OpenRouter qualified as OpenRouter
import System.Environment (lookupEnv)

import Provider.Types
import Storage.Storage qualified as Storage

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure helper functions (no IO, easy to test)
-- ═══════════════════════════════════════════════════════════════════════════

{- | Extract a text field from a JSON object by key.

This is a generalized helper that replaces the duplicate extractToken/extractMethod
functions. It safely extracts a text value from a JSON object.

==== Examples

@
extractTextField "token" (object ["token" .= "abc"]) == Just "abc"
extractTextField "missing" (object ["token" .= "abc"]) == Nothing
extractTextField "token" (String "not an object") == Nothing
@
-}
extractTextField :: Text -> Value -> Maybe Text
extractTextField key (Object obj) = case KM.lookup (K.fromText key) obj of
    Just (String t) -> Just t
    _other -> Nothing
extractTextField _key _other = Nothing

{- | Find a provider by ID in a list of providers.

Pure function for provider lookup, avoiding IO when not needed.

==== Examples

@
findProvider "anthropic" builtinProviders == Just anthropicProvider
findProvider "unknown" builtinProviders == Nothing
@
-}
findProvider :: Text -> [Provider] -> Maybe Provider
findProvider pid providers = lookup pid [(providerId p, p) | p <- providers]

{- | Find a model by ID within a provider.

Pure function for model lookup.
-}
findModel :: Text -> Provider -> Maybe Model
findModel mid provider = Map.lookup mid (providerModels provider)

{- | Update a specific provider's models in a provider list.

Returns a new list with the specified provider's models replaced.
If the provider is not found, returns the original list unchanged.

==== Parameters

* @targetId@ - The provider ID to update
* @newModels@ - The new model map to use
* @providers@ - The list of providers to update
-}
updateProviderModels :: Text -> Map.Map Text Model -> [Provider] -> [Provider]
updateProviderModels targetId newModels providers =
    [ if providerId p == targetId
        then p{providerModels = newModels}
        else p
    | p <- providers
    ]

{- | Determine the authentication method based on stored and environment auth.

This is a pure function that encapsulates the auth method determination logic,
making it easy to test without IO.

==== Parameters

* @storedMethod@ - Method from stored auth (if any)
* @hasStored@ - Whether there is stored auth
* @hasEnvAuth@ - Whether environment variable auth is present

==== Returns

The authentication method string, or Nothing if not authenticated.
-}
determineAuthMethod :: Maybe Text -> Bool -> Bool -> Maybe Text
determineAuthMethod storedMethod hasStored hasEnvAuth =
    case storedMethod of
        Just m -> Just m
        Nothing
            | hasStored -> Just "api_key"
            | hasEnvAuth -> Just "env"
            | otherwise -> Nothing

{- | Build a model map from a list of models.

Creates a map keyed by model ID for efficient lookup.
-}
buildModelMap :: [Model] -> Map.Map Text Model
buildModelMap models = Map.fromList [(modelId m, m) | m <- models]

-- ═══════════════════════════════════════════════════════════════════════════
-- Built-in providers
-- ═══════════════════════════════════════════════════════════════════════════

{- | Built-in provider definitions.

Contains only provider metadata (ID, name, env vars). Models are loaded
dynamically from external APIs when available, not hardcoded here.

* __anthropic__ - Anthropic (Claude models)
* __openai__ - OpenAI (GPT and o-series models)
* __openrouter__ - OpenRouter (aggregator with many models)
-}
builtinProviders :: [Provider]
builtinProviders =
    [ Provider
        { providerId = "anthropic"
        , providerName = "Anthropic"
        , providerEnv = ["ANTHROPIC_API_KEY"]
        , providerModels = Map.empty
        , providerApi = Nothing
        , providerNpm = Nothing
        }
    , Provider
        { providerId = "openai"
        , providerName = "OpenAI"
        , providerEnv = ["OPENAI_API_KEY"]
        , providerModels = Map.empty
        , providerApi = Nothing
        , providerNpm = Nothing
        }
    , Provider
        { providerId = "openrouter"
        , providerName = "OpenRouter"
        , providerEnv = ["OPENROUTER_API_KEY"]
        , providerModels = Map.empty
        , providerApi = Nothing
        , providerNpm = Nothing
        }
    ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Provider query operations
-- ═══════════════════════════════════════════════════════════════════════════

{- | List all built-in providers.

Returns the static list of providers without fetching dynamic models.
For providers with dynamic model loading (e.g., OpenRouter), use 'listWithModels'.
-}
list :: IO [Provider]
list = pure builtinProviders

{- | List all providers with dynamically fetched models.

For OpenRouter, fetches models from API if an API key is available.
Falls back to built-in providers on API errors.

This function performs network IO and may be slow. Consider caching
the results if called frequently.
-}
listWithModels :: Storage.StorageConfig -> IO [Provider]
listWithModels storage = do
    mKey <- getApiKey storage "openrouter"
    case mKey of
        Nothing -> pure builtinProviders
        Just apiKey -> fetchAndUpdateOpenRouterModels apiKey

{- | Fetch OpenRouter models and update the provider list.

Internal helper that handles the OpenRouter API call and integrates
the results into the provider list.
-}
fetchAndUpdateOpenRouterModels :: Text -> IO [Provider]
fetchAndUpdateOpenRouterModels apiKey = do
    client <- OpenRouter.newClient apiKey
    result <- OpenRouter.fetchModels client
    case result of
        Left _err -> pure builtinProviders
        Right models ->
            let modelMap = buildModelMap models
             in pure $ updateProviderModels "openrouter" modelMap builtinProviders

{- | Get a provider by ID.

Looks up a provider in the built-in provider list.
-}
get :: Text -> IO (Maybe Provider)
get pid = pure $ findProvider pid builtinProviders

{- | Get a model by provider and model ID.

Looks up a specific model within a provider.

==== Example

@
mModel <- getModel "anthropic" "claude-sonnet-4-20250514"
@
-}
getModel :: Text -> Text -> IO (Maybe Model)
getModel providerID mid = do
    mProvider <- get providerID
    pure $ mProvider >>= findModel mid

-- ═══════════════════════════════════════════════════════════════════════════
-- Authentication operations
-- ═══════════════════════════════════════════════════════════════════════════

{- | Get API key for a provider.

Checks for API keys in the following order:

1. Environment variables (takes precedence)
2. Stored authentication

Returns 'Nothing' if no key is found in either location.

==== Example

@
mKey <- getApiKey storage "anthropic"
case mKey of
    Just key -> useKey key
    Nothing -> error "No API key configured"
@
-}
getApiKey :: Storage.StorageConfig -> Text -> IO (Maybe Text)
getApiKey storage providerID = do
    envKey <- getEnvApiKey providerID
    case envKey of
        Just k -> pure (Just k)
        Nothing -> getStoredApiKey storage providerID

{- | Get API key from environment variables.

Internal helper that checks all environment variables configured
for a provider.
-}
getEnvApiKey :: Text -> IO (Maybe Text)
getEnvApiKey providerID = do
    mProvider <- get providerID
    case mProvider of
        Nothing -> pure Nothing
        Just p -> do
            keys <- mapM (lookupEnv . T.unpack) (providerEnv p)
            pure $ firstNonEmpty keys
  where
    firstNonEmpty :: [Maybe String] -> Maybe Text
    firstNonEmpty envVals =
        case [k | Just k <- envVals, not (null k)] of
            (k : _) -> Just (T.pack k)
            [] -> Nothing

{- | Get API key from stored authentication.

Internal helper that retrieves the token from storage.
-}
getStoredApiKey :: Storage.StorageConfig -> Text -> IO (Maybe Text)
getStoredApiKey storage providerID = do
    stored <- readStoredAuth storage providerID
    pure $ stored >>= extractTextField "token"

{- | Read stored authentication value for a provider.

Internal helper that safely reads auth from storage,
returning Nothing on any error (not found, decode error, etc.).
-}
readStoredAuth :: Storage.StorageConfig -> Text -> IO (Maybe Value)
readStoredAuth storage providerID =
    Control.Exception.catch
        (Just <$> (Storage.read storage ["auth", providerID] :: IO Value))
        (\(_ :: Control.Exception.SomeException) -> pure Nothing)

{- | Get authentication status for all providers.

Returns the authentication status for each built-in provider,
indicating whether it's authenticated and by what method.
-}
authStatus :: Storage.StorageConfig -> IO [ProviderAuth]
authStatus storage = do
    providers <- list
    mapM (checkAuth storage) providers

{- | Check authentication status for a single provider.

Internal helper that checks both stored auth and environment variables.
-}
checkAuth :: Storage.StorageConfig -> Provider -> IO ProviderAuth
checkAuth storage provider = do
    stored <- readStoredAuth storage (providerId provider)
    envAuth <- anyM hasEnv (providerEnv provider)
    let storedMethod = stored >>= extractTextField "method"
    let hasAuth = isJust stored || envAuth
    let method = determineAuthMethod storedMethod (isJust stored) envAuth
    pure $
        ProviderAuth
            { paProviderID = providerId provider
            , paAuthenticated = hasAuth
            , paMethod = method
            }

{- | Check if an environment variable is set and non-empty.

Returns 'True' if the variable exists and has a non-empty value.
-}
hasEnv :: Text -> IO Bool
hasEnv key = do
    val <- lookupEnv (T.unpack key)
    pure $ isNonEmpty val
  where
    isNonEmpty :: Maybe String -> Bool
    isNonEmpty Nothing = False
    isNonEmpty (Just "") = False
    isNonEmpty (Just _) = True

{- | Monadic version of 'any' for predicates returning IO Bool.

Short-circuits on the first True result.
-}
anyM :: (a -> IO Bool) -> [a] -> IO Bool
anyM _ [] = pure False
anyM f (x : xs) = do
    ok <- f x
    if ok then pure True else anyM f xs

{- | List provider IDs that have valid authentication.

Returns IDs of providers that have either:

* A valid environment variable set
* Stored authentication credentials
-}
listConnected :: Storage.StorageConfig -> IO [Text]
listConnected storage = do
    providers <- list
    let providerIds = map providerId providers
    filterM (fmap isJust . getApiKey storage) providerIds

{- | Store authentication credentials for a provider.

Saves the API key token to storage. The stored auth uses the @"api_key"@
method by default.

==== Example

@
setAuth storage "openai" "sk-..."
@
-}
setAuth :: Storage.StorageConfig -> Text -> Text -> IO ()
setAuth storage providerID token =
    Storage.write storage ["auth", providerID] (object ["token" .= token, "method" .= ("api_key" :: Text)])

{- | Remove stored authentication for a provider.

Deletes any stored credentials. Note that environment variable
authentication will still work after removal.
-}
removeAuth :: Storage.StorageConfig -> Text -> IO ()
removeAuth storage providerID =
    Storage.remove storage ["auth", providerID]
