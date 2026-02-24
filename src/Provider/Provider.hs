{-# LANGUAGE OverloadedStrings #-}

{- | Provider module - AI provider management
Mirrors the TypeScript Provider namespace
-}
module Provider.Provider (
    -- * Types
    Provider.Types.Provider (..),
    Provider.Types.Model (..),
    Provider.Types.ModelCost (..),
    Provider.Types.ModelLimit (..),
    Provider.Types.ModelInterleaved (..),
    Provider.Types.ModelModalities (..),
    Provider.Types.ProviderAuth (..),

    -- * Operations
    list,
    listWithModels,
    get,
    getModel,
    getApiKey,
    authStatus,
    listConnected,
    setAuth,
    removeAuth,

    -- * Built-in providers
    builtinProviders,
) where

import Control.Exception qualified
import Control.Monad (filterM)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.OpenRouter qualified as OpenRouter
import System.Environment (lookupEnv)

import Provider.Types
import Storage.Storage qualified as Storage

-- | Built-in provider definitions
builtinProviders :: [Provider]
builtinProviders =
    [ Provider
        { providerId = "anthropic"
        , providerName = "Anthropic"
        , providerEnv = ["ANTHROPIC_API_KEY"]
        , providerModels =
            Map.fromList
                [
                    ( "claude-sonnet-4-20250514"
                    , (defaultModel "claude-sonnet-4-20250514" "Claude Sonnet 4" "2025-05-14" (ModelLimit 200000 Nothing 16384))
                        { modelReasoning = True
                        , modelFamily = Just "claude"
                        , modelCost = Just $ ModelCost 3.0 15.0 (Just 0.3) (Just 3.75) Nothing
                        , modelModalities = Just $ ModelModalities ["text", "image", "pdf"] ["text"]
                        }
                    )
                ,
                    ( "claude-opus-4-20250514"
                    , (defaultModel "claude-opus-4-20250514" "Claude Opus 4" "2025-05-14" (ModelLimit 200000 Nothing 32000))
                        { modelReasoning = True
                        , modelFamily = Just "claude"
                        , modelCost = Just $ ModelCost 15.0 75.0 (Just 1.5) (Just 18.75) Nothing
                        , modelModalities = Just $ ModelModalities ["text", "image", "pdf"] ["text"]
                        }
                    )
                ]
        , providerApi = Nothing
        , providerNpm = Nothing
        }
    , Provider
        { providerId = "openai"
        , providerName = "OpenAI"
        , providerEnv = ["OPENAI_API_KEY"]
        , providerModels =
            Map.fromList
                [
                    ( "gpt-4o"
                    , (defaultModel "gpt-4o" "GPT-4o" "2024-05-13" (ModelLimit 128000 Nothing 16384))
                        { modelFamily = Just "gpt"
                        , modelCost = Just $ ModelCost 2.5 10.0 Nothing Nothing Nothing
                        , modelModalities = Just $ ModelModalities ["text", "image"] ["text"]
                        }
                    )
                ,
                    ( "o3"
                    , (defaultModel "o3" "o3" "2025-01-01" (ModelLimit 200000 Nothing 100000))
                        { modelReasoning = True
                        , modelTemperature = False
                        , modelFamily = Just "o"
                        , modelCost = Just $ ModelCost 10.0 40.0 Nothing Nothing Nothing
                        , modelModalities = Just $ ModelModalities ["text", "image"] ["text"]
                        }
                    )
                ]
        , providerApi = Nothing
        , providerNpm = Nothing
        }
    , Provider
        { providerId = "openrouter"
        , providerName = "OpenRouter"
        , providerEnv = ["OPENROUTER_API_KEY"]
        , providerModels = Map.empty -- Loaded dynamically
        , providerApi = Nothing
        , providerNpm = Nothing
        }
    ]

-- | List all providers (without fetching dynamic models)
list :: IO [Provider]
list = pure builtinProviders

{- | List all providers with dynamically fetched models
For OpenRouter, fetches models from API if an API key is available.
-}
listWithModels :: Storage.StorageConfig -> IO [Provider]
listWithModels storage = do
    -- Try to get OpenRouter API key
    mKey <- getApiKey storage "openrouter"
    case mKey of
        Nothing -> pure builtinProviders
        Just apiKey -> do
            -- Fetch models from OpenRouter
            client <- OpenRouter.newClient apiKey
            result <- OpenRouter.fetchModels client
            case result of
                Left _err -> pure builtinProviders -- Fall back to empty models on error
                Right models -> do
                    let modelMap = Map.fromList [(modelId m, m) | m <- models]
                    -- Update the openrouter provider with fetched models
                    pure
                        [ if providerId p == "openrouter"
                            then p{providerModels = modelMap}
                            else p
                        | p <- builtinProviders
                        ]

{- | Get API key for a provider
Checks environment variable first, then falls back to stored auth.
-}
getApiKey :: Storage.StorageConfig -> Text -> IO (Maybe Text)
getApiKey storage providerID = do
    -- Check environment variable first
    mProvider <- get providerID
    envKey <- case mProvider of
        Just p -> do
            keys <- mapM (lookupEnv . T.unpack) (providerEnv p)
            pure $ case [k | Just k <- keys, not (null k)] of
                (k : _) -> Just (T.pack k)
                [] -> Nothing
        Nothing -> pure Nothing
    case envKey of
        Just k -> pure (Just k)
        Nothing -> do
            -- Fall back to stored auth
            stored <-
                Control.Exception.catch
                    (Just <$> (Storage.read storage ["auth", providerID] :: IO Value))
                    (\(_ :: Control.Exception.SomeException) -> pure Nothing)
            pure $ stored >>= extractToken

-- | Extract token from stored auth value
extractToken :: Value -> Maybe Text
extractToken (Object obj) = case KM.lookup "token" obj of
    Just (String t) -> Just t
    Just _otherValue -> Nothing
    Nothing -> Nothing
extractToken _otherValue = Nothing

-- | Get a provider by ID
get :: Text -> IO (Maybe Provider)
get pid = do
    providers <- list
    pure $ lookup pid [(providerId p, p) | p <- providers]

-- | Get a model by provider and model ID
getModel :: Text -> Text -> IO (Maybe Model)
getModel providerID modelID = do
    mprovider <- get providerID
    case mprovider of
        Nothing -> pure Nothing
        Just provider -> pure $ Map.lookup modelID (providerModels provider)

-- | Get auth status for all providers
authStatus :: Storage.StorageConfig -> IO [ProviderAuth]
authStatus storage = do
    providers <- list
    mapM (checkAuth storage) providers

-- | Check auth for a single provider
checkAuth :: Storage.StorageConfig -> Provider -> IO ProviderAuth
checkAuth storage provider = do
    stored <-
        Control.Exception.catch
            (Just <$> (Storage.read storage ["auth", providerId provider] :: IO Value))
            -- Catch NotFoundError and any other exceptions (including JSON decode errors)
            (\(_ :: Control.Exception.SomeException) -> pure Nothing)
    envAuth <- anyM hasEnv (providerEnv provider)
    let storedMethod = stored >>= extractMethod
    let hasAuth = isJust stored || envAuth
    let method =
            case storedMethod of
                Just m -> Just m
                Nothing ->
                    if isJust stored
                        then Just "api_key"
                        else
                            if envAuth
                                then Just "env"
                                else Nothing
    pure $
        ProviderAuth
            { paProviderID = providerId provider
            , paAuthenticated = hasAuth
            , paMethod = method
            }

extractMethod :: Value -> Maybe Text
extractMethod (Object obj) = case KM.lookup "method" obj of
    Just (String t) -> Just t
    Just _otherValue -> Nothing
    Nothing -> Nothing
extractMethod _otherValue = Nothing

hasEnv :: Text -> IO Bool
hasEnv key = do
    val <- lookupEnv (T.unpack key)
    pure $ case val of
        Nothing -> False
        Just "" -> False
        Just _otherValue -> True

anyM :: (a -> IO Bool) -> [a] -> IO Bool
anyM _ [] = pure False
anyM f (x : xs) = do
    ok <- f x
    if ok then pure True else anyM f xs

-- | List connected provider IDs (those with env auth OR stored auth)
listConnected :: Storage.StorageConfig -> IO [Text]
listConnected storage = do
    providers <- list
    let providerIds = map providerId providers
    -- Check which providers have API keys (either from env or storage)
    filterM (fmap isJust . getApiKey storage) providerIds

-- | Set auth for a provider
setAuth :: Storage.StorageConfig -> Text -> Text -> IO ()
setAuth storage providerID token =
    Storage.write storage ["auth", providerID] (object ["token" .= token, "method" .= ("api_key" :: Text)])

-- | Remove auth for a provider
removeAuth :: Storage.StorageConfig -> Text -> IO ()
removeAuth storage providerID =
    Storage.remove storage ["auth", providerID]
