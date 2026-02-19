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
    get,
    getModel,
    authStatus,
    listConnected,
    setAuth,
    removeAuth,

    -- * Built-in providers
    builtinProviders,
) where

import Control.Exception qualified
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
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
                    , Model
                        { modelId = "claude-sonnet-4-20250514"
                        , modelName = "Claude Sonnet 4"
                        , modelReleaseDate = "2025-05-14"
                        , modelAttachment = True
                        , modelReasoning = True
                        , modelTemperature = True
                        , modelToolCall = True
                        , modelLimit = ModelLimit 200000 Nothing 16384
                        , modelOptions = Map.empty
                        , modelFamily = Just "claude"
                        , modelInterleaved = Nothing
                        , modelCost = Just $ ModelCost 3.0 15.0 (Just 0.3) (Just 3.75) Nothing
                        , modelModalities = Just $ ModelModalities ["text", "image", "pdf"] ["text"]
                        , modelExperimental = Nothing
                        , modelStatus = Nothing
                        , modelHeaders = Nothing
                        , modelProvider = Nothing
                        , modelVariants = Nothing
                        }
                    )
                ,
                    ( "claude-opus-4-20250514"
                    , Model
                        { modelId = "claude-opus-4-20250514"
                        , modelName = "Claude Opus 4"
                        , modelReleaseDate = "2025-05-14"
                        , modelAttachment = True
                        , modelReasoning = True
                        , modelTemperature = True
                        , modelToolCall = True
                        , modelLimit = ModelLimit 200000 Nothing 32000
                        , modelOptions = Map.empty
                        , modelFamily = Just "claude"
                        , modelInterleaved = Nothing
                        , modelCost = Just $ ModelCost 15.0 75.0 (Just 1.5) (Just 18.75) Nothing
                        , modelModalities = Just $ ModelModalities ["text", "image", "pdf"] ["text"]
                        , modelExperimental = Nothing
                        , modelStatus = Nothing
                        , modelHeaders = Nothing
                        , modelProvider = Nothing
                        , modelVariants = Nothing
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
                    , Model
                        { modelId = "gpt-4o"
                        , modelName = "GPT-4o"
                        , modelReleaseDate = "2024-05-13"
                        , modelAttachment = True
                        , modelReasoning = False
                        , modelTemperature = True
                        , modelToolCall = True
                        , modelLimit = ModelLimit 128000 Nothing 16384
                        , modelOptions = Map.empty
                        , modelFamily = Just "gpt"
                        , modelInterleaved = Nothing
                        , modelCost = Just $ ModelCost 2.5 10.0 Nothing Nothing Nothing
                        , modelModalities = Just $ ModelModalities ["text", "image"] ["text"]
                        , modelExperimental = Nothing
                        , modelStatus = Nothing
                        , modelHeaders = Nothing
                        , modelProvider = Nothing
                        , modelVariants = Nothing
                        }
                    )
                ,
                    ( "o3"
                    , Model
                        { modelId = "o3"
                        , modelName = "o3"
                        , modelReleaseDate = "2025-01-01"
                        , modelAttachment = True
                        , modelReasoning = True
                        , modelTemperature = False
                        , modelToolCall = True
                        , modelLimit = ModelLimit 200000 Nothing 100000
                        , modelOptions = Map.empty
                        , modelFamily = Just "o"
                        , modelInterleaved = Nothing
                        , modelCost = Just $ ModelCost 10.0 40.0 Nothing Nothing Nothing
                        , modelModalities = Just $ ModelModalities ["text", "image"] ["text"]
                        , modelExperimental = Nothing
                        , modelStatus = Nothing
                        , modelHeaders = Nothing
                        , modelProvider = Nothing
                        , modelVariants = Nothing
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

-- | List all providers
list :: IO [Provider]
list = pure builtinProviders

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
    let hasAuth = stored /= Nothing || envAuth
    let method =
            case storedMethod of
                Just m -> Just m
                Nothing ->
                    if stored /= Nothing
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
    _ -> Nothing
extractMethod _ = Nothing

hasEnv :: Text -> IO Bool
hasEnv key = do
    val <- lookupEnv (T.unpack key)
    pure $ case val of
        Nothing -> False
        Just "" -> False
        Just _ -> True

anyM :: (a -> IO Bool) -> [a] -> IO Bool
anyM _ [] = pure False
anyM f (x : xs) = do
    ok <- f x
    if ok then pure True else anyM f xs

-- | List connected provider IDs (those with stored auth)
listConnected :: Storage.StorageConfig -> IO [Text]
listConnected storage = do
    keys <- Storage.list storage ["auth"]
    -- Each key is ["auth", providerID], so extract the second element
    return [pid | [_, pid] <- keys]

-- | Set auth for a provider
setAuth :: Storage.StorageConfig -> Text -> Text -> IO ()
setAuth storage providerID token =
    Storage.write storage ["auth", providerID] (object ["token" .= token, "method" .= ("api_key" :: Text)])

-- | Remove auth for a provider
removeAuth :: Storage.StorageConfig -> Text -> IO ()
removeAuth storage providerID =
    Storage.remove storage ["auth", providerID]
