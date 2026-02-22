{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Provider type definitions
Mirrors the TypeScript Provider namespace, matching the inline schema
used by provider.list endpoint.
-}
module Provider.Types (
    Provider (..),
    Model (..),
    ModelCost (..),
    ModelLimit (..),
    ModelInterleaved (..),
    ModelModalities (..),
    ModelProvider (..),
    ProviderAuth (..),
    AuthMethod (..),
    -- * Smart constructors
    defaultModel,
)
where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Helper for optional JSON fields: converts Maybe value to optional key-value pair
optField :: ToJSON v => Key -> Maybe v -> Maybe Pair
optField k = fmap (k .=)

-- | Model cost information (per million tokens)
-- Flat structure with cache_read/cache_write per provider.list schema
data ModelCost = ModelCost
    { mcInput :: Double
    , mcOutput :: Double
    , mcCacheRead :: Maybe Double
    , mcCacheWrite :: Maybe Double
    , mcContextOver200k :: Maybe ModelCost  -- recursive for nested cost
    }
    deriving (Show, Eq, Generic)

instance ToJSON ModelCost where
    toJSON mc =
        object $
            [ "input" .= mcInput mc
            , "output" .= mcOutput mc
            ] ++ catMaybes
                [ optField "cache_read" (mcCacheRead mc)
                , optField "cache_write" (mcCacheWrite mc)
                , optField "context_over_200k" (mcContextOver200k mc)
                ]

instance FromJSON ModelCost where
    parseJSON = withObject "ModelCost" $ \v ->
        ModelCost
            <$> v .: "input"
            <*> v .: "output"
            <*> v .:? "cache_read"
            <*> v .:? "cache_write"
            <*> v .:? "context_over_200k"

-- | Model limits
data ModelLimit = ModelLimit
    { mlContext :: Int
    , mlInput :: Maybe Int
    , mlOutput :: Int
    }
    deriving (Show, Eq, Generic)

instance ToJSON ModelLimit where
    toJSON ml =
        object $
            [ "context" .= mlContext ml
            , "output" .= mlOutput ml
            ] ++ catMaybes [optField "input" (mlInput ml)]

instance FromJSON ModelLimit where
    parseJSON = withObject "ModelLimit" $ \v ->
        ModelLimit
            <$> v .: "context"
            <*> v .:? "input"
            <*> v .: "output"

-- | Interleaved can be bool or object with field
data ModelInterleaved
    = InterleavedBool Bool
    | InterleavedField Text  -- "reasoning_content" or "reasoning_details"
    deriving (Show, Eq, Generic)

instance ToJSON ModelInterleaved where
    toJSON (InterleavedBool b) = Bool b
    toJSON (InterleavedField f) = object ["field" .= f]

instance FromJSON ModelInterleaved where
    parseJSON (Bool b) = pure $ InterleavedBool b
    parseJSON v = withObject "ModelInterleaved" (\o -> InterleavedField <$> o .: "field") v

-- | Model modalities
data ModelModalities = ModelModalities
    { mmInput :: [Text]   -- ["text", "audio", "image", "video", "pdf"]
    , mmOutput :: [Text]
    }
    deriving (Show, Eq, Generic)

instance ToJSON ModelModalities where
    toJSON m =
        object
            [ "input" .= mmInput m
            , "output" .= mmOutput m
            ]

instance FromJSON ModelModalities where
    parseJSON = withObject "ModelModalities" $ \v ->
        ModelModalities
            <$> v .: "input"
            <*> v .: "output"

-- | Model provider reference
data ModelProvider = ModelProvider
    { mpNpm :: Maybe Text
    , mpApi :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON ModelProvider where
    toJSON p =
        object $ catMaybes
            [ optField "npm" (mpNpm p)
            , optField "api" (mpApi p)
            ]

instance FromJSON ModelProvider where
    parseJSON = withObject "ModelProvider" $ \v ->
        ModelProvider
            <$> v .:? "npm"
            <*> v .:? "api"

-- | Model information (matching provider.list inline schema)
data Model = Model
    { modelId :: Text                             -- required
    , modelName :: Text                           -- required
    , modelReleaseDate :: Text                    -- required
    , modelAttachment :: Bool                     -- required
    , modelReasoning :: Bool                      -- required
    , modelTemperature :: Bool                    -- required
    , modelToolCall :: Bool                       -- required
    , modelLimit :: ModelLimit                    -- required
    , modelOptions :: Map.Map Text Value          -- required (can be empty)
    -- Optional fields below
    , modelFamily :: Maybe Text
    , modelInterleaved :: Maybe ModelInterleaved
    , modelCost :: Maybe ModelCost
    , modelModalities :: Maybe ModelModalities
    , modelExperimental :: Maybe Bool
    , modelStatus :: Maybe Text                   -- "alpha" | "beta" | "deprecated"
    , modelHeaders :: Maybe (Map.Map Text Text)
    , modelProvider :: Maybe ModelProvider
    , modelVariants :: Maybe (Map.Map Text (Map.Map Text Value))
    }
    deriving (Show, Eq, Generic)

instance ToJSON Model where
    toJSON m =
        object $
            [ "id" .= modelId m
            , "name" .= modelName m
            , "release_date" .= modelReleaseDate m
            , "attachment" .= modelAttachment m
            , "reasoning" .= modelReasoning m
            , "temperature" .= modelTemperature m
            , "tool_call" .= modelToolCall m
            , "limit" .= modelLimit m
            , "options" .= modelOptions m
            ] ++ catMaybes
                [ optField "family" (modelFamily m)
                , optField "interleaved" (modelInterleaved m)
                , optField "cost" (modelCost m)
                , optField "modalities" (modelModalities m)
                , optField "experimental" (modelExperimental m)
                , optField "status" (modelStatus m)
                , optField "headers" (modelHeaders m)
                , optField "provider" (modelProvider m)
                , optField "variants" (modelVariants m)
                ]

instance FromJSON Model where
    parseJSON = withObject "Model" $ \v ->
        Model
            <$> v .: "id"
            <*> v .: "name"
            <*> v .:? "release_date" .!= ""
            <*> v .:? "attachment" .!= False
            <*> v .:? "reasoning" .!= False
            <*> v .:? "temperature" .!= True
            <*> v .:? "tool_call" .!= False
            <*> v .:? "limit" .!= ModelLimit 0 Nothing 0
            <*> v .:? "options" .!= Map.empty
            <*> v .:? "family"
            <*> v .:? "interleaved"
            <*> v .:? "cost"
            <*> v .:? "modalities"
            <*> v .:? "experimental"
            <*> v .:? "status"
            <*> v .:? "headers"
            <*> v .:? "provider"
            <*> v .:? "variants"

-- | Auth method for a provider
data AuthMethod = AuthMethod
    { amType :: Text -- "api_key" | "oauth"
    , amEnvVars :: [Text]
    , amUrl :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON AuthMethod where
    toJSON am =
        object
            [ "type" .= amType am
            , "envVars" .= amEnvVars am
            , "url" .= amUrl am
            ]

instance FromJSON AuthMethod where
    parseJSON = withObject "AuthMethod" $ \v ->
        AuthMethod
            <$> v .: "type"
            <*> v .:? "envVars" .!= []
            <*> v .:? "url"

-- | Provider information (matching provider.list inline schema)
-- Required: name, env, id, models
data Provider = Provider
    { providerId :: Text
    , providerName :: Text
    , providerEnv :: [Text]
    , providerModels :: Map.Map Text Model
    -- Optional fields
    , providerApi :: Maybe Text
    , providerNpm :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON Provider where
    toJSON p =
        object $
            [ "id" .= providerId p
            , "name" .= providerName p
            , "env" .= providerEnv p
            , "models" .= providerModels p
            ] ++ catMaybes
                [ optField "api" (providerApi p)
                , optField "npm" (providerNpm p)
                ]

instance FromJSON Provider where
    parseJSON = withObject "Provider" $ \v ->
        Provider
            <$> v .: "id"
            <*> v .: "name"
            <*> v .:? "env" .!= []
            <*> v .:? "models" .!= Map.empty
            <*> v .:? "api"
            <*> v .:? "npm"

-- | Provider auth status
data ProviderAuth = ProviderAuth
    { paProviderID :: Text
    , paAuthenticated :: Bool
    , paMethod :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON ProviderAuth where
    toJSON pa =
        object
            [ "providerID" .= paProviderID pa
            , "authenticated" .= paAuthenticated pa
            , "method" .= paMethod pa
            ]

instance FromJSON ProviderAuth where
    parseJSON = withObject "ProviderAuth" $ \v ->
        ProviderAuth
            <$> v .: "providerID"
            <*> v .: "authenticated"
            <*> v .:? "method"

-- | Smart constructor for Model with sensible defaults for optional fields
-- Required: id, name, releaseDate, limit
-- Defaults: attachment=True, reasoning=False, temperature=True, toolCall=True
defaultModel :: Text -> Text -> Text -> ModelLimit -> Model
defaultModel mid name releaseDate limit = Model
    { modelId = mid
    , modelName = name
    , modelReleaseDate = releaseDate
    , modelAttachment = True
    , modelReasoning = False
    , modelTemperature = True
    , modelToolCall = True
    , modelLimit = limit
    , modelOptions = Map.empty
    , modelFamily = Nothing
    , modelInterleaved = Nothing
    , modelCost = Nothing
    , modelModalities = Nothing
    , modelExperimental = Nothing
    , modelStatus = Nothing
    , modelHeaders = Nothing
    , modelProvider = Nothing
    , modelVariants = Nothing
    }
