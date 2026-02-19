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
)
where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)

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
            ] ++ maybe [] (\v -> ["cache_read" .= v]) (mcCacheRead mc)
              ++ maybe [] (\v -> ["cache_write" .= v]) (mcCacheWrite mc)
              ++ maybe [] (\v -> ["context_over_200k" .= v]) (mcContextOver200k mc)

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
            ] ++ maybe [] (\i -> ["input" .= i]) (mlInput ml)

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
        object $
            maybe [] (\v -> ["npm" .= v]) (mpNpm p)
            ++ maybe [] (\v -> ["api" .= v]) (mpApi p)

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
            ] ++ maybe [] (\f -> ["family" .= f]) (modelFamily m)
              ++ maybe [] (\i -> ["interleaved" .= i]) (modelInterleaved m)
              ++ maybe [] (\c -> ["cost" .= c]) (modelCost m)
              ++ maybe [] (\x -> ["modalities" .= x]) (modelModalities m)
              ++ maybe [] (\e -> ["experimental" .= e]) (modelExperimental m)
              ++ maybe [] (\s -> ["status" .= s]) (modelStatus m)
              ++ maybe [] (\h -> ["headers" .= h]) (modelHeaders m)
              ++ maybe [] (\p -> ["provider" .= p]) (modelProvider m)
              ++ maybe [] (\v -> ["variants" .= v]) (modelVariants m)

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
            ] ++ maybe [] (\a -> ["api" .= a]) (providerApi p)
              ++ maybe [] (\n -> ["npm" .= n]) (providerNpm p)

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
