{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Provider.Types
Description : Type definitions for AI providers and models
Stability   : experimental

This module defines the core data types for AI provider management,
including providers, models, costs, limits, and authentication status.
All types are designed to match the inline schema used by the provider.list
endpoint, mirroring the TypeScript Provider namespace.

== Overview

The type hierarchy is:

* 'Provider' - A provider (e.g., Anthropic, OpenAI) containing models
* 'Model' - A specific model with capabilities, limits, and costs
* 'ModelCost' - Token pricing information
* 'ModelLimit' - Context and output token limits
* 'ProviderAuth' - Authentication status for a provider
-}
module Provider.Types (
    -- * Provider types
    Provider (..),

    -- * Model types
    Model (..),
    ModelCost (..),
    ModelLimit (..),
    ModelInterleaved (..),
    ModelModalities (..),
    ModelProvider (..),

    -- * Authentication types
    ProviderAuth (..),
    AuthMethod (..),

    -- * Smart constructors
    defaultModel,

    -- * JSON helpers
    optField,
)
where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Helper for optional JSON fields: converts Maybe value to optional key-value pair.

This is useful for building JSON objects with optional fields without
including 'null' values. Use with 'catMaybes' when constructing object pairs.

@
instance ToJSON MyType where
    toJSON x = object $ ["required" .= requiredField x]
        ++ catMaybes [optField "optional" (optionalField x)]
@
-}
optField :: (ToJSON v) => Key -> Maybe v -> Maybe Pair
optField k = fmap (k .=)

{- | Model cost information (per million tokens).

Represents the pricing structure for a model's token usage.
All costs are expressed in dollars per million tokens.

* 'mcInput' - Cost per million input tokens
* 'mcOutput' - Cost per million output tokens
* 'mcCacheRead' - Optional reduced cost for cached input tokens (prompt caching)
* 'mcCacheWrite' - Optional cost for writing to cache
* 'mcContextOver200k' - Optional different pricing for contexts over 200k tokens
-}
data ModelCost = ModelCost
    { mcInput :: Double
    -- ^ Cost per million input tokens (required)
    , mcOutput :: Double
    -- ^ Cost per million output tokens (required)
    , mcCacheRead :: Maybe Double
    -- ^ Optional reduced cost for reading cached tokens
    , mcCacheWrite :: Maybe Double
    -- ^ Optional cost for writing to cache
    , mcContextOver200k :: Maybe ModelCost
    -- ^ Optional nested cost structure for large contexts (recursive)
    }
    deriving (Show, Eq, Generic)

instance ToJSON ModelCost where
    toJSON mc =
        object $
            [ "input" .= mcInput mc
            , "output" .= mcOutput mc
            ]
                ++ catMaybes
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

{- | Model token limits.

Defines the maximum token counts for a model's context window
and input/output constraints.
-}
data ModelLimit = ModelLimit
    { mlContext :: Int
    -- ^ Maximum context window size in tokens
    , mlInput :: Maybe Int
    -- ^ Optional maximum input token limit (if different from context)
    , mlOutput :: Int
    -- ^ Maximum output token limit
    }
    deriving (Show, Eq, Generic)

instance ToJSON ModelLimit where
    toJSON ml =
        object $
            [ "context" .= mlContext ml
            , "output" .= mlOutput ml
            ]
                ++ catMaybes [optField "input" (mlInput ml)]

instance FromJSON ModelLimit where
    parseJSON = withObject "ModelLimit" $ \v ->
        ModelLimit
            <$> v .: "context"
            <*> v .:? "input"
            <*> v .: "output"

{- | Interleaved reasoning configuration.

Controls how reasoning/thinking content is interleaved with regular output.
Can be a simple boolean toggle or specify a particular field name.

* 'InterleavedBool' - Simple on/off toggle for interleaved reasoning
* 'InterleavedField' - Specify the field name (e.g., @"reasoning_content"@ or @"reasoning_details"@)
-}
data ModelInterleaved
    = -- | Simple boolean toggle
      InterleavedBool Bool
    | -- | Field name for reasoning content
      InterleavedField Text
    deriving (Show, Eq, Generic)

instance ToJSON ModelInterleaved where
    toJSON (InterleavedBool b) = Bool b
    toJSON (InterleavedField f) = object ["field" .= f]

instance FromJSON ModelInterleaved where
    parseJSON (Bool b) = pure $ InterleavedBool b
    parseJSON v = withObject "ModelInterleaved" (\o -> InterleavedField <$> o .: "field") v

{- | Model input/output modalities.

Specifies what types of content a model can accept as input
and produce as output.

Common modalities include: @"text"@, @"audio"@, @"image"@, @"video"@, @"pdf"@
-}
data ModelModalities = ModelModalities
    { mmInput :: [Text]
    -- ^ Supported input modalities (e.g., @["text", "image", "pdf"]@)
    , mmOutput :: [Text]
    -- ^ Supported output modalities (e.g., @["text"]@)
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

{- | Model provider reference for SDK integration.

Contains optional references to the provider's npm package
and API endpoint for direct integration.
-}
data ModelProvider = ModelProvider
    { mpNpm :: Maybe Text
    -- ^ Optional npm package name (e.g., @"\@anthropic-ai/sdk"@)
    , mpApi :: Maybe Text
    -- ^ Optional API endpoint URL
    }
    deriving (Show, Eq, Generic)

instance ToJSON ModelProvider where
    toJSON p =
        object $
            catMaybes
                [ optField "npm" (mpNpm p)
                , optField "api" (mpApi p)
                ]

instance FromJSON ModelProvider where
    parseJSON = withObject "ModelProvider" $ \v ->
        ModelProvider
            <$> v .:? "npm"
            <*> v .:? "api"

{- | Complete model information.

Represents an AI model with all its capabilities, constraints, and metadata.
Matches the provider.list inline schema from the OpenCode API.

Required fields: 'modelId', 'modelName', 'modelReleaseDate', 'modelLimit'
Optional fields: costs, modalities, provider references, etc.
-}
data Model = Model
    { modelId :: Text
    -- ^ Unique model identifier (required)
    , modelName :: Text
    -- ^ Human-readable model name (required)
    , modelReleaseDate :: Text
    -- ^ Release date in YYYY-MM-DD format (required)
    , modelAttachment :: Bool
    -- ^ Whether the model supports file attachments (required)
    , modelReasoning :: Bool
    -- ^ Whether the model supports reasoning/thinking (required)
    , modelTemperature :: Bool
    -- ^ Whether temperature parameter is supported (required)
    , modelToolCall :: Bool
    -- ^ Whether the model supports tool/function calling (required)
    , modelLimit :: ModelLimit
    -- ^ Token limits for the model (required)
    , modelOptions :: Map.Map Text Value
    -- ^ Additional model-specific options (required, can be empty)
    , modelFamily :: Maybe Text
    -- ^ Model family (e.g., @"claude"@, @"gpt"@, @"o"@)
    , modelInterleaved :: Maybe ModelInterleaved
    -- ^ Interleaved reasoning configuration
    , modelCost :: Maybe ModelCost
    -- ^ Token pricing information
    , modelModalities :: Maybe ModelModalities
    -- ^ Supported input/output modalities
    , modelExperimental :: Maybe Bool
    -- ^ Whether this is an experimental model
    , modelStatus :: Maybe Text
    -- ^ Model status: @"alpha"@, @"beta"@, or @"deprecated"@
    , modelHeaders :: Maybe (Map.Map Text Text)
    -- ^ Custom HTTP headers for API requests
    , modelProvider :: Maybe ModelProvider
    -- ^ Provider SDK/API references
    , modelVariants :: Maybe (Map.Map Text (Map.Map Text Value))
    -- ^ Model variants with different configurations
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
            ]
                ++ catMaybes
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

{- | Authentication method configuration for a provider.

Describes how to authenticate with a provider, including
the type of authentication, relevant environment variables,
and optional OAuth URLs.
-}
data AuthMethod = AuthMethod
    { amType :: Text
    -- ^ Authentication type: @"api_key"@ or @"oauth"@
    , amEnvVars :: [Text]
    -- ^ Environment variable names for API keys
    , amUrl :: Maybe Text
    -- ^ Optional OAuth authorization URL
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

{- | AI provider information.

Represents a provider like Anthropic, OpenAI, or OpenRouter,
including its available models and authentication configuration.
Matches the provider.list inline schema from the OpenCode API.

Required fields: 'providerId', 'providerName', 'providerEnv', 'providerModels'
-}
data Provider = Provider
    { providerId :: Text
    -- ^ Unique provider identifier (e.g., @"anthropic"@, @"openai"@)
    , providerName :: Text
    -- ^ Human-readable provider name (e.g., @"Anthropic"@, @"OpenAI"@)
    , providerEnv :: [Text]
    -- ^ Environment variable names for API keys
    , providerModels :: Map.Map Text Model
    -- ^ Available models, keyed by model ID
    , providerApi :: Maybe Text
    -- ^ Optional direct API endpoint URL
    , providerNpm :: Maybe Text
    -- ^ Optional npm package name for the provider SDK
    }
    deriving (Show, Eq, Generic)

instance ToJSON Provider where
    toJSON p =
        object $
            [ "id" .= providerId p
            , "name" .= providerName p
            , "env" .= providerEnv p
            , "models" .= providerModels p
            ]
                ++ catMaybes
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

{- | Provider authentication status.

Represents the current authentication state for a provider,
including whether it's authenticated and by what method.
-}
data ProviderAuth = ProviderAuth
    { paProviderID :: Text
    -- ^ Provider identifier this status refers to
    , paAuthenticated :: Bool
    -- ^ Whether the provider is currently authenticated
    , paMethod :: Maybe Text
    -- ^ Authentication method used: @"api_key"@, @"env"@, or @"oauth"@
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

{- | Smart constructor for 'Model' with sensible defaults.

Creates a model with the required fields and sensible defaults
for all optional fields, making it easy to define models with
minimal boilerplate.

==== Required parameters

* @mid@ - Unique model identifier
* @name@ - Human-readable model name
* @releaseDate@ - Release date in YYYY-MM-DD format
* @limit@ - Token limits

==== Default values

* @attachment@ = 'True' (supports file attachments)
* @reasoning@ = 'False' (no reasoning/thinking mode)
* @temperature@ = 'True' (supports temperature parameter)
* @toolCall@ = 'True' (supports tool/function calling)
* All optional fields = 'Nothing'

==== Example

@
let myModel = defaultModel "my-model" "My Model" "2024-01-01" (ModelLimit 128000 Nothing 4096)
    modelWithCost = myModel { modelCost = Just (ModelCost 1.0 3.0 Nothing Nothing Nothing) }
@
-}
defaultModel :: Text -> Text -> Text -> ModelLimit -> Model
defaultModel mid name releaseDate limit =
    Model
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
