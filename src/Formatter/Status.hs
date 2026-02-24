{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Formatter.Status (
    FormatterStatus (..),
    statusFor,
    statusForConfig,
    baseFormatters,
    formattersFor,
)
where

import Config.Config qualified as Config
import Config.Types qualified as CT
import Data.Aeson (ToJSON (..), object, (.=))

import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Directory (findExecutable)

data FormatterInfo = FormatterInfo
    { fiName :: Text
    , fiExtensions :: [Text]
    , fiEnabled :: FilePath -> IO Bool
    }

data FormatterStatus = FormatterStatus
    { fsName :: Text
    , fsExtensions :: [Text]
    , fsEnabled :: Bool
    }
    deriving (Show, Eq, Generic)

instance ToJSON FormatterStatus where
    toJSON status =
        object
            [ "name" .= fsName status
            , "extensions" .= fsExtensions status
            , "enabled" .= fsEnabled status
            ]

-- | Get formatter status (requires DhallCache)
statusFor :: Config.DhallCache -> FilePath -> IO [FormatterStatus]
statusFor cache dir = do
    cfg <- Config.load cache dir
    statusForConfig dir cfg

statusForConfig :: FilePath -> CT.Config -> IO [FormatterStatus]
statusForConfig dir cfg = mapM (toStatus dir) (formattersFor cfg)

toStatus :: FilePath -> FormatterInfo -> IO FormatterStatus
toStatus dir info = do
    enabled <- fiEnabled info dir
    pure $
        FormatterStatus
            { fsName = fiName info
            , fsExtensions = fiExtensions info
            , fsEnabled = enabled
            }

formattersFor :: CT.Config -> [FormatterInfo]
formattersFor cfg =
    case CT.cfgFormatter cfg of
        Nothing -> baseFormatters
        Just fmtCfg -> applyConfig fmtCfg baseFormatters

applyConfig :: CT.FormatterConfig -> [FormatterInfo] -> [FormatterInfo]
applyConfig CT.FormatterDisabled _infos = [] -- All formatters disabled
applyConfig (CT.FormatterEnabled _enabledMap) infos = infos -- Config specifies which are enabled, but we just check executables

baseFormatters :: [FormatterInfo]
baseFormatters =
    [ FormatterInfo
        { fiName = "prettier"
        , fiExtensions = [".js", ".ts", ".jsx", ".tsx", ".json", ".css", ".html", ".md"]
        , fiEnabled = checkExe "prettier"
        }
    , FormatterInfo
        { fiName = "black"
        , fiExtensions = [".py"]
        , fiEnabled = checkExe "black"
        }
    , FormatterInfo
        { fiName = "gofmt"
        , fiExtensions = [".go"]
        , fiEnabled = checkExe "gofmt"
        }
    , FormatterInfo
        { fiName = "rustfmt"
        , fiExtensions = [".rs"]
        , fiEnabled = checkExe "rustfmt"
        }
    ]

checkExe :: String -> FilePath -> IO Bool
checkExe name _dir = isJust <$> findExecutable name
