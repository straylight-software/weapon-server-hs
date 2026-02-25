{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Formatter.Status (
    FormatterStatus (..),
    statusFor,
    statusForConfig,
    baseFormatters,
    formattersFor,

    -- * Executable cache (re-exported from Util.ExeCache)
    ExeCache,
    newExeCache,
)
where

import Config.Config qualified as Config
import Config.Types qualified as CT
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import Util.ExeCache (ExeCache, findExecutableCached, newExeCache)

data FormatterInfo = FormatterInfo
    { fiName :: Text
    , fiExtensions :: [Text]
    , fiExeName :: String
    -- ^ Executable name to check
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

-- | Get formatter status (requires DhallCache and ExeCache)
statusFor :: Config.DhallCache -> ExeCache -> FilePath -> IO [FormatterStatus]
statusFor dhallCache exeCache dir = do
    cfg <- Config.load dhallCache dir
    statusForConfig exeCache dir cfg

statusForConfig :: ExeCache -> FilePath -> CT.Config -> IO [FormatterStatus]
statusForConfig exeCache _dir cfg = mapM (toStatus exeCache) (formattersFor cfg)

toStatus :: ExeCache -> FormatterInfo -> IO FormatterStatus
toStatus exeCache info = do
    enabled <- isJust <$> findExecutableCached exeCache (fiExeName info)
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
        , fiExeName = "prettier"
        }
    , FormatterInfo
        { fiName = "black"
        , fiExtensions = [".py"]
        , fiExeName = "black"
        }
    , FormatterInfo
        { fiName = "gofmt"
        , fiExtensions = [".go"]
        , fiExeName = "gofmt"
        }
    , FormatterInfo
        { fiName = "rustfmt"
        , fiExtensions = [".rs"]
        , fiExeName = "rustfmt"
        }
    ]
