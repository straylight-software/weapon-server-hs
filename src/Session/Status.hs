{-# LANGUAGE OverloadedStrings #-}

module Session.Status (
    SessionStatus (..),
    SessionStatusType (..),
) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)

-- | Session status type matching OpenAPI spec
data SessionStatusType
    = StatusIdle
    | StatusRetry Int Text Int  -- attempt, message, next
    | StatusActive Text  -- stepID
    deriving (Eq, Show)

-- | Session status for a single session
data SessionStatus = SessionStatus
    { ssType :: SessionStatusType
    }
    deriving (Eq, Show)

instance ToJSON SessionStatus where
    toJSON s = case ssType s of
        StatusIdle ->
            object ["type" .= ("idle" :: Text)]
        StatusRetry attempt msg next ->
            object
                [ "type" .= ("retry" :: Text)
                , "attempt" .= attempt
                , "message" .= msg
                , "next" .= next
                ]
        StatusActive stepID ->
            object
                [ "type" .= ("active" :: Text)
                , "stepID" .= stepID
                ]
