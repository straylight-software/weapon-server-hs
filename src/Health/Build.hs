module Health.Build (
    buildHealth,
) where

import Api (Health (..))
import Data.Text (Text)

buildHealth :: Text -> Health
buildHealth = Health True
