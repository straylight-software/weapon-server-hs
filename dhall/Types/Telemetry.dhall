-- Telemetry.dhall
-- Telemetry and R2 storage configuration
-- Field names match Haskell record field names exactly

-- R2 storage config record type (matches Config.Types.R2StorageConfig)
let R2Config =
      { r2sAccountId : Optional Text
      , r2sAccessKeyId : Optional Text
      , r2sSecretKey : Optional Text
      , r2sBucket : Optional Text
      , r2sPrefix : Optional Text
      , r2sEndpoint : Optional Text
      }

-- TelemetryConfig is now a newtype around R2StorageConfig
-- Presence = enabled, absence = disabled
let Telemetry =
      { Type = { telR2 : R2Config }
      }

in  { Telemetry, R2Config }
