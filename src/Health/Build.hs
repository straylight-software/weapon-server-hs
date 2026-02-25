-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                  // weapon-server // health
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--

{- | Health check endpoint builder.

Constructs the 'Health' response for the @\/global\/health@ endpoint.
This module is intentionally pure and simple - the health response
indicates whether the server process is running and returns the
current version string.

== Usage

@
import Health.Build (buildHealth)
import Api (Health(..))

-- In your handler:
healthHandler :: Text -> Health
healthHandler version = buildHealth version
@

== Design Notes

The server is considered "healthy" if it can respond to the health
endpoint at all. The 'healthy' field is always 'True' because an
unhealthy server wouldn't be able to respond. More sophisticated
health checks (database connectivity, external service availability)
would be added here if needed.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-}
module Health.Build (
    -- * Health Response Construction
    buildHealth,

    -- * Constants
    defaultHealthy,
) where

import Api (Health (..))
import Data.Text (Text)

{- | The default healthy status for a running server.

A server that can respond to health checks is, by definition, healthy.
This constant exists for documentation and potential future extension
where we might want different health states.
-}
defaultHealthy :: Bool
defaultHealthy = True

{- | Construct a 'Health' response with the given version string.

The returned 'Health' value indicates that the server is running
and responding to requests. The version string is passed through
unchanged to allow clients to verify they're communicating with
the expected server version.

==== __Examples__

>>> buildHealth "1.0.0"
Health {healthy = True, version = "1.0.0"}

>>> buildHealth ""
Health {healthy = True, version = ""}

==== __Properties__

prop> healthy (buildHealth v) == True
prop> version (buildHealth v) == v
-}
buildHealth :: Text -> Health
buildHealth = Health defaultHealthy
