{-# LANGUAGE OverloadedStrings #-}

-- | OpenAPI schema-based property tests using haskemathesis
--
-- This module runs generated property tests against the server's WAI application
-- to verify compliance with the OpenAPI specification.
module Integration.HaskemathesisTest (tests) where

import Api (api)
import Data.Text (Text)
import Data.Text qualified as T
import Handlers (server)
import Haskemathesis.Config (TestConfig (..), defaultTestConfig)
import Haskemathesis.Integration.Tasty (testTreeForAppNegative, testTreeForAppWithConfig)
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Log qualified
import Middleware (supplyEmptyBody)
import Network.Wai (Application)
import Servant (serve)
import State (initialState)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)

-- | Path to the OpenAPI spec
openApiSpecPath :: FilePath
openApiSpecPath = "./sdk/openapi.json"

-- | Endpoints to skip (WebSocket endpoints that can't be tested with WAI)
--
-- Note: SSE streaming endpoints (text/event-stream, application/x-ndjson) are
-- automatically skipped by haskemathesis WAI integration.
skipEndpoints :: [Text]
skipEndpoints =
  [ "pty.connect" -- WebSocket (uses Upgrade header, not content-type based)
  ]

-- | Filter out non-testable endpoints
operationFilter :: ResolvedOperation -> Bool
operationFilter op =
  case roOperationId op of
    Just opId -> opId `notElem` skipEndpoints
    Nothing ->
      -- For operations without IDs, check path
      roPath op /= "/pty/{ptyID}/connect"

-- | Create a test WAI application with isolated state
--
-- Note: We use newLogger directly instead of withLogger because
-- withLogger uses bracket which would close the logger before
-- the Application is actually used by the tests.
createTestApp :: IO Application
createTestApp = do
  cwd <- getCurrentDirectory
  let storageDir = cwd </> ".opencode-test" </> "haskemathesis"
  createDirectoryIfMissing True storageDir

  -- Create a persistent logger (not using withLogger bracket pattern)
  logger <- Log.newLogger "test"
  state <- initialState storageDir "test_project" (T.pack cwd) logger
  pure $ supplyEmptyBody $ serve api (server state)

-- | Test configuration for positive tests (10,000 tests)
positiveConfig :: TestConfig
positiveConfig =
  defaultTestConfig
    { tcPropertyCount = 100,
      tcNegativeTesting = False,
      tcOperationFilter = operationFilter
    }

-- | Test configuration for negative tests (10,000 tests)
negativeConfig :: TestConfig
negativeConfig =
  defaultTestConfig
    { tcPropertyCount = 100,
      tcNegativeTesting = True,
      tcOperationFilter = operationFilter
    }

-- | All haskemathesis tests
tests :: TestTree
tests = unsafePerformIO $ do
  specResult <- loadOpenApiFile openApiSpecPath
  case specResult of
    Left err -> error $ "Failed to load OpenAPI spec: " <> show err
    Right openApi -> do
      app <- createTestApp
      pure $
        testGroup
          "Haskemathesis OpenAPI Compliance"
          [ testTreeForAppWithConfig positiveConfig openApi app,
            testTreeForAppNegative negativeConfig openApi app
          ]
{-# NOINLINE tests #-}
