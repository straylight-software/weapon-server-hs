{-# LANGUAGE OverloadedStrings #-}

-- | OpenAPI schema-based property tests using haskemathesis
--
-- This module runs generated property tests against the server's WAI application
-- to verify compliance with the OpenAPI specification.
module Integration.HaskemathesisTest (tests) where

import Api (api)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Data.Text qualified as T
import Handlers (server)
import Haskemathesis.Config (TestConfig (..), defaultTestConfig)
import Haskemathesis.Execute.Types (ExecutorWithTimeout)
import Haskemathesis.Execute.Wai (executeWaiWithTimeout)
import Haskemathesis.Integration.Tasty (testTreeForExecutorWithConfig, testTreeForExecutorNegative)
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Log qualified
import Middleware (supplyEmptyBody)
import Network.Wai (Application)
import Servant (serve)
import State (initialStateNoProxyWithHome)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)

-- | Path to the OpenAPI spec
openApiSpecPath :: FilePath
openApiSpecPath = "./sdk/openapi.json"

-- | Endpoints to skip (WebSocket and SSE endpoints that can't be tested with WAI)
skipEndpoints :: [Text]
skipEndpoints =
  [ "pty.connect"       -- WebSocket (uses Upgrade header)
  , "event.subscribe"   -- SSE streaming endpoint (text/event-stream)
  , "global.event"      -- SSE streaming endpoint (text/event-stream)
  , "session.subscribe" -- SSE streaming endpoint
  ]

-- | Filter out non-testable endpoints
operationFilter :: ResolvedOperation -> Bool
operationFilter op =
  case roOperationId op of
    Just opId -> opId `notElem` skipEndpoints
    Nothing ->
      -- For operations without IDs, check path
      roPath op /= "/pty/{ptyID}/connect"

-- | Global counter for unique storage directories
storageCounter :: IORef Int
storageCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE storageCounter #-}

-- | Create a test WAI application with isolated state
--
-- Each call creates a unique storage directory to avoid file lock conflicts
-- when tests run in parallel. The storage dir is also used as the project
-- directory AND home directory to isolate all config file access.
createTestApp :: IO Application
createTestApp = do
  cwd <- getCurrentDirectory
  -- Get unique ID for this test instance
  uniqueId <- atomicModifyIORef' storageCounter (\n -> (n + 1, n))
  let storageDir = cwd </> ".opencode-test" </> "haskemathesis" </> show uniqueId
  createDirectoryIfMissing True storageDir
  -- Create .config/weapon directory for global config isolation
  createDirectoryIfMissing True (storageDir </> ".config" </> "weapon")

  -- Create a persistent logger (not using withLogger bracket pattern)
  logger <- Log.newLogger "test"
  -- Use storageDir as storage, project dir, AND home dir to fully isolate config files
  state <- initialStateNoProxyWithHome (Just storageDir) storageDir "test_project" (T.pack storageDir) logger
  pure $ supplyEmptyBody $ serve api (server state)

-- | Create an executor with a unique app instance
-- Each executor has its own app, so different tests (properties) don't conflict
createExecutorForOperation :: IO ExecutorWithTimeout
createExecutorForOperation = do
  app <- createTestApp
  pure $ executeWaiWithTimeout app

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
--
-- We create a separate executor (with unique storage) for each operation
-- to avoid file lock conflicts when tests run in parallel.
tests :: TestTree
tests = unsafePerformIO $ do
  specResult <- loadOpenApiFile openApiSpecPath
  case specResult of
    Left err -> error $ "Failed to load OpenAPI spec: " <> show err
    Right openApi -> do
      let operations = resolveOperations openApi
          filteredOps = filter operationFilter operations
      -- Create test trees for each operation with isolated storage
      positiveTrees <- mapM (makeOperationTest openApi positiveConfig) filteredOps
      negativeTrees <- mapM (makeOperationTestNegative openApi negativeConfig) filteredOps
      pure $
        testGroup
          "Haskemathesis OpenAPI Compliance"
          [ testGroup "OpenAPI Conformance" positiveTrees,
            testGroup "OpenAPI Conformance (Negative)" negativeTrees
          ]
{-# NOINLINE tests #-}

-- | Create a test for a single operation with isolated storage
makeOperationTest :: OpenApi -> TestConfig -> ResolvedOperation -> IO TestTree
makeOperationTest openApi config op = do
  executor <- createExecutorForOperation
  pure $ testTreeForExecutorWithConfig openApi config executor [op]

-- | Create a negative test for a single operation with isolated storage
makeOperationTestNegative :: OpenApi -> TestConfig -> ResolvedOperation -> IO TestTree
makeOperationTestNegative openApi config op = do
  executor <- createExecutorForOperation
  pure $ testTreeForExecutorNegative openApi config executor [op]
