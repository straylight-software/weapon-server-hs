{-# LANGUAGE OverloadedStrings #-}

-- | OpenAPI schema-based property tests using haskemathesis
--
-- This module runs generated property tests against the server's WAI application
-- to verify compliance with the OpenAPI specification.
--
-- Key features:
-- - Each operation gets isolated storage to avoid file lock conflicts
-- - Pre-seeds sessions so session endpoints can be properly tested
-- - Rewrites random session IDs in requests to use pre-seeded sessions
module Integration.HaskemathesisTest (tests) where

import Api (api)

import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Data.Text qualified as T
import Handlers (server)
import Haskemathesis.Config (TestConfig (..), defaultTestConfig)
import Haskemathesis.Execute.Types (ApiRequest (..), ExecutorWithTimeout)
import Haskemathesis.Execute.Wai (executeWaiWithTimeout)
import Haskemathesis.Integration.Tasty (testTreeForExecutorWithConfig, testTreeForExecutorNegative)
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Log qualified
import Middleware (supplyEmptyBody)
import Network.Wai (Application)
import Servant (serveWithContext)
import Session.Session qualified as Session
import Session.Types (Session (..), SessionTime (..))
import Server.ErrorFormatters (errorFormattersContext)
import State (AppState (..), initialStateNoProxyWithHome)
import Storage.Storage qualified as Storage
import Util.StorageKeys (sessionKey)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, removeDirectoryRecursive)
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

-- | Known session ID that we pre-seed for testing
knownSessionId :: Text
knownSessionId = "ses_test_1"

-- | Known session IDs that we pre-seed for testing
knownSessionIds :: [Text]
knownSessionIds = [knownSessionId, "ses_test_2", "ses_test_3"]



-- | Create a test WAI application with isolated state and pre-seeded data
--
-- Each call creates a unique storage directory to avoid file lock conflicts
-- when tests run in parallel. The storage dir is also used as the project
-- directory AND home directory to isolate all config file access.
--
-- Pre-seeds sessions so session endpoints can be properly tested.
createTestApp :: IO (Application, AppState)
createTestApp = do
  cwd <- getCurrentDirectory
  -- Get unique ID for this test instance
  uniqueId <- atomicModifyIORef' storageCounter (\n -> (n + 1, n))
  let storageDir = cwd </> ".opencode-test" </> "haskemathesis" </> show uniqueId
  exists <- doesDirectoryExist storageDir
  when exists $ removeDirectoryRecursive storageDir
  createDirectoryIfMissing True storageDir
  -- Create .config/weapon directory for global config isolation
  createDirectoryIfMissing True (storageDir </> ".config" </> "weapon")

  -- Create a persistent logger (not using withLogger bracket pattern)
  logger <- Log.newLogger "test"
  -- Use storageDir as storage, project dir, AND home dir to fully isolate config files
  state <- initialStateNoProxyWithHome (Just storageDir) storageDir "test_project" (T.pack storageDir) logger

  -- Pre-seed sessions with known IDs
  preSeedSessions state

  let app = supplyEmptyBody $ serveWithContext api errorFormattersContext (server state)
  pure (app, state)

-- | Pre-seed sessions with known IDs for testing
preSeedSessions :: AppState -> IO ()
preSeedSessions state = do
  let ctx = Session.SessionContext
        { Session.scStorage = stStorage state
        , Session.scBus = stBus state
        , Session.scProjectID = stProjectID state
        , Session.scDirectory = stDirectory state
        , Session.scVersion = stVersion state
        }
  -- Create sessions with known IDs by directly writing to storage
  -- (bypassing the ID generation in Session.create)
  mapM_ (createSessionWithId ctx) knownSessionIds
  where
    createSessionWithId ctx sid = do
      let session = Session
            { sessionId = sid
            , sessionSlug = "test_slug"
            , sessionProjectID = Session.scProjectID ctx
            , sessionDirectory = Session.scDirectory ctx
            , sessionParentID = Nothing
            , sessionTitle = "Test Session " <> sid
            , sessionVersion = Session.scVersion ctx
            , sessionTime = SessionTime 0 0 Nothing Nothing
            , sessionSummary = Nothing
            , sessionShare = Nothing
            , sessionRevert = Nothing
            }
      Storage.write (Session.scStorage ctx) (sessionKey (Session.scProjectID ctx) sid) session

-- | Rewrite a request to use known session IDs instead of random ones
--
-- This intercepts requests to /session/{sessionID}/... and replaces the
-- random sessionID with one of our known pre-seeded session IDs.
-- Does NOT rewrite special paths like /session/status
rewriteSessionId :: ApiRequest -> ApiRequest
rewriteSessionId req =
  let path = reqPath req
      segments = T.splitOn "/" path
  in case segments of
    -- Skip literal paths that aren't session ID captures
    ("" : "session" : "status" : _) -> req
    -- /session/{sessionID}/... (with or without trailing path)
    ("" : "session" : _randomSid : rest) ->
      let newPath = T.intercalate "/" ("" : "session" : knownSessionId : rest)
      in req { reqPath = newPath }
    _ -> req

-- | Create an executor with a unique app instance that rewrites session IDs
-- Each executor has its own app, so different tests (properties) don't conflict
createExecutorForOperation :: IO ExecutorWithTimeout
createExecutorForOperation = do
  (app, _state) <- createTestApp
  pure $ \timeout req -> do
    let req' = rewriteSessionId req
    executeWaiWithTimeout app timeout req'

-- | Test configuration for positive tests (100 tests)
positiveConfig :: TestConfig
positiveConfig =
  defaultTestConfig
    { tcPropertyCount = 100,
      tcNegativeTesting = False,
      tcOperationFilter = operationFilter
    }

-- | Test configuration for negative tests (100 tests)
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
