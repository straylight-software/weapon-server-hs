{-# LANGUAGE OverloadedStrings #-}

{- | OpenAPI schema-based property tests using haskemathesis

This module runs generated property tests against the server's WAI application
to verify compliance with the OpenAPI specification.

Key features:
- Pre-seeds sessions so session endpoints can be properly tested
- Rewrites random session IDs in requests to use pre-seeded sessions
- Automatic streaming endpoint detection (SSE/WebSocket operations are skipped)
-}
module Integration.HaskemathesisTest (tests) where

import Api (api)

import Control.Monad (unless, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Handlers (server)
import Haskemathesis.Check.Standard (strictChecks)
import Haskemathesis.Config (TestConfig (..), defaultStatefulChecks, defaultTestConfig)
import Haskemathesis.Coverage.HPC (hpcAvailable)
import Haskemathesis.Execute.Types (ApiRequest (..), ExecutorWithTimeout)
import Haskemathesis.Execute.Wai (executeWaiWithTimeout)
import Haskemathesis.Execute.WaiCoverage (CoverageResult (..), executeWaiWithCoverage)
import Haskemathesis.Integration.Tasty (testTreeForExecutorFactoryIO, testTreeForExecutorFactoryNegativeIO)
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Stateful.Checks (ensureModificationPersisted)

import Config.Dhall qualified as Dhall
import Katip (Severity (ErrorS))
import Log qualified
import Middleware (rejectEmptyPathSegments, rejectInvalidContentType, requireContentType, supplyEmptyBody)
import Network.Wai (Application)
import Servant (serveWithContext)
import Server.ErrorFormatters (errorFormattersContext)
import Session.Session qualified as Session
import Session.Types (Session (..), SessionTime (..))
import State (AppState (..), initialStateNoProxyWithCache)
import Storage.Storage qualified as Storage
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Test.Tasty (TestTree, testGroup)
import Util.StorageKeys (sessionKey)

-- | Path to the OpenAPI spec
openApiSpecPath :: FilePath
openApiSpecPath = "./sdk/openapi.json"

{- | Additional endpoints to skip (beyond auto-detected streaming endpoints)

Note: SSE and WebSocket endpoints are automatically skipped by haskemathesis.
This list is for operations that fail for other reasons.
-}
additionalSkipEndpoints :: [Text]
additionalSkipEndpoints =
    [ "part.update" -- Uses $ref to Part schema with anyOf that haskemathesis can't resolve
    ]

-- | Filter out non-testable endpoints
operationFilter :: ResolvedOperation -> Bool
operationFilter op =
    case roOperationId op of
        Just opId -> opId `notElem` additionalSkipEndpoints
        Nothing -> True

-- | Known session ID that we pre-seed for testing
knownSessionId :: Text
knownSessionId = "ses_test_1"

-- | Known session IDs that we pre-seed for testing
knownSessionIds :: [Text]
knownSessionIds = [knownSessionId, "ses_test_2", "ses_test_3"]

{- | Create a test WAI application with isolated state and pre-seeded data

The storage dir is used as the project directory AND home directory
to isolate all config file access.

Pre-seeds sessions so session endpoints can be properly tested.
-}
createTestApp :: Dhall.DhallCache -> IORef Int -> IO (Application, AppState)
createTestApp dhallCache counter = do
    cwd <- getCurrentDirectory
    -- Get unique ID for this test instance
    uniqueId <- atomicModifyIORef' counter (\n -> (n + 1, n))
    let storageDir = cwd </> ".opencode-test" </> "haskemathesis" </> show uniqueId
    exists <- doesDirectoryExist storageDir
    when exists $ removeDirectoryRecursive storageDir
    createDirectoryIfMissing True storageDir
    -- Create .config/weapon directory for global config isolation
    createDirectoryIfMissing True (storageDir </> ".config" </> "weapon")

    -- Create a persistent logger (not using withLogger bracket pattern)
    -- Use ErrorS to suppress INFO-level logs during tests
    logger <- Log.newLoggerWithLevel "test" ErrorS
    -- Use storageDir as storage, project dir, AND home dir to fully isolate config files
    -- Use shared dhallCache to avoid re-parsing Dhall files for each test
    state <- initialStateNoProxyWithCache dhallCache (Just storageDir) storageDir "test_project" (T.pack storageDir) logger

    -- Pre-seed sessions with known IDs
    preSeedSessions state

    let app =
            rejectEmptyPathSegments $
                rejectInvalidContentType $
                    requireContentType $
                        supplyEmptyBody $
                            serveWithContext api errorFormattersContext (server state)
    pure (app, state)

-- | Pre-seed sessions with known IDs for testing
preSeedSessions :: AppState -> IO ()
preSeedSessions state = do
    let ctx =
            Session.SessionContext
                { Session.scStorage = stStorage state
                , Session.scBus = stBus state
                , Session.scProjectID = stProjectID state
                , Session.scDirectory = stDirectory state
                , Session.scVersion = stVersion state
                , Session.scIdGen = stIdGen state
                }
    -- Create sessions with known IDs by directly writing to storage
    -- (bypassing the ID generation in Session.create)
    mapM_ (createSessionWithId ctx) knownSessionIds
  where
    createSessionWithId ctx sid = do
        let session =
                Session
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

{- | Rewrite a request to use known session IDs instead of random ones

This intercepts requests to /session/{sessionID}/... and replaces the
random sessionID with one of our known pre-seeded session IDs.
Does NOT rewrite special paths like /session/status
-}
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
                 in req{reqPath = newPath}
            _otherSegments -> req

-- | Create a standard executor (no coverage tracking)
createExecutor :: Application -> ExecutorWithTimeout
createExecutor app timeout req = do
    let req' = rewriteSessionId req
    executeWaiWithTimeout app timeout req'

{- | Create a coverage-tracking executor

Uses HPC to track code coverage during test execution. This enables
coverage-guided fuzzing where inputs that discover new code paths are
prioritized.
-}
createCoverageExecutor :: Application -> ExecutorWithTimeout
createCoverageExecutor app timeout req = do
    let req' = rewriteSessionId req
    result <- executeWaiWithCoverage app timeout req'
    pure (crResponse result)

-- | Test configuration with all features enabled (except response time checks)
testConfig :: TestConfig
testConfig =
    defaultTestConfig
        { tcChecks = strictChecks -- Maximum validation (schema, status, content-type, headers, valid requests)
        , tcPropertyCount = 100
        , tcNegativeTesting = True -- Also generate negative tests (invalid inputs)
        , tcStatefulTesting = True -- Test CRUD operation sequences
        , tcStatefulChecks = defaultStatefulChecks ++ [ensureModificationPersisted] -- Verify modifications persist
        , tcMaxSequenceLength = 10 -- Longer sequences for more complex scenarios
        , tcOperationFilter = operationFilter
        }

{- | All haskemathesis tests

Uses testTreeForExecutorFactoryIO which creates isolated executors per operation,
automatically filters streaming endpoints, and returns which operations were skipped.

The factory pattern ensures each operation gets its own isolated storage directory,
preventing file lock conflicts between concurrent tests.

Coverage-guided fuzzing is enabled when HPC instrumentation is available
(compile with coverage: True in cabal.project.local).
-}
tests :: Dhall.DhallCache -> IO TestTree
tests dhallCache = do
    counter <- newIORef 0
    specResult <- loadOpenApiFile openApiSpecPath
    case specResult of
        Left err -> error $ "Failed to load OpenAPI spec: " <> show err
        Right openApi -> do
            -- Check if HPC coverage is available
            hasCoverage <- hpcAvailable
            when hasCoverage $
                hPutStrLn stderr "Coverage-guided fuzzing enabled (HPC available)"

            -- Factory creates isolated executor per operation
            -- Each operation gets its own storage directory to prevent file lock conflicts
            let mkExecutor :: ResolvedOperation -> IO ExecutorWithTimeout
                mkExecutor _op = do
                    (app, _state) <- createTestApp dhallCache counter
                    pure $
                        if hasCoverage
                            then createCoverageExecutor app
                            else createExecutor app

            -- Use the factory IO variants that create isolated executors per operation
            (positiveTree, skippedStreaming) <- testTreeForExecutorFactoryIO openApi testConfig mkExecutor
            (negativeTree, _) <- testTreeForExecutorFactoryNegativeIO openApi testConfig mkExecutor

            -- Report skipped operations
            unless (null skippedStreaming) $
                hPutStrLn stderr $
                    "Auto-skipped streaming operations: " ++ show skippedStreaming
            unless (null additionalSkipEndpoints) $
                hPutStrLn stderr $
                    "Manually skipped operations: " ++ show additionalSkipEndpoints

            pure $
                testGroup
                    "Haskemathesis OpenAPI Compliance"
                    [ testGroup "OpenAPI Conformance" [positiveTree]
                    , testGroup "OpenAPI Conformance (Negative)" [negativeTree]
                    ]
