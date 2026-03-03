{-# LANGUAGE OverloadedStrings #-}

{- | OpenAPI schema-based property tests using haskemathesis

This module runs generated property tests against the server's WAI application
to verify compliance with the OpenAPI specification.

Key features:
- Pre-seeds sessions so session endpoints can be properly tested
- Rewrites random session IDs in requests to use pre-seeded sessions
- Automatic streaming endpoint detection and skipping
- Automatic HPC coverage tracking when compiled with coverage
-}
module Integration.HaskemathesisTest (tests) where

import Api (api)

import Control.Monad (unless, when)
import Data.Function ((&))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Handlers (server)
import Haskemathesis.Check.Standard (strictChecks)
import Haskemathesis.Execute.Types (ApiRequest (..))
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.Tasty (
    AppFactory,
    SkippedOperation (..),
    TestResult (..),
    forApp,
    runTests,
    setChecks,
    withFull,
    withIsolated,
    withMaxSequenceLength,
    withPropertyCount,
    withRequestTransform,
 )

import Config.Dhall qualified as Dhall
import Katip (Severity (ErrorS))
import Log qualified
import Middleware (addAllowHeader, rejectDoubleEncodedPaths, rejectDuplicateQueryParams, rejectEmptyPathSegments, rejectHeadMethod, rejectInvalidCharset, rejectInvalidContentType, rejectMethodMismatch, rejectNullBytePaths, rejectUnknownQueryParams, rejectUnsupportedMethods, requireContentType, supplyEmptyBody)
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
import Test.Tasty (TestTree)
import Util.StorageKeys (sessionKey)

-- | Path to the OpenAPI spec
openApiSpecPath :: FilePath
openApiSpecPath = "./sdk/openapi.json"

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
createTestApp :: Dhall.DhallCache -> IORef Int -> IO Application
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

    pure $
        -- RFC 9110 compliance: Add Allow header to 405 responses
        addAllowHeader $
            -- Method validation for globally unsupported methods (HEAD, TRACE, etc.)
            rejectHeadMethod $
                rejectUnsupportedMethods $
                    -- Path validation (security checks before routing)
                    rejectNullBytePaths $
                        rejectDoubleEncodedPaths $
                            rejectEmptyPathSegments $
                                -- Method/route mismatch (after path is validated)
                                rejectMethodMismatch $
                                    -- Query parameter validation
                                    rejectDuplicateQueryParams $
                                        rejectUnknownQueryParams $
                                            -- Content-Type validation
                                            rejectInvalidCharset $
                                                rejectInvalidContentType $
                                                    requireContentType $
                                                        -- Body handling
                                                        supplyEmptyBody $
                                                            serveWithContext api errorFormattersContext (server state)

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

Does NOT rewrite:
- Special paths like /session/status
- Session IDs that look malicious (contain %, null bytes, etc.) - these are
  negative tests that should reach the server to test security validation
-}
rewriteSessionId :: ApiRequest -> ApiRequest
rewriteSessionId req =
    let path = reqPath req
        segments = T.splitOn "/" path
     in case segments of
            -- Skip literal paths that aren't session ID captures
            ("" : "session" : "status" : _) -> req
            -- /session/{sessionID}/... (with or without trailing path)
            ("" : "session" : randomSid : rest)
                -- Only rewrite if the session ID looks like a normal test ID
                -- Skip rewriting for malicious inputs (negative tests)
                | isSafeSessionId randomSid ->
                    let newPath = T.intercalate "/" ("" : "session" : knownSessionId : rest)
                     in req{reqPath = newPath}
            _otherSegments -> req

{- | Check if a session ID is safe to rewrite (not a negative test input)
Returns False for IDs containing URL-encoded chars, null bytes, etc.
-}
isSafeSessionId :: T.Text -> Bool
isSafeSessionId sid =
    not (T.any (\c -> c == '%' || c == '\x00' || c == '/' || c == '\\') sid)
        && not (T.null sid)

{- | All haskemathesis tests

Uses the composable builder API:
- forApp: Test a WAI application with auto-coverage
- withFull: Run Standard + Negative + Stateful tests
- withIsolated: Per-operation isolation with auto-coverage
- withRequestTransform: Session ID rewriting

Coverage is automatically enabled when HPC instrumentation is available
(compile with coverage: True in cabal.project.local).
-}
tests :: Dhall.DhallCache -> IO TestTree
tests dhallCache = do
    counter <- newIORef 0
    specResult <- loadOpenApiFile openApiSpecPath
    case specResult of
        Left err -> error $ "Failed to load OpenAPI spec: " <> show err
        Right openApi -> do
            -- Create shared app for stateful tests
            sharedApp <- createTestApp dhallCache counter

            -- Factory returns Application - coverage is automatic!
            let mkApp :: AppFactory
                mkApp _op = createTestApp dhallCache counter

            -- Simple, composable configuration
            -- Note: defaultStatefulChecks already includes all 3 checks:
            -- useAfterFree, ensureResourceAvailability, ensureModificationPersisted
            let spec =
                    forApp openApi sharedApp
                        & withFull
                        & withIsolated mkApp
                        & withRequestTransform rewriteSessionId
                        & setChecks strictChecks
                        & withPropertyCount 100
                        & withMaxSequenceLength 10

            (tree, result) <- runTests spec

            -- Report skipped streaming operations
            let skipped = trSkippedOperations result
            unless (null skipped) $
                hPutStrLn stderr $
                    "Auto-skipped streaming operations: " ++ show (map soLabel skipped)

            when (trCoverageEnabled result) $
                hPutStrLn stderr "Coverage-guided fuzzing enabled (HPC available)"

            pure tree
