{-# LANGUAGE OverloadedStrings #-}

-- | API handler unit tests
module Unit.ApiSpec where

import Api
import Data.Aeson
import Data.Text qualified as T
import Test.Hspec

import Config.Dhall qualified as Dhall
import Handlers (server)
import Katip (Severity (ErrorS))
import Log qualified
import Network.HTTP.Types (status200)
import Network.Wai (pathInfo, rawPathInfo, requestMethod)
import Network.Wai.Test
import Servant (serve)
import State (initialStateNoProxyWithCache)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))

-- | API spec that takes a shared DhallCache
spec :: Dhall.DhallCache -> Spec
spec dhallCache = do
    describe "Health API" $ do
        it "should create a healthy response" $ do
            let health = Health True "0.1.0"
            encode health `shouldBe` "{\"healthy\":true,\"version\":\"0.1.0\"}"

        it "should parse health JSON" $ do
            let json = "{\"healthy\":true,\"version\":\"0.1.0\"}"
            decode json `shouldBe` Just (Health True "0.1.0")

    describe "PathInfo API" $ do
        it "should serialize PathInfo correctly" $ do
            let pathInfo =
                    PathInfo
                        { home = "/home/user"
                        , state = "/home/user/.opencode/state"
                        , config = "/home/user/.opencode/config"
                        , worktree = "/home/user/project"
                        , directory = "/home/user/project"
                        }
            let json = encode pathInfo
            T.isInfixOf "home" (T.pack $ show json) `shouldBe` True

    describe "Project API" $ do
        it "should serialize Project with all fields" $ do
            let project =
                    Project
                        { Api.id = "proj_123"
                        , Api.worktree = "/home/user/project"
                        , Api.name = Just "My Project"
                        , Api.time = ProjectTime 1709312000 1709312100 Nothing
                        , Api.sandboxes = []
                        }
            let json = encode project
            T.isInfixOf "proj_123" (T.pack $ show json) `shouldBe` True

        it "should handle Project without name" $ do
            let project =
                    Project
                        { Api.id = "proj_456"
                        , Api.worktree = "/home/user/other"
                        , Api.name = Nothing
                        , Api.time = ProjectTime 1709312000 1709312100 Nothing
                        , Api.sandboxes = []
                        }
            decode (encode project) `shouldBe` Just project

    describe "FileType API" $ do
        it "should serialize FileTypeFile as 'file'" $ do
            encode FileTypeFile `shouldBe` "\"file\""

        it "should serialize FileTypeDirectory as 'directory'" $ do
            encode FileTypeDirectory `shouldBe` "\"directory\""

    describe "ContentType API" $ do
        it "should serialize ContentTypeText as 'text'" $ do
            encode ContentTypeText `shouldBe` "\"text\""

        it "should serialize ContentTypeBinary as 'binary'" $ do
            encode ContentTypeBinary `shouldBe` "\"binary\""

    describe "WAI Integration" $ do
        it "should respond to /global/health" $ do
            cwd <- getCurrentDirectory
            let storageDir = cwd </> ".opencode-test" </> "wai-unit"
            createDirectoryIfMissing True storageDir
            logger <- Log.newLoggerWithLevel "unit-test" ErrorS
            state <- initialStateNoProxyWithCache dhallCache Nothing storageDir "test_project" (T.pack cwd) logger
            let app = serve api (server state)

            let waiReq = defaultRequest{requestMethod = "GET", rawPathInfo = "/global/health", pathInfo = ["global", "health"]}
            let req = SRequest waiReq ""
            response <- runSession (srequest req) app
            simpleStatus response `shouldBe` status200

    describe "SessionTime API" $ do
        it "should round-trip SessionTime" $ do
            let st =
                    SessionTime
                        { stCreated = 1234567890
                        , stUpdated = 1234567891
                        , stCompacting = Nothing
                        , stArchived = Just 1234567892
                        }
            decode (encode st) `shouldBe` Just st

    describe "Strict Input Parsing" $ do
        describe "SessionCommandInput" $ do
            it "should parse valid input" $ do
                let json = "{\"command\":\"echo\",\"arguments\":\"hello\"}"
                case decode json of
                    Just (SessionCommandInput cmd args _ _ _ _ _) -> do
                        cmd `shouldBe` "echo"
                        args `shouldBe` "hello"
                    Nothing -> expectationFailure "Failed to parse valid SessionCommandInput"

            it "should reject unknown properties" $ do
                let json = "{\"command\":\"echo\",\"arguments\":\"hello\",\"unknown\":\"bad\"}"
                (decode json :: Maybe SessionCommandInput) `shouldBe` Nothing

        describe "SessionShellInput" $ do
            it "should parse valid input" $ do
                let json = "{\"agent\":\"test\",\"command\":\"ls\"}"
                case decode json of
                    Just (SessionShellInput agent cmd _) -> do
                        agent `shouldBe` "test"
                        cmd `shouldBe` "ls"
                    Nothing -> expectationFailure "Failed to parse valid SessionShellInput"

            it "should reject unknown properties" $ do
                let json = "{\"agent\":\"test\",\"command\":\"ls\",\"extra\":123}"
                (decode json :: Maybe SessionShellInput) `shouldBe` Nothing

        describe "PermissionReplyInput" $ do
            it "should parse valid input" $ do
                let json = "{\"reply\":\"once\"}"
                case decode json of
                    Just (PermissionReplyInput reply _) -> reply `shouldBe` "once"
                    Nothing -> expectationFailure "Failed to parse valid PermissionReplyInput"

            it "should reject unknown properties" $ do
                let json = "{\"reply\":\"once\",\"injected\":\"malicious\"}"
                (decode json :: Maybe PermissionReplyInput) `shouldBe` Nothing

        describe "WorktreeRemoveInput" $ do
            it "should parse valid input" $ do
                let json = "{\"directory\":\"/path/to/worktree\"}"
                case decode json of
                    Just (WorktreeRemoveInput dir) -> dir `shouldBe` "/path/to/worktree"
                    Nothing -> expectationFailure "Failed to parse valid WorktreeRemoveInput"

            it "should reject unknown properties" $ do
                let json = "{\"directory\":\"/path\",\"__proto__\":{\"admin\":true}}"
                (decode json :: Maybe WorktreeRemoveInput) `shouldBe` Nothing

        describe "AppendPromptInput (TUI)" $ do
            it "should parse valid input" $ do
                let json = "{\"text\":\"hello world\"}"
                case decode json of
                    Just (AppendPromptInput txt _) -> txt `shouldBe` Just "hello world"
                    Nothing -> expectationFailure "Failed to parse valid AppendPromptInput"

            it "should reject unknown properties" $ do
                let json = "{\"text\":\"hello\",\"extra\":\"field\"}"
                (decode json :: Maybe AppendPromptInput) `shouldBe` Nothing
