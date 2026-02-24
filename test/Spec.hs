-- | Main test runner for opencode-server
module Main where

import Config.Dhall qualified as Dhall
import Integration.HaskemathesisTest qualified as HaskemathesisTest
import Property.AgentLogicProps qualified as AgentLogicProps
import Property.AgentTypesProps qualified as AgentTypesProps
import Property.ApiFileProps qualified as ApiFileProps
import Property.ApiTypesProps qualified as ApiTypesProps
import Property.BusProps qualified as BusProps
import Property.ConfigProps qualified as ConfigProps
import Property.ConfigTypesProps qualified as ConfigTypesProps
import Property.DiffProps qualified as DiffProps
import Property.EventProps qualified as EventProps
import Property.ExperimentalProps qualified as ExperimentalProps
import Property.FindParseProps qualified as FindParseProps
import Property.FormatterProps qualified as FormatterProps
import Property.HandlerProps qualified as HandlerProps
import Property.HealthProps qualified as HealthProps
import Property.IdentifierProps qualified as IdentifierProps
import Property.LLMProps qualified as LLMProps
import Property.LLMTypesProps qualified as LLMTypesProps
import Property.LspProps qualified as LspProps
import Property.MessagePartProps qualified as MessagePartProps
import Property.MessageProps qualified as MessageProps
import Property.MiddlewareProps qualified as MiddlewareProps
import Property.OAuthProps qualified as OAuthProps
import Property.PathProps qualified as PathProps
import Property.ProjectDiscoveryProps qualified as ProjectDiscoveryProps
import Property.ProjectProps qualified as ProjectProps
import Property.PromptAsyncProps qualified as PromptAsyncProps
import Property.ProviderProps qualified as ProviderProps
import Property.ProxyTypesProps qualified as ProxyTypesProps
import Property.PtyProps qualified as PtyProps
import Property.PtyTypesProps qualified as PtyTypesProps
import Property.RequestProps qualified as RequestProps
import Property.SandboxProps qualified as SandboxProps
import Property.SandboxTypesProps qualified as SandboxTypesProps
import Property.SessionProps qualified as SessionProps
import Property.SessionStatusProps qualified as SessionStatusProps
import Property.SessionTypesProps qualified as SessionTypesProps
import Property.SkillProps qualified as SkillProps
import Property.StorageKeyProps qualified as StorageKeyProps
import Property.StorageProps qualified as StorageProps
import Property.TodoProps qualified as TodoProps
import Property.ToolProps qualified as ToolProps
import Property.ToolStreamingProps qualified as ToolStreamingProps
import Property.ToolTypesProps qualified as ToolTypesProps
import Property.TuiProps qualified as TuiProps
import Property.VcsStatusProps qualified as VcsStatusProps
import System.Posix.Signals (Handler (Ignore), installHandler, sigHUP, sigTERM)
import Test.Tasty
import Test.Tasty.Hspec
import Unit.ApiSpec qualified as ApiSpec

main :: IO ()
main = do
    -- Install signal handlers at startup to prevent SIGTERM/SIGHUP from
    -- subprocess tests affecting the test runner. PTY processes can send
    -- these signals when they terminate.
    _ <- installHandler sigTERM Ignore Nothing
    _ <- installHandler sigHUP Ignore Nothing

    -- Create shared DhallCache once for all tests that need it
    -- This avoids re-parsing Dhall config files (~75% speedup)
    dhallCache <- Dhall.newDhallCache

    apiTests <- testSpec "API Unit Tests" (ApiSpec.spec dhallCache)
    haskemathesisTests <- HaskemathesisTest.tests dhallCache
    defaultMain $
        testGroup
            "All Tests"
            [ testGroup
                "Property Tests"
                [ StorageProps.tests
                , StorageKeyProps.tests
                , BusProps.tests
                , ConfigProps.tests
                , DiffProps.tests
                , EventProps.tests
                , FormatterProps.tests dhallCache
                , FindParseProps.tests
                , ExperimentalProps.tests
                , HandlerProps.tests dhallCache
                , HealthProps.tests
                , IdentifierProps.tests
                , PathProps.tests
                , SessionProps.tests
                , SessionStatusProps.tests
                , SkillProps.tests dhallCache
                , ToolProps.tests
                , ToolStreamingProps.tests
                , MessageProps.tests
                , MessagePartProps.tests
                , OAuthProps.tests
                , LLMProps.tests
                , LspProps.tests
                , ProviderProps.tests
                , PtyProps.tests
                , ProjectProps.tests
                , ProjectDiscoveryProps.tests
                , RequestProps.tests
                , PromptAsyncProps.tests
                , TodoProps.tests
                , VcsStatusProps.tests
                , TuiProps.tests
                , SandboxProps.tests
                , AgentLogicProps.tests
                , AgentTypesProps.tests
                , MiddlewareProps.tests
                , PtyTypesProps.tests
                , SandboxTypesProps.tests
                , ToolTypesProps.tests
                , ConfigTypesProps.tests
                , LLMTypesProps.tests
                , SessionTypesProps.tests
                , ApiFileProps.tests
                , ApiTypesProps.tests
                , ProxyTypesProps.tests
                ]
            , apiTests
            , haskemathesisTests
            ]
