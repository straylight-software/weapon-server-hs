-- | Main test runner for weapon-server
module Main where

import Config.Dhall qualified as Dhall
import Formatter.Status qualified as Formatter
import Integration.ConfigLoadingSpec qualified as ConfigLoadingSpec
import Integration.FindSearchSpec qualified as FindSearchSpec
import Integration.HandlerSubprocessSpec qualified as HandlerSubprocessSpec
import Integration.HaskemathesisTest qualified as HaskemathesisTest
import Integration.StorageSpec qualified as StorageSpec
import Integration.ToolExecSpec qualified as ToolExecSpec
import Property.AgentAgentProps qualified as AgentAgentProps
import Property.AgentContextProps qualified as AgentContextProps
import Property.AgentTypesProps qualified as AgentTypesProps
import Property.ApiFileProps qualified as ApiFileProps
import Property.ApiSessionProps qualified as ApiSessionProps
import Property.ApiTypesProps qualified as ApiTypesProps
import Property.BusProps qualified as BusProps
import Property.ConfigDhallProps qualified as ConfigDhallProps
import Property.ConfigMergeProps qualified as ConfigMergeProps
import Property.ConfigProps qualified as ConfigProps
import Property.ConfigTypesProps qualified as ConfigTypesProps
import Property.DiffProps qualified as DiffProps
import Property.ErrorFormattersProps qualified as ErrorFormattersProps
import Property.EventProps qualified as EventProps
import Property.EvringProps qualified as EvringProps
import Property.ExeCacheProps qualified as ExeCacheProps
import Property.ExperimentalProps qualified as ExperimentalProps
import Property.FileSystemProps qualified as FileSystemProps
import Property.FindParseProps qualified as FindParseProps
import Property.FormatterProps qualified as FormatterProps
import Property.GitProps qualified as GitProps
import Property.HandlerProps qualified as HandlerProps
import Property.HealthProps qualified as HealthProps
import Property.IdentifierProps qualified as IdentifierProps
import Property.IoUringProps qualified as IoUringProps
import Property.LLMProps qualified as LLMProps
import Property.LLMTypesProps qualified as LLMTypesProps
import Property.LspProps qualified as LspProps
import Property.MessagePartProps qualified as MessagePartProps
import Property.MessageProps qualified as MessageProps
import Property.MessageTypesProps qualified as MessageTypesProps
import Property.MiddlewareProps qualified as MiddlewareProps
import Property.OAuthProps qualified as OAuthProps
import Property.OpenRouterProps qualified as OpenRouterProps
import Property.PathProps qualified as PathProps
import Property.ProjectDiscoveryProps qualified as ProjectDiscoveryProps
import Property.ProjectProps qualified as ProjectProps
import Property.PromptAsyncProps qualified as PromptAsyncProps
import Property.ProviderProps qualified as ProviderProps
import Property.ProxyProps qualified as ProxyProps
import Property.ProxyTypesProps qualified as ProxyTypesProps
import Property.PtyInternalProps qualified as PtyInternalProps
import Property.PtyProps qualified as PtyProps
import Property.PtyTypesProps qualified as PtyTypesProps
import Property.RequestProps qualified as RequestProps
import Property.SandboxProps qualified as SandboxProps
import Property.SandboxTypesProps qualified as SandboxTypesProps
import Property.SessionFilterProps qualified as SessionFilterProps
import Property.SessionProps qualified as SessionProps
import Property.SessionStatusProps qualified as SessionStatusProps
import Property.SessionTypesProps qualified as SessionTypesProps
import Property.SkillProps qualified as SkillProps
import Property.StorageKeyProps qualified as StorageKeyProps
import Property.StorageProps qualified as StorageProps
import Property.TodoProps qualified as TodoProps
import Property.ToolDefsProps qualified as ToolDefsProps
import Property.ToolProps qualified as ToolProps
import Property.ToolTypesProps qualified as ToolTypesProps
import Property.TuiProps qualified as TuiProps
import Property.VcsInternalProps qualified as VcsInternalProps
import Property.VcsStatusProps qualified as VcsStatusProps
import System.Posix.Signals (Handler (Ignore), installHandler, sigHUP, sigTERM)
import Test.Tasty
import Test.Tasty.Hspec
import Unit.AgentSpec qualified as AgentSpec
import Unit.ApiSpec qualified as ApiSpec
import Unit.ProviderSpec qualified as ProviderSpec
import Unit.ToolStreamingSpec qualified as ToolStreamingSpec

main :: IO ()
main = do
    -- Install signal handlers at startup to prevent SIGTERM/SIGHUP from
    -- subprocess tests affecting the test runner. PTY processes can send
    -- these signals when they terminate.
    _ <- installHandler sigTERM Ignore Nothing
    _ <- installHandler sigHUP Ignore Nothing

    -- Create shared caches once for all tests that need them
    -- This avoids re-parsing Dhall config files and re-searching PATH (~75% speedup)
    dhallCache <- Dhall.newDhallCache
    exeCache <- Formatter.newExeCache

    agentTests <- testSpec "Agent Unit Tests" AgentSpec.spec
    apiTests <- testSpec "API Unit Tests" (ApiSpec.spec dhallCache)
    configLoadingTests <- testSpec "Config Loading Integration Tests" ConfigLoadingSpec.spec
    findSearchTests <- testSpec "Find Search Integration Tests" FindSearchSpec.spec
    handlerSubprocessTests <- testSpec "Handler Subprocess Integration Tests" (HandlerSubprocessSpec.spec dhallCache exeCache)
    providerTests <- testSpec "Provider Unit Tests" ProviderSpec.spec
    toolStreamingTests <- testSpec "Tool Streaming Unit Tests" ToolStreamingSpec.spec
    toolExecTests <- testSpec "Tool Exec Integration Tests" ToolExecSpec.spec
    haskemathesisTests <- HaskemathesisTest.tests dhallCache
    storageIntegrationTests <- testSpec "Storage Integration Tests" StorageSpec.spec
    defaultMain $
        testGroup
            "All Tests"
            [ testGroup
                "Property Tests"
                [ StorageProps.tests
                , StorageKeyProps.tests
                , BusProps.tests
                , ConfigDhallProps.tests
                , ConfigMergeProps.tests
                , ConfigProps.tests
                , DiffProps.tests
                , EventProps.tests
                , EvringProps.tests
                , FormatterProps.tests dhallCache exeCache
                , FindParseProps.tests
                , ExperimentalProps.tests
                , HandlerProps.tests dhallCache exeCache
                , HealthProps.tests
                , IdentifierProps.tests
                , IoUringProps.tests
                , PathProps.tests
                , SessionFilterProps.tests
                , SessionProps.tests
                , SessionStatusProps.tests
                , SkillProps.tests dhallCache
                , ToolDefsProps.tests
                , ToolProps.tests
                , MessageProps.tests
                , MessagePartProps.tests
                , MessageTypesProps.tests
                , OAuthProps.tests
                , LLMProps.tests
                , OpenRouterProps.tests
                , LspProps.tests
                , ProviderProps.tests
                , PtyInternalProps.tests
                , PtyProps.tests
                , ProjectProps.tests
                , ProjectDiscoveryProps.tests
                , RequestProps.tests
                , PromptAsyncProps.tests
                , TodoProps.tests
                , VcsInternalProps.tests
                , VcsStatusProps.tests
                , TuiProps.tests
                , SandboxProps.tests
                , AgentAgentProps.tests
                , AgentContextProps.tests
                , AgentTypesProps.tests
                , MiddlewareProps.tests
                , PtyTypesProps.tests
                , SandboxTypesProps.tests
                , ToolTypesProps.tests
                , ConfigTypesProps.tests
                , LLMTypesProps.tests
                , SessionTypesProps.tests
                , ApiFileProps.tests
                , ApiSessionProps.tests
                , ApiTypesProps.tests
                , ProxyProps.tests
                , ProxyTypesProps.tests
                , ErrorFormattersProps.tests
                , ExeCacheProps.tests
                , GitProps.tests
                , FileSystemProps.tests
                ]
            , agentTests
            , apiTests
            , configLoadingTests
            , providerTests
            , toolStreamingTests
            , toolExecTests
            , findSearchTests
            , handlerSubprocessTests
            , haskemathesisTests
            , storageIntegrationTests
            ]
