{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Unit.AgentSpec
Description : Unit tests for Agent module
Stability   : experimental

This module contains unit tests for the 'Agent.Agent' module,
covering the basic functionality of agent management operations.
-}
module Unit.AgentSpec (spec) where

import Agent.Agent qualified as Agent
import Agent.Types (agentName, isHidden)
import Data.Set qualified as Set
import Test.Helpers (listLength)
import Test.Hspec

spec :: Spec
spec = do
    describe "Agent.builtinAgents" $ do
        it "is not empty" $ do
            let agents = Agent.builtinAgents
            null agents `shouldBe` False

        it "has unique names" $ do
            let agents = Agent.builtinAgents
            let names = map agentName agents
            listLength names `shouldBe` Set.size (Set.fromList names)

        it "includes both primary and subagent modes" $ do
            let primaries = Agent.filterByMode Agent.Primary Agent.builtinAgents
            let subagents = Agent.filterByMode Agent.Subagent Agent.builtinAgents
            null primaries `shouldBe` False
            null subagents `shouldBe` False

        it "has some hidden agents" $ do
            let hidden = filter isHidden Agent.builtinAgents
            null hidden `shouldBe` False

    describe "Agent.get" $ do
        it "returns correct agent for known names" $ do
            let agents = Agent.builtinAgents
            results <- mapM (Agent.get . agentName) agents
            let pairs = zip agents results
            mapM_
                ( \(agent, mResult) -> case mResult of
                    Nothing -> expectationFailure $ "Expected agent " ++ show (agentName agent)
                    Just retrieved -> agentName retrieved `shouldBe` agentName agent
                )
                pairs

        it "returns Nothing for unknown names" $ do
            result <- Agent.get "unknown_agent_xyz123"
            result `shouldBe` Nothing

    describe "Agent.list" $ do
        it "returns all builtin agents" $ do
            listed <- Agent.list
            let builtins = Agent.builtinAgents
            listLength listed `shouldBe` listLength builtins

    describe "Agent.filterVisible" $ do
        it "returns fewer agents than total" $ do
            let visible = Agent.filterVisible Agent.builtinAgents
            listLength visible `shouldSatisfy` (< listLength Agent.builtinAgents)

        it "returns only non-hidden agents" $ do
            let visible = Agent.filterVisible Agent.builtinAgents
            not (any isHidden visible) `shouldBe` True

    describe "Agent.findAgentByName" $ do
        it "finds existing agent" $ do
            let result = Agent.findAgentByName "armed" Agent.builtinAgents
            case result of
                Nothing -> expectationFailure "Expected to find 'armed' agent"
                Just agent -> agentName agent `shouldBe` "armed"

        it "returns Nothing for missing agent" $ do
            let result = Agent.findAgentByName "nonexistent" Agent.builtinAgents
            result `shouldBe` Nothing
