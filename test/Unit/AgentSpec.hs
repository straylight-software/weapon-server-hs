{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for Agent module
module Unit.AgentSpec where

import Agent.Agent qualified as Agent
import Agent.Types (agentName)
import Data.List qualified as List
import Data.Set qualified as Set
import Test.Hspec

listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0

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
