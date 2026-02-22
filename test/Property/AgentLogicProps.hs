{-# LANGUAGE OverloadedStrings #-}

-- | Agent logic property tests
module Property.AgentLogicProps where

import Agent.Agent qualified as Agent
import Agent.Types (Agent (..), agentName)
import Data.Foldable (forM_)
import Data.List (nub)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- ═══════════════════════════════════════════════════════════════════════════
-- Agent Logic Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: builtinAgents is not empty
prop_builtinAgentsNonEmpty :: Property
prop_builtinAgentsNonEmpty = withTests 1 $ property $ do
    let agents = Agent.builtinAgents
    assert $ not (null agents)

-- | Property: All builtin agents have unique names
prop_builtinAgentsUniqueNames :: Property
prop_builtinAgentsUniqueNames = withTests 1 $ property $ do
    let agents = Agent.builtinAgents
    let names = map agentName agents
    length names === length (nub names)

-- | Property: get returns correct agent for known names
prop_getReturnsBuiltin :: Property
prop_getReturnsBuiltin = withTests 1 $ property $ do
    let agents = Agent.builtinAgents
    -- Test each builtin agent can be retrieved
    results <- evalIO $ mapM (\a -> Agent.get (agentName a)) agents
    -- All results should be Just with matching names
    forM_ (zip agents results) $ \(agent, mResult) -> do
        case mResult of
            Nothing -> failure
            Just retrieved -> agentName retrieved === agentName agent

-- | Property: get returns Nothing for unknown names
prop_getReturnsNothingUnknown :: Property
prop_getReturnsNothingUnknown = property $ do
    -- Generate a name that's unlikely to match any builtin
    randomName <- forAll $ Gen.text (Range.linear 10 30) Gen.alphaNum
    let unknownName = "unknown_agent_" <> randomName
    result <- evalIO $ Agent.get unknownName
    result === Nothing

-- | Property: list returns all builtin agents
prop_listReturnsAllBuiltins :: Property
prop_listReturnsAllBuiltins = withTests 1 $ property $ do
    listed <- evalIO Agent.list
    let builtins = Agent.builtinAgents
    length listed === length builtins

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Agent Logic Property Tests"
        [ testProperty "builtinAgents is not empty" prop_builtinAgentsNonEmpty
        , testProperty "builtin agents have unique names" prop_builtinAgentsUniqueNames
        , testProperty "get returns correct agent for known names" prop_getReturnsBuiltin
        , testProperty "get returns Nothing for unknown names" prop_getReturnsNothingUnknown
        , testProperty "list returns all builtin agents" prop_listReturnsAllBuiltins
        ]
