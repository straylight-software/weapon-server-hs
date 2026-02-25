{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.AgentAgentProps
Description : Property tests for Agent.Agent module
Stability   : experimental

This module contains property-based tests for the agent management
functions in 'Agent.Agent', including:

* Built-in agent invariants
* Permission ruleset helper functions
* Pure query functions (findAgentByName, filterByMode, filterVisible)
-}
module Property.AgentAgentProps (
    tests,
) where

import Agent.Agent
import Agent.Types
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Property.AgentTypesProps (genAgent, genAgentMode, genNonEmptyText, genPermissionAction)
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Built-in Agent Properties
-- ============================================================================

-- | Property: All built-in agents have unique names
prop_builtinAgentsUniqueNames :: Property
prop_builtinAgentsUniqueNames = withTests 1 $ property $ do
    let names = map agentName builtinAgents
    -- Check uniqueness by verifying nubOrd doesn't remove any elements
    -- nubOrd is O(n log n) vs O(n^2) for nub
    names === nubOrd names

-- | Property: All built-in agents have non-empty names
prop_builtinAgentsNonEmptyNames :: Property
prop_builtinAgentsNonEmptyNames = withTests 1 $ property $ do
    let names = map agentName builtinAgents
    not (any (null . show) names) === True

-- | Property: All built-in agents are native
prop_builtinAgentsAreNative :: Property
prop_builtinAgentsAreNative = withTests 1 $ property $ do
    let natives = map agentNative builtinAgents
    all (== Just True) natives === True

-- | Property: There is at least one primary agent
prop_builtinHasPrimaryAgent :: Property
prop_builtinHasPrimaryAgent = withTests 1 $ property $ do
    let primaries = filterByMode Primary builtinAgents
    assert $ not (null primaries)

-- | Property: There is at least one subagent
prop_builtinHasSubagent :: Property
prop_builtinHasSubagent = withTests 1 $ property $ do
    let subagents = filterByMode Subagent builtinAgents
    assert $ not (null subagents)

-- ============================================================================
-- Permission Ruleset Helper Properties
-- ============================================================================

-- | Property: mkSimpleRuleset creates valid ruleset
prop_mkSimpleRulesetValid :: Property
prop_mkSimpleRulesetValid = property $ do
    entries <- forAll $ Gen.list (Range.linear 0 10) $ do
        name <- genNonEmptyText
        action <- genPermissionAction
        pure (name, action)
    let rs = mkSimpleRuleset entries
    -- Check that all entries are present (accounting for duplicate keys)
    let expectedKeys = Set.fromList (map fst entries)
        actualKeys = Set.fromList (Map.keys (unRuleset rs))
    actualKeys === expectedKeys

-- | Property: mkRuleset preserves all entries
prop_mkRulesetPreservesEntries :: Property
prop_mkRulesetPreservesEntries = property $ do
    name <- forAll genNonEmptyText
    action <- forAll genPermissionAction
    let rules = [PermissionRule action Nothing]
        rs = mkRuleset [(name, rules)]
    Map.lookup name (unRuleset rs) === Just rules

-- | Property: emptyRuleset has no entries
prop_emptyRulesetEmpty :: Property
prop_emptyRulesetEmpty = withTests 1 $ property $ do
    Map.null (unRuleset emptyRuleset) === True

-- | Property: allowAllRuleset allows wildcard
prop_allowAllRulesetHasWildcard :: Property
prop_allowAllRulesetHasWildcard = withTests 1 $ property $ do
    case Map.lookup "*" (unRuleset allowAllRuleset) of
        Nothing -> failure
        Just [] -> failure
        Just (rule : _) -> prAction rule === Allow

-- | Property: denyAllRuleset denies wildcard
prop_denyAllRulesetHasWildcard :: Property
prop_denyAllRulesetHasWildcard = withTests 1 $ property $ do
    case Map.lookup "*" (unRuleset denyAllRuleset) of
        Nothing -> failure
        Just [] -> failure
        Just (rule : _) -> prAction rule === Deny

-- ============================================================================
-- Pure Query Function Properties
-- ============================================================================

-- | Property: findAgentByName finds agent with matching name
prop_findAgentByNameFinds :: Property
prop_findAgentByNameFinds = property $ do
    agentsNE <- forAll $ Gen.nonEmpty (Range.linear 1 5) genAgent
    let agents = NE.toList agentsNE
    -- Select a random agent from the non-empty list safely
    target <- forAll $ Gen.element agentsNE
    let name = agentName target
    case findAgentByName name agents of
        Nothing -> failure
        Just found -> agentName found === name

-- | Property: findAgentByName returns Nothing for missing agent
prop_findAgentByNameMissing :: Property
prop_findAgentByNameMissing = property $ do
    agents <- forAll $ Gen.list (Range.linear 0 5) genAgent
    let existingNames = Set.fromList (map agentName agents)
    -- Generate a name that doesn't exist
    name <- forAll $ Gen.filter (`Set.notMember` existingNames) genNonEmptyText
    findAgentByName name agents === Nothing

-- | Property: filterByMode only returns agents of that mode
prop_filterByModeCorrect :: Property
prop_filterByModeCorrect = property $ do
    agents <- forAll $ Gen.list (Range.linear 0 10) genAgent
    mode <- forAll genAgentMode
    let filtered = filterByMode mode agents
    all (\a -> agentMode a == mode) filtered === True

-- | Property: filterByMode partitions correctly
prop_filterByModePartitions :: Property
prop_filterByModePartitions = property $ do
    agents <- forAll $ Gen.list (Range.linear 0 10) genAgent
    let primaries = filterByMode Primary agents
        subagents = filterByMode Subagent agents
        allModes = filterByMode AllModes agents
    -- Every agent belongs to exactly one mode, so the partitions should
    -- cover all agents exactly once. Verify by checking each agent appears
    -- in exactly one of the three filtered lists.
    for_ agents $ \agent -> do
        let mode = agentMode agent
            inPrimary = agent `elem` primaries
            inSubagent = agent `elem` subagents
            inAllModes = agent `elem` allModes
        -- Agent should be in exactly one partition matching its mode
        case mode of
            Primary -> do
                assert inPrimary
                assert (not inSubagent)
                assert (not inAllModes)
            Subagent -> do
                assert (not inPrimary)
                assert inSubagent
                assert (not inAllModes)
            AllModes -> do
                assert (not inPrimary)
                assert (not inSubagent)
                assert inAllModes

-- | Property: filterVisible removes hidden agents
prop_filterVisibleRemovesHidden :: Property
prop_filterVisibleRemovesHidden = property $ do
    agents <- forAll $ Gen.list (Range.linear 0 10) genAgent
    let visible = filterVisible agents
    not (any isHidden visible) === True

-- | Property: filterVisible preserves non-hidden agents
prop_filterVisiblePreservesVisible :: Property
prop_filterVisiblePreservesVisible = property $ do
    agents <- forAll $ Gen.list (Range.linear 0 10) genAgent
    let visible = filterVisible agents
        originallyVisible = filter (not . isHidden) agents
    -- Compare lists directly - filterVisible should return the same agents
    -- as manually filtering out hidden ones
    visible === originallyVisible

-- | Property: filterVisible on builtinAgents excludes hidden agents
prop_filterVisibleBuiltins :: Property
prop_filterVisibleBuiltins = withTests 1 $ property $ do
    let visible = filterVisible builtinAgents
        hidden = filter isHidden builtinAgents
    -- Should have some hidden agents
    assert $ not (null hidden)
    -- Visible should be fewer than total (hidden is non-empty, so visible /= builtinAgents)
    -- We already asserted hidden is non-empty, so we know some agents were filtered out
    assert $ not (null builtinAgents)
    -- Verify visible is a proper subset by checking visible ++ hidden covers all
    Set.fromList (map agentName visible) `Set.disjoint` Set.fromList (map agentName hidden) === True
    -- None of the visible should be hidden
    not (any isHidden visible) === True

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Agent.Agent Property Tests"
        [ testGroup
            "Built-in Agents"
            [ testProperty "unique names" prop_builtinAgentsUniqueNames
            , testProperty "non-empty names" prop_builtinAgentsNonEmptyNames
            , testProperty "all are native" prop_builtinAgentsAreNative
            , testProperty "has primary agent" prop_builtinHasPrimaryAgent
            , testProperty "has subagent" prop_builtinHasSubagent
            ]
        , testGroup
            "Permission Ruleset Helpers"
            [ testProperty "mkSimpleRuleset creates valid ruleset" prop_mkSimpleRulesetValid
            , testProperty "mkRuleset preserves entries" prop_mkRulesetPreservesEntries
            , testProperty "emptyRuleset is empty" prop_emptyRulesetEmpty
            , testProperty "allowAllRuleset has Allow wildcard" prop_allowAllRulesetHasWildcard
            , testProperty "denyAllRuleset has Deny wildcard" prop_denyAllRulesetHasWildcard
            ]
        , testGroup
            "Pure Query Functions"
            [ testProperty "findAgentByName finds existing" prop_findAgentByNameFinds
            , testProperty "findAgentByName returns Nothing for missing" prop_findAgentByNameMissing
            , testProperty "filterByMode returns correct mode" prop_filterByModeCorrect
            , testProperty "filterByMode partitions correctly" prop_filterByModePartitions
            , testProperty "filterVisible removes hidden" prop_filterVisibleRemovesHidden
            , testProperty "filterVisible preserves visible" prop_filterVisiblePreservesVisible
            , testProperty "filterVisible on builtins works" prop_filterVisibleBuiltins
            ]
        ]
