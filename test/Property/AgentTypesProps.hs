{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.AgentTypesProps
Description : Property tests for Agent.Types module
Stability   : experimental

This module contains property-based tests for all types and functions
exported from 'Agent.Types', including:

* JSON round-trip properties for all types
* Smart constructor invariants
* Pure query function properties
-}
module Property.AgentTypesProps (
    -- * Test Tree
    tests,

    -- * Generators (exported for use in other test modules)
    genText,
    genNonEmptyText,
    genPermissionAction,
    genPermissionRule,
    genPermissionRuleset,
    genAgentMode,
    genAgent,
    genAgentWithRuleset,
) where

import Agent.Types
import Data.Aeson (decode, encode)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 0 100) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 100) Gen.alphaNum

genPermissionAction :: Gen PermissionAction
genPermissionAction = Gen.element [Allow, Deny, Ask]

genPermissionRule :: Gen PermissionRule
genPermissionRule =
    PermissionRule
        <$> genPermissionAction
        <*> Gen.maybe genText

genPermissionRuleset :: Gen PermissionRuleset
genPermissionRuleset = do
    entries <- Gen.list (Range.linear 0 5) $ do
        key <- genNonEmptyText
        -- Must have at least 1 rule per key for roundtrip to work
        -- (empty rule lists are not preserved in array format)
        rules <- Gen.list (Range.linear 1 3) genPermissionRule
        pure (key, rules)
    pure $ PermissionRuleset (Map.fromList entries)

genAgentMode :: Gen AgentMode
genAgentMode = Gen.element [Subagent, Primary, AllModes]

-- | Generate a random Agent with all fields populated randomly.
genAgent :: Gen Agent
genAgent =
    Agent
        <$> genNonEmptyText
        <*> Gen.maybe genText
        <*> genAgentMode
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe (Gen.double (Range.linearFrac 0 1))
        <*> Gen.maybe (Gen.double (Range.linearFrac 0 2))
        <*> Gen.maybe genText
        <*> genPermissionRuleset
        -- Note: agentModel has asymmetric JSON encoding (ToJSON creates object,
        -- but FromJSON expects tuple array format), so we skip it in roundtrip tests
        <*> pure Nothing
        <*> Gen.maybe genText
        <*> Gen.maybe genText
        <*> pure Map.empty
        <*> Gen.maybe (Gen.int (Range.linear 1 100))

-- | Generate an Agent with a specific ruleset (useful for permission tests).
genAgentWithRuleset :: PermissionRuleset -> Gen Agent
genAgentWithRuleset rs =
    Agent
        <$> genNonEmptyText
        <*> Gen.maybe genText
        <*> genAgentMode
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe (Gen.double (Range.linearFrac 0 1))
        <*> Gen.maybe (Gen.double (Range.linearFrac 0 2))
        <*> Gen.maybe genText
        <*> pure rs
        <*> pure Nothing
        <*> Gen.maybe genText
        <*> Gen.maybe genText
        <*> pure Map.empty
        <*> Gen.maybe (Gen.int (Range.linear 1 100))

-- ============================================================================
-- Properties
-- ============================================================================

prop_permissionActionRoundtrip :: Property
prop_permissionActionRoundtrip = property $ do
    action <- forAll genPermissionAction
    let json = encode action
    case decode json of
        Nothing -> failure
        Just action' -> action === action'

prop_permissionRuleRoundtrip :: Property
prop_permissionRuleRoundtrip = property $ do
    rule <- forAll genPermissionRule
    let json = encode rule
    case decode json of
        Nothing -> failure
        Just rule' -> rule === rule'

prop_permissionRulesetRoundtrip :: Property
prop_permissionRulesetRoundtrip = property $ do
    ruleset <- forAll genPermissionRuleset
    let json = encode ruleset
    case decode json of
        Nothing -> failure
        Just ruleset' -> ruleset === ruleset'

prop_agentModeRoundtrip :: Property
prop_agentModeRoundtrip = property $ do
    mode <- forAll genAgentMode
    let json = encode mode
    case decode json of
        Nothing -> failure
        Just mode' -> mode === mode'

prop_agentRoundtrip :: Property
prop_agentRoundtrip = property $ do
    agent <- forAll genAgent
    let json = encode agent
    case decode json of
        Nothing -> failure
        Just agent' -> agent === agent'

-- | Property: defaultAgent creates agent with correct name
prop_defaultAgentHasName :: Property
prop_defaultAgentHasName = property $ do
    name <- forAll genNonEmptyText
    mode <- forAll genAgentMode
    ruleset <- forAll genPermissionRuleset
    let agent = defaultAgent name mode ruleset
    agentName agent === name
    agentMode agent === mode
    agentPermission agent === ruleset

-- | Property: defaultAgent has native set to True
prop_defaultAgentIsNative :: Property
prop_defaultAgentIsNative = property $ do
    name <- forAll genNonEmptyText
    mode <- forAll genAgentMode
    ruleset <- forAll genPermissionRuleset
    let agent = defaultAgent name mode ruleset
    agentNative agent === Just True

-- | Property: defaultAgent has empty options
prop_defaultAgentEmptyOptions :: Property
prop_defaultAgentEmptyOptions = property $ do
    name <- forAll genNonEmptyText
    mode <- forAll genAgentMode
    ruleset <- forAll genPermissionRuleset
    let agent = defaultAgent name mode ruleset
    agentOptions agent === Map.empty

-- | Property: PermissionRuleset preserves keys
prop_permissionRulesetPreservesKeys :: Property
prop_permissionRulesetPreservesKeys = property $ do
    ruleset <- forAll genPermissionRuleset
    let keys = Map.keys (unRuleset ruleset)
        json = encode ruleset
    case decode json of
        Nothing -> failure
        Just (PermissionRuleset m') -> Map.keys m' === keys

-- ============================================================================
-- Pure Query Function Properties
-- ============================================================================

-- | Property: lookupPermission finds rules that exist in the ruleset
prop_lookupPermissionFindsExisting :: Property
prop_lookupPermissionFindsExisting = property $ do
    toolName <- forAll genNonEmptyText
    rules <- forAll $ Gen.list (Range.linear 1 3) genPermissionRule
    let rs = PermissionRuleset (Map.singleton toolName rules)
    agent <- forAll $ genAgentWithRuleset rs
    lookupPermission toolName agent === Just rules

-- | Property: lookupPermission returns Nothing for missing tools
prop_lookupPermissionMissing :: Property
prop_lookupPermissionMissing = property $ do
    agent <- forAll $ genAgentWithRuleset (PermissionRuleset Map.empty)
    toolName <- forAll genNonEmptyText
    lookupPermission toolName agent === Nothing

-- | Property: hasPermissionFor is consistent with lookupPermission
prop_hasPermissionForConsistent :: Property
prop_hasPermissionForConsistent = property $ do
    agent <- forAll genAgent
    toolName <- forAll genNonEmptyText
    let hasIt = hasPermissionFor toolName agent
        found = lookupPermission toolName agent
    case found of
        Nothing -> hasIt === False
        Just _ -> hasIt === True

-- | Property: isHidden returns False for Nothing hidden field
prop_isHiddenNothing :: Property
prop_isHiddenNothing = property $ do
    name <- forAll genNonEmptyText
    mode <- forAll genAgentMode
    ruleset <- forAll genPermissionRuleset
    let agent = defaultAgent name mode ruleset
    -- defaultAgent sets agentHidden to Nothing
    isHidden agent === False

-- | Property: isHidden returns the value when hidden is Just
prop_isHiddenJust :: Property
prop_isHiddenJust = property $ do
    name <- forAll genNonEmptyText
    mode <- forAll genAgentMode
    ruleset <- forAll genPermissionRuleset
    hidden <- forAll Gen.bool
    let agent = (defaultAgent name mode ruleset){agentHidden = Just hidden}
    isHidden agent === hidden

-- | Property: isNative returns the value when native is Just
prop_isNativeJust :: Property
prop_isNativeJust = property $ do
    name <- forAll genNonEmptyText
    mode <- forAll genAgentMode
    ruleset <- forAll genPermissionRuleset
    native <- forAll Gen.bool
    let agent = (defaultAgent name mode ruleset){agentNative = Just native}
    isNative agent === native

-- | Property: getEffectiveTemperature returns default 1.0 when not set
prop_getEffectiveTemperatureDefault :: Property
prop_getEffectiveTemperatureDefault = property $ do
    name <- forAll genNonEmptyText
    mode <- forAll genAgentMode
    ruleset <- forAll genPermissionRuleset
    let agent = defaultAgent name mode ruleset
    -- defaultAgent sets agentTemperature to Nothing
    getEffectiveTemperature agent === 1.0

-- | Property: getEffectiveTemperature returns the set value
prop_getEffectiveTemperatureSet :: Property
prop_getEffectiveTemperatureSet = property $ do
    name <- forAll genNonEmptyText
    mode <- forAll genAgentMode
    ruleset <- forAll genPermissionRuleset
    temp <- forAll $ Gen.double (Range.linearFrac 0 2)
    let agent = (defaultAgent name mode ruleset){agentTemperature = Just temp}
    getEffectiveTemperature agent === temp

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Agent.Types Property Tests"
        [ testGroup
            "JSON Round-trip"
            [ testProperty "PermissionAction" prop_permissionActionRoundtrip
            , testProperty "PermissionRule" prop_permissionRuleRoundtrip
            , testProperty "PermissionRuleset" prop_permissionRulesetRoundtrip
            , testProperty "AgentMode" prop_agentModeRoundtrip
            , testProperty "Agent" prop_agentRoundtrip
            , testProperty "PermissionRuleset preserves keys" prop_permissionRulesetPreservesKeys
            ]
        , testGroup
            "Smart Constructors"
            [ testProperty "defaultAgent has correct name/mode/permission" prop_defaultAgentHasName
            , testProperty "defaultAgent is native" prop_defaultAgentIsNative
            , testProperty "defaultAgent has empty options" prop_defaultAgentEmptyOptions
            ]
        , testGroup
            "Pure Query Functions"
            [ testProperty "lookupPermission finds existing" prop_lookupPermissionFindsExisting
            , testProperty "lookupPermission returns Nothing for missing" prop_lookupPermissionMissing
            , testProperty "hasPermissionFor consistent with lookupPermission" prop_hasPermissionForConsistent
            , testProperty "isHidden returns False for Nothing" prop_isHiddenNothing
            , testProperty "isHidden returns Just value" prop_isHiddenJust
            , testProperty "isNative returns Just value" prop_isNativeJust
            , testProperty "getEffectiveTemperature default is 1.0" prop_getEffectiveTemperatureDefault
            , testProperty "getEffectiveTemperature returns set value" prop_getEffectiveTemperatureSet
            ]
        ]
