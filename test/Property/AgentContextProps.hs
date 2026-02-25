{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.AgentContextProps
Description : Property tests for Agent.Context module

Tests for:

* Environment context formatting
* System prompt construction for different agent types
* Philosophy block content
-}
module Property.AgentContextProps (tests) where

import Agent.Context
import Agent.Types (Agent (..), AgentMode (..), PermissionRuleset (..), defaultAgent)
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- ════════════════════════════════════════════════════════════════════════════
-- AgentContext Tests
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: formatEnvBlock contains working directory
prop_envBlock_containsWorkdir :: Property
prop_envBlock_containsWorkdir = property $ do
    workdir <- forAll genFilePath
    let ctx = mkTestContext workdir
        block = formatEnvBlock ctx
    assert $ T.isInfixOf (T.pack workdir) block

-- | Property: formatEnvBlock contains platform
prop_envBlock_containsPlatform :: Property
prop_envBlock_containsPlatform = property $ do
    platform <- forAll $ Gen.element ["linux", "darwin", "windows"]
    let ctx = (mkTestContext "/test"){acPlatform = platform}
        block = formatEnvBlock ctx
    assert $ T.isInfixOf platform block

-- | Property: formatEnvBlock contains date
prop_envBlock_containsDate :: Property
prop_envBlock_containsDate = property $ do
    let ctx = (mkTestContext "/test"){acDate = "Wed Feb 25 2026"}
        block = formatEnvBlock ctx
    assert $ T.isInfixOf "Wed Feb 25 2026" block

-- | Property: formatEnvBlock shows git repo status
prop_envBlock_showsGitStatus :: Property
prop_envBlock_showsGitStatus = property $ do
    isGit <- forAll Gen.bool
    let ctx = (mkTestContext "/test"){acIsGitRepo = isGit}
        block = formatEnvBlock ctx
        expected = if isGit then "yes" else "no"
    assert $ T.isInfixOf ("Is directory a git repo: " <> expected) block

-- | Property: formatEnvBlock includes git branch when present
prop_envBlock_includesGitBranch :: Property
prop_envBlock_includesGitBranch = property $ do
    branch <- forAll genBranchName
    let ctx = (mkTestContext "/test"){acIsGitRepo = True, acGitBranch = Just branch}
        block = formatEnvBlock ctx
    assert $ T.isInfixOf ("Git branch: " <> branch) block

-- | Property: formatFilesBlock is empty when no files
prop_filesBlock_emptyWhenNoFiles :: Property
prop_filesBlock_emptyWhenNoFiles = property $ do
    let ctx = (mkTestContext "/test"){acKeyFiles = []}
        block = formatFilesBlock ctx
    block === ""

-- | Property: formatFilesBlock lists files when present
prop_filesBlock_listsFiles :: Property
prop_filesBlock_listsFiles = property $ do
    files <- forAll $ Gen.list (Range.linear 1 5) genFileName
    let ctx = (mkTestContext "/test"){acKeyFiles = files}
        block = formatFilesBlock ctx
    -- Should contain <files> tags
    assert $ T.isInfixOf "<files>" block
    assert $ T.isInfixOf "</files>" block
    -- Should contain each file
    forM_ files $ \f -> assert $ T.isInfixOf (T.pack f) block

-- ════════════════════════════════════════════════════════════════════════════
-- Philosophy Block Tests
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: philosophyBlock contains Haskell mention
prop_philosophy_mentionsHaskell :: Property
prop_philosophy_mentionsHaskell = property $ do
    assert $ T.isInfixOf "Haskell" philosophyBlock

-- | Property: philosophyBlock contains Nix mention
prop_philosophy_mentionsNix :: Property
prop_philosophy_mentionsNix = property $ do
    assert $ T.isInfixOf "Nix" philosophyBlock

-- | Property: philosophyBlock contains PureScript mention
prop_philosophy_mentionsPureScript :: Property
prop_philosophy_mentionsPureScript = property $ do
    assert $ T.isInfixOf "PureScript" philosophyBlock

-- | Property: philosophyBlock contains Lean 4 mention
prop_philosophy_mentionsLean4 :: Property
prop_philosophy_mentionsLean4 = property $ do
    assert $ T.isInfixOf "Lean 4" philosophyBlock

prop_philosophy_mentionsPropertyTests :: Property
prop_philosophy_mentionsPropertyTests = property $ do
    assert $ T.isInfixOf "property test" philosophyBlock

-- | Property: philosophyBlock is wrapped in tags
prop_philosophy_hasTags :: Property
prop_philosophy_hasTags = property $ do
    assert $ T.isInfixOf "<philosophy>" philosophyBlock
    assert $ T.isInfixOf "</philosophy>" philosophyBlock

-- ════════════════════════════════════════════════════════════════════════════
-- System Prompt Building Tests
-- ════════════════════════════════════════════════════════════════════════════

-- | Property: buildSystemPrompt returns Nothing for no agent
prop_buildPrompt_nothingForNoAgent :: Property
prop_buildPrompt_nothingForNoAgent = property $ do
    let ctx = mkTestContext "/test"
    buildSystemPrompt ctx Nothing === Nothing

-- | Property: utility agents don't get context blocks
prop_buildPrompt_utilityAgentsNoContext :: Property
prop_buildPrompt_utilityAgentsNoContext = property $ do
    name <- forAll $ Gen.element ["compaction", "title", "summary"]
    let agent = (defaultAgent name Primary emptyRuleset){agentPrompt = Just "test prompt"}
        ctx = mkTestContext "/test"
        result = buildSystemPrompt ctx (Just agent)
    -- Should just return the agent prompt, not include env/files/philosophy
    case result of
        Just prompt -> do
            assert $ not (T.isInfixOf "<env>" prompt)
            assert $ not (T.isInfixOf "<philosophy>" prompt)
        Nothing -> failure

-- | Property: explore agent gets context but no philosophy
prop_buildPrompt_exploreNoPhilosophy :: Property
prop_buildPrompt_exploreNoPhilosophy = property $ do
    let agent = (defaultAgent "explore" Subagent emptyRuleset){agentPrompt = Just "explore prompt"}
        ctx = mkTestContext "/test"
        result = buildSystemPrompt ctx (Just agent)
    case result of
        Just prompt -> do
            -- Should have env block
            assert $ T.isInfixOf "<env>" prompt
            -- Should NOT have philosophy
            assert $ not (T.isInfixOf "<philosophy>" prompt)
        Nothing -> failure

-- | Property: coding agents get full context with philosophy
prop_buildPrompt_codingAgentsGetPhilosophy :: Property
prop_buildPrompt_codingAgentsGetPhilosophy = property $ do
    name <- forAll $ Gen.element ["armed", "locked", "general", "custom"]
    let agent = (defaultAgent name Primary emptyRuleset){agentPrompt = Just "test prompt"}
        ctx = mkTestContext "/test"
        result = buildSystemPrompt ctx (Just agent)
    case result of
        Just prompt -> do
            assert $ T.isInfixOf "<env>" prompt
            assert $ T.isInfixOf "<philosophy>" prompt
        Nothing -> failure

-- | Property: system prompt includes agent's base prompt
prop_buildPrompt_includesAgentPrompt :: Property
prop_buildPrompt_includesAgentPrompt = property $ do
    basePrompt <- forAll genPromptText
    let agent = (defaultAgent "test" Primary emptyRuleset){agentPrompt = Just basePrompt}
        ctx = mkTestContext "/test"
        result = buildSystemPrompt ctx (Just agent)
    case result of
        Just prompt -> assert $ T.isInfixOf basePrompt prompt
        Nothing -> failure

-- ════════════════════════════════════════════════════════════════════════════
-- Generators
-- ════════════════════════════════════════════════════════════════════════════

genFilePath :: Gen FilePath
genFilePath = do
    segments <- Gen.list (Range.linear 1 4) (Gen.string (Range.linear 1 10) Gen.alphaNum)
    pure $ "/" ++ intercalate "/" segments

genBranchName :: Gen Text
genBranchName = Gen.element ["main", "master", "develop", "feature/test", "fix/bug-123"]

genFileName :: Gen FilePath
genFileName = Gen.element ["README.md", "package.json", "flake.nix", "Makefile", "app.cabal"]

genPromptText :: Gen Text
genPromptText = Gen.text (Range.linear 10 100) Gen.alphaNum

-- | Create a test context with minimal data
mkTestContext :: FilePath -> AgentContext
mkTestContext workdir =
    AgentContext
        { acWorkingDir = workdir
        , acIsGitRepo = False
        , acGitBranch = Nothing
        , acPlatform = "linux"
        , acDate = "Wed Feb 25 2026"
        , acKeyFiles = []
        }

-- | Empty permission ruleset for testing
emptyRuleset :: PermissionRuleset
emptyRuleset = PermissionRuleset mempty

-- ════════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ════════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Agent.Context Property Tests"
        [ testGroup
            "Environment Block"
            [ testProperty "contains working directory" prop_envBlock_containsWorkdir
            , testProperty "contains platform" prop_envBlock_containsPlatform
            , testProperty "contains date" prop_envBlock_containsDate
            , testProperty "shows git repo status" prop_envBlock_showsGitStatus
            , testProperty "includes git branch when present" prop_envBlock_includesGitBranch
            ]
        , testGroup
            "Files Block"
            [ testProperty "empty when no files" prop_filesBlock_emptyWhenNoFiles
            , testProperty "lists files when present" prop_filesBlock_listsFiles
            ]
        , testGroup
            "Philosophy Block"
            [ testProperty "mentions Haskell" prop_philosophy_mentionsHaskell
            , testProperty "mentions Nix" prop_philosophy_mentionsNix
            , testProperty "mentions PureScript" prop_philosophy_mentionsPureScript
            , testProperty "mentions Lean 4" prop_philosophy_mentionsLean4
            , testProperty "mentions property tests" prop_philosophy_mentionsPropertyTests
            , testProperty "has proper tags" prop_philosophy_hasTags
            ]
        , testGroup
            "System Prompt Building"
            [ testProperty "returns Nothing for no agent" prop_buildPrompt_nothingForNoAgent
            , testProperty "utility agents don't get context" prop_buildPrompt_utilityAgentsNoContext
            , testProperty "explore agent gets no philosophy" prop_buildPrompt_exploreNoPhilosophy
            , testProperty "coding agents get philosophy" prop_buildPrompt_codingAgentsGetPhilosophy
            , testProperty "includes agent's base prompt" prop_buildPrompt_includesAgentPrompt
            ]
        ]
