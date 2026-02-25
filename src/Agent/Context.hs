{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Agent.Context
Description : Environment context gathering and system prompt building for agents

This module provides functionality for:

* Gathering environment context (working directory, git status, platform, date)
* Detecting key project files (README, build files, etc.)
* Building rich system prompts with context and philosophy
* Agent-specific prompt customization

= Architecture

The system prompt is built in layers:

1. @\<env\>@ block - working directory, git status, platform, date
2. @\<files\>@ block - key project files found
3. @\<philosophy\>@ block - functional programming principles (for coding agents)
4. Agent-specific prompt - the agent's custom instructions

= Usage

@
ctx <- gatherContext exeCache "\/path\/to\/project"
let prompt = buildSystemPrompt ctx (Just agent)
@
-}
module Agent.Context (
    -- * Context Types
    AgentContext (..),

    -- * Context Gathering
    gatherContext,
    detectKeyFiles,

    -- * Prompt Building
    buildSystemPrompt,
    formatEnvBlock,
    formatFilesBlock,
    philosophyBlock,
) where

import Agent.Types (Agent (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, utctDay)
import System.Directory (doesFileExist)
import System.Info qualified as Info
import Util.ExeCache (ExeCache)
import Vcs.Status qualified as Vcs

-- ════════════════════════════════════════════════════════════════════════════
-- Context Types
-- ════════════════════════════════════════════════════════════════════════════

{- | Environment context gathered for an agent.

Contains all the information an agent might need about its execution
environment, including working directory, version control status,
platform information, and notable project files.
-}
data AgentContext = AgentContext
    { acWorkingDir :: FilePath
    -- ^ The current working directory
    , acIsGitRepo :: Bool
    -- ^ Whether the working directory is inside a git repository
    , acGitBranch :: Maybe Text
    -- ^ Current git branch (Nothing if not a git repo or detached HEAD)
    , acPlatform :: Text
    -- ^ Operating system: "linux", "darwin", "windows", etc.
    , acDate :: Text
    -- ^ Today's date formatted as "Wed Feb 25 2026"
    , acKeyFiles :: [FilePath]
    -- ^ Notable project files found (README, build files, etc.)
    }
    deriving (Eq, Show)

-- ════════════════════════════════════════════════════════════════════════════
-- Context Gathering
-- ════════════════════════════════════════════════════════════════════════════

{- | Gather environment context for the given working directory.

This performs IO to:

* Check git repository status and branch
* Detect the operating system
* Get the current date
* Scan for key project files

==== __Example__

@
ctx <- gatherContext exeCache "\/home\/user\/myproject"
-- AgentContext { acWorkingDir = "\/home\/user\/myproject"
--              , acIsGitRepo = True
--              , acGitBranch = Just "main"
--              , acPlatform = "linux"
--              , acDate = "Wed Feb 25 2026"
--              , acKeyFiles = ["README.md", "myproject.cabal", "flake.nix"]
--              }
@
-}
gatherContext :: ExeCache -> FilePath -> IO AgentContext
gatherContext exeCache workdir = do
    -- Get git info
    mBranch <- Vcs.loadBranch exeCache workdir
    let isGitRepo = case mBranch of
            Just _ -> True
            Nothing -> False

    -- Get platform
    let platform = T.pack Info.os

    -- Get date
    now <- getCurrentTime
    let day = utctDay now
    let dateStr = formatTime defaultTimeLocale "%a %b %d %Y" day

    -- Detect key files
    keyFiles <- detectKeyFiles workdir

    pure
        AgentContext
            { acWorkingDir = workdir
            , acIsGitRepo = isGitRepo
            , acGitBranch = mBranch
            , acPlatform = platform
            , acDate = T.pack dateStr
            , acKeyFiles = keyFiles
            }

{- | Detect key project files in the given directory.

Checks for common project files like:

* Documentation: README.md, README, LICENSE
* Haskell: *.cabal, cabal.project, stack.yaml
* Nix: flake.nix, default.nix, shell.nix
* Node: package.json
* PureScript: spago.dhall, spago.yaml
* Rust: Cargo.toml
* Python: pyproject.toml, setup.py, requirements.txt
* Go: go.mod
* Build: Makefile, Justfile
-}
detectKeyFiles :: FilePath -> IO [FilePath]
detectKeyFiles workdir = do
    let candidates =
            [ "README.md"
            , "README"
            , "LICENSE"
            , -- Haskell
              "cabal.project"
            , "stack.yaml"
            , -- Nix
              "flake.nix"
            , "default.nix"
            , "shell.nix"
            , -- Node/JS
              "package.json"
            , "tsconfig.json"
            , -- PureScript
              "spago.dhall"
            , "spago.yaml"
            , -- Rust
              "Cargo.toml"
            , -- Python
              "pyproject.toml"
            , "setup.py"
            , "requirements.txt"
            , -- Go
              "go.mod"
            , -- Build
              "Makefile"
            , "Justfile"
            , -- Config
              "weapon.dhall"
            ]

    -- Check each candidate
    found <- mapM (checkFile workdir) candidates

    -- Also look for *.cabal files (variable name)
    cabalFiles <- findCabalFiles workdir

    pure $ cabalFiles ++ catMaybes found

-- | Check if a file exists and return Just the filename if it does
checkFile :: FilePath -> FilePath -> IO (Maybe FilePath)
checkFile dir filename = do
    exists <- doesFileExist (dir ++ "/" ++ filename)
    pure $ if exists then Just filename else Nothing

-- | Find *.cabal files in the directory
findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir = do
    -- Simple approach: check for common patterns
    -- A more robust approach would use directory listing
    let commonCabalNames =
            [ "weapon-server.cabal"
            , "project.cabal"
            , "app.cabal"
            , "lib.cabal"
            ]
    found <- mapM (checkFile dir) commonCabalNames
    pure $ catMaybes found

-- ════════════════════════════════════════════════════════════════════════════
-- Prompt Building
-- ════════════════════════════════════════════════════════════════════════════

{- | Build a complete system prompt for an agent with context.

The prompt structure varies by agent type:

* __Primary/Subagent coding agents__ (armed, locked, general): Full context
  with env block, files block, philosophy block, and agent prompt.

* __Explore agent__: Env and files blocks, but no philosophy (speed focus).

* __Utility agents__ (compaction, title, summary): Just the agent prompt,
  no environment context needed.

Returns 'Nothing' if the agent has no prompt and doesn't need context.
-}
buildSystemPrompt :: AgentContext -> Maybe Agent -> Maybe Text
buildSystemPrompt _ctx Nothing = Nothing
buildSystemPrompt ctx (Just agent) =
    let name = agentName agent
        basePrompt = fromMaybe "" (agentPrompt agent)
     in case name of
            -- Utility agents: no context needed
            "compaction" -> agentPrompt agent
            "title" -> agentPrompt agent
            "summary" -> agentPrompt agent
            -- Explore agent: context but no philosophy
            "explore" ->
                Just $
                    T.unlines
                        [ formatEnvBlock ctx
                        , formatFilesBlock ctx
                        , basePrompt
                        ]
            -- Coding agents: full context with philosophy
            _ ->
                Just $
                    T.unlines
                        [ formatEnvBlock ctx
                        , formatFilesBlock ctx
                        , philosophyBlock
                        , basePrompt
                        ]

{- | Format the environment block for a system prompt.

==== __Example Output__

@
\<env\>
  Working directory: \/home\/user\/myproject
  Is directory a git repo: yes
  Git branch: main
  Platform: linux
  Today's date: Wed Feb 25 2026
\<\/env\>
@
-}
formatEnvBlock :: AgentContext -> Text
formatEnvBlock AgentContext{..} =
    T.unlines
        [ "<env>"
        , "  Working directory: " <> T.pack acWorkingDir
        , "  Is directory a git repo: " <> if acIsGitRepo then "yes" else "no"
        , maybe "" ("  Git branch: " <>) acGitBranch
        , "  Platform: " <> acPlatform
        , "  Today's date: " <> acDate
        , "</env>"
        ]

{- | Format the files block for a system prompt.

Lists key project files found in the working directory.

==== __Example Output__

@
\<files\>
  README.md
  weapon-server.cabal
  flake.nix
\<\/files\>
@

Returns an empty string if no key files were found.
-}
formatFilesBlock :: AgentContext -> Text
formatFilesBlock AgentContext{..}
    | null acKeyFiles = ""
    | otherwise =
        T.unlines $
            ["<files>"]
                ++ map (\f -> "  " <> T.pack f) acKeyFiles
                ++ ["</files>"]

{- | The philosophy block encouraging functional programming rigor.

This block is included for coding agents (primary and subagent) to guide
their approach to problem-solving and code design.
-}
philosophyBlock :: Text
philosophyBlock =
    T.unlines
        [ "<philosophy>"
        , "You approach problems with the rigor of a functional programmer. You:"
        , "- Favor Haskell for backends, PureScript for frontends, and typed languages in general"
        , "- Build with Nix when given the chance - reproducibility matters"
        , "- Never settle for bad abstractions - refactor until the types tell the story"
        , "- Have strong opinions backed by experience, but remain open to better ideas"
        , "- Use the type system to make invalid states unrepresentable"
        , "- Prefer pure functions, explicit effects, and composable designs"
        , "- Value correctness over convenience, but recognize pragmatic tradeoffs"
        , "- Write code that is easy to delete, not easy to extend"
        , "- When in doubt, make it a pure function that returns data"
        , "- Reach for property tests before example tests - generate, don't enumerate"
        , "- When correctness is paramount, suggest Lean 4 proofs to verify critical invariants"
        , "</philosophy>"
        ]
