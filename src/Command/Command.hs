{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Command.Command
Description : Command discovery and listing for weapon server

Commands are AI prompt templates that can be invoked with @\/name@.
They come from multiple sources:

1. Built-in default commands (init, review)
2. User-defined commands from config
3. Skills (loaded as commands)

= Command Format

Commands in the OpenAPI response have:

* @name@ - Command identifier (required)
* @template@ - Prompt template with placeholders (required)
* @hints@ - Extracted placeholders like @$1@, @$ARGUMENTS@ (required)
* @description@ - Human-readable description (optional)
* @agent@ - Agent to use (optional)
* @model@ - Model to use (optional)
* @source@ - Either "command" or "skill" (optional)
* @subtask@ - Whether to run as subtask (optional)
-}
module Command.Command (
    -- * Types
    CommandInfo (..),
    CommandSource (..),

    -- * Listing
    listCommands,

    -- * Hint Extraction
    extractHints,
) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Config.Config qualified as Config
import Config.Types (CommandConfig (..), Config (..))
import Skill.Skill (SkillInfo (..))
import Skill.Skill qualified as Skill

-- | Source of a command
data CommandSource
    = CommandSourceCommand
    | CommandSourceSkill
    deriving (Eq, Show)

instance ToJSON CommandSource where
    toJSON CommandSourceCommand = "command"
    toJSON CommandSourceSkill = "skill"

-- | Information about a command for the API response
data CommandInfo = CommandInfo
    { ciName :: Text
    -- ^ Command identifier
    , ciTemplate :: Text
    -- ^ Prompt template
    , ciHints :: [Text]
    -- ^ Extracted placeholders
    , ciDescription :: Maybe Text
    -- ^ Human-readable description
    , ciAgent :: Maybe Text
    -- ^ Agent to use
    , ciModel :: Maybe Text
    -- ^ Model to use
    , ciSource :: Maybe CommandSource
    -- ^ Source: "command" or "skill"
    , ciSubtask :: Maybe Bool
    -- ^ Run as subtask
    }
    deriving (Eq, Show)

instance ToJSON CommandInfo where
    toJSON CommandInfo{..} =
        object $
            [ "name" .= ciName
            , "template" .= ciTemplate
            , "hints" .= ciHints
            ]
                ++ catMaybes
                    [ ("description" .=) <$> ciDescription
                    , ("agent" .=) <$> ciAgent
                    , ("model" .=) <$> ciModel
                    , ("source" .=) <$> ciSource
                    , ("subtask" .=) <$> ciSubtask
                    ]

{- | Extract hints (placeholders) from a template string.

Finds numbered placeholders ($1, $2, etc.) and $ARGUMENTS.

==== Examples

>>> extractHints "Review $1 for issues"
["$1"]

>>> extractHints "Run $1 with $ARGUMENTS"
["$1", "$ARGUMENTS"]

>>> extractHints "No placeholders here"
[]
-}
extractHints :: Text -> [Text]
extractHints template =
    let
        -- Find numbered placeholders like $1, $2, etc.
        numbered = findNumbered template
        -- Check for $ARGUMENTS
        hasArgs = "$ARGUMENTS" `T.isInfixOf` template
     in
        numbered ++ ["$ARGUMENTS" | hasArgs]
  where
    findNumbered :: Text -> [Text]
    findNumbered t =
        -- Use Set.toAscList for O(n log n) dedup+sort instead of O(n²) nub
        -- Use T.zip to avoid O(n) T.length and O(n) T.index calls
        Set.toAscList . Set.fromList $
            [ "$" <> T.singleton c
            | (prev, c) <- T.zip t (T.drop 1 t)
            , prev == '$'
            , c >= '1' && c <= '9'
            ]

-- | Built-in default commands
defaultCommands :: FilePath -> [CommandInfo]
defaultCommands worktree =
    [ CommandInfo
        { ciName = "init"
        , ciTemplate = initTemplate worktree
        , ciHints = []
        , ciDescription = Just "create/update AGENTS.md"
        , ciAgent = Nothing
        , ciModel = Nothing
        , ciSource = Just CommandSourceCommand
        , ciSubtask = Nothing
        }
    , CommandInfo
        { ciName = "review"
        , ciTemplate = reviewTemplate worktree
        , ciHints = extractHints (reviewTemplate worktree)
        , ciDescription = Just "review changes [commit|branch|pr], defaults to uncommitted"
        , ciAgent = Nothing
        , ciModel = Nothing
        , ciSource = Just CommandSourceCommand
        , ciSubtask = Just True
        }
    ]

-- | Template for the init command
initTemplate :: FilePath -> Text
initTemplate path =
    T.unlines
        [ "Analyze this codebase and create or update the AGENTS.md file at the root."
        , ""
        , "The AGENTS.md file should contain:"
        , "- Project overview and structure"
        , "- Key conventions and patterns used"
        , "- Important files and their purposes"
        , "- Development workflow guidelines"
        , ""
        , "Working directory: " <> T.pack path
        ]

-- | Template for the review command
reviewTemplate :: FilePath -> Text
reviewTemplate path =
    T.unlines
        [ "Review the changes in $1 (defaults to uncommitted changes if not specified)."
        , ""
        , "For each file changed:"
        , "- Summarize what changed"
        , "- Identify potential issues or bugs"
        , "- Suggest improvements if applicable"
        , ""
        , "Working directory: " <> T.pack path
        ]

-- | Convert a config command to CommandInfo
configToCommand :: Text -> CommandConfig -> CommandInfo
configToCommand name CommandConfig{..} =
    CommandInfo
        { ciName = name
        , ciTemplate = cmdTemplate
        , ciHints = extractHints cmdTemplate
        , ciDescription = cmdDescription
        , ciAgent = cmdAgent
        , ciModel = cmdModel
        , ciSource = Just CommandSourceCommand
        , ciSubtask = cmdSubtask
        }

-- | Convert a skill to CommandInfo
skillToCommand :: SkillInfo -> CommandInfo
skillToCommand SkillInfo{..} =
    CommandInfo
        { ciName = skillName
        , ciTemplate = skillContent
        , ciHints = [] -- Skills don't typically have numbered placeholders
        , ciDescription = Just skillDescription
        , ciAgent = Nothing
        , ciModel = Nothing
        , ciSource = Just CommandSourceSkill
        , ciSubtask = Nothing
        }

{- | List all available commands.

Combines built-in defaults, config commands, and skills.
Config commands take precedence over skills with the same name.
-}
listCommands :: Config.DhallCache -> FilePath -> IO [CommandInfo]
listCommands dhallCache dir = do
    -- Load config
    cfg <- Config.load dhallCache dir

    -- Get skills
    skills <- Skill.listSkills dhallCache dir

    -- Start with built-in defaults
    let defaults = defaultCommands dir

    -- Add config commands
    let configCmds = case cfgCommand cfg of
            Nothing -> []
            Just cmdMap ->
                [ configToCommand name cmd
                | (name, cmd) <- Map.toList cmdMap
                ]

    -- Add skills (but don't override existing commands)
    let existingNames = map ciName defaults ++ map ciName configCmds
    let skillCmds =
            [ skillToCommand skill
            | skill <- skills
            , skillName skill `notElem` existingNames
            ]

    return $ defaults ++ configCmds ++ skillCmds
