{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Skill.Skill
Description : Skill discovery and management for weapon server

This module provides functionality for discovering, loading, and managing
skills from multiple sources:

* Directory-based skills from project and global skill directories
* Configuration-based skills from Dhall config files
* Remote skills fetched from HTTP URLs

= Skill Format

Skills are defined in @SKILL.md@ files with YAML frontmatter:

@
---
name: my-skill
description: A description of what this skill does
---
The skill content/instructions go here.
@

= Discovery Order

Skills are discovered from:

1. Global directories (@~\/.config\/weapon\/skills@, @~\/.claude\/skills@, etc.)
2. Project directories (walking up from the project root)
3. Dhall configuration (@skill@ map in config)

Config-based skills take precedence over file-based skills with the same name.
-}
module Skill.Skill (
    -- * Types
    SkillInfo (..),
    SkillIndex (..),
    SkillIndexEntry (..),

    -- * Discovery
    listSkills,
    listSkillsWithConfig,

    -- * Parsing (Pure)
    parseSkill,
    parseSkillIndex,
    parseFrontmatter,
    parseMeta,

    -- * Path utilities (Pure)
    walkUp,
    skillDirsForPath,
    globalSkillDirs,

    -- * Config conversion (Pure)
    skillsFromConfig,
    skillConfigToInfo,
) where

import Config.Config qualified as Config
import Config.Types qualified as CT
import Control.Exception (try, SomeException)
import Control.Monad (foldM, forM)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Log qualified
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory, makeAbsolute)
import System.FilePath (takeDirectory, takeFileName, (</>))

-- ════════════════════════════════════════════════════════════════════════════
--                                                                       Types
-- ════════════════════════════════════════════════════════════════════════════

{- | Information about a discovered skill.

A skill represents a reusable prompt or instruction set that can be
loaded by the AI agent to perform specific tasks.
-}
data SkillInfo = SkillInfo
    { skillName :: Text
    -- ^ Unique identifier for the skill
    , skillDescription :: Text
    -- ^ Human-readable description of what the skill does
    , skillLocation :: Text
    -- ^ Source location (file path or @config:name@ for config-based skills)
    , skillContent :: Text
    -- ^ The actual skill instructions/prompt content
    }
    deriving (Show, Eq, Generic)

instance ToJSON SkillInfo where
    toJSON skill =
        object
            [ "name" .= skillName skill
            , "description" .= skillDescription skill
            , "location" .= skillLocation skill
            , "content" .= skillContent skill
            ]

{- | Index of skills available from a remote source.

Used for discovering skills from HTTP URLs that provide a @index.json@
listing available skills and their files.
-}
newtype SkillIndex = SkillIndex
    { siSkills :: [SkillIndexEntry]
    -- ^ List of skill entries in the index
    }
    deriving (Show, Eq, Generic)

-- | Entry in a skill index describing a single skill.
data SkillIndexEntry = SkillIndexEntry
    { sieName :: Text
    -- ^ Name/identifier of the skill
    , sieDescription :: Maybe Text
    -- ^ Optional description
    , sieFiles :: [Text]
    -- ^ List of files that make up the skill
    }
    deriving (Show, Eq, Generic)

instance FromJSON SkillIndex where
    parseJSON = withObject "SkillIndex" $ \v ->
        SkillIndex <$> v .:? "skills" .!= []

instance FromJSON SkillIndexEntry where
    parseJSON = withObject "SkillIndexEntry" $ \v ->
        SkillIndexEntry
            <$> v .: "name"
            <*> v .:? "description"
            <*> v .:? "files" .!= []

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Pure Functions
-- ════════════════════════════════════════════════════════════════════════════

{- | Parse a skill from its file content.

Extracts the name and description from YAML frontmatter, and the
remaining content as the skill body.

Returns 'Nothing' if:

* No frontmatter is found (content doesn't start with @---@)
* The frontmatter doesn't contain a @name@ field
* The frontmatter doesn't contain a @description@ field

==== __Examples__

>>> let content = "---\nname: test\ndescription: A test\n---\nBody"
>>> parseSkill "/path/SKILL.md" content
Just (SkillInfo {skillName = "test", ...})

>>> parseSkill "/path/SKILL.md" "no frontmatter"
Nothing
-}
parseSkill :: FilePath -> Text -> Maybe SkillInfo
parseSkill path content = do
    (meta, body) <- parseFrontmatter (T.lines content)
    name <- Map.lookup "name" meta
    desc <- Map.lookup "description" meta
    pure $
        SkillInfo
            { skillName = name
            , skillDescription = desc
            , skillLocation = T.pack path
            , skillContent = T.unlines body
            }

{- | Parse a skill index from JSON bytes.

Returns 'Nothing' if the JSON is invalid or doesn't match the expected schema.
-}
parseSkillIndex :: ByteString -> Maybe SkillIndex
parseSkillIndex = either (const Nothing) Just . eitherDecodeStrict

{- | Parse YAML-style frontmatter from lines of text.

Frontmatter must be enclosed in @---@ delimiters at the start of the content.
Returns a map of key-value pairs and the remaining lines.

==== __Examples__

>>> parseFrontmatter ["---", "key: value", "---", "body"]
Just (Map.fromList [("key", "value")], ["body"])

>>> parseFrontmatter ["no", "frontmatter"]
Nothing
-}
parseFrontmatter :: [Text] -> Maybe (Map.Map Text Text, [Text])
parseFrontmatter lines' = case lines' of
    [] -> Nothing
    (first : rest)
        | T.strip first /= "---" -> Nothing
        | otherwise -> go rest []
  where
    go remaining acc = case remaining of
        [] -> Nothing
        (line : more)
            | T.strip line == "---" ->
                let meta = Map.fromList (mapMaybe parseMeta acc)
                 in Just (meta, more)
            | otherwise -> go more (acc <> [line])

{- | Parse a single @key: value@ line from frontmatter.

Returns 'Nothing' if the line doesn't contain a colon separator.

==== __Examples__

>>> parseMeta "name: my-skill"
Just ("name", "my-skill")

>>> parseMeta "no-colon"
Nothing
-}
parseMeta :: Text -> Maybe (Text, Text)
parseMeta line =
    let (key, rest) = T.breakOn ":" line
     in if T.null rest
            then Nothing
            else Just (T.strip key, T.strip (T.drop 1 rest))

{- | Walk up directory tree from a starting path.

Returns all directories from the starting path up to (and including) the root.
Useful for finding project-level config files.

==== __Examples__

>>> walkUp "/home/user/project/src"
["/home/user/project/src", "/home/user/project", "/home/user", "/home", "/"]
-}
walkUp :: FilePath -> [FilePath]
walkUp start = go start id
  where
    go dir acc =
        let parent = takeDirectory dir
            next = acc . (dir :)
         in if parent == dir then next [] else go parent next

{- | Get skill directory paths for a given base directory.

Returns the standard skill directory locations relative to the base.
-}
skillDirsForPath :: FilePath -> [FilePath]
skillDirsForPath dir =
    [ dir </> ".weapon" </> "skill"
    , dir </> ".weapon" </> "skills"
    , dir </> ".claude" </> "skills"
    , dir </> ".agents" </> "skills"
    ]

{- | Get global skill directory paths given the home directory.

Returns the standard global skill directory locations.
-}
globalSkillDirs :: FilePath -> [FilePath]
globalSkillDirs home =
    [ home </> ".config" </> "weapon" </> "skills"
    , home </> ".claude" </> "skills"
    , home </> ".agents" </> "skills"
    ]

{- | Convert SkillConfig records from Dhall config to SkillInfo.

Transforms all skills defined in the configuration into 'SkillInfo' records.
The location is prefixed with @config:@ to distinguish from file-based skills.
-}
skillsFromConfig :: CT.Config -> [SkillInfo]
skillsFromConfig cfg = case CT.cfgSkill cfg of
    Nothing -> []
    Just skillMap -> map skillConfigToInfo (Map.toList skillMap)

-- | Convert a single (name, SkillConfig) pair to SkillInfo.
skillConfigToInfo :: (Text, CT.SkillConfig) -> SkillInfo
skillConfigToInfo (name, sc) =
    SkillInfo
        { skillName = CT.skillName sc
        , skillDescription = CT.skillDescription sc
        , skillLocation = "config:" <> name
        , skillContent = CT.skillPrompt sc
        }

-- ════════════════════════════════════════════════════════════════════════════
--                                                                IO Functions
-- ════════════════════════════════════════════════════════════════════════════

{- | List all available skills.

Discovers skills from all sources: global directories, project directories,
and Dhall configuration. Requires a 'Config.DhallCache' for efficient
config loading.

Skills from configuration take precedence over file-based skills with
the same name.
-}
listSkills :: Config.DhallCache -> FilePath -> IO [SkillInfo]
listSkills cache root = do
    cfg <- Config.load cache root
    listSkillsWithConfig cfg root

{- | List skills with a pre-loaded config.

Use this variant when you already have a loaded 'CT.Config' to avoid
redundant config parsing.
-}
listSkillsWithConfig :: CT.Config -> FilePath -> IO [SkillInfo]
listSkillsWithConfig cfg root = do
    home <- getHomeDirectory
    projectDirs <- projectSkillRoots root
    let configSkills = skillsFromConfig cfg
    let globalDirs = globalSkillDirs home
    files <- discoverSkillFiles (globalDirs ++ projectDirs)
    fileInfos <- loadSkillFiles files
    -- Merge config skills with file skills (config takes precedence)
    let merged = Map.union (Map.fromList [(skillName s, s) | s <- configSkills]) fileInfos
    pure (Map.elems merged)

{- | Discover skill files from multiple directories.

Recursively searches each directory for @SKILL.md@ files.
-}
discoverSkillFiles :: [FilePath] -> IO [FilePath]
discoverSkillFiles dirs = concat <$> mapM findSkills dirs

{- | Load skill files and accumulate into a map.

Parses each file and builds a map keyed by skill name.
Invalid or unparseable files are silently skipped.
-}
loadSkillFiles :: [FilePath] -> IO (Map.Map Text SkillInfo)
loadSkillFiles = foldM addSkill Map.empty

{- | Get project skill root directories.

Walks up the directory tree from the project root and returns
all potential skill directories.
-}
projectSkillRoots :: FilePath -> IO [FilePath]
projectSkillRoots root = do
    base <- makeAbsolute root
    let dirs = walkUp base
    pure $ concatMap skillDirsForPath dirs

{- | Recursively find all SKILL.md files in a directory.

Returns an empty list if the directory doesn't exist.
-}
findSkills :: FilePath -> IO [FilePath]
findSkills dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else scanDirectory dir

-- | Scan a directory recursively for SKILL.md files.
scanDirectory :: FilePath -> IO [FilePath]
scanDirectory path = do
    entries <- listDirectory path
    parts <- forM entries $ \entry -> do
        let item = path </> entry
        isDir <- doesDirectoryExist item
        if isDir
            then scanDirectory item
            else checkSkillFile item
    pure (concat parts)

-- | Check if a file is a SKILL.md and return it if so.
checkSkillFile :: FilePath -> IO [FilePath]
checkSkillFile item = do
    isFile <- doesFileExist item
    pure [item | isFile && takeFileName item == "SKILL.md"]

{- | Add a skill from a file to the accumulator map.

Reads and parses the file. If parsing fails, the accumulator
is returned unchanged.
-}
addSkill :: Map.Map Text SkillInfo -> FilePath -> IO (Map.Map Text SkillInfo)
addSkill acc path = do
    absolute <- makeAbsolute path
    content <- TIO.readFile absolute
    case parseSkill absolute content of
        Nothing -> pure acc
        Just skill -> pure (Map.insert (skillName skill) skill acc)

-- ════════════════════════════════════════════════════════════════════════════
--                                                           Remote Skill Fetch
-- ════════════════════════════════════════════════════════════════════════════

{- | Pull skills from a remote URL.

Fetches the skill index from @\<url\>\/index.json@ and downloads all
referenced skill files to the local cache.

Note: This is kept for backwards compatibility but skills in config
are now defined inline via Dhall.
-}
pullSkills :: Log.Logger -> String -> IO [FilePath]
pullSkills logger url = do
    manager <- newTlsManager
    let baseUrl = normalizeBaseUrl url
    mIndex <- fetchSkillIndex logger manager baseUrl
    case mIndex of
        Nothing -> pure []
        Just idx -> do
            cache <- skillCacheDir
            results <- mapM (downloadSkillEntry logger manager cache baseUrl) (siSkills idx)
            pure (concat results)

-- | Normalize a base URL to ensure it ends with a slash.
normalizeBaseUrl :: String -> String
normalizeBaseUrl url
    | "/" `T.isSuffixOf` T.pack url = url
    | otherwise = url <> "/"

-- | Fetch the skill index from a remote URL.
fetchSkillIndex :: Log.Logger -> Manager -> String -> IO (Maybe SkillIndex)
fetchSkillIndex logger manager baseUrl = do
    let indexUrl = baseUrl <> "index.json"
    result <- try $ do
        indexReq <- parseRequest indexUrl
        httpLbs indexReq manager
    case result of
        Left (err :: SomeException) -> do
            Log.logWarn logger ("Failed to fetch skill index from " <> T.pack indexUrl <> ": " <> T.pack (show err)) ()
            pure Nothing
        Right indexResp -> do
            let code = statusCode (responseStatus indexResp)
            if code /= 200
                then do
                    Log.logWarn logger ("Failed to fetch skill index from " <> T.pack indexUrl <> ": HTTP " <> T.pack (show code)) ()
                    pure Nothing
                else case parseSkillIndex (BSL.toStrict (responseBody indexResp)) of
                    Nothing -> do
                        Log.logWarn logger ("Failed to parse skill index from " <> T.pack indexUrl <> ": invalid JSON") ()
                        pure Nothing
                    Just idx -> pure (Just idx)

-- | Download all files for a skill entry.
downloadSkillEntry :: Log.Logger -> Manager -> FilePath -> String -> SkillIndexEntry -> IO [FilePath]
downloadSkillEntry logger manager cache baseUrl entry = do
    let name = sieName entry
    let root = cache </> T.unpack name
    createDirectoryIfMissing True root
    mapM_ (downloadSkillFile logger manager baseUrl root name) (sieFiles entry)
    checkSkillMdExists root

-- | Check if SKILL.md exists in a directory and return the directory if so.
checkSkillMdExists :: FilePath -> IO [FilePath]
checkSkillMdExists root = do
    let md = root </> "SKILL.md"
    exists <- doesFileExist md
    pure [root | exists]

-- | Download a single skill file.
downloadSkillFile :: Log.Logger -> Manager -> String -> FilePath -> Text -> Text -> IO ()
downloadSkillFile logger manager baseUrl root name file = do
    let url = baseUrl <> T.unpack name <> "/" <> T.unpack file
    result <- try $ do
        req <- parseRequest url
        httpLbs req manager
    case result of
        Left (err :: SomeException) -> do
            Log.logWarn logger ("Failed to download skill file " <> name <> "/" <> file <> ": " <> T.pack (show err)) ()
        Right resp -> do
            let code = statusCode (responseStatus resp)
            if code /= 200
                then Log.logError logger ("Failed to download skill " <> name <> "/" <> file <> ": HTTP " <> T.pack (show code)) ()
                else do
                    let dest = root </> T.unpack file
                    createDirectoryIfMissing True (takeDirectory dest)
                    BS.writeFile dest (BSL.toStrict (responseBody resp))

{- | Get the skill cache directory.

Creates the directory if it doesn't exist.
-}
skillCacheDir :: IO FilePath
skillCacheDir = do
    home <- getHomeDirectory
    let dir = home </> ".cache" </> "opencode" </> "skills"
    createDirectoryIfMissing True dir
    pure dir

-- Silence unused warning for pullSkills (kept for potential future use)
_unusedPullSkills :: Log.Logger -> String -> IO [FilePath]
_unusedPullSkills = pullSkills
