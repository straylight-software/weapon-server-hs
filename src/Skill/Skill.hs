{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Skill.Skill (
    SkillInfo (..),
    SkillIndex (..),
    listSkills,
    parseSkill,
    parseSkillIndex,
) where

import Config.Config qualified as Config
import Config.Types qualified as CT
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
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory, makeAbsolute)
import System.FilePath (takeDirectory, takeFileName, (</>))

data SkillInfo = SkillInfo
    { skillName :: Text
    , skillDescription :: Text
    , skillLocation :: Text
    , skillContent :: Text
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

listSkills :: FilePath -> IO [SkillInfo]
listSkills root = do
    cfg <- Config.get root
    home <- getHomeDirectory
    projectDirs <- projectSkillRoots root
    -- Get skills from config (Dhall SkillConfig records)
    let configSkills = skillsFromConfig cfg
    -- Scan directory-based skills
    let globalDirs =
            [ home </> ".config" </> "weapon" </> "skills"
            , home </> ".claude" </> "skills"
            , home </> ".agents" </> "skills"
            ]
    files <- fmap concat (mapM findSkills (globalDirs ++ projectDirs))
    fileInfos <- foldM addSkill Map.empty files
    -- Merge config skills with file skills (config takes precedence)
    let merged = Map.union (Map.fromList [(skillName s, s) | s <- configSkills]) fileInfos
    pure (Map.elems merged)

-- | Convert SkillConfig records from Dhall config to SkillInfo
skillsFromConfig :: CT.Config -> [SkillInfo]
skillsFromConfig cfg = case CT.cfgSkill cfg of
    Nothing -> []
    Just skillMap -> map toSkillInfo (Map.toList skillMap)
  where
    toSkillInfo (name, sc) =
        SkillInfo
            { skillName = CT.skillName sc
            , skillDescription = CT.skillDescription sc
            , skillLocation = "config:" <> name
            , skillContent = CT.skillPrompt sc
            }

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

newtype SkillIndex = SkillIndex
    { siSkills :: [SkillIndexEntry]
    }
    deriving (Show, Eq, Generic)

data SkillIndexEntry = SkillIndexEntry
    { sieName :: Text
    , sieDescription :: Maybe Text
    , sieFiles :: [Text]
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

parseSkillIndex :: ByteString -> Maybe SkillIndex
parseSkillIndex = either (const Nothing) Just . eitherDecodeStrict

projectSkillRoots :: FilePath -> IO [FilePath]
projectSkillRoots root = do
    base <- makeAbsolute root
    let dirs = walkUp base
    pure $ concatMap skillDirs dirs
  where
    skillDirs dir =
        [ dir </> ".weapon" </> "skill"
        , dir </> ".weapon" </> "skills"
        , dir </> ".claude" </> "skills"
        , dir </> ".agents" </> "skills"
        ]

walkUp :: FilePath -> [FilePath]
walkUp start = go start id
  where
    go dir acc =
        let parent = takeDirectory dir
            next = acc . (dir :)
         in if parent == dir then next [] else go parent next

findSkills :: FilePath -> IO [FilePath]
findSkills dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else scan dir
  where
    scan path = do
        entries <- listDirectory path
        parts <- forM entries $ \entry -> do
            let item = path </> entry
            isDir <- doesDirectoryExist item
            if isDir
                then scan item
                else do
                    isFile <- doesFileExist item
                    if isFile && takeFileName item == "SKILL.md"
                        then pure [item]
                        else pure []
        pure (concat parts)

addSkill :: Map.Map Text SkillInfo -> FilePath -> IO (Map.Map Text SkillInfo)
addSkill acc path = do
    absolute <- makeAbsolute path
    content <- TIO.readFile absolute
    case parseSkill absolute content of
        Nothing -> pure acc
        Just skill -> pure (Map.insert (skillName skill) skill acc)

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

parseMeta :: Text -> Maybe (Text, Text)
parseMeta line =
    let (key, rest) = T.breakOn ":" line
     in if T.null rest
            then Nothing
            else Just (T.strip key, T.strip (T.drop 1 rest))

-- URL-based skill fetching (for remote skill indexes)
-- This is kept for backwards compatibility but skills in config
-- are now defined inline via Dhall

pullSkills :: String -> IO [FilePath]
pullSkills url = do
    manager <- newTlsManager
    let base = if "/" `T.isSuffixOf` T.pack url then url else url <> "/"
    let indexUrl = base <> "index.json"
    indexReq <- parseRequest indexUrl
    indexResp <- httpLbs indexReq manager
    if statusCode (responseStatus indexResp) /= 200
        then pure []
        else case parseSkillIndex (BSL.toStrict (responseBody indexResp)) of
            Nothing -> pure []
            Just idx -> do
                cache <- skillCacheDir
                results <- mapM (downloadSkill manager cache base) (siSkills idx)
                pure (concat results)

downloadSkill :: Manager -> FilePath -> String -> SkillIndexEntry -> IO [FilePath]
downloadSkill manager cache base entry = do
    let name = sieName entry
    let root = cache </> T.unpack name
    createDirectoryIfMissing True root
    mapM_ (downloadFile manager base root name) (sieFiles entry)
    let md = root </> "SKILL.md"
    exists <- doesFileExist md
    if exists then pure [root] else pure []

downloadFile :: Manager -> String -> FilePath -> Text -> Text -> IO ()
downloadFile manager base root name file = do
    let url = base <> T.unpack name <> "/" <> T.unpack file
    req <- parseRequest url
    resp <- httpLbs req manager
    if statusCode (responseStatus resp) /= 200
        then pure ()
        else do
            let dest = root </> T.unpack file
            createDirectoryIfMissing True (takeDirectory dest)
            BS.writeFile dest (BSL.toStrict (responseBody resp))

skillCacheDir :: IO FilePath
skillCacheDir = do
    home <- getHomeDirectory
    let dir = home </> ".cache" </> "opencode" </> "skills"
    createDirectoryIfMissing True dir
    pure dir

-- Silence unused warning for pullSkills (kept for potential future use)
_unusedPullSkills :: String -> IO [FilePath]
_unusedPullSkills = pullSkills
