{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.SkillProps
Description : Property tests for Skill.Skill module

Property-based tests for skill discovery, parsing, and path utilities.
-}
module Property.SkillProps where

import Config.Dhall qualified as Dhall
import Config.Types qualified as CT
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Skill.Skill (
    SkillIndex (..),
    SkillInfo (..),
    globalSkillDirs,
    listSkills,
    parseFrontmatter,
    parseMeta,
    parseSkill,
    parseSkillIndex,
    skillConfigToInfo,
    skillDirsForPath,
    skillsFromConfig,
    walkUp,
 )
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO.Temp (createTempDirectory)
import Test.Helpers (listLength)
import Test.Tasty
import Test.Tasty.Hedgehog

-- ════════════════════════════════════════════════════════════════════════════
--                                                        Parsing Properties
-- ════════════════════════════════════════════════════════════════════════════

-- | Valid frontmatter should always parse successfully
prop_parseSkillFrontmatter :: Property
prop_parseSkillFrontmatter = property $ do
    name <- forAll genNonEmptyText
    desc <- forAll genNonEmptyText
    body <- forAll $ Gen.list (Range.linear 0 5) genText
    let content =
            T.unlines $
                ["---", "name: " <> name, "description: " <> desc, "---"] <> body
    case parseSkill "/tmp/SKILL.md" content of
        Nothing -> failure
        Just skill -> do
            skillName skill === name
            skillDescription skill === desc
            skillContent skill === T.unlines body

-- | Missing frontmatter should fail parsing
prop_parseSkillMissingFrontmatter :: Property
prop_parseSkillMissingFrontmatter = property $ do
    body <- forAll genText
    let content = "no-frontmatter\n" <> body
    parseSkill "/tmp/SKILL.md" content === Nothing

-- | parseMeta should correctly split key:value pairs
prop_parseMetaValid :: Property
prop_parseMetaValid = property $ do
    key <- forAll genIdentifier
    value <- forAll genNonEmptyText
    let line = key <> ": " <> value
    case parseMeta line of
        Nothing -> failure
        Just (k, v) -> do
            k === key
            v === value

-- | parseMeta should handle whitespace around key and value
prop_parseMetaWhitespace :: Property
prop_parseMetaWhitespace = property $ do
    key <- forAll genIdentifier
    value <- forAll genNonEmptyText
    leadingSpaces <- forAll $ Gen.int (Range.linear 0 3)
    trailingSpaces <- forAll $ Gen.int (Range.linear 0 3)
    let spaces n = T.replicate n " "
    let line = spaces leadingSpaces <> key <> spaces trailingSpaces <> ":" <> spaces leadingSpaces <> value <> spaces trailingSpaces
    case parseMeta line of
        Nothing -> failure
        Just (k, v) -> do
            -- Key should be trimmed
            T.strip k === key
            -- Value should be trimmed
            T.strip v === value

-- | parseMeta should return Nothing for lines without colon
prop_parseMetaNoColon :: Property
prop_parseMetaNoColon = property $ do
    text <- forAll $ Gen.filter (not . T.isInfixOf ":") genText
    parseMeta text === Nothing

-- | parseFrontmatter should require opening delimiter
prop_parseFrontmatterNoOpening :: Property
prop_parseFrontmatterNoOpening = property $ do
    lines' <- forAll $ Gen.list (Range.linear 1 5) genText
    -- Ensure first line is not ---
    let firstLine = case lines' of
            [] -> "not-delimiter"
            (x : _) | T.strip x == "---" -> "not-delimiter"
            (x : _) -> x
    let content = firstLine : drop 1 lines'
    parseFrontmatter content === Nothing

-- | parseFrontmatter should require closing delimiter
prop_parseFrontmatterNoClosing :: Property
prop_parseFrontmatterNoClosing = property $ do
    metaLines <- forAll $ Gen.list (Range.linear 1 3) $ Gen.filter (\x -> T.strip x /= "---") genText
    let content = ["---"] <> metaLines
    parseFrontmatter content === Nothing

-- | parseFrontmatter roundtrip: valid frontmatter should parse
prop_parseFrontmatterRoundtrip :: Property
prop_parseFrontmatterRoundtrip = property $ do
    kvPairs <- forAll $ Gen.list (Range.linear 1 5) $ do
        k <- genIdentifier
        v <- genNonEmptyText
        pure (k, v)
    bodyLines <- forAll $ Gen.list (Range.linear 0 3) $ Gen.filter (\x -> T.strip x /= "---") genText

    let metaLines = map (\(k, v) -> k <> ": " <> v) kvPairs
    let content = ["---"] <> metaLines <> ["---"] <> bodyLines

    case parseFrontmatter content of
        Nothing -> failure
        Just (meta, body) -> do
            -- All keys should be present
            mapM_ (\(k, _) -> assert (Map.member k meta)) kvPairs
            -- Body should match
            body === bodyLines

-- ════════════════════════════════════════════════════════════════════════════
--                                                       Path Properties
-- ════════════════════════════════════════════════════════════════════════════

-- | walkUp should always include the starting directory
prop_walkUpIncludesStart :: Property
prop_walkUpIncludesStart = property $ do
    path <- forAll genAbsolutePath
    let result = walkUp path
    assert (path `elem` result)

-- | walkUp should end at root
prop_walkUpEndsAtRoot :: Property
prop_walkUpEndsAtRoot = property $ do
    path <- forAll genAbsolutePath
    let result = walkUp path
    -- Last element should be root (its parent equals itself)
    case List.unsnoc result of
        Nothing -> failure
        Just (_, lastDir) -> takeDirectory lastDir === lastDir

-- | walkUp results should all be absolute paths
prop_walkUpAbsolutePaths :: Property
prop_walkUpAbsolutePaths = property $ do
    path <- forAll genAbsolutePath
    let result = walkUp path
    mapM_ (assert . isAbsolute) result

-- | walkUp should produce directories in descending order (child before parent)
prop_walkUpDescending :: Property
prop_walkUpDescending = property $ do
    path <- forAll genAbsolutePath
    let result = walkUp path
    -- Each element should be the parent of the previous (or equal for root)
    let checkPairs [] = True
        checkPairs [_] = True
        checkPairs (x : y : rest) = takeDirectory x == y && checkPairs (y : rest)
    assert (checkPairs result)

-- | skillDirsForPath should return 4 directories
prop_skillDirsCount :: Property
prop_skillDirsCount = property $ do
    path <- forAll genAbsolutePath
    listLength (skillDirsForPath path) === 4

-- | globalSkillDirs should return 3 directories
prop_globalSkillDirsCount :: Property
prop_globalSkillDirsCount = property $ do
    home <- forAll genAbsolutePath
    listLength (globalSkillDirs home) === 3

-- | All skill dirs should be under the given path
prop_skillDirsUnderPath :: Property
prop_skillDirsUnderPath = property $ do
    path <- forAll genAbsolutePath
    let dirs = skillDirsForPath path
    -- All dirs should start with path
    let pathText = T.pack path
    mapM_ (\d -> assert (pathText `T.isPrefixOf` T.pack d)) dirs

-- ════════════════════════════════════════════════════════════════════════════
--                                                      Config Properties
-- ════════════════════════════════════════════════════════════════════════════

-- | skillsFromConfig should return empty list for config with no skills
prop_skillsFromConfigEmpty :: Property
prop_skillsFromConfigEmpty = property $ do
    let cfg = CT.defaultConfig{CT.cfgSkill = Nothing}
    skillsFromConfig cfg === []

-- | skillsFromConfig should convert all skills from config
prop_skillsFromConfigCount :: Property
prop_skillsFromConfigCount = property $ do
    n <- forAll $ Gen.int (Range.linear 1 5)
    skills <- forAll $ Gen.list (Range.singleton n) genSkillConfig
    let skillMap = Map.fromList skills
    let cfg = CT.defaultConfig{CT.cfgSkill = Just skillMap}
    listLength (skillsFromConfig cfg) === n

-- | skillConfigToInfo should preserve name and description
prop_skillConfigToInfoPreserves :: Property
prop_skillConfigToInfoPreserves = property $ do
    (name, sc) <- forAll genSkillConfig
    let info = skillConfigToInfo (name, sc)
    skillName info === CT.skillName sc
    skillDescription info === CT.skillDescription sc
    skillContent info === CT.skillPrompt sc

-- | skillConfigToInfo location should be prefixed with "config:"
prop_skillConfigToInfoLocation :: Property
prop_skillConfigToInfoLocation = property $ do
    (name, sc) <- forAll genSkillConfig
    let info = skillConfigToInfo (name, sc)
    assert ("config:" `T.isPrefixOf` skillLocation info)

-- ════════════════════════════════════════════════════════════════════════════
--                                                      Index Properties
-- ════════════════════════════════════════════════════════════════════════════

-- | parseSkillIndex should parse valid JSON
prop_parseSkillIndex :: Property
prop_parseSkillIndex = property $ do
    name <- forAll genNonEmptyText
    file <- forAll genNonEmptyText
    let payload =
            object
                [ "skills"
                    .= [ object
                            [ "name" .= name
                            , "files" .= [file]
                            ]
                       ]
                ]
    case parseSkillIndex (BSL.toStrict (encode payload)) of
        Nothing -> failure
        Just idx -> listLength (siSkills idx) === 1

-- | parseSkillIndex should handle multiple entries
prop_parseSkillIndexMultiple :: Property
prop_parseSkillIndexMultiple = property $ do
    name1 <- forAll genNonEmptyText
    name2 <- forAll genNonEmptyText
    file1 <- forAll genNonEmptyText
    file2 <- forAll genNonEmptyText
    let payload =
            object
                [ "skills"
                    .= [ object ["name" .= name1, "files" .= [file1]]
                       , object ["name" .= name2, "files" .= [file2]]
                       ]
                ]
    case parseSkillIndex (BSL.toStrict (encode payload)) of
        Nothing -> failure
        Just idx -> listLength (siSkills idx) === 2

-- | parseSkillIndex should return Nothing for invalid JSON
prop_parseSkillIndexInvalid :: Property
prop_parseSkillIndexInvalid = property $ do
    let payload = "not-json"
    parseSkillIndex payload === Nothing

-- | parseSkillIndex should handle empty skills list
prop_parseSkillIndexEmpty :: Property
prop_parseSkillIndexEmpty = property $ do
    let payload = object ["skills" .= ([] :: [()])]
    case parseSkillIndex (BSL.toStrict (encode payload)) of
        Nothing -> failure
        Just idx -> siSkills idx === []

-- | parseSkillIndex should handle missing skills field (defaults to empty)
prop_parseSkillIndexMissingField :: Property
prop_parseSkillIndexMissingField = property $ do
    let payload = object []
    case parseSkillIndex (BSL.toStrict (encode payload)) of
        Nothing -> failure
        Just idx -> siSkills idx === []

-- ════════════════════════════════════════════════════════════════════════════
--                                                   Integration Properties
-- ════════════════════════════════════════════════════════════════════════════

{- | Test skill discovery from project .weapon/skills directory
(This is the standard discovery path, not config-based)
-}
prop_skillDiscoveryFromProjectDir :: Dhall.DhallCache -> Property
prop_skillDiscoveryFromProjectDir cache = property $ do
    name <- forAll genNonEmptyText
    desc <- forAll genNonEmptyText
    result <- evalIO $ do
        tmp <- createTempDirectory "/tmp" "skill-project"
        -- Use the standard project skill directory
        let skillsDir = tmp </> ".weapon" </> "skills" </> T.unpack name
        createDirectoryIfMissing True skillsDir
        let path = skillsDir </> "SKILL.md"
        let content =
                T.unlines
                    [ "---"
                    , "name: " <> name
                    , "description: " <> desc
                    , "---"
                    , "Body"
                    ]
        TIO.writeFile path content
        skills <- listSkills cache tmp
        let found = any (\skill -> skillName skill == name) skills
        removeDirectoryRecursive tmp
        pure found
    assert result

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Generators
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate arbitrary text
genText :: Gen Text
genText = Gen.text (Range.linear 0 200) Gen.alphaNum

-- | Generate non-empty text
genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 50) Gen.alphaNum

-- | Generate valid identifier (for keys)
genIdentifier :: Gen Text
genIdentifier = Gen.text (Range.linear 1 20) Gen.alpha

-- | Generate absolute path
genAbsolutePath :: Gen FilePath
genAbsolutePath = do
    components <- Gen.list (Range.linear 1 5) (Gen.string (Range.linear 1 10) Gen.alphaNum)
    pure ("/" <> joinComponents components)
  where
    -- Safe joining for finite test lists (avoids partial foldr1)
    joinComponents [] = ""
    joinComponents [x] = x
    joinComponents (x : xs) = x <> "/" <> joinComponents xs

-- | Generate a SkillConfig pair (name, config)
genSkillConfig :: Gen (Text, CT.SkillConfig)
genSkillConfig = do
    name <- genNonEmptyText
    desc <- genNonEmptyText
    prompt <- genNonEmptyText
    let sc =
            CT.SkillConfig
                { CT.skillName = name
                , CT.skillDescription = desc
                , CT.skillPrompt = prompt
                , CT.skillTools = Nothing
                , CT.skillAgent = Nothing
                , CT.skillModel = Nothing
                }
    pure (name, sc)

-- ════════════════════════════════════════════════════════════════════════════
--                                                              Test Tree
-- ════════════════════════════════════════════════════════════════════════════

tests :: Dhall.DhallCache -> TestTree
tests cache =
    testGroup
        "Skill Property Tests"
        [ testGroup
            "Parsing"
            [ testProperty "parse skill frontmatter" prop_parseSkillFrontmatter
            , testProperty "missing frontmatter fails" prop_parseSkillMissingFrontmatter
            , testProperty "parseMeta valid" prop_parseMetaValid
            , testProperty "parseMeta handles whitespace" prop_parseMetaWhitespace
            , testProperty "parseMeta no colon fails" prop_parseMetaNoColon
            , testProperty "parseFrontmatter no opening fails" prop_parseFrontmatterNoOpening
            , testProperty "parseFrontmatter no closing fails" prop_parseFrontmatterNoClosing
            , testProperty "parseFrontmatter roundtrip" prop_parseFrontmatterRoundtrip
            ]
        , testGroup
            "Path Utilities"
            [ testProperty "walkUp includes start" prop_walkUpIncludesStart
            , testProperty "walkUp ends at root" prop_walkUpEndsAtRoot
            , testProperty "walkUp produces absolute paths" prop_walkUpAbsolutePaths
            , testProperty "walkUp descending order" prop_walkUpDescending
            , testProperty "skillDirsForPath count" prop_skillDirsCount
            , testProperty "globalSkillDirs count" prop_globalSkillDirsCount
            , testProperty "skill dirs under path" prop_skillDirsUnderPath
            ]
        , testGroup
            "Config Conversion"
            [ testProperty "empty config gives empty skills" prop_skillsFromConfigEmpty
            , testProperty "skillsFromConfig count" prop_skillsFromConfigCount
            , testProperty "skillConfigToInfo preserves fields" prop_skillConfigToInfoPreserves
            , testProperty "skillConfigToInfo location prefix" prop_skillConfigToInfoLocation
            ]
        , testGroup
            "Skill Index"
            [ testProperty "parse skill index" prop_parseSkillIndex
            , testProperty "parse skill index multiple" prop_parseSkillIndexMultiple
            , testProperty "parse skill index invalid" prop_parseSkillIndexInvalid
            , testProperty "parse skill index empty" prop_parseSkillIndexEmpty
            , testProperty "parse skill index missing field" prop_parseSkillIndexMissingField
            ]
        , testGroup
            "Integration"
            [ testProperty "discover skills from project dir" (prop_skillDiscoveryFromProjectDir cache)
            ]
        ]
