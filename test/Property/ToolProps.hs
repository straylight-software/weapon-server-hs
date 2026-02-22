{-# LANGUAGE OverloadedStrings #-}

-- | Tool execution property tests
module Property.ToolProps where

import Control.Monad (forM_)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath ((</>))
import Test.Fixture (propertyWithTempDir, withTempDir)
import Test.Tasty
import Test.Tasty.Hedgehog
import Tool.Defs qualified as Tool
import Tool.Exec (execute)
import Tool.Types

-- | Create a test context
testContext :: FilePath -> ToolContext
testContext workdir =
    ToolContext
        { tcSessionID = "test_session"
        , tcMessageID = "test_message"
        , tcWorkdir = workdir
        }

-- | Property: read tool returns file content
prop_readTool :: Property
prop_readTool = propertyWithTempDir $ \tmpDir -> do
    content <- forAll $ Gen.text (Range.linear 1 500) Gen.unicode
    filename <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum

    result <- evalIO $ do
        let path = tmpDir </> T.unpack filename
        TIO.writeFile path content
        let input =
                object
                    [ "filePath" .= path
                    , "offset" .= (1 :: Int)
                    , "limit" .= (1000 :: Int)
                    ]
        execute (testContext tmpDir) "read" input

    assert $ not (toIsError result)
    assert $ not (T.null (toOutput result))

-- | Property: write tool creates file
prop_writeTool :: Property
prop_writeTool = propertyWithTempDir $ \tmpDir -> do
    content <- forAll $ Gen.text (Range.linear 0 500) Gen.unicode
    filename <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum

    result <- evalIO $ do
        let path = tmpDir </> T.unpack filename
        let input =
                object
                    [ "filePath" .= path
                    , "content" .= content
                    ]
        output <- execute (testContext tmpDir) "write" input
        exists <- doesFileExist path
        pure (output, exists)

    let (output, exists) = result
    assert $ not (toIsError output)
    assert exists

prop_writeReadToolRoundtrip :: Property
prop_writeReadToolRoundtrip = propertyWithTempDir $ \tmpDir -> do
    content <- forAll $ Gen.text (Range.linear 1 200) Gen.unicode
    filename <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    result <- evalIO $ do
        let path = tmpDir </> T.unpack filename
        let writeInput =
                object
                    [ "filePath" .= path
                    , "content" .= content
                    ]
        _ <- execute (testContext tmpDir) "write" writeInput
        let readInput =
                object
                    [ "filePath" .= path
                    , "offset" .= (1 :: Int)
                    , "limit" .= (1000 :: Int)
                    ]
        execute (testContext tmpDir) "read" readInput
    assert $ T.isInfixOf content (toOutput result)

-- | Property: edit tool modifies file
prop_editTool :: Property
prop_editTool = propertyWithTempDir $ \tmpDir -> do
    oldText <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
    newText <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
    prefix <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
    suffix <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
    filename <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum

    -- Ensure oldText doesn't appear in prefix or suffix to avoid ambiguity
    let uniqueOldText = "OLDTEXT_" <> oldText
    let originalContent = prefix <> uniqueOldText <> suffix

    result <- evalIO $ do
        let path = tmpDir </> T.unpack filename
        TIO.writeFile path originalContent
        let input =
                object
                    [ "filePath" .= path
                    , "oldString" .= uniqueOldText
                    , "newString" .= newText
                    , "replaceAll" .= False
                    ]
        output <- execute (testContext tmpDir) "edit" input
        editedContent <- TIO.readFile path
        pure (output, editedContent)

    let (output, editedContent) = result
    assert $ not (toIsError output)
    assert $ T.isInfixOf newText editedContent
    assert $ not (T.isInfixOf "OLDTEXT_" editedContent)

prop_editToolMissingOldString :: Property
prop_editToolMissingOldString = propertyWithTempDir $ \tmpDir -> do
    content <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    filename <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    result <- evalIO $ do
        let path = tmpDir </> T.unpack filename
        TIO.writeFile path content
        let input =
                object
                    [ "filePath" .= path
                    , "oldString" .= ("missing" :: Text)
                    , "newString" .= ("new" :: Text)
                    , "replaceAll" .= False
                    ]
        execute (testContext tmpDir) "edit" input
    assert $ toIsError result

prop_editToolMultipleMatchesError :: Property
prop_editToolMultipleMatchesError = propertyWithTempDir $ \tmpDir -> do
    let content = "dup dup"
    filename <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    result <- evalIO $ do
        let path = tmpDir </> T.unpack filename
        TIO.writeFile path content
        let input =
                object
                    [ "filePath" .= path
                    , "oldString" .= ("dup" :: Text)
                    , "newString" .= ("new" :: Text)
                    , "replaceAll" .= False
                    ]
        execute (testContext tmpDir) "edit" input
    assert $ toIsError result

-- | Property: bash tool executes commands
prop_bashTool :: Property
prop_bashTool = propertyWithTempDir $ \tmpDir -> do
    cmd <- forAll $ Gen.element ["echo hello" :: Text, "pwd", "whoami"]

    result <- evalIO $ do
        let input =
                object
                    [ "command" .= (cmd :: Text)
                    , "description" .= ("test command" :: Text)
                    , "timeout" .= (5000 :: Int)
                    ]
        execute (testContext tmpDir) "bash" input

    assert $ not (toIsError result)
    assert $ not (T.null (toOutput result))

prop_bashToolUsesWorkdir :: Property
prop_bashToolUsesWorkdir = propertyWithTempDir $ \tmpDir -> do
    (result, dir) <- evalIO $ do
        -- Canonicalize the path to resolve symlinks (e.g., /tmp -> /run/user/...)
        canonicalDir <- canonicalizePath tmpDir
        let input =
                object
                    [ "command" .= ("pwd -P" :: Text) -- -P to get physical path
                    , "description" .= ("test workdir" :: Text)
                    , "timeout" .= (5000 :: Int)
                    , "workdir" .= T.pack canonicalDir
                    ]
        output <- execute (testContext canonicalDir) "bash" input
        -- Also canonicalize what pwd returned for comparison
        let pwdOutput = T.strip (toOutput output)
        canonicalOutput <-
            if T.null pwdOutput
                then pure pwdOutput
                else T.pack <$> canonicalizePath (T.unpack pwdOutput)
        -- Also canonicalize the dir again to be sure
        canonicalDir' <- canonicalizePath canonicalDir
        pure (output{toOutput = canonicalOutput}, canonicalDir')
    assert $ not (toIsError result)
    -- Use equality check on stripped paths for more reliable comparison
    assert $ T.strip (toOutput result) == T.pack dir

prop_bashToolTimeout :: Property
prop_bashToolTimeout = propertyWithTempDir $ \tmpDir -> do
    result <- evalIO $ do
        let input =
                object
                    [ "command" .= ("sleep 2" :: Text)
                    , "description" .= ("timeout test" :: Text)
                    , "timeout" .= (50 :: Int)
                    ]
        execute (testContext tmpDir) "bash" input
    assert $ toIsError result

-- This test creates 150 files, so it needs a fresh directory each time
prop_globToolLimitsResults :: Property
prop_globToolLimitsResults = property $ do
    lineCount <- evalIO $ withTempDir $ \tmpDir -> do
        let writeFileLoop idx =
                if idx > (150 :: Int)
                    then pure ()
                    else do
                        let path = tmpDir </> ("file" <> show idx <> ".txt")
                        TIO.writeFile path "content"
                        writeFileLoop (idx + 1)
        writeFileLoop 1
        let input =
                object
                    [ "pattern" .= ("*.txt" :: Text)
                    , "path" .= T.pack tmpDir
                    ]
        result <- execute (testContext tmpDir) "glob" input
        let ls = T.lines (toOutput result)
        pure (listLength ls)
    assert $ lineCount <= 100

prop_grepToolNoMatchesNotError :: Property
prop_grepToolNoMatchesNotError = propertyWithTempDir $ \tmpDir -> do
    filename <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    result <- evalIO $ do
        let path = tmpDir </> T.unpack filename
        TIO.writeFile path "hello world"
        let input =
                object
                    [ "pattern" .= ("missing_pattern" :: Text)
                    , "path" .= T.pack tmpDir
                    ]
        execute (testContext tmpDir) "grep" input
    assert $ not (toIsError result)

prop_toolOutputJsonRoundtrip :: Property
prop_toolOutputJsonRoundtrip = property $ do
    title <- forAll genText
    output <- forAll genText
    isErr <- forAll Gen.bool
    meta <- forAll genMaybeValue
    let out = ToolOutput title output isErr meta
    case decode (encode out) of
        Nothing -> failure
        Just out' -> out' === out

-- | Property: tool definitions list is non-empty
prop_toolDefinitionsNotEmpty :: Property
prop_toolDefinitionsNotEmpty = property $ do
    assert $ not (null Tool.toolDefinitions)

-- | Property: all tools have valid names
prop_allToolsHaveNames :: Property
prop_allToolsHaveNames = property $ do
    let tools = Tool.allTools
    assert $ not (null tools)
    -- Each tool should have a non-empty name
    forM_ tools $ \tool -> do
        assert $ not (T.null (tdName tool))

-- | Property: tool definitions are valid JSON
prop_toolDefinitionsValidJson :: Property
prop_toolDefinitionsValidJson = property $ do
    let defs = Tool.toolDefinitions
    forM_ defs $ \def -> do
        -- Encode and decode should work
        let encoded = encode def
        case decode encoded of
            Nothing -> failure
            Just (_ :: Value) -> success

-- | Property: tool names are unique
prop_toolNamesUnique :: Property
prop_toolNamesUnique = property $ do
    let tools = Tool.allTools
    let names = map tdName tools
    let uniqueCount = Set.size (Set.fromList names)
    listLength names === uniqueCount

listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0

-- | Property: tool list returns consistent results
prop_toolListConsistent :: Property
prop_toolListConsistent = property $ do
    let list1 = Tool.allTools
    let list2 = Tool.allTools
    list1 === list2

-- | Property: tool definitions contain read tool
prop_toolListContainsRead :: Property
prop_toolListContainsRead = property $ do
    let tools = Tool.allTools
    assert $ any (\t -> tdName t == "read") tools

-- | Property: tool definitions contain write tool
prop_toolListContainsWrite :: Property
prop_toolListContainsWrite = property $ do
    let tools = Tool.allTools
    assert $ any (\t -> tdName t == "write") tools

-- | Property: tool definitions contain bash tool
prop_toolListContainsBash :: Property
prop_toolListContainsBash = property $ do
    let tools = Tool.allTools
    assert $ any (\t -> tdName t == "bash") tools

-- | Property: tool schemas have type field
prop_toolSchemasHaveType :: Property
prop_toolSchemasHaveType = property $ do
    let tools = Tool.allTools
    forM_ tools $ \tool -> do
        let schema = tdInputSchema tool
        case decode (encode schema) of
            Nothing -> failure
            Just (Object obj) -> do
                case KM.lookup "type" obj of
                    Just (String "object") -> success
                    Just (String _otherType) -> failure
                    Just _otherValue -> failure
                    Nothing -> failure
            Just _otherValue -> failure

-- | Property: tool required params are subset of all params
prop_toolRequiredParamsValid :: Property
prop_toolRequiredParamsValid = property $ do
    let tools = Tool.allTools
    forM_ tools $ \tool -> do
        let schema = tdInputSchema tool
        case decode (encode schema) of
            Nothing -> success -- Skip if no schema
            Just (Object obj) -> do
                case (KM.lookup "required" obj, KM.lookup "properties" obj) of
                    (Just (Array req), Just (Object props)) -> do
                        let reqList = [r | String r <- toList req]
                        let propKeys = map Key.toText (KM.keys props)
                        assert $ all (`elem` propKeys) reqList
                    _otherFields -> success
            Just _otherValue -> success

-- ═══════════════════════════════════════════════════════════════════════════
-- Edge Case Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: read tool returns error for nonexistent file
prop_readNonexistentFile :: Property
prop_readNonexistentFile = propertyWithTempDir $ \tmpDir -> do
    filename <- forAll $ Gen.text (Range.linear 10 30) Gen.alphaNum
    result <- evalIO $ do
        let path = tmpDir </> T.unpack filename </> "nonexistent.txt"
        let input =
                object
                    [ "filePath" .= path
                    , "offset" .= (1 :: Int)
                    , "limit" .= (100 :: Int)
                    ]
        execute (testContext tmpDir) "read" input
    assert $ toIsError result

-- | Property: edit tool returns error for nonexistent file
prop_editNonexistentFile :: Property
prop_editNonexistentFile = propertyWithTempDir $ \tmpDir -> do
    filename <- forAll $ Gen.text (Range.linear 10 30) Gen.alphaNum
    result <- evalIO $ do
        let path = tmpDir </> T.unpack filename </> "nonexistent.txt"
        let input =
                object
                    [ "filePath" .= path
                    , "oldString" .= ("old" :: Text)
                    , "newString" .= ("new" :: Text)
                    ]
        execute (testContext tmpDir) "edit" input
    assert $ toIsError result

-- | Property: bash tool with invalid command returns error
prop_bashInvalidCommand :: Property
prop_bashInvalidCommand = propertyWithTempDir $ \tmpDir -> do
    result <- evalIO $ do
        let input =
                object
                    [ "command" .= ("nonexistent_command_xyz123" :: Text)
                    , "description" .= ("test invalid" :: Text)
                    , "timeout" .= (2000 :: Int)
                    ]
        execute (testContext tmpDir) "bash" input
    -- The command should complete but with non-zero exit or error output
    -- Either isError is true, or output contains error message
    assert $ toIsError result || T.isInfixOf "not found" (toOutput result) || T.isInfixOf "command not found" (toOutput result)

-- | Property: glob tool returns empty for nonexistent pattern
prop_globEmptyForNonexistent :: Property
prop_globEmptyForNonexistent = propertyWithTempDir $ \tmpDir -> do
    result <- evalIO $ do
        let input =
                object
                    [ "pattern" .= ("**/*.nonexistent_extension_xyz" :: Text)
                    , "path" .= T.pack tmpDir
                    ]
        execute (testContext tmpDir) "glob" input
    assert $ not (toIsError result)
    -- Output should indicate no matches
    assert $ T.null (T.strip (toOutput result)) || T.isInfixOf "No files" (toOutput result) || toOutput result == ""

-- Generators
genText :: Gen Text
genText = Gen.text (Range.linear 0 100) Gen.alphaNum

genMaybeValue :: Gen (Maybe Value)
genMaybeValue =
    Gen.choice
        [ pure Nothing
        , Just . object <$> Gen.list (Range.linear 0 3) genPair
        ]
  where
    genPair = do
        key <- genText
        val <- genText
        pure (Key.fromText key .= val)

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Tool Property Tests"
        [ testProperty "read tool" prop_readTool
        , testProperty "write tool" prop_writeTool
        , testProperty "write/read roundtrip" prop_writeReadToolRoundtrip
        , testProperty "edit tool" prop_editTool
        , testProperty "edit missing oldString" prop_editToolMissingOldString
        , testProperty "edit multiple matches error" prop_editToolMultipleMatchesError
        , testProperty "bash tool" prop_bashTool
        , testProperty "bash tool uses workdir" prop_bashToolUsesWorkdir
        , testProperty "bash tool timeout" (withTests 1 prop_bashToolTimeout)
        , testProperty "glob tool limits results" prop_globToolLimitsResults
        , testProperty "grep no matches not error" prop_grepToolNoMatchesNotError
        , testProperty "tool output JSON roundtrip" prop_toolOutputJsonRoundtrip
        , testProperty "tool definitions not empty" prop_toolDefinitionsNotEmpty
        , testProperty "all tools have names" prop_allToolsHaveNames
        , testProperty "tool definitions valid JSON" prop_toolDefinitionsValidJson
        , testProperty "tool names unique" prop_toolNamesUnique
        , testProperty "tool list consistent" prop_toolListConsistent
        , testProperty "tool list contains read" prop_toolListContainsRead
        , testProperty "tool list contains write" prop_toolListContainsWrite
        , testProperty "tool list contains bash" prop_toolListContainsBash
        , testProperty "tool schemas have type" prop_toolSchemasHaveType
        , testProperty "tool required params valid" prop_toolRequiredParamsValid
        , -- Edge cases
          testProperty "read nonexistent file" prop_readNonexistentFile
        , testProperty "edit nonexistent file" prop_editNonexistentFile
        , testProperty "bash invalid command" prop_bashInvalidCommand
        , testProperty "glob empty for nonexistent" prop_globEmptyForNonexistent
        ]
