{-# LANGUAGE OverloadedStrings #-}

-- | Tool execution property tests
module Property.ToolProps where

import Control.Monad (forM_)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

import Test.Fixture (propertyWithTempDir)
import Test.Helpers (listLength)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog
import Tool.Defs qualified as Tool
import Tool.Exec (execute, processBashOutput, processGlobOutput, processGrepOutput)
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
-- Pure output processing tests (no subprocesses, fully deterministic)
-- ═══════════════════════════════════════════════════════════════════════════

-- | processBashOutput: success with output returns success
prop_bashOutputSuccess :: Property
prop_bashOutputSuccess = property $ do
    desc <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
    stdout <- forAll $ Gen.text (Range.linear 1 100) Gen.alphaNum
    let result = processBashOutput desc ExitSuccess stdout ""
    toIsError result === False
    toTitle result === desc
    assert $ T.isInfixOf stdout (toOutput result)

-- | processBashOutput: success with empty output gives default message
prop_bashOutputSuccessEmpty :: Property
prop_bashOutputSuccessEmpty = property $ do
    desc <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
    let result = processBashOutput desc ExitSuccess "" ""
    toIsError result === False
    assert $ T.isInfixOf "Command completed successfully" (toOutput result)

-- | processBashOutput: failure returns error
prop_bashOutputFailure :: Property
prop_bashOutputFailure = property $ do
    desc <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
    exitCode <- forAll $ Gen.int (Range.linear 1 255)
    stdout <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    let result = processBashOutput desc (ExitFailure exitCode) stdout ""
    toIsError result === True
    toTitle result === "Command failed"

-- | processBashOutput: stderr is appended with marker
prop_bashOutputStderr :: Property
prop_bashOutputStderr = property $ do
    desc <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
    stdout <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    stderr <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    let result = processBashOutput desc ExitSuccess stdout stderr
    toIsError result === False
    assert $ T.isInfixOf "[stderr]" (toOutput result)
    assert $ T.isInfixOf stderr (toOutput result)

-- | processBashOutput: no stderr marker when stderr is empty
prop_bashOutputNoStderrMarker :: Property
prop_bashOutputNoStderrMarker = property $ do
    desc <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
    stdout <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    let result = processBashOutput desc ExitSuccess stdout ""
    assert $ not $ T.isInfixOf "[stderr]" (toOutput result)

-- | processGlobOutput: success returns matching lines
prop_globOutputSuccess :: Property
prop_globOutputSuccess = property $ do
    pat <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    -- Generate some file paths
    numLines <- forAll $ Gen.int (Range.linear 1 50)
    let lines' = [T.pack ("file" <> show i <> ".txt") | i <- [1 .. numLines]]
    let stdout = T.unlines lines'
    let result = processGlobOutput pat ExitSuccess stdout ""
    toIsError result === False
    toTitle result === ("Glob " <> pat)
    -- All lines should be in output
    forM_ lines' $ \line ->
        assert $ T.isInfixOf line (toOutput result)

-- | processGlobOutput: truncates to 100 lines
prop_globOutputTruncates :: Property
prop_globOutputTruncates = property $ do
    pat <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let lines' = [T.pack ("file" <> show i <> ".txt") | i <- [1 .. 200 :: Int]]
    let stdout = T.unlines lines'
    let result = processGlobOutput pat ExitSuccess stdout ""
    toIsError result === False
    -- Output should have at most 100 lines (T.unlines adds trailing newline, T.lines will give 101 with empty last)
    let outputLines = filter (not . T.null) (T.lines (toOutput result))
    assert $ listLength outputLines <= 100

-- | processGlobOutput: failure returns error with stderr preferred
prop_globOutputFailure :: Property
prop_globOutputFailure = property $ do
    pat <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    exitCode <- forAll $ Gen.int (Range.linear 1 255)
    stderr <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    let result = processGlobOutput pat (ExitFailure exitCode) "" stderr
    toIsError result === True
    toTitle result === "Glob Error"
    assert $ T.isInfixOf stderr (toOutput result)

-- | processGlobOutput: failure with empty stderr uses stdout
prop_globOutputFailureStdout :: Property
prop_globOutputFailureStdout = property $ do
    pat <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    exitCode <- forAll $ Gen.int (Range.linear 1 255)
    stdout <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    let result = processGlobOutput pat (ExitFailure exitCode) stdout ""
    toIsError result === True
    assert $ T.isInfixOf stdout (toOutput result)

-- | processGrepOutput: success returns matching lines
prop_grepOutputSuccess :: Property
prop_grepOutputSuccess = property $ do
    pat <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    numLines <- forAll $ Gen.int (Range.linear 1 50)
    let lines' = [T.pack ("file.txt:" <> show i <> ":match") | i <- [1 .. numLines]]
    let stdout = T.unlines lines'
    let result = processGrepOutput pat ExitSuccess stdout ""
    toIsError result === False
    toTitle result === ("Grep " <> pat)
    forM_ lines' $ \line ->
        assert $ T.isInfixOf line (toOutput result)

-- | processGrepOutput: truncates to 100 lines
prop_grepOutputTruncates :: Property
prop_grepOutputTruncates = property $ do
    pat <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let lines' = [T.pack ("file.txt:" <> show i <> ":match") | i <- [1 .. 200 :: Int]]
    let stdout = T.unlines lines'
    let result = processGrepOutput pat ExitSuccess stdout ""
    toIsError result === False
    let outputLines = filter (not . T.null) (T.lines (toOutput result))
    assert $ listLength outputLines <= 100

-- | processGrepOutput: exit code 1 with no output is "no matches" (success)
prop_grepOutputNoMatches :: Property
prop_grepOutputNoMatches = property $ do
    pat <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let result = processGrepOutput pat (ExitFailure 1) "" ""
    toIsError result === False
    toTitle result === ("Grep " <> pat)
    toOutput result === ""

-- | processGrepOutput: exit code 2+ is a real error
prop_grepOutputRealError :: Property
prop_grepOutputRealError = property $ do
    pat <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    exitCode <- forAll $ Gen.int (Range.linear 2 255)
    stderr <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    let result = processGrepOutput pat (ExitFailure exitCode) "" stderr
    toIsError result === True
    toTitle result === "Grep Error"
    assert $ T.isInfixOf stderr (toOutput result)

-- | processGrepOutput: failure with empty stderr uses stdout
prop_grepOutputFailureStdout :: Property
prop_grepOutputFailureStdout = property $ do
    pat <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    exitCode <- forAll $ Gen.int (Range.linear 2 255)
    stdout <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    let result = processGrepOutput pat (ExitFailure exitCode) stdout ""
    toIsError result === True
    assert $ T.isInfixOf stdout (toOutput result)

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
        [ -- Pure tests (no subprocesses)
          testProperty "read tool" prop_readTool
        , testProperty "write tool" prop_writeTool
        , testProperty "write/read roundtrip" prop_writeReadToolRoundtrip
        , testProperty "edit tool" prop_editTool
        , testProperty "edit missing oldString" prop_editToolMissingOldString
        , testProperty "edit multiple matches error" prop_editToolMultipleMatchesError
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
        , testProperty "read nonexistent file" prop_readNonexistentFile
        , testProperty "edit nonexistent file" prop_editNonexistentFile
        , -- Pure output processing tests (fully deterministic, no subprocesses)
          testProperty "bash output success" prop_bashOutputSuccess
        , testProperty "bash output success empty" prop_bashOutputSuccessEmpty
        , testProperty "bash output failure" prop_bashOutputFailure
        , testProperty "bash output stderr" prop_bashOutputStderr
        , testProperty "bash output no stderr marker" prop_bashOutputNoStderrMarker
        , testProperty "glob output success" prop_globOutputSuccess
        , testProperty "glob output truncates" prop_globOutputTruncates
        , testProperty "glob output failure" prop_globOutputFailure
        , testProperty "glob output failure stdout" prop_globOutputFailureStdout
        , testProperty "grep output success" prop_grepOutputSuccess
        , testProperty "grep output truncates" prop_grepOutputTruncates
        , testProperty "grep output no matches" prop_grepOutputNoMatches
        , testProperty "grep output real error" prop_grepOutputRealError
        , testProperty "grep output failure stdout" prop_grepOutputFailureStdout
        ]
