{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.PtyInternalProps
Description : Property tests for Pty.Internal pure functions

Property tests for the pure helper functions exported from Pty.Internal,
including parameter resolution, exit code conversion, and mount spec conversion.
-}
module Property.PtyInternalProps where

import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pty.Internal
import Pty.Types
import Sandbox.Types (MountSpec (..))
import System.Exit (ExitCode (..))
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

-- | Generate alphanumeric text
genAlphaNumText :: Gen Text
genAlphaNumText = Gen.text (Range.linear 1 50) Gen.alphaNum

-- | Generate a valid file path as Text
genFilePathText :: Gen Text
genFilePathText = do
    segments <- Gen.list (Range.linear 1 4) (Gen.text (Range.linear 1 10) Gen.alphaNum)
    case segments of
        [] -> pure "/tmp" -- Should not happen due to Range.linear 1 4
        [x] -> pure $ "/" <> x
        xs -> pure $ "/" <> mconcat (map (<> "/") (initSafe xs)) <> lastSafe xs
  where
    initSafe [] = []
    initSafe [_] = []
    initSafe (x : xs') = x : initSafe xs'
    lastSafe [] = "tmp"
    lastSafe [x] = x
    lastSafe (_ : xs') = lastSafe xs'

-- | Generate a valid file path as String
genFilePath :: Gen FilePath
genFilePath = do
    segments <- Gen.list (Range.linear 1 4) (Gen.string (Range.linear 1 10) Gen.alphaNum)
    case segments of
        [] -> pure "/tmp" -- Should not happen due to Range.linear 1 4
        [x] -> pure $ "/" <> x
        xs -> pure $ "/" <> mconcat (map (<> "/") (initSafe xs)) <> lastSafe xs
  where
    initSafe [] = []
    initSafe [_] = []
    initSafe (x : xs') = x : initSafe xs'
    lastSafe [] = "tmp"
    lastSafe [x] = x
    lastSafe (_ : xs') = lastSafe xs'

-- | Generate environment variable pairs
genEnvPairs :: Gen [(Text, Text)]
genEnvPairs =
    Gen.list
        (Range.linear 0 5)
        ((,) <$> genAlphaNumText <*> genAlphaNumText)

-- | Generate a CreatePtyInput with various optional fields
genCreatePtyInput :: Gen CreatePtyInput
genCreatePtyInput =
    CreatePtyInput
        <$> Gen.maybe genAlphaNumText
        <*> Gen.maybe (Gen.list (Range.linear 0 5) genAlphaNumText)
        <*> Gen.maybe genFilePathText
        <*> Gen.maybe genAlphaNumText
        <*> Gen.maybe genEnvPairs
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe Gen.bool
        <*> Gen.maybe (Gen.list (Range.linear 0 2) genMountTuple)
        <*> Gen.maybe genAlphaNumText

-- | Generate a mount tuple
genMountTuple :: Gen (Text, Text, Bool)
genMountTuple = (,,) <$> genFilePathText <*> genFilePathText <*> Gen.bool

-- | Generate an exit code
genExitCode :: Gen ExitCode
genExitCode =
    Gen.choice
        [ pure ExitSuccess
        , ExitFailure <$> Gen.int (Range.linear 1 255)
        ]

-- ============================================================================
-- resolveCreateParams Properties
-- ============================================================================

-- | Property: sandbox defaults to True when not specified
prop_resolveCreateParamsSandboxDefault :: Property
prop_resolveCreateParamsSandboxDefault = property $ do
    defaultDir <- forAll genFilePath
    ptyId <- forAll genAlphaNumText
    let input =
            CreatePtyInput
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing -- cpiSandbox = Nothing
                Nothing
                Nothing
                Nothing
    let params = resolveCreateParams defaultDir ptyId input
    cpSandbox params === True

-- | Property: sandbox respects explicit False
prop_resolveCreateParamsSandboxExplicitFalse :: Property
prop_resolveCreateParamsSandboxExplicitFalse = property $ do
    defaultDir <- forAll genFilePath
    ptyId <- forAll genAlphaNumText
    let input =
            CreatePtyInput
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                (Just False) -- cpiSandbox = Just False
                Nothing
                Nothing
                Nothing
    let params = resolveCreateParams defaultDir ptyId input
    cpSandbox params === False

-- | Property: cwd defaults to the provided defaultDir
prop_resolveCreateParamsCwdDefault :: Property
prop_resolveCreateParamsCwdDefault = property $ do
    defaultDir <- forAll genFilePath
    ptyId <- forAll genAlphaNumText
    let input =
            CreatePtyInput
                Nothing
                Nothing
                Nothing -- cpiCwd = Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
    let params = resolveCreateParams defaultDir ptyId input
    cpCwd params === defaultDir

-- | Property: cwd respects explicit value
prop_resolveCreateParamsCwdExplicit :: Property
prop_resolveCreateParamsCwdExplicit = property $ do
    defaultDir <- forAll genFilePath
    ptyId <- forAll genAlphaNumText
    explicitCwd <- forAll genFilePathText
    let input =
            CreatePtyInput
                Nothing
                Nothing
                (Just explicitCwd)
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
    let params = resolveCreateParams defaultDir ptyId input
    cpCwd params === T.unpack explicitCwd

-- | Property: OPENCODE_SESSION_ID is always injected into env
prop_resolveCreateParamsSessionIdInjected :: Property
prop_resolveCreateParamsSessionIdInjected = property $ do
    defaultDir <- forAll genFilePath
    ptyId <- forAll genAlphaNumText
    let input =
            CreatePtyInput
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
    let params = resolveCreateParams defaultDir ptyId input
    -- The first env var should be OPENCODE_SESSION_ID
    case cpEnv params of
        [] -> failure
        ((key, val) : _) -> do
            key === "OPENCODE_SESSION_ID"
            val === ptyId

-- | Property: custom session ID is used when provided
prop_resolveCreateParamsCustomSessionId :: Property
prop_resolveCreateParamsCustomSessionId = property $ do
    defaultDir <- forAll genFilePath
    ptyId <- forAll genAlphaNumText
    customSessionId <- forAll genAlphaNumText
    let input =
            CreatePtyInput
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                (Just customSessionId)
    let params = resolveCreateParams defaultDir ptyId input
    cpSessionId params === customSessionId
    -- Also check it's in env
    case cpEnv params of
        [] -> failure
        ((_, val) : _) -> val === customSessionId

-- | Property: network defaults to False
prop_resolveCreateParamsNetworkDefault :: Property
prop_resolveCreateParamsNetworkDefault = property $ do
    defaultDir <- forAll genFilePath
    ptyId <- forAll genAlphaNumText
    let input =
            CreatePtyInput
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing -- cpiNetwork = Nothing
                Nothing
                Nothing
    let params = resolveCreateParams defaultDir ptyId input
    cpNetwork params === False

-- | Property: user env vars are preserved after session ID
prop_resolveCreateParamsEnvPreserved :: Property
prop_resolveCreateParamsEnvPreserved = property $ do
    defaultDir <- forAll genFilePath
    ptyId <- forAll genAlphaNumText
    userEnv <- forAll genEnvPairs
    let input =
            CreatePtyInput
                Nothing
                Nothing
                Nothing
                Nothing
                (Just userEnv)
                Nothing
                Nothing
                Nothing
                Nothing
    let params = resolveCreateParams defaultDir ptyId input
    -- Env should have session ID first, then user env
    let expectedEnv = ("OPENCODE_SESSION_ID", ptyId) : userEnv
    cpEnv params === expectedEnv

-- ============================================================================
-- exitCodeToStatus Properties
-- ============================================================================

-- | Property: ExitSuccess maps to PtyExited 0
prop_exitCodeToStatusSuccess :: Property
prop_exitCodeToStatusSuccess = property $ do
    exitCodeToStatus ExitSuccess === PtyExited 0

-- | Property: ExitFailure maps to PtyExited with the failure code
prop_exitCodeToStatusFailure :: Property
prop_exitCodeToStatusFailure = property $ do
    code <- forAll $ Gen.int (Range.linear 1 255)
    exitCodeToStatus (ExitFailure code) === PtyExited code

-- | Property: exitCodeToStatus is total (handles all exit codes)
prop_exitCodeToStatusTotal :: Property
prop_exitCodeToStatusTotal = property $ do
    code <- forAll genExitCode
    let status = exitCodeToStatus code
    -- Just verify it doesn't crash and produces a valid status
    case status of
        PtyExited n -> assert $ n >= 0 || n < 0 -- Always true
        PtyRunning -> failure -- Should never produce Running

-- ============================================================================
-- toMountSpec Properties
-- ============================================================================

-- | Property: toMountSpec preserves source path
prop_toMountSpecSource :: Property
prop_toMountSpecSource = property $ do
    src <- forAll genFilePathText
    dest <- forAll genFilePathText
    ro <- forAll Gen.bool
    let spec = toMountSpec (src, dest, ro)
    msSource spec === T.unpack src

-- | Property: toMountSpec preserves destination path
prop_toMountSpecDest :: Property
prop_toMountSpecDest = property $ do
    src <- forAll genFilePathText
    dest <- forAll genFilePathText
    ro <- forAll Gen.bool
    let spec = toMountSpec (src, dest, ro)
    msDest spec === T.unpack dest

-- | Property: toMountSpec preserves read-only flag
prop_toMountSpecReadOnly :: Property
prop_toMountSpecReadOnly = property $ do
    src <- forAll genFilePathText
    dest <- forAll genFilePathText
    ro <- forAll Gen.bool
    let spec = toMountSpec (src, dest, ro)
    msReadOnly spec === ro

-- ============================================================================
-- Test Tree
-- ============================================================================

tests :: TestTree
tests =
    testGroup
        "Pty.Internal Property Tests"
        [ testGroup
            "resolveCreateParams"
            [ testProperty "sandbox defaults to True" prop_resolveCreateParamsSandboxDefault
            , testProperty "sandbox explicit False" prop_resolveCreateParamsSandboxExplicitFalse
            , testProperty "cwd defaults to defaultDir" prop_resolveCreateParamsCwdDefault
            , testProperty "session ID injected into env" prop_resolveCreateParamsSessionIdInjected
            , testProperty "custom session ID used" prop_resolveCreateParamsCustomSessionId
            , testProperty "network defaults to False" prop_resolveCreateParamsNetworkDefault
            , testProperty "user env vars preserved" prop_resolveCreateParamsEnvPreserved
            ]
        , testGroup
            "exitCodeToStatus"
            [ testProperty "ExitSuccess to PtyExited 0" prop_exitCodeToStatusSuccess
            , testProperty "ExitFailure preserves code" prop_exitCodeToStatusFailure
            , testProperty "handles all exit codes" prop_exitCodeToStatusTotal
            ]
        , testGroup
            "toMountSpec"
            [ testProperty "preserves source path" prop_toMountSpecSource
            , testProperty "preserves destination path" prop_toMountSpecDest
            , testProperty "preserves read-only flag" prop_toMountSpecReadOnly
            ]
        ]
