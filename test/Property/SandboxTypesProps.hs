{-# LANGUAGE OverloadedStrings #-}

-- | Sandbox.Types property tests
module Property.SandboxTypesProps where

import Data.Aeson (decode, encode)
import Data.Text (Text)
import Data.Word ()
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Sandbox.Types
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 0 100) Gen.alphaNum

genFilePath :: Gen FilePath
genFilePath = do
    segments <- Gen.list (Range.linear 1 5) (Gen.list (Range.linear 1 20) Gen.alphaNum)
    pure $ "/" <> mconcat (map (<> "/") (init segments)) <> last segments

genNetworkMode :: Gen NetworkMode
genNetworkMode = Gen.element [NetworkNone, NetworkHost, NetworkSlirp]

genMountSpec :: Gen MountSpec
genMountSpec =
    MountSpec
        <$> genFilePath
        <*> genFilePath
        <*> Gen.bool

genResourceLimits :: Gen ResourceLimits
genResourceLimits =
    ResourceLimits
        <$> Gen.maybe (Gen.word64 (Range.linear 0 (4 * 1024 * 1024 * 1024)))
        <*> Gen.maybe (Gen.word64 (Range.linear 0 1000000))
        <*> Gen.word64 (Range.linear 1000 1000000)
        <*> Gen.maybe (Gen.word64 (Range.linear 1 10000))
        <*> Gen.bool

genCoeffects :: Gen Coeffects
genCoeffects =
    Coeffects
        <$> Gen.bool
        <*> Gen.list (Range.linear 0 3) genText
        <*> Gen.list (Range.linear 0 3) genFilePath

genSandboxConfig :: Gen SandboxConfig
genSandboxConfig =
    SandboxConfig
        <$> Gen.maybe genFilePath
        <*> genFilePath
        <*> genNetworkMode
        <*> Gen.list (Range.linear 0 3) genMountSpec
        <*> Gen.list (Range.linear 0 3) ((,) <$> genText <*> genText)
        <*> genResourceLimits
        <*> genCoeffects
        <*> Gen.bool
        <*> Gen.word64 (Range.linear (64 * 1024 * 1024) (2 * 1024 * 1024 * 1024))

genSandboxStatus :: Gen SandboxStatus
genSandboxStatus =
    Gen.choice
        [ pure SandboxRunning
        , SandboxExited <$> Gen.int (Range.linear 0 255)
        , SandboxKilled <$> Gen.int (Range.linear 1 31)
        ]

-- ============================================================================
-- Properties
-- ============================================================================

prop_networkModeRoundtrip :: Property
prop_networkModeRoundtrip = property $ do
    mode <- forAll genNetworkMode
    let json = encode mode
    case decode json of
        Nothing -> failure
        Just mode' -> mode === mode'

prop_mountSpecRoundtrip :: Property
prop_mountSpecRoundtrip = property $ do
    spec <- forAll genMountSpec
    let json = encode spec
    case decode json of
        Nothing -> failure
        Just spec' -> spec === spec'

prop_resourceLimitsRoundtrip :: Property
prop_resourceLimitsRoundtrip = property $ do
    limits <- forAll genResourceLimits
    let json = encode limits
    case decode json of
        Nothing -> failure
        Just limits' -> limits === limits'

prop_coeffectsRoundtrip :: Property
prop_coeffectsRoundtrip = property $ do
    coeffects <- forAll genCoeffects
    let json = encode coeffects
    case decode json of
        Nothing -> failure
        Just coeffects' -> coeffects === coeffects'

prop_sandboxConfigRoundtrip :: Property
prop_sandboxConfigRoundtrip = property $ do
    config <- forAll genSandboxConfig
    let json = encode config
    case decode json of
        Nothing -> failure
        Just config' -> config === config'

-- | Property: defaultLimits has sensible values
prop_defaultLimitsValues :: Property
prop_defaultLimitsValues = property $ do
    -- Memory limit is 2GB
    rlMemoryMax defaultLimits === Just (2 * 1024 * 1024 * 1024)
    -- No CPU limit
    rlCpuMax defaultLimits === Nothing
    -- CPU period is 100ms
    rlCpuPeriod defaultLimits === 100000
    -- 1000 processes max
    rlPidsMax defaultLimits === Just 1000
    -- No new privs
    rlNoNewPrivs defaultLimits === True

-- | Property: pureCoeffects has no network
prop_pureCoeffectsNoNetwork :: Property
prop_pureCoeffectsNoNetwork = property $ do
    cfNetwork pureCoeffects === False

-- | Property: pureCoeffects has no auth
prop_pureCoeffectsNoAuth :: Property
prop_pureCoeffectsNoAuth = property $ do
    cfAuth pureCoeffects === []

-- | Property: pureCoeffects has no filesystem
prop_pureCoeffectsNoFilesystem :: Property
prop_pureCoeffectsNoFilesystem = property $ do
    cfFilesystem pureCoeffects === []

-- | Property: defaultConfig uses NetworkNone
prop_defaultConfigNetworkNone :: Property
prop_defaultConfigNetworkNone = property $ do
    workdir <- forAll genFilePath
    let config = defaultConfig workdir
    scNetwork config === NetworkNone

-- | Property: defaultConfig enables seccomp
prop_defaultConfigSeccomp :: Property
prop_defaultConfigSeccomp = property $ do
    workdir <- forAll genFilePath
    let config = defaultConfig workdir
    scSeccomp config === True

-- | Property: defaultConfig uses provided workdir
prop_defaultConfigWorkdir :: Property
prop_defaultConfigWorkdir = property $ do
    workdir <- forAll genFilePath
    let config = defaultConfig workdir
    scWorkdir config === workdir

-- | Property: defaultConfig has 512MB tmpfs
prop_defaultConfigTmpfsSize :: Property
prop_defaultConfigTmpfsSize = property $ do
    workdir <- forAll genFilePath
    let config = defaultConfig workdir
    scTmpfsSize config === 512 * 1024 * 1024

-- | Property: SandboxStatus encodes correctly
prop_sandboxStatusEncodes :: Property
prop_sandboxStatusEncodes = property $ do
    status <- forAll genSandboxStatus
    let json = encode status
    -- Just verify encoding works
    assert $ json /= ""

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Sandbox.Types Property Tests"
        [ testProperty "NetworkMode round-trip" prop_networkModeRoundtrip
        , testProperty "MountSpec round-trip" prop_mountSpecRoundtrip
        , testProperty "ResourceLimits round-trip" prop_resourceLimitsRoundtrip
        , testProperty "Coeffects round-trip" prop_coeffectsRoundtrip
        , testProperty "SandboxConfig round-trip" prop_sandboxConfigRoundtrip
        , testProperty "defaultLimits values" prop_defaultLimitsValues
        , testProperty "pureCoeffects no network" prop_pureCoeffectsNoNetwork
        , testProperty "pureCoeffects no auth" prop_pureCoeffectsNoAuth
        , testProperty "pureCoeffects no filesystem" prop_pureCoeffectsNoFilesystem
        , testProperty "defaultConfig network none" prop_defaultConfigNetworkNone
        , testProperty "defaultConfig seccomp" prop_defaultConfigSeccomp
        , testProperty "defaultConfig workdir" prop_defaultConfigWorkdir
        , testProperty "defaultConfig tmpfs size" prop_defaultConfigTmpfsSize
        , testProperty "SandboxStatus encodes" prop_sandboxStatusEncodes
        ]
