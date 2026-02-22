{-# LANGUAGE OverloadedStrings #-}

-- | Sandbox property tests
module Property.SandboxProps where

import Data.Aeson (decode, encode)
import Data.Foldable (forM_)
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Sandbox.Sandbox (buildBwrapArgs)
import Sandbox.Types
import Test.Tasty
import Test.Tasty.Hedgehog

-- ═══════════════════════════════════════════════════════════════════════════
-- Type JSON Round-trip Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: NetworkMode JSON round-trip
prop_networkModeRoundtrip :: Property
prop_networkModeRoundtrip = property $ do
    mode <- forAll genNetworkMode
    let json = encode mode
    case decode json of
        Nothing -> failure
        Just mode' -> mode === mode'

-- | Property: ResourceLimits JSON round-trip
prop_resourceLimitsRoundtrip :: Property
prop_resourceLimitsRoundtrip = property $ do
    limits <- forAll genResourceLimits
    let json = encode limits
    case decode json of
        Nothing -> failure
        Just limits' -> limits === limits'

-- | Property: Coeffects JSON round-trip
prop_coeffectsRoundtrip :: Property
prop_coeffectsRoundtrip = property $ do
    coeff <- forAll genCoeffects
    let json = encode coeff
    case decode json of
        Nothing -> failure
        Just coeff' -> coeff === coeff'

-- | Property: SandboxConfig JSON round-trip
prop_sandboxConfigRoundtrip :: Property
prop_sandboxConfigRoundtrip = property $ do
    config <- forAll genSandboxConfig
    let json = encode config
    case decode json of
        Nothing -> failure
        Just config' -> config === config'

-- | Property: SandboxStatus JSON round-trip (ToJSON only - no FromJSON)
prop_sandboxStatusToJSON :: Property
prop_sandboxStatusToJSON = property $ do
    status <- forAll genSandboxStatus
    -- Just verify it encodes without error
    let json = encode status
    assert $ json /= ""

-- ═══════════════════════════════════════════════════════════════════════════
-- buildBwrapArgs Logic Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: NetworkNone includes --unshare-net
prop_buildBwrapArgsNetworkNone :: Property
prop_buildBwrapArgsNetworkNone = property $ do
    workdir <- forAll genFilePath
    let config = (defaultConfig workdir) { scNetwork = NetworkNone }
    let args = buildBwrapArgs config
    assert $ "--unshare-net" `elem` args

-- | Property: NetworkHost does NOT include --unshare-net
prop_buildBwrapArgsNetworkHost :: Property
prop_buildBwrapArgsNetworkHost = property $ do
    workdir <- forAll genFilePath
    let config = (defaultConfig workdir) { scNetwork = NetworkHost }
    let args = buildBwrapArgs config
    assert $ "--unshare-net" `notElem` args

-- | Property: Workdir appears in --chdir and --bind args
prop_buildBwrapArgsWorkdir :: Property
prop_buildBwrapArgsWorkdir = property $ do
    workdir <- forAll genFilePath
    let config = defaultConfig workdir
    let args = buildBwrapArgs config
    -- Check that --chdir workdir appears
    assert $ ["--chdir", workdir] `isSublistOf` args
    -- Check that --bind workdir workdir appears
    assert $ ["--bind", workdir, workdir] `isSublistOf` args

-- | Property: Each MountSpec produces correct bind args
prop_buildBwrapArgsMounts :: Property
prop_buildBwrapArgsMounts = property $ do
    workdir <- forAll genFilePath
    mounts <- forAll $ Gen.list (Range.linear 0 5) genMountSpec
    let config = (defaultConfig workdir) { scMounts = mounts }
    let args = buildBwrapArgs config
    -- Each mount should produce either --bind or --ro-bind
    forM_ mounts $ \mount -> do
        if msReadOnly mount
            then assert $ ["--ro-bind", msSource mount, msDest mount] `isSublistOf` args
            else assert $ ["--bind", msSource mount, msDest mount] `isSublistOf` args

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

genText :: Gen Text
genText = Gen.text (Range.linear 1 50) Gen.alphaNum

genFilePath :: Gen FilePath
genFilePath = do
    parts <- Gen.list (Range.linear 1 4) (Gen.string (Range.linear 1 20) Gen.alphaNum)
    pure $ "/" ++ foldr (\a b -> a ++ "/" ++ b) "" parts

genNetworkMode :: Gen NetworkMode
genNetworkMode = Gen.element [NetworkNone, NetworkHost, NetworkSlirp]

genResourceLimits :: Gen ResourceLimits
genResourceLimits =
    ResourceLimits
        <$> Gen.maybe (Gen.word64 (Range.linear 1 (10 * 1024 * 1024 * 1024)))
        <*> Gen.maybe (Gen.word64 (Range.linear 1 100000))
        <*> Gen.word64 (Range.linear 1000 1000000)
        <*> Gen.maybe (Gen.word64 (Range.linear 1 10000))
        <*> Gen.bool

genCoeffects :: Gen Coeffects
genCoeffects =
    Coeffects
        <$> Gen.bool
        <*> Gen.list (Range.linear 0 3) genText
        <*> Gen.list (Range.linear 0 3) genFilePath

genMountSpec :: Gen MountSpec
genMountSpec =
    MountSpec
        <$> genFilePath
        <*> genFilePath
        <*> Gen.bool

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
        <*> Gen.word64 (Range.linear 1 (1024 * 1024 * 1024))

genSandboxStatus :: Gen SandboxStatus
genSandboxStatus =
    Gen.choice
        [ pure SandboxRunning
        , SandboxExited <$> Gen.int (Range.linear 0 255)
        , SandboxKilled <$> Gen.int (Range.linear 1 31)
        ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Helpers
-- ═══════════════════════════════════════════════════════════════════════════

-- | Check if a list is a sublist (contiguous) of another list
isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf [] _ = True
isSublistOf _ [] = False
isSublistOf sub@(x:xs) (y:ys)
    | x == y && xs `isPrefixOf'` ys = True
    | otherwise = sub `isSublistOf` ys
  where
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (a:as) (b:bs) = a == b && isPrefixOf' as bs

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Sandbox Property Tests"
        [ testGroup "Type Round-trips"
            [ testProperty "NetworkMode round-trip" prop_networkModeRoundtrip
            , testProperty "ResourceLimits round-trip" prop_resourceLimitsRoundtrip
            , testProperty "Coeffects round-trip" prop_coeffectsRoundtrip
            , testProperty "SandboxConfig round-trip" prop_sandboxConfigRoundtrip
            , testProperty "SandboxStatus ToJSON" prop_sandboxStatusToJSON
            ]
        , testGroup "buildBwrapArgs Logic"
            [ testProperty "NetworkNone includes --unshare-net" prop_buildBwrapArgsNetworkNone
            , testProperty "NetworkHost excludes --unshare-net" prop_buildBwrapArgsNetworkHost
            , testProperty "Workdir in --chdir and --bind" prop_buildBwrapArgsWorkdir
            , testProperty "Mounts produce correct bind args" prop_buildBwrapArgsMounts
            ]
        ]
