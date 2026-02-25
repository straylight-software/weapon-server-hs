{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.SandboxProps
Description : Property tests for Sandbox module

Property-based tests for the Sandbox module, focusing on:

- JSON round-trip properties for all types
- Pure function behavior (buildBwrapArgs and helpers)
- Security invariants (namespaces, capabilities)
- Path calculation correctness
-}
module Property.SandboxProps where

import Data.Aeson (decode, encode)
import Data.Foldable (forM_)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Sandbox.Sandbox
import Sandbox.Types
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Type JSON Round-trip Tests
-- ============================================================================

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

-- ============================================================================
-- sandboxDirPaths Tests
-- ============================================================================

-- | Property: sandboxDirPaths produces consistent directory structure
prop_sandboxDirPathsStructure :: Property
prop_sandboxDirPathsStructure = property $ do
    sandboxId <- forAll genSandboxId
    let SandboxDirPaths{sdpBaseDir, sdpUpperDir, sdpWorkDir, sdpMergedDir} = sandboxDirPaths sandboxId

    -- Base dir should contain sandbox ID
    assert $ T.unpack sandboxId `isInfixOf` sdpBaseDir

    -- Sub-directories should be children of base
    assert $ sdpUpperDir == sdpBaseDir </> "upper"
    assert $ sdpWorkDir == sdpBaseDir </> "work"
    assert $ sdpMergedDir == sdpBaseDir </> "merged"

-- | Property: sandboxDirPaths base directory is in /tmp
prop_sandboxDirPathsInTmp :: Property
prop_sandboxDirPathsInTmp = property $ do
    sandboxId <- forAll genSandboxId
    let SandboxDirPaths{sdpBaseDir} = sandboxDirPaths sandboxId
    assert $ "/tmp/" `isPrefixOf` sdpBaseDir

-- | Property: upperDirPath is consistent with sandboxDirPaths
prop_upperDirPathConsistent :: Property
prop_upperDirPathConsistent = property $ do
    sandboxId <- forAll genSandboxId
    let SandboxDirPaths{sdpBaseDir, sdpUpperDir} = sandboxDirPaths sandboxId
    upperDirPath sdpBaseDir === sdpUpperDir

-- ============================================================================
-- buildBwrapArgs Tests
-- ============================================================================

-- | Property: NetworkNone includes --unshare-net
prop_buildBwrapArgsNetworkNone :: Property
prop_buildBwrapArgsNetworkNone = property $ do
    workdir <- forAll genFilePath
    let config = (defaultConfig workdir){scNetwork = NetworkNone}
    let args = buildBwrapArgs config
    assert $ "--unshare-net" `elem` args

-- | Property: NetworkHost does NOT include --unshare-net
prop_buildBwrapArgsNetworkHost :: Property
prop_buildBwrapArgsNetworkHost = property $ do
    workdir <- forAll genFilePath
    let config = (defaultConfig workdir){scNetwork = NetworkHost}
    let args = buildBwrapArgs config
    assert $ "--unshare-net" `notElem` args

-- | Property: NetworkSlirp includes --unshare-net (for slirp4netns)
prop_buildBwrapArgsNetworkSlirp :: Property
prop_buildBwrapArgsNetworkSlirp = property $ do
    workdir <- forAll genFilePath
    let config = (defaultConfig workdir){scNetwork = NetworkSlirp}
    let args = buildBwrapArgs config
    assert $ "--unshare-net" `elem` args

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
    let config = (defaultConfig workdir){scMounts = mounts}
    let args = buildBwrapArgs config
    -- Each mount should produce either --bind or --ro-bind
    forM_ mounts $ \mount -> do
        if msReadOnly mount
            then assert $ ["--ro-bind", msSource mount, msDest mount] `isSublistOf` args
            else assert $ ["--bind", msSource mount, msDest mount] `isSublistOf` args

-- | Property: Always includes required namespace isolation args
prop_buildBwrapArgsAlwaysUnshares :: Property
prop_buildBwrapArgsAlwaysUnshares = property $ do
    config <- forAll genSandboxConfig
    let args = buildBwrapArgs config

    -- These are always required for security
    assert $ "--unshare-user" `elem` args
    assert $ "--unshare-pid" `elem` args
    assert $ "--unshare-uts" `elem` args
    assert $ "--unshare-ipc" `elem` args
    assert $ "--die-with-parent" `elem` args

-- | Property: Shell is always the last argument after --
prop_buildBwrapArgsShellLast :: Property
prop_buildBwrapArgsShellLast = property $ do
    config <- forAll genSandboxConfig
    let args = buildBwrapArgs config
        shellPath = scShell config

    -- Find -- and check what follows
    case dropWhile (/= "--") args of
        [dashDash, s, dashL] -> do
            dashDash === "--"
            dashL === "-l"
            s === shellPath
        [] -> failure
        [_] -> failure
        [_, _] -> failure
        (_ : _ : _ : _ : _) -> failure

-- | Property: Environment is cleared before setting variables
prop_buildBwrapArgsClearenv :: Property
prop_buildBwrapArgsClearenv = property $ do
    config <- forAll genSandboxConfig
    let args = buildBwrapArgs config
    assert $ "--clearenv" `elem` args

-- ============================================================================
-- buildNetworkArgs Tests
-- ============================================================================

-- | Property: buildNetworkArgs NetworkNone returns --unshare-net
prop_buildNetworkArgsNone :: Property
prop_buildNetworkArgsNone = property $ do
    let args = buildNetworkArgs NetworkNone
    args === ["--unshare-net"]

-- | Property: buildNetworkArgs NetworkHost returns empty
prop_buildNetworkArgsHost :: Property
prop_buildNetworkArgsHost = property $ do
    let args = buildNetworkArgs NetworkHost
    args === []

-- | Property: buildNetworkArgs NetworkSlirp returns --unshare-net
prop_buildNetworkArgsSlirp :: Property
prop_buildNetworkArgsSlirp = property $ do
    let args = buildNetworkArgs NetworkSlirp
    args === ["--unshare-net"]

-- ============================================================================
-- buildNamespaceArgs Tests
-- ============================================================================

-- | Property: buildNamespaceArgs always includes core namespaces
prop_buildNamespaceArgsCoreNamespaces :: Property
prop_buildNamespaceArgsCoreNamespaces = property $ do
    mode <- forAll genNetworkMode
    let args = buildNamespaceArgs mode
    assert $ "--unshare-user" `elem` args
    assert $ "--unshare-pid" `elem` args
    assert $ "--unshare-uts" `elem` args
    assert $ "--unshare-ipc" `elem` args
    assert $ "--die-with-parent" `elem` args

-- ============================================================================
-- buildFilesystemArgs Tests
-- ============================================================================

-- | Property: buildFilesystemArgs always sets up /dev and /proc
prop_buildFilesystemArgsDevProc :: Property
prop_buildFilesystemArgsDevProc = property $ do
    workdir <- forAll genFilePath
    mounts <- forAll $ Gen.list (Range.linear 0 3) genMountSpec
    let args = buildFilesystemArgs workdir mounts
    assert $ ["--dev", "/dev"] `isSublistOf` args
    assert $ ["--proc", "/proc"] `isSublistOf` args

-- | Property: buildFilesystemArgs binds host root read-only
prop_buildFilesystemArgsRoRoot :: Property
prop_buildFilesystemArgsRoRoot = property $ do
    workdir <- forAll genFilePath
    let args = buildFilesystemArgs workdir []
    assert $ ["--ro-bind", "/", "/"] `isSublistOf` args

-- | Property: buildFilesystemArgs sets up tmpfs for /tmp and /var/tmp
prop_buildFilesystemArgsTmpfs :: Property
prop_buildFilesystemArgsTmpfs = property $ do
    workdir <- forAll genFilePath
    let args = buildFilesystemArgs workdir []
    assert $ ["--tmpfs", "/tmp"] `isSublistOf` args
    assert $ ["--tmpfs", "/var/tmp"] `isSublistOf` args

-- ============================================================================
-- buildSecurityArgs Tests
-- ============================================================================

-- | Property: buildSecurityArgs with seccomp enabled includes --new-session
prop_buildSecurityArgsSeccomp :: Property
prop_buildSecurityArgsSeccomp = property $ do
    limits <- forAll genResourceLimits
    let args = buildSecurityArgs True limits
    assert $ "--new-session" `elem` args

-- | Property: buildSecurityArgs with seccomp disabled excludes --new-session
prop_buildSecurityArgsNoSeccomp :: Property
prop_buildSecurityArgsNoSeccomp = property $ do
    limits <- forAll genResourceLimits
    let args = buildSecurityArgs False limits
    assert $ "--new-session" `notElem` args

-- | Property: buildSecurityArgs with NoNewPrivs drops all capabilities
prop_buildSecurityArgsNoNewPrivs :: Property
prop_buildSecurityArgsNoNewPrivs = property $ do
    let limits = defaultLimits{rlNoNewPrivs = True}
    let args = buildSecurityArgs True limits
    assert $ ["--cap-drop", "ALL"] `isSublistOf` args

-- | Property: buildSecurityArgs without NoNewPrivs doesn't drop caps
prop_buildSecurityArgsWithPrivs :: Property
prop_buildSecurityArgsWithPrivs = property $ do
    let limits = defaultLimits{rlNoNewPrivs = False}
    let args = buildSecurityArgs True limits
    assert $ "--cap-drop" `notElem` args

-- ============================================================================
-- mountToArgs Tests
-- ============================================================================

-- | Property: mountToArgs produces --ro-bind for read-only mounts
prop_mountToArgsReadOnly :: Property
prop_mountToArgsReadOnly = property $ do
    source <- forAll genFilePath
    dest <- forAll genFilePath
    let mount = MountSpec source dest True
    mountToArgs mount === ["--ro-bind", source, dest]

-- | Property: mountToArgs produces --bind for read-write mounts
prop_mountToArgsReadWrite :: Property
prop_mountToArgsReadWrite = property $ do
    source <- forAll genFilePath
    dest <- forAll genFilePath
    let mount = MountSpec source dest False
    mountToArgs mount === ["--bind", source, dest]

-- ============================================================================
-- envToArgs Tests
-- ============================================================================

-- | Property: envToArgs produces correct --setenv format
prop_envToArgsFormat :: Property
prop_envToArgsFormat = property $ do
    key <- forAll genText
    val <- forAll genText
    envToArgs (key, val) === ["--setenv", T.unpack key, T.unpack val]

-- ============================================================================
-- buildDefaultEnv Tests
-- ============================================================================

-- | Property: buildDefaultEnv always sets HOME from config
prop_buildDefaultEnvHome :: Property
prop_buildDefaultEnvHome = property $ do
    config <- forAll genSandboxConfig
    let args = buildDefaultEnv config
    assert $ ["--setenv", "HOME", scHome config] `isSublistOf` args

-- | Property: buildDefaultEnv always sets OPENCODE_SANDBOX=1
prop_buildDefaultEnvSandboxFlag :: Property
prop_buildDefaultEnvSandboxFlag = property $ do
    config <- forAll genSandboxConfig
    let args = buildDefaultEnv config
    assert $ ["--setenv", "OPENCODE_SANDBOX", "1"] `isSublistOf` args

-- | Property: buildDefaultEnv sets proxy environment variables
prop_buildDefaultEnvProxy :: Property
prop_buildDefaultEnvProxy = property $ do
    config <- forAll genSandboxConfig
    let args = buildDefaultEnv config
    assert $ "--setenv" `elem` args
    assert $ "HTTP_PROXY" `elem` args
    assert $ "HTTPS_PROXY" `elem` args

-- ============================================================================
-- buildRsyncArgs Tests
-- ============================================================================

-- | Property: buildRsyncArgs includes -a flag for archive mode
prop_buildRsyncArgsArchive :: Property
prop_buildRsyncArgsArchive = property $ do
    upper <- forAll genFilePath
    workdir <- forAll genFilePath
    let args = buildRsyncArgs upper workdir
    assert $ "-a" `elem` args

-- | Property: buildRsyncArgs paths end with /
prop_buildRsyncArgsTrailingSlash :: Property
prop_buildRsyncArgsTrailingSlash = property $ do
    upper <- forAll genFilePath
    workdir <- forAll genFilePath
    let args = buildRsyncArgs upper workdir
    -- Both paths should end with /
    assert $ (upper <> "/") `elem` args
    assert $ (workdir <> "/") `elem` args

-- ============================================================================
-- Generators
-- ============================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 1 50) Gen.alphaNum

genSandboxId :: Gen Text
genSandboxId = Gen.text (Range.linear 8 32) Gen.alphaNum

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
        <*> genFilePath -- scShell
        <*> genFilePath -- scHome
        <*> genText -- scUser

genSandboxStatus :: Gen SandboxStatus
genSandboxStatus =
    Gen.choice
        [ pure SandboxRunning
        , SandboxExited <$> Gen.int (Range.linear 0 255)
        , SandboxKilled <$> Gen.int (Range.linear 1 31)
        ]

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Check if a list is a sublist (contiguous) of another list
isSublistOf :: (Eq a) => [a] -> [a] -> Bool
isSublistOf [] _ = True
isSublistOf _ [] = False
isSublistOf sub@(x : xs) (y : ys)
    | x == y && xs `isPrefixOf` ys = True
    | otherwise = sub `isSublistOf` ys

-- ============================================================================
-- Test Tree
-- ============================================================================

tests :: TestTree
tests =
    testGroup
        "Sandbox Property Tests"
        [ testGroup
            "Type Round-trips"
            [ testProperty "NetworkMode round-trip" prop_networkModeRoundtrip
            , testProperty "ResourceLimits round-trip" prop_resourceLimitsRoundtrip
            , testProperty "Coeffects round-trip" prop_coeffectsRoundtrip
            , testProperty "SandboxConfig round-trip" prop_sandboxConfigRoundtrip
            , testProperty "SandboxStatus ToJSON" prop_sandboxStatusToJSON
            ]
        , testGroup
            "sandboxDirPaths"
            [ testProperty "produces consistent directory structure" prop_sandboxDirPathsStructure
            , testProperty "base directory is in /tmp" prop_sandboxDirPathsInTmp
            , testProperty "upperDirPath is consistent" prop_upperDirPathConsistent
            ]
        , testGroup
            "buildBwrapArgs"
            [ testProperty "NetworkNone includes --unshare-net" prop_buildBwrapArgsNetworkNone
            , testProperty "NetworkHost excludes --unshare-net" prop_buildBwrapArgsNetworkHost
            , testProperty "NetworkSlirp includes --unshare-net" prop_buildBwrapArgsNetworkSlirp
            , testProperty "Workdir in --chdir and --bind" prop_buildBwrapArgsWorkdir
            , testProperty "Mounts produce correct bind args" prop_buildBwrapArgsMounts
            , testProperty "always includes namespace isolation" prop_buildBwrapArgsAlwaysUnshares
            , testProperty "shell is last argument" prop_buildBwrapArgsShellLast
            , testProperty "environment is cleared" prop_buildBwrapArgsClearenv
            ]
        , testGroup
            "buildNetworkArgs"
            [ testProperty "NetworkNone -> --unshare-net" prop_buildNetworkArgsNone
            , testProperty "NetworkHost -> empty" prop_buildNetworkArgsHost
            , testProperty "NetworkSlirp -> --unshare-net" prop_buildNetworkArgsSlirp
            ]
        , testGroup
            "buildNamespaceArgs"
            [ testProperty "always includes core namespaces" prop_buildNamespaceArgsCoreNamespaces
            ]
        , testGroup
            "buildFilesystemArgs"
            [ testProperty "sets up /dev and /proc" prop_buildFilesystemArgsDevProc
            , testProperty "binds host root read-only" prop_buildFilesystemArgsRoRoot
            , testProperty "sets up tmpfs" prop_buildFilesystemArgsTmpfs
            ]
        , testGroup
            "buildSecurityArgs"
            [ testProperty "seccomp enabled includes --new-session" prop_buildSecurityArgsSeccomp
            , testProperty "seccomp disabled excludes --new-session" prop_buildSecurityArgsNoSeccomp
            , testProperty "NoNewPrivs drops capabilities" prop_buildSecurityArgsNoNewPrivs
            , testProperty "without NoNewPrivs keeps caps" prop_buildSecurityArgsWithPrivs
            ]
        , testGroup
            "mountToArgs"
            [ testProperty "read-only uses --ro-bind" prop_mountToArgsReadOnly
            , testProperty "read-write uses --bind" prop_mountToArgsReadWrite
            ]
        , testGroup
            "envToArgs"
            [ testProperty "produces --setenv format" prop_envToArgsFormat
            ]
        , testGroup
            "buildDefaultEnv"
            [ testProperty "sets HOME from config" prop_buildDefaultEnvHome
            , testProperty "sets OPENCODE_SANDBOX=1" prop_buildDefaultEnvSandboxFlag
            , testProperty "sets proxy variables" prop_buildDefaultEnvProxy
            ]
        , testGroup
            "buildRsyncArgs"
            [ testProperty "includes -a flag" prop_buildRsyncArgsArchive
            , testProperty "paths end with /" prop_buildRsyncArgsTrailingSlash
            ]
        ]
