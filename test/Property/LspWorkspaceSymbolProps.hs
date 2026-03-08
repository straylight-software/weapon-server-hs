{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.LspWorkspaceSymbolProps
Description : Property tests for Lsp.WorkspaceSymbol pure helpers

Adversarial properties target edge cases in symbol filtering and location normalization.
-}
module Property.LspWorkspaceSymbolProps where

import Data.Aeson (Value, toJSON)
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.LSP.Protocol.Types (Null (Null), type (|?) (..))
import Language.LSP.Protocol.Types qualified as LSP
import Lsp.WorkspaceSymbol qualified as LspWS
import Test.Tasty
import Test.Tasty.Hedgehog

prop_zeroRangeIsZero :: Property
prop_zeroRangeIsZero = property $ do
    let LSP.Range{_start = LSP.Position sl sc, _end = LSP.Position el ec} = LspWS.zeroRange
    sl === 0
    sc === 0
    el === 0
    ec === 0

prop_normalizeLocationIdentity :: Property
prop_normalizeLocationIdentity = property $ do
    loc <- forAll genLocation
    LspWS.normalizeLocation (InL loc) === loc

prop_normalizeLocationUriOnly :: Property
prop_normalizeLocationUriOnly = property $ do
    uri <- forAll genUri
    let loc = LspWS.normalizeLocation (InR (LSP.LocationUriOnly uri))
    let LSP.Location{_uri = outUri, _range = outRange} = loc
    outUri === uri
    outRange === LspWS.zeroRange

prop_workspaceToSymbolInfoCarriesFields :: Property
prop_workspaceToSymbolInfoCarriesFields = property $ do
    ws <- forAll genWorkspaceSymbol
    let info = LspWS.workspaceToSymbolInfo ws
    let (wsName, wsKind, wsTags, wsContainer, wsLoc) = unpackWorkspace ws
    let (infoName, infoKind, infoTags, infoContainer, infoLoc) = unpackSymbolInfo info
    infoName === wsName
    infoKind === wsKind
    infoTags === wsTags
    infoContainer === wsContainer
    infoLoc === LspWS.normalizeLocation wsLoc

prop_filterSymbolInfosAllowedOnly :: Property
prop_filterSymbolInfosAllowedOnly = property $ do
    infos <- forAll $ Gen.list (Range.linear 0 30) genSymbolInformation
    let filtered = LspWS.filterSymbolInfos infos
    assert $ all (\s -> symbolInfoKind s `elem` allowedKinds) filtered

prop_filterWorkspaceSymbolsAllowedOnly :: Property
prop_filterWorkspaceSymbolsAllowedOnly = property $ do
    symbols <- forAll $ Gen.list (Range.linear 0 30) genWorkspaceSymbol
    let filtered = LspWS.filterWorkspaceSymbols symbols
    assert $ all (\s -> workspaceSymbolKind s `elem` allowedKinds) filtered

prop_extractSymbolsInLMatchesFilter :: Property
prop_extractSymbolsInLMatchesFilter = property $ do
    infos <- forAll $ Gen.list (Range.linear 0 30) genSymbolInformation
    let expected = map toJSON (LspWS.filterSymbolInfos infos)
    LspWS.extractSymbols (InL infos) === expected

prop_extractSymbolsNullEmpty :: Property
prop_extractSymbolsNullEmpty = property $ do
    LspWS.extractSymbols (InR (InR Null)) === ([] :: [Value])

prop_parseRootUriOverride :: Property
prop_parseRootUriOverride = property $ do
    let uriText = "file:///tmp/project"
    LspWS.parseRootUri (Just uriText) "/ignored" === InL (LSP.Uri uriText)

prop_parseRootUriFallback :: Property
prop_parseRootUriFallback = property $ do
    let root = "/tmp/project"
    LspWS.parseRootUri Nothing root === InL (LSP.filePathToUri root)

prop_decodeInitOptionsInvalid :: Property
prop_decodeInitOptionsInvalid = property $ do
    txt <- forAll $ Gen.element ["{", "not-json", "\"unterminated"]
    LspWS.decodeInitOptions (Just txt) === (Nothing :: Maybe Value)

-- Generators

genText :: Gen Text
genText = Gen.text (Range.linear 1 20) Gen.alphaNum

genUri :: Gen LSP.Uri
genUri = do
    name <- genText
    pure $ LSP.Uri ("file:///tmp/" <> name)

genPosition :: Gen LSP.Position
genPosition = do
    line <- Gen.int (Range.linear 0 500)
    char <- Gen.int (Range.linear 0 200)
    pure $ LSP.Position (fromIntegral line) (fromIntegral char)

genRange :: Gen LSP.Range
genRange = do
    start <- genPosition
    LSP.Range start <$> genPosition

genLocation :: Gen LSP.Location
genLocation = do
    uri <- genUri
    LSP.Location uri <$> genRange

genSymbolKindAllowed :: Gen LSP.SymbolKind
genSymbolKindAllowed = Gen.element allowedKinds

genSymbolKindDisallowed :: Gen LSP.SymbolKind
genSymbolKindDisallowed = Gen.element disallowedKinds

genSymbolInformation :: Gen LSP.SymbolInformation
genSymbolInformation = do
    name <- genText
    kind <- Gen.choice [genSymbolKindAllowed, genSymbolKindDisallowed]
    loc <- genLocation
    pure $
        LSP.SymbolInformation
            { _name = name
            , _kind = kind
            , _tags = Nothing
            , _containerName = Nothing
            , _deprecated = Nothing
            , _location = loc
            }

genWorkspaceSymbol :: Gen LSP.WorkspaceSymbol
genWorkspaceSymbol = do
    name <- genText
    kind <- Gen.choice [genSymbolKindAllowed, genSymbolKindDisallowed]
    loc <- Gen.choice [InL <$> genLocation, InR . LSP.LocationUriOnly <$> genUri]
    pure $
        LSP.WorkspaceSymbol
            { _name = name
            , _kind = kind
            , _tags = Nothing
            , _containerName = Nothing
            , _location = loc
            , _data_ = Nothing
            }

allowedKinds :: [LSP.SymbolKind]
allowedKinds =
    [ LSP.SymbolKind_Class
    , LSP.SymbolKind_Function
    , LSP.SymbolKind_Method
    , LSP.SymbolKind_Interface
    , LSP.SymbolKind_Variable
    , LSP.SymbolKind_Constant
    , LSP.SymbolKind_Struct
    , LSP.SymbolKind_Enum
    ]

disallowedKinds :: [LSP.SymbolKind]
disallowedKinds =
    [ LSP.SymbolKind_File
    , LSP.SymbolKind_Module
    , LSP.SymbolKind_Namespace
    , LSP.SymbolKind_Package
    , LSP.SymbolKind_Property
    , LSP.SymbolKind_Field
    , LSP.SymbolKind_Constructor
    , LSP.SymbolKind_String
    , LSP.SymbolKind_Number
    , LSP.SymbolKind_Boolean
    , LSP.SymbolKind_Array
    , LSP.SymbolKind_Object
    , LSP.SymbolKind_Key
    , LSP.SymbolKind_Null
    , LSP.SymbolKind_EnumMember
    , LSP.SymbolKind_Event
    , LSP.SymbolKind_Operator
    , LSP.SymbolKind_TypeParameter
    ]

symbolInfoKind :: LSP.SymbolInformation -> LSP.SymbolKind
symbolInfoKind LSP.SymbolInformation{_kind = kind} = kind

workspaceSymbolKind :: LSP.WorkspaceSymbol -> LSP.SymbolKind
workspaceSymbolKind LSP.WorkspaceSymbol{_kind = kind} = kind

unpackSymbolInfo :: LSP.SymbolInformation -> (Text, LSP.SymbolKind, Maybe [LSP.SymbolTag], Maybe Text, LSP.Location)
unpackSymbolInfo LSP.SymbolInformation{_name = name, _kind = kind, _tags = tags, _containerName = container, _location = loc} =
    (name, kind, tags, container, loc)

unpackWorkspace :: LSP.WorkspaceSymbol -> (Text, LSP.SymbolKind, Maybe [LSP.SymbolTag], Maybe Text, LSP.Location |? LSP.LocationUriOnly)
unpackWorkspace LSP.WorkspaceSymbol{_name = name, _kind = kind, _tags = tags, _containerName = container, _location = loc} =
    (name, kind, tags, container, loc)

tests :: TestTree
tests =
    testGroup
        "LSP Workspace Symbol Property Tests"
        [ testProperty "zeroRange is zero" prop_zeroRangeIsZero
        , testProperty "normalizeLocation identity for full location" prop_normalizeLocationIdentity
        , testProperty "normalizeLocation fills zero range for uri-only" prop_normalizeLocationUriOnly
        , testProperty "workspaceToSymbolInfo preserves fields" prop_workspaceToSymbolInfoCarriesFields
        , testProperty "filterSymbolInfos keeps allowed kinds only" prop_filterSymbolInfosAllowedOnly
        , testProperty "filterWorkspaceSymbols keeps allowed kinds only" prop_filterWorkspaceSymbolsAllowedOnly
        , testProperty "extractSymbols InL matches filtered JSON" prop_extractSymbolsInLMatchesFilter
        , testProperty "extractSymbols Null yields empty list" prop_extractSymbolsNullEmpty
        , testProperty "parseRootUri uses override" prop_parseRootUriOverride
        , testProperty "parseRootUri falls back to filePathToUri" prop_parseRootUriFallback
        , testProperty "decodeInitOptions invalid JSON returns Nothing" prop_decodeInitOptionsInvalid
        ]
