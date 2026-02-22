{-# LANGUAGE OverloadedStrings #-}

-- | Api.File property tests
module Property.ApiFileProps where

import Api.File
import Data.Aeson (decode, encode)
import Data.List qualified as List
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- ============================================================================
-- Generators
-- ============================================================================

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.alphaNum

genNonEmptyText :: Gen Text
genNonEmptyText = Gen.text (Range.linear 1 50) Gen.alphaNum

genFilePath :: Gen Text
genFilePath = do
    segments <- Gen.list (Range.linear 1 5) (Gen.text (Range.linear 1 20) Gen.alphaNum)
    pure $ case List.unsnoc segments of
        Nothing -> "/" -- Should not happen with Range.linear 1 5
        Just ([], lastSeg) -> "/" <> lastSeg
        Just (initSegs, lastSeg) -> "/" <> mconcat (map (<> "/") initSegs) <> lastSeg

genFileType :: Gen FileType
genFileType = Gen.element [FileTypeFile, FileTypeDirectory]

genFileNode :: Gen FileNode
genFileNode =
    FileNode
        <$> genNonEmptyText
        <*> genFilePath
        <*> genFilePath
        <*> genFileType
        <*> Gen.bool

genContentType :: Gen ContentType
genContentType = Gen.element [ContentTypeText, ContentTypeBinary]

genFileContent :: Gen FileContent
genFileContent =
    FileContent
        <$> genContentType
        <*> genText

-- ============================================================================
-- Properties
-- ============================================================================

prop_fileTypeRoundtrip :: Property
prop_fileTypeRoundtrip = property $ do
    ft <- forAll genFileType
    let json = encode ft
    case decode json of
        Nothing -> failure
        Just ft' -> ft === ft'

prop_fileNodeRoundtrip :: Property
prop_fileNodeRoundtrip = property $ do
    fn <- forAll genFileNode
    let json = encode fn
    case decode json of
        Nothing -> failure
        Just fn' -> fn === fn'

prop_contentTypeRoundtrip :: Property
prop_contentTypeRoundtrip = property $ do
    ct <- forAll genContentType
    let json = encode ct
    case decode json of
        Nothing -> failure
        Just ct' -> ct === ct'

prop_fileContentRoundtrip :: Property
prop_fileContentRoundtrip = property $ do
    fc <- forAll genFileContent
    let json = encode fc
    case decode json of
        Nothing -> failure
        Just fc' -> fc === fc'

-- | Property: FileTypeFile encodes as "file"
prop_fileTypeFileEncoding :: Property
prop_fileTypeFileEncoding = property $ do
    let json = encode FileTypeFile
    json === "\"file\""

-- | Property: FileTypeDirectory encodes as "directory"
prop_fileTypeDirectoryEncoding :: Property
prop_fileTypeDirectoryEncoding = property $ do
    let json = encode FileTypeDirectory
    json === "\"directory\""

-- | Property: ContentTypeText encodes as "text"
prop_contentTypeTextEncoding :: Property
prop_contentTypeTextEncoding = property $ do
    let json = encode ContentTypeText
    json === "\"text\""

-- | Property: ContentTypeBinary encodes as "binary"
prop_contentTypeBinaryEncoding :: Property
prop_contentTypeBinaryEncoding = property $ do
    let json = encode ContentTypeBinary
    json === "\"binary\""

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Api.File Property Tests"
        [ testProperty "FileType round-trip" prop_fileTypeRoundtrip
        , testProperty "FileNode round-trip" prop_fileNodeRoundtrip
        , testProperty "ContentType round-trip" prop_contentTypeRoundtrip
        , testProperty "FileContent round-trip" prop_fileContentRoundtrip
        , testProperty "FileTypeFile encoding" prop_fileTypeFileEncoding
        , testProperty "FileTypeDirectory encoding" prop_fileTypeDirectoryEncoding
        , testProperty "ContentTypeText encoding" prop_contentTypeTextEncoding
        , testProperty "ContentTypeBinary encoding" prop_contentTypeBinaryEncoding
        ]
