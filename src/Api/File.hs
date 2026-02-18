-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                  // weapon-server // api/file
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- File system types and API endpoints. Provides directory listing, content
-- reading, and file status operations for the workspace.
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.File
    ( -- * File Types
      FileType (..)
    , FileNode (..)
    , ContentType (..)
    , FileContent (..)

      -- * File API Endpoints
    , FileListAPI
    , FileReadAPI
    , FileStatusAPI
    ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant


-- ═══════════════════════════════════════════════════════════════════════════
-- // file type //
-- ═══════════════════════════════════════════════════════════════════════════

data FileType = FileTypeFile | FileTypeDirectory
    deriving (Eq, Show, Generic)

instance ToJSON FileType where
    toJSON FileTypeFile = String "file"
    toJSON FileTypeDirectory = String "directory"

instance FromJSON FileType where
    parseJSON = withText "FileType" $ \case
        "file" -> pure FileTypeFile
        "directory" -> pure FileTypeDirectory
        _ -> fail "Invalid file type"


-- ═══════════════════════════════════════════════════════════════════════════
-- // file node //
-- ═══════════════════════════════════════════════════════════════════════════

data FileNode = FileNode
    { fnName :: Text
    , fnPath :: Text
    , fnAbsolute :: Text
    , fnType :: FileType
    , fnIgnored :: Bool
    }
    deriving (Eq, Show, Generic)

instance ToJSON FileNode where
    toJSON node =
        object
            [ "name" .= fnName node
            , "path" .= fnPath node
            , "absolute" .= fnAbsolute node
            , "type" .= fnType node
            , "ignored" .= fnIgnored node
            ]

instance FromJSON FileNode where
    parseJSON = withObject "FileNode" $ \v ->
        FileNode
            <$> v .: "name"
            <*> v .: "path"
            <*> v .: "absolute"
            <*> v .: "type"
            <*> v .: "ignored"


-- ═══════════════════════════════════════════════════════════════════════════
-- // content type //
-- ═══════════════════════════════════════════════════════════════════════════

data ContentType = ContentTypeText | ContentTypeBinary
    deriving (Eq, Show, Generic)

instance ToJSON ContentType where
    toJSON ContentTypeText = String "text"
    toJSON ContentTypeBinary = String "binary"

instance FromJSON ContentType where
    parseJSON = withText "ContentType" $ \case
        "text" -> pure ContentTypeText
        "binary" -> pure ContentTypeBinary
        _ -> fail "Invalid content type"


-- ═══════════════════════════════════════════════════════════════════════════
-- // file content //
-- ═══════════════════════════════════════════════════════════════════════════

data FileContent = FileContent
    { fcType :: ContentType
    , fcContent :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON FileContent where
    toJSON content =
        object
            [ "type" .= fcType content
            , "content" .= fcContent content
            ]

instance FromJSON FileContent where
    parseJSON = withObject "FileContent" $ \v ->
        FileContent
            <$> v .: "type"
            <*> v .: "content"


-- ═══════════════════════════════════════════════════════════════════════════
-- // api type definitions //
-- ═══════════════════════════════════════════════════════════════════════════

type FileListAPI =
    "file"
        :> QueryParam "directory" Text
        :> QueryParam' '[Required] "path" Text
        :> Get '[JSON] [FileNode]

type FileReadAPI =
    "file"
        :> "content"
        :> QueryParam "directory" Text
        :> QueryParam' '[Required] "path" Text
        :> Get '[JSON] FileContent

type FileStatusAPI =
    "file"
        :> "status"
        :> QueryParam "directory" Text
        :> QueryParam "path" Text
        :> Get '[JSON] [Value]
