{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- | ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                 // weapon-server // api/file
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

File system types and API endpoints. Provides directory listing, content
reading, and file status operations for the workspace.

= Overview

The File API enables clients to:

* List directory contents ('FileListAPI')
* Read file contents ('FileReadAPI')
* Get file status information ('FileStatusAPI')

Files are represented as 'FileNode' objects with type ('FileType'),
path information, and ignore status.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-}
module Api.File (
    -- * File Types

    -- ** File Type Enumeration
    FileType (..),

    -- ** File Node
    FileNode (..),

    -- ** Content Types
    ContentType (..),
    FileContent (..),

    -- * File API Endpoints
    FileListAPI,
    FileReadAPI,
    FileStatusAPI,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (String),
    object,
    withObject,
    withText,
    (.:),
    (.=),
 )
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (
    Get,
    JSON,
    QueryParam,
    QueryParam',
    Required,
    type (:>),
 )

-- ═══════════════════════════════════════════════════════════════════════════
-- // file type //
-- ═══════════════════════════════════════════════════════════════════════════

{- | Type of a file system entry.

Distinguishes between regular files and directories for directory listings.
-}
data FileType
    = -- | Regular file
      FileTypeFile
    | -- | Directory
      FileTypeDirectory
    deriving (Eq, Show, Generic)

instance ToJSON FileType where
    toJSON FileTypeFile = String "file"
    toJSON FileTypeDirectory = String "directory"

instance FromJSON FileType where
    parseJSON = withText "FileType" $ \case
        "file" -> pure FileTypeFile
        "directory" -> pure FileTypeDirectory
        other -> fail $ "Invalid file type: " ++ show other

-- ═══════════════════════════════════════════════════════════════════════════
-- // file node //
-- ═══════════════════════════════════════════════════════════════════════════

{- | A file or directory entry in a directory listing.

Contains both relative and absolute paths, along with metadata
about the file type and ignore status.

==== Example JSON

@
{
  "name": "Main.hs",
  "path": "src/Main.hs",
  "absolute": "/home/user/project/src/Main.hs",
  "type": "file",
  "ignored": false
}
@
-}
data FileNode = FileNode
    { fnName :: Text
    -- ^ File or directory name (basename)
    , fnPath :: Text
    -- ^ Relative path from project root
    , fnAbsolute :: Text
    -- ^ Absolute path on the filesystem
    , fnType :: FileType
    -- ^ Whether this is a file or directory
    , fnIgnored :: Bool
    -- ^ Whether the file matches ignore patterns (e.g., .gitignore)
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

{- | Type of file content.

Determines how the file content should be interpreted and displayed.
-}
data ContentType
    = -- | Text content (UTF-8)
      ContentTypeText
    | -- | Binary content (base64 encoded)
      ContentTypeBinary
    deriving (Eq, Show, Generic)

instance ToJSON ContentType where
    toJSON ContentTypeText = String "text"
    toJSON ContentTypeBinary = String "binary"

instance FromJSON ContentType where
    parseJSON = withText "ContentType" $ \case
        "text" -> pure ContentTypeText
        "binary" -> pure ContentTypeBinary
        other -> fail $ "Invalid content type: " ++ show other

-- ═══════════════════════════════════════════════════════════════════════════
-- // file content //
-- ═══════════════════════════════════════════════════════════════════════════

{- | File content with type information.

For text files, content is UTF-8 encoded text.
For binary files, content is base64 encoded.

==== Example JSON (text file)

@
{ "type": "text", "content": "module Main where\\n..." }
@

==== Example JSON (binary file)

@
{ "type": "binary", "content": "SGVsbG8gV29ybGQh" }
@
-}
data FileContent = FileContent
    { fcType :: ContentType
    -- ^ Content encoding type
    , fcContent :: Text
    -- ^ File content (text or base64-encoded binary)
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

{- | @GET /file@ - List directory contents.

Lists files and directories at the specified path.

__Required query parameters:__

* @path@ - Relative path to list

__Optional query parameters:__

* @directory@ - Project directory (defaults to current)
-}
type FileListAPI =
    "file"
        :> QueryParam "directory" Text
        :> QueryParam' '[Required] "path" Text
        :> Get '[JSON] [FileNode]

{- | @GET /file/content@ - Read file contents.

Returns the content of a file with type information.

__Required query parameters:__

* @path@ - Relative path to the file

__Optional query parameters:__

* @directory@ - Project directory (defaults to current)
-}
type FileReadAPI =
    "file"
        :> "content"
        :> QueryParam "directory" Text
        :> QueryParam' '[Required] "path" Text
        :> Get '[JSON] FileContent

{- | @GET /file/status@ - Get file status.

Returns status information about files (e.g., git status).

__Optional query parameters:__

* @directory@ - Project directory
* @path@ - Specific file path to check
-}
type FileStatusAPI =
    "file"
        :> "status"
        :> QueryParam "directory" Text
        :> QueryParam "path" Text
        :> Get '[JSON] [Value]
