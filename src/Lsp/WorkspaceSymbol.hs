{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Lsp.WorkspaceSymbol (
    workspaceSymbols,

    -- * Pure helpers (for testing)
    decodeInitOptions,
    parseRootUri,
    extractSymbols,
    filterSymbolInfos,
    filterWorkspaceSymbols,
    workspaceToSymbolInfo,
    normalizeLocation,
    zeroRange,
) where

import Config.Config qualified as Config
import Config.Dhall qualified as Dhall
import Config.Types (Config (..), LSPConfig (..), LSPEntry (..))
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value, toJSON)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Language.LSP.Client (runSessionWithHandles)
import Language.LSP.Client.Session (MonadSession, getResponseResult, request, sendNotification)
import Language.LSP.Protocol.Capabilities (fullLatestClientCaps)
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Protocol.Types (Null (Null), type (|?) (..))
import Language.LSP.Protocol.Types qualified as LSP
import System.IO (BufferMode (NoBuffering), Handle, hClose, hSetBuffering)
import System.Posix.Process (getProcessID)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, terminateProcess, waitForProcess)

workspaceSymbols :: Dhall.DhallCache -> FilePath -> Text -> IO [Value]
workspaceSymbols dhallCache root query = do
    config <- Config.load dhallCache root
    case cfgLsp config of
        Nothing -> pure []
        Just LSPDisabled -> pure []
        Just (LSPEnabled servers) -> do
            results <- forM (Map.elems servers) $ \entry ->
                workspaceSymbolsForEntry root entry query
            pure $ take 10 (concat results)

workspaceSymbolsForEntry :: FilePath -> LSPEntry -> Text -> IO [Value]
workspaceSymbolsForEntry root entry query = do
    let cmdParts = lspCommand entry <> fromMaybe [] (lspArgs entry)
    case cmdParts of
        [] -> pure []
        (cmd : args) -> do
            let initOptions = decodeInitOptions (lspInitializationOptions entry)
            let rootUri = parseRootUri (lspRootUri entry) root
            result <-
                try $
                    withLspProcess root (T.unpack cmd) (map T.unpack args) $ \serverOut serverIn ->
                        runSessionWithHandles serverOut serverIn $ do
                            _ <- initializeWithRoot root initOptions rootUri
                            response <- request SMethod_WorkspaceSymbol (LSP.WorkspaceSymbolParams Nothing Nothing query)
                            let symbols = extractSymbols (getResponseResult response)
                            _ <- request SMethod_Shutdown Nothing
                            sendNotification SMethod_Exit (Nothing :: Maybe Void)
                            pure symbols
            case result of
                Left (_ :: SomeException) -> pure []
                Right symbols -> pure symbols

decodeInitOptions :: Maybe Text -> Maybe Value
decodeInitOptions = (Aeson.decodeStrict' . TE.encodeUtf8 =<<)

parseRootUri :: Maybe Text -> FilePath -> LSP.Uri |? Null
parseRootUri mRoot root =
    case mRoot of
        Just t -> InL (LSP.Uri t)
        Nothing -> InL (LSP.filePathToUri root)

initializeWithRoot :: (MonadSession m, MonadIO m) => FilePath -> Maybe Value -> (LSP.Uri |? Null) -> m LSP.InitializeResult
initializeWithRoot root initOptions rootUri = do
    pid <- liftIO getProcessID
    response <-
        request
            SMethod_Initialize
            LSP.InitializeParams
                { _workDoneToken = Nothing
                , _processId = InL (fromIntegral pid)
                , _clientInfo = Just (LSP.ClientInfo "weapon-server" (Just "0.1.0"))
                , _locale = Nothing
                , _rootPath = Just (InL (T.pack root))
                , _rootUri = rootUri
                , _capabilities = fullLatestClientCaps
                , _initializationOptions = initOptions
                , _trace = Just LSP.TraceValue_Off
                , _workspaceFolders = Nothing
                }
    sendNotification SMethod_Initialized LSP.InitializedParams
    pure $ getResponseResult response

extractSymbols ::
    ([LSP.SymbolInformation] |? ([LSP.WorkspaceSymbol] |? Null)) ->
    [Value]
extractSymbols result =
    case result of
        InL infos -> map toJSON (filterSymbolInfos infos)
        InR (InL ws) -> map (toJSON . workspaceToSymbolInfo) (filterWorkspaceSymbols ws)
        InR (InR Null) -> []

filterSymbolInfos :: [LSP.SymbolInformation] -> [LSP.SymbolInformation]
filterSymbolInfos =
    filter (\(LSP.SymbolInformation{_kind = kind}) -> kind `elem` allowedKinds)

filterWorkspaceSymbols :: [LSP.WorkspaceSymbol] -> [LSP.WorkspaceSymbol]
filterWorkspaceSymbols =
    filter (\(LSP.WorkspaceSymbol{_kind = kind}) -> kind `elem` allowedKinds)

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

workspaceToSymbolInfo :: LSP.WorkspaceSymbol -> LSP.SymbolInformation
workspaceToSymbolInfo LSP.WorkspaceSymbol{_name = name, _kind = kind, _tags = tags, _containerName = container, _location = loc} =
    LSP.SymbolInformation
        { _name = name
        , _kind = kind
        , _tags = tags
        , _containerName = container
        , _deprecated = Nothing
        , _location = normalizeLocation loc
        }

normalizeLocation :: (LSP.Location |? LSP.LocationUriOnly) -> LSP.Location
normalizeLocation loc =
    case loc of
        InL fullLoc -> fullLoc
        InR LSP.LocationUriOnly{_uri = uri} ->
            LSP.Location
                { _uri = uri
                , _range = zeroRange
                }

zeroRange :: LSP.Range
zeroRange =
    LSP.Range
        { _start = LSP.Position 0 0
        , _end = LSP.Position 0 0
        }

withLspProcess :: FilePath -> FilePath -> [FilePath] -> (Handle -> Handle -> IO a) -> IO a
withLspProcess root cmd args action =
    bracket
        (createProcess (proc cmd args){cwd = Just root, std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit})
        cleanup
        $ \(mIn, mOut, _mErr, _ph) -> do
            let serverIn = fromMaybe (error "LSP stdin missing") mIn
            let serverOut = fromMaybe (error "LSP stdout missing") mOut
            hSetBuffering serverIn NoBuffering
            hSetBuffering serverOut NoBuffering
            action serverOut serverIn
  where
    cleanup (mIn, mOut, _mErr, ph) = do
        forM_ [mIn, mOut] $ \mh -> maybe (pure ()) hClose mh
        terminateProcess ph
        _ <- waitForProcess ph
        pure ()
