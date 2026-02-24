{-# LANGUAGE OverloadedStrings #-}

module Integration.WaiDebugTest (debugTest) where

import Api (api)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Handlers (server)
import qualified Log
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Servant (serve)
import State (initialStateNoProxy)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))

debugTest :: IO ()
debugTest = do
    cwd <- getCurrentDirectory
    let storageDir = cwd </> ".opencode-test" </> "wai-debug"
    createDirectoryIfMissing True storageDir
    logger <- Log.newLogger "debug"
    state <- initialStateNoProxy storageDir "test_project" (T.pack cwd) logger
    let app = serve api (server state)

    -- Test the health endpoint
    let req = SRequest defaultRequest{requestMethod = "GET", rawPathInfo = "/global/health"} ""
    response <- runSession (srequest req) app
    putStrLn $ "Status: " ++ show (simpleStatus response)
    putStrLn $ "Body: " ++ show (LBS.take 500 $ simpleBody response)
