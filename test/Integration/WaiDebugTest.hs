{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Integration.WaiDebugTest (debugTest) where

import Api (api)
import Config.Dhall qualified as Dhall
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Handlers (server)
import Katip (Severity (ErrorS))
import Log qualified
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Servant (serve)
import State (initialStateNoProxyWithCache)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))

debugTest :: IO ()
debugTest = do
    cwd <- getCurrentDirectory
    let storageDir = cwd </> ".opencode-test" </> "wai-debug"
    createDirectoryIfMissing True storageDir
    dhallCache <- Dhall.newDhallCache
    logger <- Log.newLoggerWithLevel "debug" ErrorS
    state <- initialStateNoProxyWithCache dhallCache Nothing storageDir "test_project" (T.pack cwd) logger
    let app = serve api (server state)

    -- Test the health endpoint
    let req = SRequest defaultRequest{requestMethod = "GET", rawPathInfo = "/global/health"} ""
    response <- runSession (srequest req) app
    putStrLn $ "Status: " ++ show (simpleStatus response)
    putStrLn $ "Body: " ++ show (LBS.take 500 $ simpleBody response)
