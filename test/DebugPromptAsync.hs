{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Message (CreateMessageInput (..))
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
    let json = "{\"parts\":[]}"
    putStrLn $ "Parsing: " ++ LBS.unpack json
    case eitherDecode json :: Either String CreateMessageInput of
        Left err -> putStrLn $ "FAILED: " ++ err
        Right cmi -> putStrLn $ "SUCCESS: " ++ show cmi
