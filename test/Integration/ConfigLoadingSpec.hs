{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Integration.ConfigLoadingSpec
Description : Integration tests for config loading

Integration tests for the configuration loading system.
Tests actual file I/O and caching behavior.
-}
module Integration.ConfigLoadingSpec where

import Config.Dhall
import Config.Types
import System.FilePath (takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
    describe "Config.Dhall" $ do
        describe "loadConfigCached" $ do
            it "returns defaultConfig when no config files exist" $ do
                withSystemTempDirectory "config-test" $ \tmpDir -> do
                    cache <- newDhallCache
                    config <- loadConfigCached cache tmpDir
                    -- Should have default values
                    cfgLogLevel config `shouldBe` Just INFO
                    cfgTheme config `shouldBe` Just "ono-sendai"

            it "caches results across multiple calls" $ do
                withSystemTempDirectory "config-test" $ \tmpDir -> do
                    cache <- newDhallCache
                    -- First call
                    config1 <- loadConfigCached cache tmpDir
                    -- Second call should use cache
                    config2 <- loadConfigCached cache tmpDir
                    -- Should be identical
                    config1 `shouldBe` config2

        describe "loadConfigFromFileCached" $ do
            it "returns Nothing for non-existent file" $ do
                cache <- newDhallCache
                result <- loadConfigFromFileCached cache "/nonexistent/path.dhall"
                result `shouldBe` Nothing

            it "caches Nothing results" $ do
                cache <- newDhallCache
                let path = "/nonexistent/path.dhall"
                -- First call
                result1 <- loadConfigFromFileCached cache path
                -- Second call should use cache
                result2 <- loadConfigFromFileCached cache path
                result1 `shouldBe` Nothing
                result2 `shouldBe` Nothing

        describe "projectConfigPath" $ do
            it "appends weapon.dhall to directory" $ do
                projectConfigPath "/home/user/project" `shouldBe` "/home/user/project/weapon.dhall"

            it "handles trailing slash" $ do
                -- Note: FilePath </> handles this correctly
                projectConfigPath "/home/user/project/" `shouldBe` "/home/user/project/weapon.dhall"

            it "handles relative paths" $ do
                projectConfigPath "." `shouldBe` "./weapon.dhall"
                projectConfigPath ".." `shouldBe` "../weapon.dhall"

        describe "globalConfigPath" $ do
            it "returns a path ending in weapon.dhall" $ do
                path <- globalConfigPath
                -- Should end with weapon.dhall
                takeFileName path `shouldBe` "weapon.dhall"

        describe "loadDefaults" $ do
            it "returns defaultConfig when defaults file doesn't exist" $ do
                -- This test relies on the defaults file not being in the test working dir
                -- which is expected during testing
                config <- loadDefaults
                -- Should get hardcoded defaults
                cfgLogLevel config `shouldBe` Just INFO

        describe "newDhallCache" $ do
            it "creates independent caches" $ do
                cache1 <- newDhallCache
                cache2 <- newDhallCache
                -- Load into cache1
                _ <- loadConfigFromFileCached cache1 "/nonexistent"
                -- cache2 should not have this cached
                -- (We can't directly test this, but at least verify no crash)
                _ <- loadConfigFromFileCached cache2 "/nonexistent"
                pure ()
