{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: LensDB.ConfigSpec
-- Description: Test suite for LensDB.Config module
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
module LensDB.ConfigSpec (spec) where

import Data.Word (Word16)
import LensDB.Config
import LensDB.Config (Config (..), LoggingConfig (..), PersistenceConfig (..), ServerConfig (..), StorageConfig (..))
import System.Directory (removeFile)
import System.Environment (setEnv, unsetEnv)
import System.IO (withTempFile)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Configuration" $ do
    describe "Default configuration" $ do
      it "provides sensible defaults" $ do
        config <- defaultConfig

        -- Server defaults
        serverHost (configServer config) `shouldBe` "127.0.0.1"
        serverPort (configServer config) `shouldBe` 8080
        serverMaxConnections (configServer config) `shouldBe` 1000

        -- Storage defaults
        storageMaxMemory (configStorage config) `shouldBe` "1GB"
        storageMaxKeys (configStorage config) `shouldBe` 0

        -- Logging defaults
        loggingLevel (configLogging config) `shouldBe` "info"
        loggingFile (configLogging config) `shouldBe` ""

        -- Persistence defaults
        persistenceEnabled (configPersistence config) `shouldBe` True
        persistenceDataDir (configPersistence config) `shouldBe` "./data"

    describe "Configuration validation" $ do
      it "validates correct configuration" $ do
        config <- defaultConfig
        result <- validateConfig config
        result `shouldBe` Right ()

      it "rejects invalid server port" $ do
        let config = defaultConfig {configServer = (configServer defaultConfig) {serverPort = 0}}
        result <- validateConfig config
        result `shouldBe` Left "Server port cannot be 0"

      it "rejects invalid log level" $ do
        let config = defaultConfig {configLogging = (configLogging defaultConfig) {loggingLevel = "invalid"}}
        result <- validateConfig config
        result `shouldBe` Left "Invalid logging level"

      it "rejects negative cleanup interval" $ do
        let config = defaultConfig {configStorage = (configStorage defaultConfig) {storageCleanupInterval = -1}}
        result <- validateConfig config
        result `shouldBe` Left "Storage cleanup interval must be positive"

    describe "Configuration merging" $ do
      it "merges server configurations" $ do
        let config1 = defaultConfig
        let config2 =
              defaultConfig
                { configServer =
                    (configServer defaultConfig)
                      { serverHost = "0.0.0.0",
                        serverPort = 9090
                      }
                }

        let merged = mergeConfig config1 config2

        serverHost (configServer merged) `shouldBe` "0.0.0.0"
        serverPort (configServer merged) `shouldBe` 9090
        serverMaxConnections (configServer merged) `shouldBe` 1000 -- From config1
      it "merges storage configurations" $ do
        let config1 = defaultConfig
        let config2 =
              defaultConfig
                { configStorage =
                    (configStorage defaultConfig)
                      { storageMaxMemory = "2GB",
                        storageMaxKeys = 1000
                      }
                }

        let merged = mergeConfig config1 config2

        storageMaxMemory (configStorage merged) `shouldBe` "2GB"
        storageMaxKeys (configStorage merged) `shouldBe` 1000
        storageCleanupInterval (configStorage merged) `shouldBe` 300 -- From config1
    describe "Configuration utilities" $ do
      it "gets configuration values by key" $ do
        config <- defaultConfig

        getConfigValue config "server.host" `shouldBe` Just "127.0.0.1"
        getConfigValue config "server.port" `shouldBe` Just "8080"
        getConfigValue config "storage.max_memory" `shouldBe` Just "1GB"
        getConfigValue config "nonexistent.key" `shouldBe` Nothing

      it "sets configuration values by key" $ do
        let config = defaultConfig
        let updated = setConfigValue config "server.host" "0.0.0.0"

        getConfigValue updated "server.host" `shouldBe` Just "0.0.0.0"
        getConfigValue updated "server.port" `shouldBe` Just "8080" -- Unchanged
  describe "Environment variable loading" $ do
    before_
      ( do
          setEnv "LENSDB_SERVER_HOST" "env-host"
          setEnv "LENSDB_SERVER_PORT" "9999"
          setEnv "LENSDB_STORAGE_MAX_MEMORY" "512MB"
          setEnv "LENSDB_LOGGING_LEVEL" "debug"
          setEnv "LENSDB_PERSISTENCE_ENABLED" "false"
          setEnv "LENSDB_PERSISTENCE_DATA_DIR" "/env/data"
      )
      $ after_
        ( do
            unsetEnv "LENSDB_SERVER_HOST"
            unsetEnv "LENSDB_SERVER_PORT"
            unsetEnv "LENSDB_STORAGE_MAX_MEMORY"
            unsetEnv "LENSDB_LOGGING_LEVEL"
            unsetEnv "LENSDB_PERSISTENCE_ENABLED"
            unsetEnv "LENSDB_PERSISTENCE_DATA_DIR"
        )
      $ do
        describe "Environment variable overrides" $ do
          it "loads configuration from environment variables" $ do
            config <- defaultConfig
            envConfig <- loadFromEnv config

            serverHost (configServer envConfig) `shouldBe` "env-host"
            serverPort (configServer envConfig) `shouldBe` 9999
            storageMaxMemory (configStorage envConfig) `shouldBe` "512MB"
            loggingLevel (configLogging envConfig) `shouldBe` "debug"
            persistenceEnabled (configPersistence envConfig) `shouldBe` False
            persistenceDataDir (configPersistence envConfig) `shouldBe` "/env/data"

  describe "File operations" $ do
    describe "Configuration file loading" $ do
      it "returns default config when file doesn't exist" $ do
        config <- loadFromFile "/nonexistent/config.yaml"
        case config of
          Right cfg -> cfg `shouldBe` defaultConfig
          Left err -> expectationFailure $ "Should return default config, got error: " ++ show err

      it "loads configuration from valid YAML file" $ do
        withTempFile "." "config.yaml" $ \tempPath _ -> do
          -- Write a valid YAML configuration
          writeFile tempPath $
            unlines
              [ "server:",
                "  host: \"0.0.0.0\"",
                "  port: 9090",
                "storage:",
                "  max_memory: \"2GB\"",
                "logging:",
                "  level: \"debug\"",
                "persistence:",
                "  enabled: false"
              ]

          result <- loadFromFile tempPath
          case result of
            Right config -> do
              serverHost (configServer config) `shouldBe` "0.0.0.0"
              serverPort (configServer config) `shouldBe` 9090
              storageMaxMemory (configStorage config) `shouldBe` "2GB"
              loggingLevel (configLogging config) `shouldBe` "debug"
              persistenceEnabled (configPersistence config) `shouldBe` False
            Left err -> expectationFailure $ "Failed to load config: " ++ show err

      it "handles invalid YAML gracefully" $ do
        withTempFile "." "invalid.yaml" $ \tempPath _ -> do
          -- Write invalid YAML
          writeFile tempPath "invalid: yaml: content: ["

          result <- loadFromFile tempPath
          case result of
            Left _ -> return () -- Expected to fail
            Right _ -> expectationFailure "Should have failed to parse invalid YAML"

    describe "Configuration file saving" $ do
      it "saves configuration to file" $ do
        withTempFile "." "save-config.yaml" $ \tempPath _ -> do
          let config =
                defaultConfig
                  { configServer =
                      (configServer defaultConfig)
                        { serverHost = "saved-host",
                          serverPort = 8888
                        }
                  }

          saveConfig config tempPath

          -- Verify it can be loaded back
          result <- loadFromFile tempPath
          case result of
            Right loadedConfig -> do
              serverHost (configServer loadedConfig) `shouldBe` "saved-host"
              serverPort (configServer loadedConfig) `shouldBe` 8888
            Left err -> expectationFailure $ "Failed to load saved config: " ++ show err

-- QuickCheck properties
prop_config_merge_idempotent :: Config -> Property
prop_config_merge_idempotent config = property $ do
  let merged = mergeConfig config config
  return $ merged == config

prop_config_get_set_consistency :: Config -> String -> String -> Property
prop_config_get_set_consistency config key value = property $ do
  let updated = setConfigValue config key value
      retrieved = getConfigValue updated key
  return $ retrieved == Just value
