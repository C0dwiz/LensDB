{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: LensDB.Config
-- Description: Configuration management for LensDB
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
--
-- This module provides configuration management functionality including
-- loading from YAML files, environment variables, and command-line arguments.
module LensDB.Config
  ( -- * Configuration Types
    Config (..),
    ServerConfig (..),
    StorageConfig (..),
    LoggingConfig (..),
    PersistenceConfig (..),

    -- * Configuration Loading
    loadConfig,
    defaultConfig,
    saveConfig,
    validateConfig,

    -- * Configuration Sources
    loadFromFile,
    loadFromEnv,
    loadFromArgs,

    -- * Configuration Utilities
    mergeConfig,
    getConfigValue,
    setConfigValue,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Word (Word16)
import Data.Yaml (ParseException, decodeFileEither, encodeFile)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getArgs, getEnvironment)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | Main configuration structure
data Config = Config
  { -- | Server configuration
    configServer :: !ServerConfig,
    -- | Storage configuration
    configStorage :: !StorageConfig,
    -- | Logging configuration
    configLogging :: !LoggingConfig,
    -- | Persistence configuration
    configPersistence :: !PersistenceConfig
  }
  deriving (Show, Eq, Generic)

instance FromJSON Config

instance ToJSON Config

-- | Server configuration
data ServerConfig = ServerConfig
  { -- | Server host address
    serverHost :: !String,
    -- | Server port
    serverPort :: !Word16,
    -- | Maximum number of concurrent connections
    serverMaxConnections :: !Int,
    -- | Connection timeout in seconds
    serverConnectionTimeout :: !Int,
    -- | Buffer size for network operations
    serverBufferSize :: !Int,
    -- | Number of worker threads
    serverWorkers :: !Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ServerConfig

instance ToJSON ServerConfig

-- | Storage configuration
data StorageConfig = StorageConfig
  { -- | Maximum memory usage (e.g., "1GB", "512MB")
    storageMaxMemory :: !String,
    -- | Maximum number of keys (0 = unlimited)
    storageMaxKeys :: !Int,
    -- | Cleanup interval in seconds
    storageCleanupInterval :: !Int,
    -- | Compaction threshold (0.0-1.0)
    storageCompactionThreshold :: !Double,
    -- | Eviction policy ("LRU", "LFU", "FIFO")
    storageEvictionPolicy :: !String
  }
  deriving (Show, Eq, Generic)

instance FromJSON StorageConfig

instance ToJSON StorageConfig

-- | Logging configuration
data LoggingConfig = LoggingConfig
  { -- | Logging level ("debug", "info", "warn", "error")
    loggingLevel :: !String,
    -- | Log file path (empty = stdout)
    loggingFile :: !String,
    -- | Maximum log file size
    loggingMaxSize :: !String,
    -- | Number of backup log files
    loggingBackupCount :: !Int,
    -- | Log format ("json", "text")
    loggingFormat :: !String
  }
  deriving (Show, Eq, Generic)

instance FromJSON LoggingConfig

instance ToJSON LoggingConfig

-- | Persistence configuration
data PersistenceConfig = PersistenceConfig
  { -- | Whether persistence is enabled
    persistenceEnabled :: !Bool,
    -- | Directory for data files
    persistenceDataDir :: !String,
    -- | Sync interval in seconds
    persistenceSyncInterval :: !Int,
    -- | Whether to compress data files
    persistenceCompression :: !Bool,
    -- | Whether to enable backups
    persistenceBackupEnabled :: !Bool,
    -- | Backup interval in seconds
    persistenceBackupInterval :: !Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON PersistenceConfig

instance ToJSON PersistenceConfig

-- | Default configuration
defaultConfig :: Config
defaultConfig =
  Config
    { configServer = defaultServerConfig,
      configStorage = defaultStorageConfig,
      configLogging = defaultLoggingConfig,
      configPersistence = defaultPersistenceConfig
    }

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { serverHost = "127.0.0.1",
      serverPort = 8080,
      serverMaxConnections = 1000,
      serverConnectionTimeout = 300,
      serverBufferSize = 4096,
      serverWorkers = 4
    }

-- | Default storage configuration
defaultStorageConfig :: StorageConfig
defaultStorageConfig =
  StorageConfig
    { storageMaxMemory = "1GB",
      storageMaxKeys = 0,
      storageCleanupInterval = 300,
      storageCompactionThreshold = 0.8,
      storageEvictionPolicy = "LRU"
    }

-- | Default logging configuration
defaultLoggingConfig :: LoggingConfig
defaultLoggingConfig =
  LoggingConfig
    { loggingLevel = "info",
      loggingFile = "",
      loggingMaxSize = "100MB",
      loggingBackupCount = 5,
      loggingFormat = "text"
    }

-- | Default persistence configuration
defaultPersistenceConfig :: PersistenceConfig
defaultPersistenceConfig =
  PersistenceConfig
    { persistenceEnabled = True,
      persistenceDataDir = "./data",
      persistenceSyncInterval = 60,
      persistenceCompression = False,
      persistenceBackupEnabled = True,
      persistenceBackupInterval = 3600
    }

-- | Load configuration from file
loadFromFile :: FilePath -> IO (Either ParseException Config)
loadFromFile filePath = do
  exists <- doesFileExist filePath
  if exists
    then decodeFileEither filePath
    else return $ Right defaultConfig

-- | Load configuration from environment variables
loadFromEnv :: Config -> IO Config
loadFromEnv config = do
  env <- getEnvironment

  -- Update server config from environment
  let serverConfig' = configServer config
      serverHost' = maybe (serverHost serverConfig') id $ lookup "LENSDB_SERVER_HOST" env
      serverPort' = maybe (serverPort serverConfig') readPort $ lookup "LENSDB_SERVER_PORT" env
      serverMaxConnections' = maybe (serverMaxConnections serverConfig') readInt $ lookup "LENSDB_SERVER_MAX_CONNECTIONS" env
      serverConfig'' =
        serverConfig'
          { serverHost = serverHost',
            serverPort = serverPort',
            serverMaxConnections = serverMaxConnections'
          }

  -- Update storage config from environment
  let storageConfig' = configStorage config
      storageMaxMemory' = maybe (storageMaxMemory storageConfig') id $ lookup "LENSDB_STORAGE_MAX_MEMORY" env
      storageConfig'' =
        storageConfig'
          { storageMaxMemory = storageMaxMemory'
          }

  -- Update logging config from environment
  let loggingConfig' = configLogging config
      loggingLevel' = maybe (loggingLevel loggingConfig') id $ lookup "LENSDB_LOGGING_LEVEL" env
      loggingFile' = maybe (loggingFile loggingConfig') id $ lookup "LENSDB_LOGGING_FILE" env
      loggingConfig'' =
        loggingConfig'
          { loggingLevel = loggingLevel',
            loggingFile = loggingFile'
          }

  -- Update persistence config from environment
  let persistenceConfig' = configPersistence config
      persistenceEnabled' = maybe (persistenceEnabled persistenceConfig') readBool $ lookup "LENSDB_PERSISTENCE_ENABLED" env
      persistenceDataDir' = maybe (persistenceDataDir persistenceConfig') id $ lookup "LENSDB_PERSISTENCE_DATA_DIR" env
      persistenceConfig'' =
        persistenceConfig'
          { persistenceEnabled = persistenceEnabled',
            persistenceDataDir = persistenceDataDir'
          }

  return
    config
      { configServer = serverConfig'',
        configStorage = storageConfig'',
        configLogging = loggingConfig'',
        configPersistence = persistenceConfig''
      }
  where
    readPort :: String -> Word16
    readPort s = case readMaybe s of
      Just p -> p
      Nothing -> 8080

    readInt :: String -> Int
    readInt s = case readMaybe s of
      Just i -> i
      Nothing -> 0

    readBool :: String -> Bool
    readBool s = case map toLowerChar s of
      "true" -> True
      "1" -> True
      "yes" -> True
      _ -> False

    toLowerChar :: Char -> Char
    toLowerChar c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- | Load configuration from command-line arguments
loadFromArgs :: Config -> IO Config
loadFromArgs config = do
  args <- getArgs
  return $ parseArgs config args
  where
    parseArgs :: Config -> [String] -> Config
    parseArgs cfg [] = cfg
    parseArgs cfg ("--host" : host : rest) =
      parseArgs (cfg {configServer = (configServer cfg) {serverHost = host}}) rest
    parseArgs cfg ("--port" : port : rest) =
      case readMaybe port of
        Just p -> parseArgs (cfg {configServer = (configServer cfg) {serverPort = p}}) rest
        Nothing -> parseArgs cfg rest
    parseArgs cfg ("--max-connections" : maxConn : rest) =
      case readMaybe maxConn of
        Just mc -> parseArgs (cfg {configServer = (configServer cfg) {serverMaxConnections = mc}}) rest
        Nothing -> parseArgs cfg rest
    parseArgs cfg ("--data-dir" : dataDir : rest) =
      parseArgs (cfg {configPersistence = (configPersistence cfg) {persistenceDataDir = dataDir}}) rest
    parseArgs cfg ("--log-level" : logLevel : rest) =
      parseArgs (cfg {configLogging = (configLogging cfg) {loggingLevel = logLevel}}) rest
    parseArgs cfg ("--config" : configFile : rest) =
      cfg -- Would load from file in real implementation
    parseArgs cfg (_ : rest) = parseArgs cfg rest

-- | Load configuration from all sources
loadConfig :: Maybe FilePath -> IO Config
loadConfig maybeConfigFile = do
  -- Start with default config
  config <- return defaultConfig

  -- Load from file if specified
  configFromFile <- case maybeConfigFile of
    Just filePath -> loadFromFile filePath
    Nothing -> loadFromFile "lensdb.yaml"

  config <- case configFromFile of
    Left err -> do
      putStrLn $ "Warning: Failed to load config file: " ++ show err
      return config
    Right cfg -> return cfg

  -- Override with environment variables
  config <- loadFromEnv config

  -- Override with command-line arguments
  config <- loadFromArgs config

  -- Validate configuration
  case validateConfig config of
    Left err -> error $ "Invalid configuration: " ++ err
    Right _ -> return config

-- | Save configuration to file
saveConfig :: Config -> FilePath -> IO ()
saveConfig config filePath = Data.Aeson.encodeFile filePath config

-- | Validate configuration
validateConfig :: Config -> Either String ()
validateConfig Config {..} = do
  -- Validate server config
  when (serverPort configServer == 0) $
    Left "Server port cannot be 0"
  when (serverMaxConnections configServer <= 0) $
    Left "Server max connections must be positive"
  when (serverConnectionTimeout configServer <= 0) $
    Left "Server connection timeout must be positive"

  -- Validate storage config
  when (storageCleanupInterval configStorage <= 0) $
    Left "Storage cleanup interval must be positive"
  when (storageCompactionThreshold configStorage < 0.0 || storageCompactionThreshold configStorage > 1.0) $
    Left "Storage compaction threshold must be between 0.0 and 1.0"

  -- Validate logging config
  when (loggingLevel configLogging `notElem` ["debug", "info", "warn", "error"]) $
    Left "Invalid logging level"

  -- Validate persistence config
  when (persistenceSyncInterval configPersistence <= 0) $
    Left "Persistence sync interval must be positive"
  when (persistenceBackupInterval configPersistence <= 0) $
    Left "Persistence backup interval must be positive"

  return ()
  where
    when True _ = Right ()
    when False _ = Left ""

-- | Merge two configurations (second overrides first)
mergeConfig :: Config -> Config -> Config
mergeConfig cfg1 cfg2 =
  Config
    { configServer = mergeServerConfig (configServer cfg1) (configServer cfg2),
      configStorage = mergeStorageConfig (configStorage cfg1) (configStorage cfg2),
      configLogging = mergeLoggingConfig (configLogging cfg1) (configLogging cfg2),
      configPersistence = mergePersistenceConfig (configPersistence cfg1) (configPersistence cfg2)
    }

mergeServerConfig :: ServerConfig -> ServerConfig -> ServerConfig
mergeServerConfig cfg1 cfg2 =
  ServerConfig
    { serverHost = if serverHost cfg2 /= "" then serverHost cfg2 else serverHost cfg1,
      serverPort = if serverPort cfg2 /= 0 then serverPort cfg2 else serverPort cfg1,
      serverMaxConnections = if serverMaxConnections cfg2 /= 0 then serverMaxConnections cfg2 else serverMaxConnections cfg1,
      serverConnectionTimeout = if serverConnectionTimeout cfg2 /= 0 then serverConnectionTimeout cfg2 else serverConnectionTimeout cfg1,
      serverBufferSize = if serverBufferSize cfg2 /= 0 then serverBufferSize cfg2 else serverBufferSize cfg1,
      serverWorkers = if serverWorkers cfg2 /= 0 then serverWorkers cfg2 else serverWorkers cfg1
    }

mergeStorageConfig :: StorageConfig -> StorageConfig -> StorageConfig
mergeStorageConfig cfg1 cfg2 =
  StorageConfig
    { storageMaxMemory = if storageMaxMemory cfg2 /= "" then storageMaxMemory cfg2 else storageMaxMemory cfg1,
      storageMaxKeys = if storageMaxKeys cfg2 /= 0 then storageMaxKeys cfg2 else storageMaxKeys cfg1,
      storageCleanupInterval = if storageCleanupInterval cfg2 /= 0 then storageCleanupInterval cfg2 else storageCleanupInterval cfg1,
      storageCompactionThreshold = if storageCompactionThreshold cfg2 /= 0.0 then storageCompactionThreshold cfg2 else storageCompactionThreshold cfg1,
      storageEvictionPolicy = if storageEvictionPolicy cfg2 /= "" then storageEvictionPolicy cfg2 else storageEvictionPolicy cfg1
    }

mergeLoggingConfig :: LoggingConfig -> LoggingConfig -> LoggingConfig
mergeLoggingConfig cfg1 cfg2 =
  LoggingConfig
    { loggingLevel = if loggingLevel cfg2 /= "" then loggingLevel cfg2 else loggingLevel cfg1,
      loggingFile = if loggingFile cfg2 /= "" then loggingFile cfg2 else loggingFile cfg1,
      loggingMaxSize = if loggingMaxSize cfg2 /= "" then loggingMaxSize cfg2 else loggingMaxSize cfg1,
      loggingBackupCount = if loggingBackupCount cfg2 /= 0 then loggingBackupCount cfg2 else loggingBackupCount cfg1,
      loggingFormat = if loggingFormat cfg2 /= "" then loggingFormat cfg2 else loggingFormat cfg1
    }

mergePersistenceConfig :: PersistenceConfig -> PersistenceConfig -> PersistenceConfig
mergePersistenceConfig cfg1 cfg2 =
  PersistenceConfig
    { persistenceEnabled = persistenceEnabled cfg2, -- Boolean, just override
      persistenceDataDir = if persistenceDataDir cfg2 /= "" then persistenceDataDir cfg2 else persistenceDataDir cfg1,
      persistenceSyncInterval = if persistenceSyncInterval cfg2 /= 0 then persistenceSyncInterval cfg2 else persistenceSyncInterval cfg1,
      persistenceCompression = persistenceCompression cfg2, -- Boolean, just override
      persistenceBackupEnabled = persistenceBackupEnabled cfg2, -- Boolean, just override
      persistenceBackupInterval = if persistenceBackupInterval cfg2 /= 0 then persistenceBackupInterval cfg2 else persistenceBackupInterval cfg1
    }

-- | Get a configuration value by key (simplified implementation)
getConfigValue :: Config -> String -> Maybe String
getConfigValue Config {..} key = case key of
  "server.host" -> Just $ serverHost configServer
  "server.port" -> Just $ show $ serverPort configServer
  "server.max_connections" -> Just $ show $ serverMaxConnections configServer
  "storage.max_memory" -> Just $ storageMaxMemory configStorage
  "storage.max_keys" -> Just $ show $ storageMaxKeys configStorage
  "logging.level" -> Just $ loggingLevel configLogging
  "logging.file" -> Just $ loggingFile configLogging
  "persistence.enabled" -> Just $ show $ persistenceEnabled configPersistence
  "persistence.data_dir" -> Just $ persistenceDataDir configPersistence
  _ -> Nothing

-- | Set a configuration value by key (simplified implementation)
setConfigValue :: Config -> String -> String -> Config
setConfigValue config key value = case key of
  "server.host" -> config {configServer = (configServer config) {serverHost = value}}
  "server.port" -> case readMaybe value of
    Just p -> config {configServer = (configServer config) {serverPort = p}}
    Nothing -> config
  "storage.max_memory" -> config {configStorage = (configStorage config) {storageMaxMemory = value}}
  "logging.level" -> config {configLogging = (configLogging config) {loggingLevel = value}}
  "persistence.data_dir" -> config {configPersistence = (configPersistence config) {persistenceDataDir = value}}
  _ -> config
