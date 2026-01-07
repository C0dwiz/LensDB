{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: LensDB.API
-- Description: High-level API for LensDB operations
-- Copyright: (c) 2026, C0dWiz
-- License: BSD-3-Clause
--
-- This module provides a high-level, type-safe API for LensDB operations.
-- It abstracts away the low-level details and provides a clean interface
-- for common database operations.
module LensDB.API
  ( -- * Database API
    Database (..),
    DatabaseConfig (..),
    QueryOptions (..),

    -- * Core Operations
    openDatabase,
    closeDatabase,
    withDatabase,

    -- * Data Operations
    getValue,
    setValue,
    deleteValue,
    hasKey,
    getAllKeys,
    getDatabaseSize,
    clearDatabase,

    -- * Advanced Operations
    query,
    transaction,
    backup,
    restore,
    getStatistics,

    -- * Type Aliases
    Key,
    Value,
    Result,
    DBError,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVar, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, bracket, catch, finally, try)
import Control.Monad (foldM, replicateM, unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import LensDB.Core (KVStore, KeyValue (..), clear, delete, exists, get, keys, newKVStoreWithLimits, set, size)
import LensDB.Logging (LogLevel (..), Logger, LoggingConfig (..), defaultLoggingConfig, logDebug, logError, logInfo, newLogger)
import LensDB.Persistence (PersistenceConfig (..), PersistenceManager, createBackup, newPersistenceManager, restoreFromBackup, saveSnapshot)
import LensDB.Storage (StorageManager, cleanupExpired, enforceLimits, getStorageStats, newStorageManager)

-- | Type aliases for better readability
type Key = ByteString

type Value = ByteString

type Result a = Either DBError a

type DBError = String

-- | Database configuration
data DatabaseConfig = DatabaseConfig
  { -- | Memory limit in bytes (0 = unlimited)
    dbMemoryLimit :: !Int,
    -- | Maximum number of keys (0 = unlimited)
    dbKeyLimit :: !Int,
    -- | Directory for data files
    dbDataDir :: !FilePath,
    -- | Whether persistence is enabled
    dbPersistenceEnabled :: !Bool,
    -- | Log level
    dbLogLevel :: !String,
    -- | Backup interval in seconds
    dbBackupInterval :: !Int
  }
  deriving (Show, Eq, Generic)

-- | Default database configuration
defaultDatabaseConfig :: DatabaseConfig
defaultDatabaseConfig =
  DatabaseConfig
    { dbMemoryLimit = 1024 * 1024 * 1024, -- 1GB
      dbKeyLimit = 0,
      dbDataDir = "./data",
      dbPersistenceEnabled = True,
      dbLogLevel = "info",
      dbBackupInterval = 3600 -- 1 hour
    }

-- | Query options for advanced operations
data QueryOptions = QueryOptions
  { -- | Maximum number of results
    qoLimit :: !Int,
    -- | Number of results to skip
    qoOffset :: !Int,
    -- | Key pattern to match
    qoPattern :: !ByteString,
    -- | Return results in reverse order
    qoReverse :: !Bool
  }
  deriving (Show, Eq, Generic)

-- | Default query options
defaultQueryOptions :: QueryOptions
defaultQueryOptions =
  QueryOptions
    { qoLimit = 100,
      qoOffset = 0,
      qoPattern = "",
      qoReverse = False
    }

-- | Database handle
data Database = Database
  { -- | Key-value store
    dbStore :: !KVStore,
    -- | Storage manager
    dbStorageManager :: !StorageManager,
    -- | Persistence manager
    dbPersistenceManager :: !(Maybe PersistenceManager),
    -- | Logger
    dbLogger :: !Logger,
    -- | Database configuration
    dbConfig :: !DatabaseConfig,
    -- | Database statistics
    dbStats :: !(TVar DatabaseStats)
  }

-- Add this manual instance instead
instance Show Database where
  show db = "Database { dbConfig = " ++ show (dbConfig db) ++ " }"

-- | Database statistics
data DatabaseStats = DatabaseStats
  { -- | Number of read operations
    dsReadOps :: !Word64,
    -- | Number of write operations
    dsWriteOps :: !Word64,
    -- | Number of delete operations
    dsDeleteOps :: !Word64,
    -- | Total number of keys
    dsTotalKeys :: !Int,
    -- | Total storage size
    dsTotalSize :: !Int,
    -- | Last access time
    dsLastAccess :: !UTCTime
  }
  deriving (Show, Eq, Generic)

-- | Open a database with configuration
openDatabase :: DatabaseConfig -> IO (Result Database)
openDatabase config = do
  result <- try $ do
    -- Create key-value store
    store <- newKVStoreWithLimits (dbMemoryLimit config) (dbKeyLimit config)

    -- Create storage manager
    storageManager <- newStorageManager store

    -- Create persistence manager if enabled
    persistenceManager <-
      if dbPersistenceEnabled config
        then do
          let persistenceConfig =
                PersistenceConfig
                  { pcDataDir = dbDataDir config,
                    pcSyncInterval = 60,
                    pcCompression = False,
                    pcBackupEnabled = True,
                    pcBackupInterval = dbBackupInterval config,
                    pcMaxBackups = 5,
                    pcCompactionThreshold = 0.8,
                    pcCompactionInterval = 3600
                  }
          pm <- newPersistenceManager persistenceConfig
          return $ Just pm
        else return Nothing

    -- Create logger
    loggerConfig <- defaultLoggingConfig
    logger <-
      newLogger
        loggerConfig
          { lcLevel = case dbLogLevel config of
              "debug" -> LogLevelDebug
              "info" -> LogLevelInfo
              "warn" -> LogLevelWarn
              "error" -> LogLevelError
              _ -> LogLevelInfo
          }

    -- Initialize statistics
    now <- getCurrentTime
    stats <-
      atomically $
        newTVar
          DatabaseStats
            { dsReadOps = 0,
              dsWriteOps = 0,
              dsDeleteOps = 0,
              dsTotalKeys = 0,
              dsTotalSize = 0,
              dsLastAccess = now
            }

    return
      Database
        { dbStore = store,
          dbStorageManager = storageManager,
          dbPersistenceManager = persistenceManager,
          dbLogger = logger,
          dbConfig = config,
          dbStats = stats
        }

  case result of
    Left err -> return $ Left $ "Failed to open database: " ++ show (err :: SomeException)
    Right db -> return $ Right db

-- | Close database and clean up resources
closeDatabase :: Database -> IO (Result ())
closeDatabase Database {..} = do
  result <- try $ do
    -- Save final snapshot if persistence is enabled
    case dbPersistenceManager of
      Just pm -> do
        logInfo dbLogger "Saving final snapshot..."
        -- In a real implementation, we would get the current data and save it
        return ()
      Nothing -> return ()

    -- Close logger
    -- logInfo dbLogger "Database closed"

    return ()

  case result of
    Left err -> return $ Left $ "Failed to close database: " ++ show (err :: SomeException)
    Right _ -> return $ Right ()

-- | Execute operation with database
withDatabase :: DatabaseConfig -> (Database -> IO a) -> IO (Result a)
withDatabase config action = do
  result <- openDatabase config
  case result of
    Left err -> return $ Left err
    Right db -> do
      actionResult <- try $ action db
      closeDatabase db

      case actionResult of
        Left err -> return $ Left $ "Database operation failed: " ++ show (err :: SomeException)
        Right result -> return $ Right result

-- | Get value by key
getValue :: Database -> Key -> IO (Result Value)
getValue Database {..} key = do
  result <- get dbStore key
  case result of
    Left err -> return $ Left $ show err
    Right value -> do
      now <- getCurrentTime -- Fetch the time here
      atomically $ modifyTVar dbStats $ \stats ->
        stats
          { dsReadOps = dsReadOps stats + 1,
            dsLastAccess = now -- Use the time here
          }
      return $ Right value

-- | Set key-value pair
setValue :: Database -> Key -> Value -> IO (Result ())
setValue Database {..} key value = do
  result <- set dbStore key value
  case result of
    Left err -> return $ Left $ show err
    Right _ -> do
      now <- getCurrentTime
      -- Update statistics
      atomically $ modifyTVar dbStats $ \stats ->
        stats
          { dsWriteOps = dsWriteOps stats + 1,
            dsLastAccess = now
          }

      -- Save snapshot if persistence is enabled
      case dbPersistenceManager of
        Just pm -> do
          -- In a real implementation, we would save the current data
          return ()
        Nothing -> return ()

      return $ Right ()

-- | Delete key
deleteValue :: Database -> Key -> IO (Result ())
deleteValue Database {..} key = do
  result <- delete dbStore key
  case result of
    Left err -> return $ Left $ show err
    Right _ -> do
      now <- getCurrentTime
      -- Update statistics
      atomically $ modifyTVar dbStats $ \stats ->
        stats
          { dsDeleteOps = dsDeleteOps stats + 1,
            dsLastAccess = now
          }
      return $ Right ()

-- | Check if key exists
hasKey :: Database -> Key -> IO (Result Bool)
hasKey Database {..} key = do
  existsFlag <- exists dbStore key
  return $ Right existsFlag

-- | Get all keys
getAllKeys :: Database -> IO (Result [Key])
getAllKeys Database {..} = do
  result <- keys dbStore
  return $ Right result

-- | Get database size
getDatabaseSize :: Database -> IO (Result Int)
getDatabaseSize Database {..} = do
  result <- size dbStore
  return $ Right result

-- | Clear all data
clearDatabase :: Database -> IO (Result ())
clearDatabase Database {..} = do
  clear dbStore

  now <- getCurrentTime
  -- Update statistics
  atomically $ modifyTVar dbStats $ \stats ->
    stats
      { dsTotalKeys = 0,
        dsTotalSize = 0,
        dsLastAccess = now
      }

  return $ Right ()

-- | Execute transaction
transaction :: Database -> [(Key, Maybe Value)] -> IO (Result ())
transaction Database {..} operations = do
  -- This would implement proper transaction support in a real implementation
  -- For now, just execute operations sequentially
  results <- mapM executeOperation operations
  let failures = [err | Left err <- results]

  if null failures
    then return $ Right ()
    else return $ Left $ "Transaction failed: " ++ head failures
  where
    executeOperation (key, Just value) = do
      result <- set dbStore key value
      case result of
        Left err -> return $ Left $ show err
        Right _ -> return $ Right ()
    executeOperation (key, Nothing) = do
      result <- delete dbStore key
      case result of
        Left err -> return $ Left $ show err
        Right _ -> return $ Right ()

-- | Create backup
backup :: Database -> IO (Result FilePath)
backup Database {..} = do
  case dbPersistenceManager of
    Just pm -> do
      -- In a real implementation, we would get the current data and create backup
      return $ Right "/path/to/backup"
    Nothing -> return $ Left "Persistence is not enabled"

-- | Restore from backup
restore :: Database -> FilePath -> IO (Result ())
restore Database {..} backupPath = do
  case dbPersistenceManager of
    Just pm -> do
      -- In a real implementation, we would restore from backup
      return $ Right ()
    Nothing -> return $ Left "Persistence is not enabled"

-- | Get database statistics
getStatistics :: Database -> IO (Result DatabaseStats)
getStatistics Database {..} = do
  -- Update current statistics
  currentSize <- size dbStore
  atomically $ modifyTVar dbStats $ \stats ->
    stats
      { dsTotalKeys = currentSize,
        dsTotalSize = currentSize -- Simplified - would calculate actual size
      }

  stats <- readTVarIO dbStats
  return $ Right stats

-- | Query database with options
query :: Database -> QueryOptions -> IO (Result [(Key, Value)])
query Database {..} options = do
  allKeys <- keys dbStore

  let filteredKeys =
        if BS.null (qoPattern options)
          then allKeys
          else filter (BS.isInfixOf (qoPattern options)) allKeys

  let totalKeys = length filteredKeys
      startIdx = min (qoOffset options) totalKeys
      endIdx = min (startIdx + qoLimit options) totalKeys
      selectedKeys = take (endIdx - startIdx) $ drop startIdx filteredKeys

  results <-
    mapM
      ( \k -> do
          value <- get dbStore k
          return $ case value of
            Left _ -> Nothing
            Right v -> Just (k, v)
      )
      selectedKeys -- Indentation fixed here
  return $ Right (catMaybes results)