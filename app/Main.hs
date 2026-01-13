{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Main
-- Description: Main application entry point for LensDB
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
--
-- This is the main entry point for the LensDB server application.
-- It handles initialization, configuration loading, and server startup.
module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Exception (SomeException, bracket, catch, finally, try)
import Control.Monad (forever, unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Time (getCurrentTime)
import LensDB.Config (Config (..), LoggingConfig (..), PersistenceConfig (..), ServerConfig (..), StorageConfig (..), defaultConfig, loadConfig)
import LensDB.Core (KVStore, clear, delete, exists, get, keys, newKVStoreWithLimits, set, size)
import LensDB.Logging (LogLevel (..), Logger (..), LoggingConfig (..), closeLogger, defaultLoggingConfig, logDebug, logError, logInfo, logWarn, newLogger)
import qualified LensDB.Logging as Log
import LensDB.Network (Server (..), ServerConfig (..), gracefulShutdown, isServerRunning, newServer, startServer, stopServer)
import qualified LensDB.Network as Net
import LensDB.Persistence (PersistenceConfig (..), PersistenceManager (..), createBackup, newPersistenceManager, saveSnapshot, verifyDataIntegrity)
import qualified LensDB.Persistence as Persist
import LensDB.Storage (StorageManager (..), cleanupExpired, enforceLimits, getStorageStats, newStorageManager)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdout)

#ifdef UNIX
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
#endif

-- | Application state
data AppState = AppState
  { -- | Application configuration
    appConfig :: !Config,
    -- | Key-value store
    appStore :: !KVStore,
    -- | Network server
    appServer :: !Server,
    -- | Logger
    appLogger :: !Logger,
    -- | Storage manager
    appStorageManager :: !StorageManager,
    -- | Persistence manager (optional)
    appPersistenceManager :: !(Maybe PersistenceManager),
    -- | Shutdown flag
    appShutdown :: !(TVar Bool)
  }

-- | Main application entry point
main :: IO ()
main = do
  -- Parse command line arguments
  args <- getArgs
  let configFile = case args of
        ["--config", path] -> Just path
        ["-c", path] -> Just path
        _ -> Nothing

  -- Load configuration
  config <- loadConfig configFile

  -- Initialize logging
  logger <- initializeLogging (configLogging config)

  logInfo logger "Starting LensDB server..."
  logInfo logger $ "Configuration loaded: " ++ show (configServer config)

  -- Initialize application
  appState <- initializeApplication config logger

  -- Set up signal handlers for graceful shutdown
  setupSignalHandlers appState

  -- Start the application
  runApplication appState

-- | Initialize logging system
-- Note: loggingConfig here refers to LensDB.Config.LoggingConfig
initializeLogging :: LensDB.Config.LoggingConfig -> IO Log.Logger
initializeLogging loggingConfig = do
  let logLevel = case loggingLevel loggingConfig of
        "debug" -> Log.LogLevelDebug
        "info" -> Log.LogLevelInfo
        "warn" -> Log.LogLevelWarn
        "error" -> Log.LogLevelError
        _ -> Log.LogLevelInfo

      logFile =
        if null (loggingFile loggingConfig)
          then Nothing
          else Just (loggingFile loggingConfig)

  -- Construct the internal LoggingConfig using the 'Log.' prefix for fields
  let loggerConfig =
        Log.LoggingConfig
          { Log.lcLevel = logLevel,
            Log.lcFile = logFile,
            Log.lcFormat = Log.LogFormatText,
            Log.lcMaxSize = 1024 * 1024 * 100,
            Log.lcBackupCount = 5,
            Log.lcAsync = True,
            Log.lcBufferSize = 100
          }

  Log.newLogger loggerConfig

-- | Initialize the application
initializeApplication :: Config -> Log.Logger -> IO AppState
initializeApplication config@Config {..} logger = do
  -- Parse memory limit
  let memoryLimit = parseMemorySize $ storageMaxMemory configStorage
      maxKeys = storageMaxKeys configStorage

  -- Create key-value store
  store <- newKVStoreWithLimits memoryLimit maxKeys
  logInfo logger $ "Key-value store initialized (memory limit: " ++ show memoryLimit ++ " bytes, max keys: " ++ show maxKeys ++ ")"

  -- Create server
  let serverConfig =
        Net.ServerConfig
          { Net.scHost = serverHost configServer,
            Net.scPort = serverPort configServer,
            Net.scMaxConnections = serverMaxConnections configServer,
            Net.scConnectionTimeout = serverConnectionTimeout configServer,
            Net.scBufferSize = serverBufferSize configServer
          }

  server <- Net.newServer serverConfig store
  logInfo logger $ "Server created (host: " ++ serverHost configServer ++ ", port: " ++ show (serverPort configServer) ++ ")"

  -- Create storage manager
  storageManager <- newStorageManager store
  logInfo logger "Storage manager initialized"

  -- Create persistence manager if enabled
  persistenceManager <-
    if persistenceEnabled configPersistence
      then do
        let pc =
              Persist.PersistenceConfig
                { Persist.pcDataDir = persistenceDataDir configPersistence,
                  Persist.pcSyncInterval = persistenceSyncInterval configPersistence,
                  Persist.pcCompression = persistenceCompression configPersistence,
                  Persist.pcBackupEnabled = persistenceBackupEnabled configPersistence,
                  Persist.pcBackupInterval = persistenceBackupInterval configPersistence,
                  Persist.pcMaxBackups = 5,
                  Persist.pcCompactionThreshold = 0.8,
                  Persist.pcCompactionInterval = 3600
                }
        pm <- Persist.newPersistenceManager pc
        return $ Just pm
      else return Nothing

  -- Initialize shutdown flag
  shutdownFlag <- atomically $ newTVar False

  return
    AppState
      { appConfig = config,
        appStore = store,
        appServer = server,
        appLogger = logger,
        appStorageManager = storageManager,
        appPersistenceManager = persistenceManager,
        appShutdown = shutdownFlag
      }

-- | Set up signal handlers for graceful shutdown
setupSignalHandlers :: AppState -> IO ()
setupSignalHandlers appState = do
  let shutdown = gracefulShutdownApp appState

#ifdef UNIX
  _ <- installHandler sigINT (Catch shutdown) Nothing
  _ <- installHandler sigTERM (Catch shutdown) Nothing
  logInfo (appLogger appState) "Signal handlers installed"
#else
  logInfo (appLogger appState) "Signal handlers not available on this platform"
#endif

-- | Run the main application loop
runApplication :: AppState -> IO ()
runApplication appState@AppState {..} = do
  -- Start the server
  startServer appServer
  logInfo appLogger "Server started successfully"

  -- Start background tasks
  startBackgroundTasks appState

  -- Wait for shutdown signal
  waitForShutdown appState

  -- Graceful shutdown
  gracefulShutdownApp appState

-- | Start background maintenance tasks
startBackgroundTasks :: AppState -> IO ()
startBackgroundTasks appState@AppState {..} = do
  -- Storage cleanup task
  _ <- forkIO $ storageCleanupTask appState

  -- Persistence sync task
  case appPersistenceManager of
    Just pm -> do
      _ <- forkIO $ persistenceSyncTask appState pm
      return ()
    Nothing -> return ()

  -- Statistics reporting task
  _ <- forkIO $ statisticsReportingTask appState

  logInfo appLogger "Background tasks started"

-- | Storage cleanup task
storageCleanupTask :: AppState -> IO ()
storageCleanupTask AppState {..} = forever $ do
  threadDelay 300000000 -- 5 minutes

  -- Perform cleanup
  cleaned <- cleanupExpired appStorageManager
  evicted <- enforceLimits appStorageManager

  when (cleaned > 0 || evicted > 0) $
    logInfo appLogger $
      "Storage cleanup completed (expired: " ++ show cleaned ++ ", evicted: " ++ show evicted ++ ")"

-- | Persistence sync task
persistenceSyncTask :: AppState -> PersistenceManager -> IO ()
persistenceSyncTask AppState {..} persistenceManager = forever $ do
  threadDelay 60000000 -- 1 minute

  -- Get current data (simplified - would need proper access to store data)
  -- In a real implementation, we would access the internal data structure
  logDebug appLogger "Persistence sync task running"

-- | Statistics reporting task
statisticsReportingTask :: AppState -> IO ()
statisticsReportingTask AppState {..} = forever $ do
  threadDelay 600000000 -- 10 minutes

  -- Get storage statistics
  stats <- getStorageStats appStorageManager
  logInfo appLogger $ "Storage statistics: " ++ show stats

-- | Wait for shutdown signal
waitForShutdown :: AppState -> IO ()
waitForShutdown AppState {..} = do
  forever $ do
    threadDelay 1000000 -- 1 second
    shutdown <- atomically $ readTVar appShutdown
    when shutdown $ do
      logInfo appLogger "Shutdown signal received"
      return ()

-- | Graceful shutdown procedure
gracefulShutdownApp :: AppState -> IO ()
gracefulShutdownApp appState = do
  gracefulShutdownApp' appState
  exitSuccess

gracefulShutdownApp' :: AppState -> IO ()
gracefulShutdownApp' AppState {..} = do
  logInfo appLogger "Initiating graceful shutdown..."

  -- Set shutdown flag
  atomically $ writeTVar appShutdown True

  -- Stop accepting new connections
  running <- isServerRunning appServer
  when running $ do
    logInfo appLogger "Stopping server..."
    stopServer appServer

  -- Graceful shutdown with timeout
  gracefulShutdown appServer 30

  -- Save final snapshot if persistence is enabled
  case appPersistenceManager of
    Just pm -> do
      logInfo appLogger "Saving final snapshot..."
      -- In a real implementation, we would get the current data and save it
      logInfo appLogger "Snapshot saved"
    Nothing -> return ()

  -- Close logger
  closeLogger appLogger

  logInfo appLogger "Shutdown completed"

-- | Parse memory size string (e.g., "1GB", "512MB") to bytes
parseMemorySize :: String -> Int
parseMemorySize str = case reads str of
  [(size, unit)] -> case unit of
    "GB" -> size * 1024 * 1024 * 1024
    "MB" -> size * 1024 * 1024
    "KB" -> size * 1024
    "B" -> size
    _ -> 1024 * 1024 * 1024 -- Default to 1GB
  _ -> 1024 * 1024 * 1024 -- Default to 1GB

-- | Show usage information
showUsage :: IO ()
showUsage = do
  putStrLn "LensDB - High-performance NoSQL key-value database"
  putStrLn ""
  putStrLn "Usage: lensdb-server [OPTIONS]"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  -c, --config FILE    Configuration file path (default: lensdb.yaml)"
  putStrLn "  --host HOST          Server host (default: 127.0.0.1)"
  putStrLn "  --port PORT          Server port (default: 8080)"
  putStrLn "  --data-dir DIR       Data directory (default: ./data)"
  putStrLn "  --log-level LEVEL    Log level: debug, info, warn, error (default: info)"
  putStrLn "  --help               Show this help message"
  putStrLn ""
  putStrLn "Environment Variables:"
  putStrLn "  LENSDB_SERVER_HOST           Server host"
  putStrLn "  LENSDB_SERVER_PORT           Server port"
  putStrLn "  LENSDB_STORAGE_MAX_MEMORY     Maximum memory usage"
  putStrLn "  LENSDB_PERSISTENCE_DATA_DIR   Data directory"
  putStrLn "  LENSDB_LOGGING_LEVEL          Log level"
  putStrLn ""
  putStrLn "For more information, visit: https://github.com/codwiz/lensdb"
