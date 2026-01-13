{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module: LensDB.Persistence
-- Description: Persistence layer for LensDB
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
--
-- This module provides persistence functionality for LensDB, including
-- data serialization, file I/O, backup management, and recovery operations.
module LensDB.Persistence
  ( -- * Persistence Types
    PersistenceManager (..),
    PersistenceConfig (..),
    Snapshot (..),
    BackupInfo (..),

    -- * Persistence Operations
    newPersistenceManager,
    saveSnapshot,
    loadSnapshot,
    createBackup,
    restoreFromBackup,
    compactDataFiles,

    -- * Serialization
    serializeKeyValue,
    deserializeKeyValue,
    serializeMap,
    deserializeMap,

    -- * File Management
    ensureDataDirectory,
    getDataFiles,
    cleanupOldDataFiles,
    verifyDataIntegrity,
  )
where

import Control.Concurrent (MVar, forkIO, newMVar, putMVar, takeMVar, threadDelay, tryTakeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException (..), bracket, catch, finally, throwIO, try)
import Control.Monad (forM, forever, liftM, replicateM, unless, void, when)
import Data.Binary (Binary (..), decode, decodeOrFail, encode, encodeFile)
import Data.Binary.Get (Get, runGetOrFail)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortBy)
import Data.Time (UTCTime (..), addUTCTime, defaultTimeLocale, formatTime, fromGregorian, getCurrentTime, secondsToDiffTime, toGregorian, utctDay, utctDayTime)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import LensDB.Core (KeyValue (..))
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getDirectoryContents,
    getModificationTime,
    removeFile,
    renameFile,
  )
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))
import System.IO
  ( Handle,
    IOMode (ReadMode, WriteMode),
    SeekMode (AbsoluteSeek),
    hClose,
    hFileSize,
    hFlush,
    hSeek,
    openBinaryFile,
  )

-- | Persistence configuration
data PersistenceConfig = PersistenceConfig
  { -- | Directory for data files
    pcDataDir :: !FilePath,
    -- | Sync interval in seconds
    pcSyncInterval :: !Int,
    -- | Whether to compress data files
    pcCompression :: !Bool,
    -- | Whether to enable automatic backups
    pcBackupEnabled :: !Bool,
    -- | Backup interval in seconds
    pcBackupInterval :: !Int,
    -- | Maximum number of backups to keep
    pcMaxBackups :: !Int,
    -- | Compaction threshold (0.0-1.0)
    pcCompactionThreshold :: !Double,
    -- | Compaction interval in seconds
    pcCompactionInterval :: !Int
  }
  deriving (Show, Eq, Generic)

-- Custom Binary instance for UTCTime using Gregorian calendar representation
instance Binary UTCTime where
  put utctime = do
    let day = utctDay utctime
        diffTime = utctDayTime utctime
        (year, month, dayOfMonth) = toGregorian day
    put year
    put month
    put dayOfMonth
    put (floor (realToFrac diffTime) :: Int)

  get = do
    year <- get
    month <- get
    dayOfMonth <- get
    seconds <- get :: Get Int
    let day = fromGregorian year month dayOfMonth
        diffTime = secondsToDiffTime (fromIntegral seconds)
    return $ UTCTime {utctDay = day, utctDayTime = diffTime}

-- | Snapshot of the database state
data Snapshot = Snapshot
  { -- | Snapshot version
    snapVersion :: !Word32,
    -- | Snapshot timestamp
    snapTimestamp :: !UTCTime,
    -- | Snapshot data
    snapData :: !(HashMap ByteString KeyValue),
    -- | Data checksum
    snapChecksum :: !Word32
  }
  deriving (Show, Eq, Generic)

instance Binary Snapshot where
  put Snapshot {..} = do
    put snapTimestamp
    put snapVersion
    put snapData
    put snapChecksum

  get = do
    snapTimestamp <- get
    snapVersion <- get
    snapData <- get
    snapChecksum <- get
    return Snapshot {..}

-- | Backup information
data BackupInfo = BackupInfo
  { -- | Path to backup file
    biPath :: !FilePath,
    -- | When backup was created
    biTimestamp :: !UTCTime,
    -- | Size of backup file
    biSize :: !Word64,
    -- | Database version at backup time
    biVersion :: !Word32,
    -- | Backup checksum
    biChecksum :: !Word32
  }
  deriving (Show, Eq, Generic)

instance Binary BackupInfo

-- | Binary instance for KeyValue
instance Binary KeyValue where
  put KeyValue {..} = do
    put kvValue
    put kvCreated
    put kvAccessed
    put kvSize
    put kvExpires

  get = do
    kvValue <- get
    kvCreated <- get
    kvAccessed <- get
    kvSize <- get
    kvExpires <- get
    return KeyValue {..}

instance Binary (HashMap ByteString KeyValue) where
  put hashMap = do
    let entries = HashMap.toList hashMap
    put (length entries)
    mapM_ (\(k, v) -> put k >> put v) entries
  
  get = do
    numEntries <- get
    entries <- replicateM numEntries $ do
      key <- get
      value <- get
      return (key, value)
    return $ HashMap.fromList entries

-- | Persistence manager state
data PersistenceManager = PersistenceManager
  { -- | Persistence configuration
    pmConfig :: !PersistenceConfig,
    -- | Current database version
    pmCurrentVersion :: !(TVar Word32),
    -- | Last sync timestamp
    pmLastSync :: !(TVar UTCTime),
    -- | Last backup timestamp
    pmLastBackup :: !(TVar UTCTime),
    -- | Trigger for sync operations
    pmSyncTrigger :: !(MVar ()),
    -- | Trigger for backup operations
    pmBackupTrigger :: !(MVar ())
  }
  deriving (Eq)

-- | Create a new persistence manager
newPersistenceManager :: PersistenceConfig -> IO PersistenceManager
newPersistenceManager config = do
  -- Ensure data directory exists
  ensureDataDirectory (pcDataDir config)

  -- Initialize state
  currentVersion <- newTVarIO 1
  now <- getCurrentTime
  lastSync <- newTVarIO now
  lastBackup <- newTVarIO now
  syncTrigger <- newMVar ()
  backupTrigger <- newMVar ()

  let manager =
        PersistenceManager
          { pmConfig = config,
            pmCurrentVersion = currentVersion,
            pmLastSync = lastSync,
            pmLastBackup = lastBackup,
            pmSyncTrigger = syncTrigger,
            pmBackupTrigger = backupTrigger
          }

  -- Start background workers
  _ <- forkIO $ syncWorker manager
  when (pcBackupEnabled config) $ do
    _ <- forkIO $ backupWorker manager
    return ()

  return manager

-- | Background sync worker
syncWorker :: PersistenceManager -> IO ()
syncWorker manager@PersistenceManager {..} = forever $ do
  -- Wait for sync trigger or timeout
  let timeout = pcSyncInterval pmConfig * 1000000 -- Convert to microseconds
  result <- tryTakeMVar pmSyncTrigger

  case result of
    Just _ -> do
      -- Triggered sync
      performSync manager
    Nothing -> do
      -- Timeout sync
      threadDelay timeout
      performSync manager

-- | Background backup worker
backupWorker :: PersistenceManager -> IO ()
backupWorker manager@PersistenceManager {..} = forever $ do
  -- Wait for backup trigger or timeout
  let timeout = pcBackupInterval pmConfig * 1000000 -- Convert to microseconds
  result <- tryTakeMVar pmBackupTrigger

  case result of
    Just _ -> do
      -- Triggered backup
      performBackup manager
    Nothing -> do
      -- Timeout backup
      threadDelay timeout
      performBackup manager

-- | Save a snapshot of the current database state
saveSnapshot :: PersistenceManager -> HashMap ByteString KeyValue -> IO ()
saveSnapshot manager@PersistenceManager {..} dataMap = do
  now <- getCurrentTime
  version <- readTVarIO pmCurrentVersion

  -- Create snapshot
  let snapshot =
        Snapshot
          { snapTimestamp = now,
            snapVersion = version,
            snapData = dataMap,
            snapChecksum = calculateChecksum dataMap
          }

  -- Write to file
  let fileName = "snapshot_" ++ formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now ++ "_" ++ show version ++ ".dat"
      filePath = pcDataDir pmConfig </> fileName

  encodeFile filePath snapshot

  -- Update version and last sync time
  atomically $ do
    modifyTVar pmCurrentVersion (+ 1)
    writeTVar pmLastSync now

  -- Trigger sync
  _ <- tryTakeMVar pmSyncTrigger
  putMVar pmSyncTrigger ()

-- | Load a snapshot from file
loadSnapshot :: PersistenceManager -> FilePath -> IO (Either String Snapshot)
loadSnapshot PersistenceManager {..} filePath = do
  exists <- doesFileExist filePath
  if not exists
    then return $ Left $ "Snapshot file does not exist: " ++ filePath
    else do
      result :: Either SomeException Snapshot <- try $ do
        contents <- LBS.readFile filePath
        case decodeOrFail contents of
          Left (_, _, err) -> error err
          Right (_, _, snapshot) -> return snapshot
      case result of
        Left err -> return $ Left $ "Failed to decode snapshot: " ++ show err
        Right snapshot -> do
          -- Verify checksum
          let expectedChecksum = calculateChecksum (snapData snapshot)
          if snapChecksum snapshot == expectedChecksum
            then return $ Right snapshot
            else return $ Left "Snapshot checksum verification failed"

-- | Create a backup of the current database
createBackup :: PersistenceManager -> HashMap ByteString KeyValue -> IO (Either String BackupInfo)
createBackup manager@PersistenceManager {..} dataMap = do
  now <- getCurrentTime
  version <- readTVarIO pmCurrentVersion

  -- Create snapshot for backup
  let snapshot =
        Snapshot
          { snapTimestamp = now,
            snapVersion = version,
            snapData = dataMap,
            snapChecksum = calculateChecksum dataMap
          }

  -- Generate backup filename
  let backupId = (fromIntegral (floor (utctDayTime now) :: Integer) :: Word32)
      fileName = "backup_" ++ formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now ++ "_" ++ show backupId ++ ".bak"
      filePath = pcDataDir pmConfig </> fileName

  -- Write backup file
  encodeFile filePath snapshot

  -- Get file size
  fileSize <- hFileSize =<< openBinaryFile filePath ReadMode

  -- Create backup info
  let backupInfo =
        BackupInfo
          { biPath = filePath,
            biTimestamp = now,
            biSize = fromIntegral fileSize,
            biVersion = version,
            biChecksum = snapChecksum snapshot
          }

  -- Update last backup time
  atomically $ writeTVar pmLastBackup now

  -- Clean up old backups
  cleanupOldBackups manager

  return $ Right backupInfo

-- | Restore database from backup
restoreFromBackup :: PersistenceManager -> FilePath -> IO (Either String (HashMap ByteString KeyValue))
restoreFromBackup manager backupPath = do
  result <- loadSnapshot manager backupPath
  case result of
    Left err -> return $ Left err
    Right snapshot -> do
      -- Verify backup integrity
      if verifySnapshotIntegrity snapshot
        then return $ Right $ snapData snapshot
        else return $ Left "Backup integrity verification failed"

-- | Perform sync operation
performSync :: PersistenceManager -> IO ()
performSync manager@PersistenceManager {..} = do
  -- This would be called by the sync worker
  -- In a real implementation, this would sync in-memory data to disk
  now <- getCurrentTime
  atomically $ writeTVar pmLastSync now

-- | Perform backup operation
performBackup :: PersistenceManager -> IO ()
performBackup manager@PersistenceManager {..} = do
  -- This would be called by the backup worker
  -- In a real implementation, this would create a backup
  now <- getCurrentTime
  atomically $ writeTVar pmLastBackup now

-- | Clean up old backup files
cleanupOldBackups :: PersistenceManager -> IO ()
cleanupOldBackups PersistenceManager {..} = do
  backupFiles <- getBackupFiles (pcDataDir pmConfig)

  -- Sort by timestamp (newest first)
  let sortedBackups = reverse $ sortByTimestamp backupFiles

  -- Remove excess backups
  let excessBackups = drop (pcMaxBackups pmConfig) sortedBackups
  mapM_ removeFile excessBackups

-- | Get all backup files in data directory
getBackupFiles :: FilePath -> IO [FilePath]
getBackupFiles dataDir = do
  exists <- doesDirectoryExist dataDir
  if not exists
    then return []
    else do
      allFiles <- getDirectoryContents dataDir
      let backupFiles = filter (\f -> takeExtension f == ".bak") allFiles
      return $ map (dataDir </>) backupFiles

-- | Sort backup files by timestamp
sortByTimestamp :: [FilePath] -> [FilePath]
sortByTimestamp files =
  -- Simplified sorting - in real implementation would parse timestamps
  files

-- | Verify snapshot integrity
verifySnapshotIntegrity :: Snapshot -> Bool
verifySnapshotIntegrity Snapshot {..} =
  let calculatedChecksum = calculateChecksum snapData
   in calculatedChecksum == snapChecksum

-- | Calculate checksum for data integrity
calculateChecksum :: HashMap ByteString KeyValue -> Word32
calculateChecksum dataMap =
  -- Simplified checksum calculation
  -- In a real implementation, would use a proper hash function like CRC32
  fromIntegral $ HashMap.size dataMap `mod` 2 ^ 32

-- | Serialize a key-value pair
serializeKeyValue :: (ByteString, KeyValue) -> ByteString
serializeKeyValue (key, KeyValue {..}) =
  let keyLen = BS.length key
      valLen = kvSize
      header =
        BS.pack
          [ fromIntegral (keyLen `shiftR` 24),
            fromIntegral (keyLen `shiftR` 16),
            fromIntegral (keyLen `shiftR` 8),
            fromIntegral keyLen,
            fromIntegral (valLen `shiftR` 24),
            fromIntegral (valLen `shiftR` 16),
            fromIntegral (valLen `shiftR` 8),
            fromIntegral valLen
          ]
   in BS.concat [header, key, kvValue]

-- | Deserialize a key-value pair
deserializeKeyValue :: ByteString -> Maybe (ByteString, KeyValue)
deserializeKeyValue bs
  | BS.length bs < 8 = Nothing
  | otherwise =
      let keyLen =
            (fromIntegral (BS.index bs 0) `shiftL` 24)
              .|. (fromIntegral (BS.index bs 1) `shiftL` 16)
              .|. (fromIntegral (BS.index bs 2) `shiftL` 8)
              .|. fromIntegral (BS.index bs 3)
          valLen =
            (fromIntegral (BS.index bs 4) `shiftL` 24)
              .|. (fromIntegral (BS.index bs 5) `shiftL` 16)
              .|. (fromIntegral (BS.index bs 6) `shiftL` 8)
              .|. fromIntegral (BS.index bs 7)
          expectedLen = 8 + keyLen + valLen
       in if BS.length bs >= expectedLen
            then
              let key = BS.take keyLen $ BS.drop 8 bs
                  value = BS.take valLen $ BS.drop (8 + keyLen) bs
                  -- Using default timestamps since we can't get current time in Maybe context
                  -- In a real implementation, you'd want to store timestamps in the serialization
                  kv =
                    KeyValue
                      { kvValue = value,
                        kvCreated = read "1970-01-01 00:00:00 UTC", -- Default timestamp
                        kvAccessed = read "1970-01-01 00:00:00 UTC", -- Default timestamp
                        kvSize = valLen,
                        kvExpires = Nothing -- No expiration for persisted data
                      }
               in Just (key, kv)
            else Nothing

-- | Serialize an entire map
serializeMap :: HashMap ByteString KeyValue -> ByteString
serializeMap dataMap =
  let entries = HashMap.toList dataMap
      serializedEntries = map serializeKeyValue entries
      count =
        BS.pack
          [ fromIntegral (length entries `shiftR` 24),
            fromIntegral (length entries `shiftR` 16),
            fromIntegral (length entries `shiftR` 8),
            fromIntegral (length entries)
          ]
   in BS.concat $ count : serializedEntries

-- | Deserialize an entire map
deserializeMap :: ByteString -> Maybe (HashMap ByteString KeyValue)
deserializeMap bs
  | BS.length bs < 4 = Nothing
  | otherwise =
      let count =
            (fromIntegral (BS.index bs 0) `shiftL` 24)
              .|. (fromIntegral (BS.index bs 1) `shiftL` 16)
              .|. (fromIntegral (BS.index bs 2) `shiftL` 8)
              .|. fromIntegral (BS.index bs 3)
          remaining = BS.drop 4 bs
       in deserializeEntries count remaining

-- | Deserialize entries from serialized data
deserializeEntries :: Int -> ByteString -> Maybe (HashMap ByteString KeyValue)
deserializeEntries 0 _ = Just HashMap.empty
deserializeEntries n bs = do
  (key, kv) <- deserializeKeyValue bs
  let entrySize = 8 + BS.length key + kvSize kv
      remaining = BS.drop entrySize bs
  rest <- deserializeEntries (n - 1) remaining
  return $ HashMap.insert key kv rest

-- | Ensure data directory exists
ensureDataDirectory :: FilePath -> IO ()
ensureDataDirectory dataDir = do
  createDirectoryIfMissing True dataDir

-- | Get all data files in directory
getDataFiles :: FilePath -> IO [FilePath]
getDataFiles dataDir = do
  exists <- doesDirectoryExist dataDir
  if not exists
    then return []
    else do
      allFiles <- getDirectoryContents dataDir
      let dataFiles = filter (\f -> takeExtension f == ".dat") allFiles
      return $ map (dataDir </>) dataFiles

-- | Clean up old data files
cleanupOldDataFiles :: PersistenceManager -> IO ()
cleanupOldDataFiles PersistenceManager {..} = do
  dataFiles <- getDataFiles (pcDataDir pmConfig)

  -- Sort by modification time (oldest first)
  sortedFiles <- sortByModificationTime dataFiles

  -- Keep only the most recent files
  let filesToKeep = drop (length sortedFiles - 10) sortedFiles -- Keep last 10 files
      filesToDelete = take (length sortedFiles - 10) sortedFiles

  mapM_ removeFile filesToDelete

-- | Sort files by modification time
sortByModificationTime :: [FilePath] -> IO [FilePath]
sortByModificationTime files = do
  fileTimes <- forM files $ \f -> do
    time <- getModificationTime f
    return (f, time)
  return $ map fst $ sortBy (\(_, t1) (_, t2) -> compare t1 t2) fileTimes

-- | Verify data integrity of all files
verifyDataIntegrity :: PersistenceManager -> IO Bool
verifyDataIntegrity PersistenceManager {..} = do
  dataFiles <- getDataFiles (pcDataDir pmConfig)

  -- Check each file
  results <- mapM verifyFileIntegrity dataFiles
  return $ all id results

-- | Verify integrity of a single file
verifyFileIntegrity :: FilePath -> IO Bool
verifyFileIntegrity filePath = do
  exists <- doesFileExist filePath
  if not exists
    then return False
    else do
      -- Try to read and decode the file
      result :: Either SomeException Snapshot <- try $ do
        contents <- LBS.readFile filePath
        case decodeOrFail contents of -- Use decodeOrFail instead
          Left (_, _, err) -> error err
          Right (_, _, snapshot) -> return snapshot
      case result of
        Left _ -> return False
        Right _ -> return True -- Simplified check

-- | Compact data files to reduce fragmentation
compactDataFiles :: PersistenceManager -> IO ()
compactDataFiles PersistenceManager {..} = do
  dataFiles <- getDataFiles (pcDataDir pmConfig)

  -- In a real implementation, this would:
  -- 1. Read all data files
  -- 2. Merge and compact the data
  -- 3. Write new compacted files
  -- 4. Replace old files

  -- For now, just clean up old files
  cleanupOldDataFiles PersistenceManager {..}
