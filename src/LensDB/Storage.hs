{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: LensDB.Storage
-- Description: Storage management and utilities
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
--
-- This module provides storage management utilities including memory
-- monitoring, cleanup operations, and storage statistics.
module LensDB.Storage
  ( -- * Storage Management
    StorageManager (..),
    newStorageManager,
    getStorageStats,
    cleanupExpired,
    enforceLimits,
    compactStorage,

    -- * Storage Statistics
    StorageStats (..),
    StorageMetrics (..),

    -- * Memory Management
    MemoryMonitor (..),
    newMemoryMonitor,
    checkMemoryUsage,
    setMemoryLimit,
  )
where

import Control.Concurrent.STM
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import LensDB.Core (KVStore, KeyValue (..), currentStorageSize, delete, keys, size)

-- | Comprehensive storage statistics
data StorageStats = StorageStats
  { -- | Total number of keys
    statsKeyCount :: !Int,
    -- | Total storage usage in bytes
    statsTotalSize :: !Int,
    -- | Average value size in bytes
    statsAvgValueSize :: !Double,
    -- | Oldest key by creation time
    statsOldestKey :: !(Maybe ByteString),
    -- | Newest key by creation time
    statsNewestKey :: !(Maybe ByteString),
    -- | Memory usage percentage
    statsMemoryUsage :: !Double,
    -- | Operations per second
    statsOperationsPerSec :: !Double
  }
  deriving (Show, Eq, Generic)

-- | Real-time storage metrics
data StorageMetrics = StorageMetrics
  { -- | Number of read operations
    metricsReadOps :: !(TVar Int),
    -- | Number of write operations
    metricsWriteOps :: !(TVar Int),
    -- | Number of delete operations
    metricsDeleteOps :: !(TVar Int),
    -- | Number of errors
    metricsErrors :: !(TVar Int),
    -- | When metrics collection started
    metricsStartTime :: !UTCTime
  }
  deriving (Eq)

-- | Storage manager with monitoring and cleanup capabilities
data StorageManager = StorageManager
  { -- | The underlying key-value store
    smStore :: !KVStore,
    -- | Performance metrics
    smMetrics :: !StorageMetrics,
    -- | Memory limit in bytes
    smMemoryLimit :: !(TVar Int),
    -- | Cleanup interval in seconds
    smCleanupInterval :: !(TVar Int),
    -- | Last cleanup timestamp
    smLastCleanup :: !(TVar UTCTime)
  }
  deriving (Eq)

-- | Memory monitoring utilities
data MemoryMonitor = MemoryMonitor
  { -- | Total system memory in bytes
    mmTotalMemory :: !(TVar Int),
    -- | Available memory in bytes
    mmAvailableMemory :: !(TVar Int),
    -- | Memory usage threshold (0.0-1.0)
    mmThreshold :: !(TVar Double),
    -- | Last memory check timestamp
    mmLastCheck :: !(TVar UTCTime)
  }
  deriving (Eq)

-- | Create a new storage manager
newStorageManager :: KVStore -> IO StorageManager
newStorageManager store = do
  now <- getCurrentTime
  readOps <- newTVarIO 0
  writeOps <- newTVarIO 0
  deleteOps <- newTVarIO 0
  errors <- newTVarIO 0

  let metrics =
        StorageMetrics
          { metricsReadOps = readOps,
            metricsWriteOps = writeOps,
            metricsDeleteOps = deleteOps,
            metricsErrors = errors,
            metricsStartTime = now
          }

  memoryLimit <- newTVarIO 0 -- 0 means unlimited
  cleanupInterval <- newTVarIO 300 -- 5 minutes default
  lastCleanup <- newTVarIO now

  return
    StorageManager
      { smStore = store,
        smMetrics = metrics,
        smMemoryLimit = memoryLimit,
        smCleanupInterval = cleanupInterval,
        smLastCleanup = lastCleanup
      }

-- | Create a new memory monitor
newMemoryMonitor :: IO MemoryMonitor
newMemoryMonitor = do
  now <- getCurrentTime
  totalMemory <- newTVarIO 0
  availableMemory <- newTVarIO 0
  threshold <- newTVarIO 0.8 -- 80% threshold default
  lastCheck <- newTVarIO now

  return
    MemoryMonitor
      { mmTotalMemory = totalMemory,
        mmAvailableMemory = availableMemory,
        mmThreshold = threshold,
        mmLastCheck = lastCheck
      }

-- | Get comprehensive storage statistics
getStorageStats :: StorageManager -> IO StorageStats
getStorageStats StorageManager {..} = do
  keyCount <- size smStore
  totalSize <- currentStorageSize smStore
  now <- getCurrentTime

  -- Calculate average value size
  let avgValueSize =
        if keyCount > 0
          then fromIntegral totalSize / fromIntegral keyCount
          else 0.0

  -- Get oldest and newest keys (simplified implementation)
  allKeys <- keys smStore
  let (oldestKey, newestKey) = case allKeys of
        [] -> (Nothing, Nothing)
        [k] -> (Just k, Just k)
        (k : ks) -> (Just k, Just $ last ks)

  -- Calculate operations per second
  readOps <- readTVarIO $ metricsReadOps smMetrics
  writeOps <- readTVarIO $ metricsWriteOps smMetrics
  deleteOps <- readTVarIO $ metricsDeleteOps smMetrics
  let totalOps = readOps + writeOps + deleteOps
      elapsedSeconds = realToFrac $ diffUTCTime now (metricsStartTime smMetrics)
      opsPerSec =
        if elapsedSeconds > 0
          then fromIntegral totalOps / elapsedSeconds
          else 0.0

  -- Memory usage (simplified - would need system calls for real usage)
  memoryLimit <- readTVarIO smMemoryLimit
  let memoryUsage =
        if memoryLimit > 0
          then fromIntegral totalSize / fromIntegral memoryLimit
          else 0.0

  return
    StorageStats
      { statsKeyCount = keyCount,
        statsTotalSize = totalSize,
        statsAvgValueSize = avgValueSize,
        statsOldestKey = oldestKey,
        statsNewestKey = newestKey,
        statsMemoryUsage = memoryUsage,
        statsOperationsPerSec = opsPerSec
      }

-- | Clean up expired data (placeholder for TTL functionality)
cleanupExpired :: StorageManager -> IO Int
cleanupExpired StorageManager {..} = do
  -- This is a placeholder for TTL-based cleanup
  -- In a real implementation, you would check expiration times
  -- and delete expired keys

  now <- getCurrentTime
  atomically $ writeTVar smLastCleanup now

  -- For now, return 0 (no expired keys cleaned up)
  return 0

-- | Enforce storage limits by removing least recently used items
enforceLimits :: StorageManager -> IO Int
enforceLimits StorageManager {..} = do
  memoryLimit <- readTVarIO smMemoryLimit
  currentSize <- currentStorageSize smStore

  if memoryLimit > 0 && currentSize > memoryLimit
    then do
      -- Implement LRU eviction strategy
      -- For simplicity, we'll delete the oldest key
      allKeys <- keys smStore
      case allKeys of
        [] -> return 0
        (oldestKey : _) -> do
          delete smStore oldestKey
          return 1
    else
      return 0

-- | Compact storage by removing fragmentation
compactStorage :: StorageManager -> IO ()
compactStorage StorageManager {..} = do
  -- This is a placeholder for storage compaction
  -- In a real implementation, you might:
  -- - Rebuild the storage map to remove fragmentation
  -- - Optimize memory layout
  -- - Compress values if possible

  -- For now, just update the last cleanup time
  now <- getCurrentTime
  atomically $ writeTVar smLastCleanup now

-- | Check current memory usage
checkMemoryUsage :: MemoryMonitor -> IO (Double, Bool)
checkMemoryUsage MemoryMonitor {..} = do
  total <- readTVarIO mmTotalMemory
  available <- readTVarIO mmAvailableMemory
  threshold <- readTVarIO mmThreshold

  let used = total - available
      usage = if total > 0 then fromIntegral used / fromIntegral total else 0.0
      isCritical = usage > threshold

  now <- getCurrentTime
  atomically $ writeTVar mmLastCheck now

  return (usage, isCritical)

-- | Set memory usage threshold
setMemoryLimit :: MemoryMonitor -> Int -> IO ()
setMemoryLimit MemoryMonitor {..} limit = do
  atomically $ writeTVar mmTotalMemory limit

-- | Increment operation counters
incrementReadOps :: StorageManager -> IO ()
incrementReadOps StorageManager {..} =
  atomically $ modifyTVar (metricsReadOps smMetrics) (+ 1)

incrementWriteOps :: StorageManager -> IO ()
incrementWriteOps StorageManager {..} =
  atomically $ modifyTVar (metricsWriteOps smMetrics) (+ 1)

incrementDeleteOps :: StorageManager -> IO ()
incrementDeleteOps StorageManager {..} =
  atomically $ modifyTVar (metricsDeleteOps smMetrics) (+ 1)

incrementErrors :: StorageManager -> IO ()
incrementErrors StorageManager {..} =
  atomically $ modifyTVar (metricsErrors smMetrics) (+ 1)

-- | Reset all metrics
resetMetrics :: StorageManager -> IO ()
resetMetrics StorageManager {..} = do
  now <- getCurrentTime
  atomically $ do
    writeTVar (metricsReadOps smMetrics) 0
    writeTVar (metricsWriteOps smMetrics) 0
    writeTVar (metricsDeleteOps smMetrics) 0
    writeTVar (metricsErrors smMetrics) 0

-- Update start time to current time
-- Note: This would require making startTime mutable in a real implementation
