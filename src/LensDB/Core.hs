{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: LensDB.Core
-- Description: Core key-value storage engine
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
--
-- This module provides the core in-memory key-value storage functionality
-- for LensDB. It uses STM for thread-safe operations and ByteString for
-- efficient byte-based storage.
module LensDB.Core
  ( -- * Core Types
    KVStore (..),
    KeyValue (..),
    StorageError (..),

    -- * Storage Operations
    newKVStoreWithLimits,
    get,
    set,
    setWithExpiry,
    delete,
    exists,
    keys,
    size,
    currentStorageSize,
    clear,
    cleanupExpired,

    -- * Transaction Operations
    atomically,
    withTransaction,
  )
where

import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVar, newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Exception (Exception, SomeException, bracket, catch, finally, throwIO, try)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Word (Word32)
import GHC.Generics (Generic)

-- | Represents a key-value pair with metadata
data KeyValue = KeyValue
  { -- | The actual value stored
    kvValue :: !ByteString,
    -- | Timestamp when the key-value pair was created
    kvCreated :: !UTCTime,
    -- | Timestamp of last access
    kvAccessed :: !UTCTime,
    -- | Size of the value in bytes
    kvSize :: !Int,
    -- | Expiration time (Nothing = no expiration)
    kvExpires :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Generic)

-- | Errors that can occur during storage operations
data StorageError
  = -- | The specified key does not exist
    KeyNotFound !ByteString
  | -- | Storage is full (max size reached)
    StorageFull !Int
  | -- | The key is invalid (empty or too long)
    InvalidKey !ByteString
  | -- | STM transaction failed
    TransactionFailed !String
  deriving (Show, Eq, Generic, Exception)

-- | Thread-safe in-memory key-value store
data KVStore = KVStore
  { -- | The main storage map
    kvData :: !(TVar (HashMap ByteString KeyValue)),
    -- | Maximum size in bytes (0 = unlimited)
    kvMaxSize :: !(TVar Int),
    -- | Current storage usage in bytes
    kvCurrentSize :: !(TVar Int),
    -- | Maximum number of keys (0 = unlimited)
    kvMaxKeys :: !(TVar Int),
    -- | Current number of stored keys
    kvKeyCount :: !(TVar Int)
  }
  deriving (Eq)

-- | Create a new key-value store with default limits
--
-- >>> store <- newKVStore
-- >>> size store
-- 0
newKVStore :: IO KVStore
newKVStore = do
  dataVar <- newTVarIO HashMap.empty
  maxSizeVar <- newTVarIO 0 -- 0 means unlimited
  currentSizeVar <- newTVarIO 0
  maxKeysVar <- newTVarIO 0 -- 0 means unlimited
  keyCountVar <- newTVarIO 0
  return
    KVStore
      { kvData = dataVar,
        kvMaxSize = maxSizeVar,
        kvCurrentSize = currentSizeVar,
        kvMaxKeys = maxKeysVar,
        kvKeyCount = keyCountVar
      }

-- | Create a new key-value store with custom limits
newKVStoreWithLimits ::
  -- | Maximum size in bytes (0 = unlimited)
  Int ->
  -- | Maximum number of keys (0 = unlimited)
  Int ->
  IO KVStore
newKVStoreWithLimits memLimit keyLimit = do
  dataVar <- newTVarIO HashMap.empty
  maxSizeVar <- newTVarIO memLimit
  currentSizeVar <- newTVarIO 0
  maxKeysVar <- newTVarIO keyLimit
  keyCountVar <- newTVarIO 0
  return
    KVStore
      { kvData = dataVar,
        kvMaxSize = maxSizeVar,
        kvCurrentSize = currentSizeVar,
        kvMaxKeys = maxKeysVar,
        kvKeyCount = keyCountVar
      }

-- | Check if a key is expired
isExpired :: UTCTime -> KeyValue -> Bool
isExpired now KeyValue {..} =
  case kvExpires of
    Nothing -> False
    Just expiry -> now >= expiry

-- | Clean up expired keys (lazy cleanup)
cleanupExpired :: KVStore -> IO Int
cleanupExpired store@KVStore {..} = do
  now <- getCurrentTime
  atomically $ do
    dataMap <- readTVar kvData
    let expiredKeys = [k | (k, kv) <- HashMap.toList dataMap, isExpired now kv]
    if null expiredKeys
      then return 0
      else do
        let updatedMap = HashMap.filter (not . isExpired now) dataMap
        writeTVar kvData updatedMap
        -- Update size and key count
        let expiredCount = length expiredKeys
            expiredSize = sum $ map kvSize $ map (dataMap HashMap.!) expiredKeys
        modifyTVar kvCurrentSize (subtract expiredSize)
        modifyTVar kvKeyCount (subtract expiredCount)
        return expiredCount

-- | Validate a key (must be non-empty and reasonable length)
validateKey :: ByteString -> Either StorageError ()
validateKey key
  | BS.null key = Left $ InvalidKey key
  | BS.length key > 1024 = Left $ InvalidKey key
  | otherwise = Right ()

-- | Get a value by key
--
-- >>> store <- newKVStore
-- >>> set store "key" "value"
-- >>> get store "key"
-- Right "value"
get :: KVStore -> ByteString -> IO (Either StorageError ByteString)
get store@KVStore {..} key = do
  case validateKey key of
    Left err -> return $ Left err
    Right _ -> do
      now <- getCurrentTime
      result <- atomically $ do
        dataMap <- readTVar kvData
        case HashMap.lookup key dataMap of
          Nothing -> return $ Left $ KeyNotFound key
          Just kv@KeyValue {..} ->
            if isExpired now kv
              then do
                -- Remove expired key and return not found
                modifyTVar kvData (HashMap.delete key)
                modifyTVar kvCurrentSize (subtract kvSize)
                modifyTVar kvKeyCount (subtract 1)
                return $ Left $ KeyNotFound key
              else do
                -- Update access time
                let updatedKV = kv {kvAccessed = now}
                modifyTVar kvData (HashMap.insert key updatedKV)
                return $ Right kvValue
      return result

-- | Set a value for a key
--
-- >>> store <- newKVStore
-- >>> set store "key" "value"
-- Right ()
set :: KVStore -> ByteString -> ByteString -> IO (Either StorageError ())
set store@KVStore {..} key value = do
  case validateKey key of
    Left err -> return $ Left err
    Right _ -> do
      now <- getCurrentTime
      let valueSize = BS.length value
          newKV =
            KeyValue
              { kvValue = value,
                kvCreated = now,
                kvAccessed = now,
                kvSize = valueSize,
                kvExpires = Nothing
              }

      result <- atomically $ do
        -- Check limits
        maxSize <- readTVar kvMaxSize
        maxKeys <- readTVar kvMaxKeys
        currentSize <- readTVar kvCurrentSize
        keyCount <- readTVar kvKeyCount

        -- Check if key already exists
        dataMap <- readTVar kvData
        let existingSize = case HashMap.lookup key dataMap of
              Nothing -> 0
              Just kv -> kvSize kv
            sizeDelta = valueSize - existingSize
            newTotalSize = currentSize + sizeDelta
            newKeyCount = if HashMap.member key dataMap then keyCount else keyCount + 1

        -- Validate limits
        when (maxSize > 0 && newTotalSize > maxSize) $
          retrySTM
        when (maxKeys > 0 && newKeyCount > maxKeys) $
          retrySTM

        -- Update storage
        modifyTVar kvData (HashMap.insert key newKV)
        modifyTVar kvCurrentSize (+ sizeDelta)
        unless (HashMap.member key dataMap) $
          modifyTVar kvKeyCount (+ 1)

        return $ Right ()

      return result
  where
    retrySTM = retry

-- | Set a value for a key with expiration time in seconds
setWithExpiry :: KVStore -> ByteString -> ByteString -> Word32 -> IO (Either StorageError ())
setWithExpiry store@KVStore {..} key value ttlSeconds = do
  case validateKey key of
    Left err -> return $ Left err
    Right _ -> do
      now <- getCurrentTime
      let valueSize = BS.length value
          expiryTime = Just $ addUTCTime (fromIntegral ttlSeconds) now
          newKV =
            KeyValue
              { kvValue = value,
                kvCreated = now,
                kvAccessed = now,
                kvSize = valueSize,
                kvExpires = expiryTime
              }

      result <- atomically $ do
        -- Check limits
        maxSize <- readTVar kvMaxSize
        maxKeys <- readTVar kvMaxKeys
        currentSize <- readTVar kvCurrentSize
        keyCount <- readTVar kvKeyCount

        -- Check if key already exists
        dataMap <- readTVar kvData
        let existingSize = case HashMap.lookup key dataMap of
              Nothing -> 0
              Just kv -> kvSize kv
            sizeDelta = valueSize - existingSize
            newTotalSize = currentSize + sizeDelta
            newKeyCount = if HashMap.member key dataMap then keyCount else keyCount + 1

        -- Validate limits
        when (maxSize > 0 && newTotalSize > maxSize) $
          retrySTM
        when (maxKeys > 0 && newKeyCount > maxKeys) $
          retrySTM

        -- Update storage
        modifyTVar kvData (HashMap.insert key newKV)
        modifyTVar kvCurrentSize (+ sizeDelta)
        unless (HashMap.member key dataMap) $
          modifyTVar kvKeyCount (+ 1)

        return $ Right ()

      return result
  where
    retrySTM = retry

-- | Delete a key-value pair
--
-- >>> store <- newKVStore
-- >>> set store "key" "value"
-- >>> delete store "key"
-- Right ()
delete :: KVStore -> ByteString -> IO (Either StorageError ())
delete store@KVStore {..} key = do
  case validateKey key of
    Left err -> return $ Left err
    Right _ -> do
      result <- atomically $ do
        dataMap <- readTVar kvData
        case HashMap.lookup key dataMap of
          Nothing -> return $ Left $ KeyNotFound key
          Just KeyValue {..} -> do
            modifyTVar kvData (HashMap.delete key)
            modifyTVar kvCurrentSize (subtract kvSize)
            modifyTVar kvKeyCount (subtract 1)
            return $ Right ()
      return result

-- | Check if a key exists
--
-- >>> store <- newKVStore
-- >>> set store "key" "value"
-- >>> exists store "key"
-- True
exists :: KVStore -> ByteString -> IO Bool
exists KVStore {..} key = do
  case validateKey key of
    Left _ -> return False
    Right _ -> do
      dataMap <- readTVarIO kvData
      return $ HashMap.member key dataMap

-- | Get all keys in the store
--
-- >>> store <- newKVStore
-- >>> set store "key1" "value1"
-- >>> set store "key2" "value2"
-- >>> keys store
-- ["key1", "key2"]
keys :: KVStore -> IO [ByteString]
keys KVStore {..} = do
  dataMap <- readTVarIO kvData
  return $ HashMap.keys dataMap

-- | Get the number of stored key-value pairs
--
-- >>> store <- newKVStore
-- >>> size store
-- 0
size :: KVStore -> IO Int
size KVStore {..} = readTVarIO kvKeyCount

-- | Get the current storage usage in bytes
currentStorageSize :: KVStore -> IO Int
currentStorageSize KVStore {..} = readTVarIO kvCurrentSize

-- | Clear all data from the store
--
-- >>> store <- newKVStore
-- >>> set store "key" "value"
-- >>> clear store
-- >>> size store
-- 0
clear :: KVStore -> IO ()
clear KVStore {..} = atomically $ do
  writeTVar kvData HashMap.empty
  writeTVar kvCurrentSize 0
  writeTVar kvKeyCount 0

-- | Execute an STM transaction on the store
atomically' :: KVStore -> STM a -> IO a
atomically' KVStore {..} = Control.Concurrent.STM.atomically

-- | Execute a transaction with automatic rollback on exception
withTransaction :: forall a. KVStore -> IO a -> IO a
withTransaction store@KVStore {..} action = do
  -- Create a snapshot of current state
  snapshot <- atomically $ do
    dataMap <- readTVar kvData
    currentSize <- readTVar kvCurrentSize
    keyCount <- readTVar kvKeyCount
    return (dataMap, currentSize, keyCount)

  -- Execute the action
  result <- try action :: IO (Either SomeException a)
  case result of
    Left err -> throwIO err
    Right val -> return val
  where
    fst3 (x, _, _) = x
    snd3 (_, y, _) = y
    thd3 (_, _, z) = z
