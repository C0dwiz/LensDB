{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: LensDB.StorageSpec
-- Description: Test suite for LensDB.Storage module
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
module LensDB.StorageSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Time (addUTCTime, getCurrentTime)
import LensDB.Core
import LensDB.Storage
import LensDB.Storage (MemoryMonitor (..), StorageManager (..), StorageStats (..))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "StorageManager" $ do
    describe "Creation" $ do
      it "creates storage manager with default settings" $ do
        store <- newKVStore
        manager <- newStorageManager store

        -- Should not throw any exceptions
        return (manager :: StorageManager)

    describe "Statistics" $ do
      it "provides initial statistics" $ do
        store <- newKVStore
        manager <- newStorageManager store

        stats <- getStorageStats manager

        statsKeyCount stats `shouldBe` 0
        statsTotalSize stats `shouldBe` 0
        statsAvgValueSize stats `shouldBe` 0.0
        statsOldestKey stats `shouldBe` Nothing
        statsNewestKey stats `shouldBe` Nothing
        statsMemoryUsage stats `shouldBe` 0.0
        statsOperationsPerSec stats `shouldBe` 0.0

      it "updates statistics after operations" $ do
        store <- newKVStore
        manager <- newStorageManager store

        -- Add some data
        _ <- set store "key1" "value1"
        _ <- set store "key2" "value2"
        _ <- set store "key3" "value3"

        stats <- getStorageStats manager

        statsKeyCount stats `shouldBe` 3
        statsTotalSize stats `shouldBe` 14 -- "value1" + "value2" + "value3"
        statsAvgValueSize stats `shouldBe` 14.0 / 3.0

    describe "Cleanup operations" $ do
      it "performs cleanup without errors" $ do
        store <- newKVStore
        manager <- newStorageManager store

        cleaned <- cleanupExpired manager
        cleaned `shouldBe` 0 -- No expired keys in this test
      it "enforces limits when needed" $ do
        store <- newKVStoreWithLimits 100 0 -- 100 byte limit
        manager <- newStorageManager store

        -- Add data that exceeds limit
        _ <- set store "key1" (BS.replicate 60 'a')
        _ <- set store "key2" (BS.repaint 60 'b')

        evicted <- enforceLimits manager
        evicted `shouldBe` 1 -- Should evict one key
  describe "MemoryMonitor" $ do
    describe "Creation" $ do
      it "creates memory monitor with default settings" $ do
        monitor <- newMemoryMonitor

        -- Should not throw any exceptions
        return (monitor :: MemoryMonitor)

    describe "Memory checking" $ do
      it "checks memory usage" $ do
        monitor <- newMemoryMonitor

        (usage, isCritical) <- checkMemoryUsage monitor

        usage `shouldSatisfy` (>= 0.0)
        usage `shouldSatisfy` (<= 1.0)
        isCritical `shouldBe` False -- Default threshold is 0.8, usage should be 0
      it "sets memory limits" $ do
        monitor <- newMemoryMonitor

        setMemoryLimit monitor 1024 * 1024 -- 1MB
        (usage, _) <- checkMemoryUsage monitor
        usage `shouldBe` 0.0 -- No actual memory usage in this test

-- QuickCheck properties
prop_storage_stats_consistency :: [(ByteString, ByteString)] -> Property
prop_storage_stats_consistency pairs =
  not (null pairs) ==> property $ do
    store <- newKVStore

    -- Add all pairs to store
    results <- mapM (\(k, v) -> set store k v) pairs
    let allSucceeded = all (== Right ()) results

    if allSucceeded
      then do
        manager <- newStorageManager store
        stats <- getStorageStats manager

        let expectedKeyCount = length pairs
            expectedTotalSize = sum $ map (BS.length . snd) pairs

        return $
          statsKeyCount stats == expectedKeyCount
            && statsTotalSize stats == expectedTotalSize
      else
        return discard
