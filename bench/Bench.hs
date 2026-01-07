{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Bench
-- Description: Performance benchmarks for LensDB
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
module Main where

import Control.DeepSeq
import Control.Monad (mapM_, replicateM_)
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import LensDB.Core
import LensDB.Protocol (ProtocolMessage (..), ProtocolResponse (..), decodeMessage, encodeMessage, encodeResponse, createSet, createSuccessResponse)
import LensDB.Storage
import System.Random

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Core Operations"
        [ bench "set operation" $ nfIO (benchmarkSet 1000),
          bench "get operation" $ nfIO (benchmarkGet 1000),
          bench "delete operation" $ nfIO (benchmarkDelete 1000),
          bench "exists operation" $ nfIO (benchmarkExists 1000)
        ],
      bgroup
        "Storage Operations"
        [ bench "storage statistics" $ nfIO (benchmarkStorageStats 1000),
          bench "cleanup operations" $ nfIO (benchmarkCleanup 1000)
        ],
      bgroup
        "Protocol Operations"
        [ bench "message serialization" $ nf benchmarkMessageSerialization testMessage,
          bench "message deserialization" $ nf benchmarkMessageDeserialization testMessageBytes,
          bench "response serialization" $ nf benchmarkResponseSerialization testResponse
        ],
      bgroup
        "Concurrent Operations"
        [ bench "concurrent sets" $ nfIO (benchmarkConcurrentSets 100 10),
          bench "concurrent gets" $ nfIO (benchmarkConcurrentGets 100 10)
        ]
    ]

-- Add this helper to Bench.hs or update calls directly
newKVStore :: IO KVStore
newKVStore = newKVStoreWithLimits 0 0 -- No memory limit, no key limit

-- Test data
testMessage :: ProtocolMessage
testMessage = createSet "benchmark-key" "benchmark-value"

testMessageBytes :: BS.ByteString
testMessageBytes = LBS.toStrict $ encodeMessage testMessage

testResponse :: ProtocolResponse
testResponse = createSuccessResponse ""

-- Benchmark core set operation
benchmarkSet :: Int -> IO ()
benchmarkSet n = do
  store <- newKVStoreWithLimits 0 0
  replicateM_ n $ set store "bench-key" "bench-value"

-- Benchmark core get operation
benchmarkGet :: Int -> IO ()
benchmarkGet n = do
  store <- newKVStore
  _ <- set store "bench-key" "bench-value"
  replicateM_ n $ get store "bench-key"

-- Benchmark core delete operation
benchmarkDelete :: Int -> IO ()
benchmarkDelete n = do
  store <- newKVStore
  replicateM_ n $ do
    _ <- set store "bench-key" "bench-value"
    delete store "bench-key"

-- Benchmark core exists operation
benchmarkExists :: Int -> IO ()
benchmarkExists n = do
  store <- newKVStore
  _ <- set store "bench-key" "bench-value"
  replicateM_ n $ exists store "bench-key"

-- Benchmark storage statistics
benchmarkStorageStats :: Int -> IO ()
benchmarkStorageStats n = do
  store <- newKVStore
  manager <- newStorageManager store

  -- Add some data
  replicateM_ n $ set store "stats-key" "stats-value"

  -- Get statistics
  _ <- getStorageStats manager
  return ()

-- Benchmark cleanup operations
benchmarkCleanup :: Int -> IO ()
benchmarkCleanup n = do
  store <- newKVStore
  manager <- newStorageManager store

  -- Add some data
  replicateM_ n $ set store "cleanup-key" "cleanup-value"

  -- Perform cleanup
  _ <- cleanupExpired manager
  _ <- enforceLimits manager
  return ()

-- Benchmark message serialization
benchmarkMessageSerialization :: ProtocolMessage -> BS.ByteString
benchmarkMessageSerialization msg = LBS.toStrict $ encodeMessage msg

-- Benchmark message deserialization
benchmarkMessageDeserialization :: BS.ByteString -> Either String ProtocolMessage
benchmarkMessageDeserialization bytes = decodeMessage (LBS.fromStrict bytes)

-- Benchmark response serialization
benchmarkResponseSerialization :: ProtocolResponse -> BS.ByteString
benchmarkResponseSerialization resp = LBS.toStrict $ encodeResponse resp

-- Benchmark concurrent sets
benchmarkConcurrentSets :: Int -> Int -> IO ()
benchmarkConcurrentSets numKeys numThreads = do
  store <- newKVStore
  let keys = map (\i -> BS8.pack $ "concurrent-key-" ++ show i) [1 .. numKeys]
      values = map (\i -> BS8.pack $ "concurrent-value-" ++ show i) [1 .. numKeys]

  replicateM_ numThreads $ do
    mapM_ (\(k, v) -> set store k v) (zip keys values)

-- Benchmark concurrent gets
benchmarkConcurrentGets :: Int -> Int -> IO ()
benchmarkConcurrentGets numKeys numThreads = do
  store <- newKVStore
  let keys = map (\i -> BS8.pack $ "concurrent-key-" ++ show i) [1 .. numKeys]
      values = map (\i -> BS8.pack $ "concurrent-value-" ++ show i) [1 .. numKeys]

  -- Pre-populate store
  mapM_ (\(k, v) -> set store k v) (zip keys values)

  replicateM_ numThreads $ do
    mapM_ (get store) keys

-- Generate random test data
generateRandomBytes :: Int -> IO ByteString
generateRandomBytes n = do
  gen <- newStdGen
  let randomChars = take n $ randomRs ('a', 'z') gen
  return $ BS8.pack randomChars

-- Generate large test data
generateLargeDataset :: Int -> IO [(ByteString, ByteString)]
generateLargeDataset n = do
  sequence $ replicate n $ do
    key <- generateRandomBytes 32
    value <- generateRandomBytes 256
    return (key, value)
