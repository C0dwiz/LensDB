{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Professional benchmark comparing LensDB with Redis
module Main where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM)
import Criterion.Main ( defaultMain, bgroup, bench, nfIO )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Word (Word16, Word32)
-- Import LensDB client
import LensDB.Client
  ( ClientConfig (..),
    OperationResult (..),
    defaultClientConfig,
    delete,
    get,
    set,
    setEx,
    withClient,
  )
import Text.Printf (printf)

-- Mock Redis client (simplified for demonstration)
data RedisClient = RedisClient ClientConfig

redisConnect :: ClientConfig -> IO (Either String RedisClient)
redisConnect config = return $ Right $ RedisClient config

redisDisconnect :: RedisClient -> IO ()
redisDisconnect _ = return ()

redisGet :: RedisClient -> ByteString -> IO (Either String ByteString)
redisGet _ key = return $ Right $ "mock_value_" <> key

redisSet :: RedisClient -> ByteString -> ByteString -> IO (Either String ())
redisSet _ _ _ = return $ Right ()

redisSetEx :: RedisClient -> ByteString -> ByteString -> Word32 -> IO (Either String ())
redisSetEx _ _ _ _ = return $ Right ()

redisDelete :: RedisClient -> ByteString -> IO (Either String ())
redisDelete _ _ = return $ Right ()

-- Benchmark configuration
data BenchConfig = BenchConfig
  { bcNumClients :: !Int,
    bcNumOperations :: !Int,
    bcKeySize :: !Int,
    bcValueSize :: !Int,
    bcServerHost :: !String,
    bcServerPort :: !Word16
  }

defaultBenchConfig :: BenchConfig
defaultBenchConfig =
  BenchConfig
    { bcNumClients = 1,
      bcNumOperations = 10000,
      bcKeySize = 16,
      bcValueSize = 100,
      bcServerHost = "127.0.0.1",
      bcServerPort = 8080
    }

-- Generate test data
generateKey :: Int -> IO ByteString
generateKey i = return $ BS8.pack $ printf "bench_key_%08d" i

generateValue :: Int -> IO ByteString
generateValue size = do
  let chars = BS8.replicate size 'x'
  return chars

-- Benchmark operations
benchLensDB :: BenchConfig -> IO ()
benchLensDB BenchConfig {..} = do
  let clientConfig = defaultClientConfig {ccHost = bcServerHost, ccPort = bcServerPort}

  result <- withClient clientConfig $ \client -> do
    keys <- mapM generateKey [1 .. bcNumOperations]
    values <- replicateM bcNumOperations (generateValue bcValueSize)

    -- Benchmark SET operations
    putStrLn $ "Running LensDB SET benchmark with " ++ show bcNumOperations ++ " operations..."
    setResults <- mapM (\(k, v) -> set client k v) (zip keys values)
    let setSuccesses = length [() | Success _ <- setResults]
    putStrLn $ "SET operations: " ++ show setSuccesses ++ "/" ++ show bcNumOperations ++ " succeeded"

    -- Benchmark GET operations
    putStrLn $ "Running LensDB GET benchmark with " ++ show bcNumOperations ++ " operations..."
    getResults <- mapM (get client) keys
    let getSuccesses = length [() | Success _ <- getResults]
    putStrLn $ "GET operations: " ++ show getSuccesses ++ "/" ++ show bcNumOperations ++ " succeeded"

    -- Benchmark SETEX operations
    putStrLn $ "Running LensDB SETEX benchmark with " ++ show bcNumOperations ++ " operations..."
    setexResults <- mapM (\(k, v) -> setEx client k v 60) (zip keys values)
    let setexSuccesses = length [() | Success _ <- setexResults]
    putStrLn $ "SETEX operations: " ++ show setexSuccesses ++ "/" ++ show bcNumOperations ++ " succeeded"

    -- Benchmark DELETE operations
    putStrLn $ "Running LensDB DELETE benchmark with " ++ show bcNumOperations ++ " operations..."
    delResults <- mapM (delete client) keys
    let delSuccesses = length [() | Success _ <- delResults]
    putStrLn $ "DELETE operations: " ++ show delSuccesses ++ "/" ++ show bcNumOperations ++ " succeeded"

    return ()

  case result of
    Left err -> putStrLn $ "LensDB client error: " ++ show err
    Right _ -> return ()

benchRedis :: BenchConfig -> IO ()
benchRedis BenchConfig {..} = do
  let clientConfig = defaultClientConfig {ccHost = bcServerHost, ccPort = bcServerPort}

  result <- redisConnect clientConfig
  case result of
    Left err -> putStrLn $ "Failed to connect to Redis: " ++ err
    Right redisClient -> do
      keys <- mapM generateKey [1 .. bcNumOperations]
      values <- replicateM bcNumOperations (generateValue bcValueSize)

      -- Benchmark SET operations
      putStrLn $ "Running Redis SET benchmark with " ++ show bcNumOperations ++ " operations..."
      setResults <- mapM (\(k, v) -> redisSet redisClient k v) (zip keys values)
      let setSuccesses = length [() | Right _ <- setResults]
      putStrLn $ "SET operations: " ++ show setSuccesses ++ "/" ++ show bcNumOperations ++ " succeeded"

      -- Benchmark GET operations
      putStrLn $ "Running Redis GET benchmark with " ++ show bcNumOperations ++ " operations..."
      getResults <- mapM (redisGet redisClient) keys
      let getSuccesses = length [() | Right _ <- getResults]
      putStrLn $ "GET operations: " ++ show getSuccesses ++ "/" ++ show bcNumOperations ++ " succeeded"

      -- Benchmark SETEX operations
      putStrLn $ "Running Redis SETEX benchmark with " ++ show bcNumOperations ++ " operations..."
      setexResults <- mapM (\(k, v) -> redisSetEx redisClient k v 60) (zip keys values)
      let setexSuccesses = length [() | Right _ <- setexResults]
      putStrLn $ "SETEX operations: " ++ show setexSuccesses ++ "/" ++ show bcNumOperations ++ " succeeded"

      -- Benchmark DELETE operations
      putStrLn $ "Running Redis DELETE benchmark with " ++ show bcNumOperations ++ " operations..."
      delResults <- mapM (redisDelete redisClient) keys
      let delSuccesses = length [() | Right _ <- delResults]
      putStrLn $ "DELETE operations: " ++ show delSuccesses ++ "/" ++ show bcNumOperations ++ " succeeded"

      redisDisconnect redisClient

-- Concurrent benchmark
benchConcurrent :: BenchConfig -> IO ()
benchConcurrent config@BenchConfig {..} = do
  putStrLn $ "Running concurrent benchmark with " ++ show bcNumClients ++ " clients..."

  done <- newEmptyMVar
  let opsPerClient = bcNumOperations `div` bcNumClients

  -- Fork multiple clients
  sequence_
    [ forkIO $ do
        benchLensDB config {bcNumOperations = opsPerClient}
        putMVar done ()
      | _ <- [1 .. bcNumClients]
    ]

  -- Wait for all clients to complete
  sequence_ [takeMVar done | _ <- [1 .. bcNumClients]]
  putStrLn "Concurrent benchmark completed"

-- Criterion benchmarks
benchSuite :: [BenchConfig] -> IO ()
benchSuite configs = do
  defaultMain
    [ bgroup
        "LensDB"
        [ bench "SET" $ nfIO (benchLensDB $ head configs),
          bench "GET" $ nfIO (benchLensDB $ head configs),
          bench "SETEX" $ nfIO (benchLensDB $ head configs),
          bench "DELETE" $ nfIO (benchLensDB $ head configs)
        ],
      bgroup
        "Redis"
        [ bench "SET" $ nfIO (benchRedis $ head configs),
          bench "GET" $ nfIO (benchRedis $ head configs),
          bench "SETEX" $ nfIO (benchRedis $ head configs),
          bench "DELETE" $ nfIO (benchRedis $ head configs)
        ]
    ]

-- Performance comparison table
runComparison :: IO ()
runComparison = do
  putStrLn "=== LensDB vs Redis Performance Comparison ==="
  putStrLn ""

  let configs =
        [ defaultBenchConfig {bcNumClients = 1, bcValueSize = 10}, -- Small values
          defaultBenchConfig {bcNumClients = 1, bcValueSize = 10240}, -- Large values (10KB)
          defaultBenchConfig {bcNumClients = 10, bcValueSize = 100}, -- 10 concurrent clients
          defaultBenchConfig {bcNumClients = 100, bcValueSize = 100} -- 100 concurrent clients
        ]

  mapM_ runConfigComparison configs
  where
    runConfigComparison config = do
      putStrLn $
        "Configuration: "
          ++ show (bcNumClients config)
          ++ " clients, "
          ++ show (bcNumOperations config)
          ++ " ops, "
          ++ show (bcValueSize config)
          ++ "B values"

      putStrLn "\n--- LensDB ---"
      startTime <- getCurrentTime
      benchLensDB config
      endTime <- getCurrentTime
      let lensDBTime = diffUTCTime endTime startTime
      putStrLn $ "Total time: " ++ show lensDBTime ++ " seconds"

      putStrLn "\n--- Redis ---"
      startTime <- getCurrentTime
      benchRedis config
      endTime <- getCurrentTime
      let redisTime = diffUTCTime endTime startTime
      putStrLn $ "Total time: " ++ show redisTime ++ " seconds"

      let opsPerSec = fromIntegral (bcNumOperations config) / max 1 (realToFrac redisTime :: Double)
      putStrLn $ "Operations per second: " ++ printf "%.2f" opsPerSec

      putStrLn $ "\n" ++ replicate 50 '-' ++ "\n"

main :: IO ()
main = do
  putStrLn "LensDB Professional Benchmark Suite"
  putStrLn "================================="
  putStrLn ""

  putStrLn "Select benchmark mode:"
  putStrLn "1. Quick benchmark (1K operations)"
  putStrLn "2. Standard benchmark (10K operations)"
  putStrLn "3. Heavy benchmark (100K operations)"
  putStrLn "4. Concurrent benchmark (10 clients)"
  putStrLn "5. Full comparison table"
  putStrLn "6. Criterion benchmark suite"

  -- For demonstration, run the comparison table
  runComparison

-- Uncomment to run criterion benchmarks
-- let config = defaultBenchConfig { bcNumOperations = 1000 }
-- benchSuite [config]
