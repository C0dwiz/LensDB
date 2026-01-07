# LensDB API Documentation

## Overview

LensDB provides a high-performance, resident NoSQL key-value database system with a clean, type-safe API. This documentation covers the client library and high-level API for integrating LensDB into your applications.

## Table of Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [Client API](#client-api)
- [High-Level API](#high-level-api)
- [Data Types](#data-types)
- [Error Handling](#error-handling)
- [Advanced Features](#advanced-features)
- [Examples](#examples)

## Installation

### Using Cabal

```bash
cabal install lensdb
```

### Using Stack

```bash
stack install lensdb
```

### From Source

```bash
git clone https://github.com/C0dWiz/lensdb.git
cd lensdb
cabal build
cabal install
```

## Quick Start

### Basic Usage

```haskell
import LensDB.API
import LensDB.Client

-- Using high-level API
main :: IO ()
main = do
    -- Open database
    result <- openDatabase defaultDatabaseConfig
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right db -> do
            -- Set a value
            setValue db "user:123" "{\"name\":\"John\",\"age\":30}"
            
            -- Get the value
            getValue db "user:123" >>= print
            
            -- Close database
            closeDatabase db

-- Using client API
mainClient :: IO ()
mainClient = do
    -- Connect to server
    result <- connect defaultClientConfig
    case result of
        Left err -> putStrLn $ "Connection error: " ++ show err
        Right client -> do
            -- Set a value
            set client "key" "value" >>= print
            
            -- Get the value
            get client "key" >>= print
            
            -- Disconnect
            disconnect client
```

## Client API

The client API provides network-based access to LensDB servers.

### Configuration

```haskell
data ClientConfig = ClientConfig
    { ccHost :: String        -- Server host (default: "127.0.0.1")
    , ccPort :: Word16       -- Server port (default: 8080)
    , ccTimeout :: Int       -- Connection timeout in seconds (default: 30)
    , ccMaxRetries :: Int   -- Maximum retries (default: 3)
    , ccBufferSize :: Int   -- Buffer size (default: 4096)
    }

defaultClientConfig :: ClientConfig
```

### Client Connection Management

```haskell
-- Connect to server
connect :: ClientConfig -> IO (Either ClientError LensDBClient)

-- Disconnect from server
disconnect :: LensDBClient -> IO (Either ClientError ())

-- Execute operation with automatic connection management
withClient :: ClientConfig -> (LensDBClient -> IO a) -> IO (Either ClientError a)
```

### Client Basic Operations

```haskell
-- Get value by key
get :: LensDBClient -> ByteString -> IO (OperationResult ByteString)

-- Set key-value pair
set :: LensDBClient -> ByteString -> ByteString -> IO (OperationResult ())

-- Delete key
delete :: LensDBClient -> ByteString -> IO (OperationResult ())

-- Check if key exists
exists :: LensDBClient -> ByteString -> IO (OperationResult Bool)

-- Get all keys
keys :: LensDBClient -> IO (OperationResult [ByteString])

-- Get database size
size :: LensDBClient -> IO (OperationResult Int)

-- Clear all data
clear :: LensDBClient -> IO (OperationResult ())

-- Ping server
ping :: LensDBClient -> IO (OperationResult ByteString)
```

### Client Batch Operations

```haskell
-- Multiple get
mget :: LensDBClient -> [ByteString] -> IO (OperationResult [(ByteString, ByteString)])

-- Multiple set
mset :: LensDBClient -> [(ByteString, ByteString)] -> IO (OperationResult ())

-- Multiple delete
mdelete :: LensDBClient -> [ByteString] -> IO (OperationResult ())
```

### Client Advanced Operations

```haskell
-- Transaction (placeholder)
transaction :: LensDBClient -> [(ByteString, Maybe ByteString)] -> IO (OperationResult ())

-- Scan with pattern (placeholder)
scan :: LensDBClient -> ByteString -> IO (OperationResult [ByteString])

-- Get server info (placeholder)
info :: LensDBClient -> IO (OperationResult [(String, String)])
```

## High-Level API

The high-level API provides in-memory database operations with optional persistence.

### Configuration

```haskell
data DatabaseConfig = DatabaseConfig
    { dbMemoryLimit :: Int        -- Memory limit in bytes (default: 1GB)
    , dbKeyLimit :: Int          -- Maximum keys (default: unlimited)
    , dbDataDir :: FilePath      -- Data directory (default: "./data")
    , dbPersistenceEnabled :: Bool -- Enable persistence (default: True)
    , dbLogLevel :: String       -- Log level (default: "info")
    , dbBackupInterval :: Int    -- Backup interval in seconds (default: 3600)
    }

defaultDatabaseConfig :: DatabaseConfig
```

### Database Management

```haskell
-- Open database
openDatabase :: DatabaseConfig -> IO (Result Database)

-- Close database
closeDatabase :: Database -> IO (Result ())

-- Execute with automatic cleanup
withDatabase :: DatabaseConfig -> (Database -> IO a) -> IO (Result a)
```

### Database Core Operations

```haskell
-- Get value
getValue :: Database -> Key -> IO (Result Value)

-- Set value
setValue :: Database -> Key -> Value -> IO (Result ())

-- Delete value
deleteValue :: Database -> Key -> IO (Result ())

-- Check key existence
hasKey :: Database -> Key -> IO (Result Bool)

-- Get all keys
getAllKeys :: Database -> IO (Result [Key])

-- Get database size
getDatabaseSize :: Database -> IO (Result Int)

-- Clear database
clearDatabase :: Database -> IO (Result ())
```

### Database Advanced Operations

```haskell
-- Query with options
query :: Database -> QueryOptions -> IO (Result [(Key, Value)])

-- Transaction
transaction :: Database -> [(Key, Maybe Value)] -> IO (Result ())

-- Backup
backup :: Database -> IO (Result FilePath)

-- Restore
restore :: Database -> FilePath -> IO (Result ())

-- Get statistics
getStatistics :: Database -> IO (Result DatabaseStats)
```

## Data Types

### Key and Value Types

```haskell
type Key = ByteString
type Value = ByteString
```

### Result Types

```haskell
-- Client operation result
data OperationResult a
    = Success a
    | Failure ClientError

-- Database operation result
type Result a = Either DBError a
```

### Error Types

```haskell
-- Client errors
data ClientError
    = ConnectionError String
    | ProtocolError String
    | ServerError ResponseStatus ByteString
    | TimeoutError
    | RetryExhausted String

-- Database errors
type DBError = String
```

### Query Options

```haskell
data QueryOptions = QueryOptions
    { qoLimit :: Int        -- Maximum results (default: 100)
    , qoOffset :: Int       -- Skip results (default: 0)
    , qoPattern :: ByteString -- Key pattern (default: "")
    , qoReverse :: Bool     -- Reverse order (default: False)
    }

defaultQueryOptions :: QueryOptions
```

### Statistics

```haskell
data DatabaseStats = DatabaseStats
    { dsReadOps :: Word64    -- Number of read operations
    , dsWriteOps :: Word64   -- Number of write operations
    , dsDeleteOps :: Word64  -- Number of delete operations
    , dsTotalKeys :: Int     -- Total number of keys
    , dsTotalSize :: Int     -- Total storage size
    , dsLastAccess :: UTCTime -- Last access time
    }
```

## Error Handling

### Error Handling Types

```haskell
-- Handle client errors
handleClientError :: ClientError -> IO ()
handleClientError err = case err of
    ConnectionError msg -> putStrLn $ "Connection failed: " ++ msg
    ProtocolError msg -> putStrLn $ "Protocol error: " ++ msg
    ServerError status msg -> putStrLn $ "Server error: " ++ show status ++ " - " ++ BS.unpack msg
    TimeoutError -> putStrLn "Operation timed out"
    RetryExhausted msg -> putStrLn $ "Retries exhausted: " ++ msg
```

### Database Error Handling

```haskell
-- Handle database errors
handleDatabaseError :: DBError -> IO ()
handleDatabaseError err = putStrLn $ "Database error: " ++ err
```

### Result Pattern Matching

```haskell
-- Pattern matching on results
case operation of
    Success result -> -- handle success
    Failure err -> handleClientError err

case databaseOperation of
    Right result -> -- handle success
    Left err -> handleDatabaseError err
```

## Advanced Features

### Transactions

```haskell
-- Execute transaction
let operations = 
        [ ("key1", Just "value1")    -- Set key1
        , ("key2", Nothing)          -- Delete key2
        , ("key3", Just "value3")    -- Set key3
        ]

result <- transaction db operations
```

### Querying

```haskell
-- Query with options
let options = QueryOptions
        { qoLimit = 50
        , qoOffset = 10
        , qoPattern = "user:"
        , qoReverse = True
        }

result <- query db options
```

### Batch Operations

```haskell
-- Batch set
let pairs = [("key1", "value1"), ("key2", "value2")]
result <- mset client pairs

-- Batch get
let keys = ["key1", "key2", "key3"]
result <- mget client keys
```

## Examples

### Simple Cache

```haskell
import LensDB.API

simpleCache :: IO ()
simpleCache = do
    db <- openDatabase defaultDatabaseConfig
    case db of
        Left err -> putStrLn $ "Failed to open cache: " ++ err
        Right cache -> do
            -- Store value
            setValue cache "user:123" "John Doe"
            
            -- Retrieve value
            getValue cache "user:123" >>= \case
                Right name -> putStrLn $ "User name: " ++ show name
                Left err -> putStrLn $ "Error: " ++ err
            
            closeDatabase cache
```

### Session Store

```haskell
import LensDB.Client

sessionStore :: IO ()
sessionStore = do
    client <- connect defaultClientConfig
    case client of
        Left err -> putStrLn $ "Connection failed: " ++ show err
        Right conn -> do
            -- Store session
            set conn "session:abc123" "{\"user_id\":123,\"expires\":1234567890}"
            
            -- Retrieve session
            get conn "session:abc123" >>= \case
                Success session -> putStrLn $ "Session: " ++ show session
                Failure err -> putStrLn $ "Error: " ++ show err
            
            disconnect conn
```

### Configuration Management

```haskell
-- Custom configuration
customConfig :: DatabaseConfig
customConfig = DatabaseConfig
    { dbMemoryLimit = 512 * 1024 * 1024  -- 512MB
    , dbKeyLimit = 10000
    , dbDataDir = "/var/lib/lensdb"
    , dbPersistenceEnabled = True
    , dbLogLevel = "debug"
    , dbBackupInterval = 1800  -- 30 minutes
    }

-- Use custom configuration
main :: IO ()
main = do
    withDatabase customConfig $ \db -> do
        setValue db "config:app_name" "MyApplication"
        getValue db "config:app_name" >>= print
```

### Performance Best Practices

```haskell
-- Memory Usage
- Use appropriate memory limits based on your use case
- Monitor memory usage with `getStatistics`
- Consider using smaller values for better cache efficiency

-- Network Operations
- Use batch operations (`mget`, `mset`) for better performance
- Configure appropriate timeouts and buffer sizes
- Implement retry logic for network failures

-- Persistence
- Enable persistence for data durability
- Configure backup intervals based on your recovery requirements
- Monitor disk space usage
```

## Best Practices

1. **Error Handling**: Always handle both success and error cases
2. **Resource Management**: Use `withDatabase` and `withClient` for automatic cleanup
3. **Batch Operations**: Use batch operations for better performance
4. **Configuration**: Choose appropriate limits and settings for your use case
5. **Monitoring**: Use statistics to monitor database performance
6. **Testing**: Test error scenarios and edge cases

## Troubleshooting

### Common Issues

1. **Connection Errors**: Check server status and network connectivity
2. **Memory Limits**: Increase memory limit or optimize data usage
3. **Performance**: Use batch operations and optimize query patterns
4. **Persistence**: Ensure data directory exists and is writable

### Debug Mode

Enable debug logging for troubleshooting:

```haskell
debugConfig :: DatabaseConfig
debugConfig = defaultDatabaseConfig { dbLogLevel = "debug" }
```

## Support

- **GitHub Repository**: <https://github.com/C0dWiz/lensdb>
- **Issues**: <https://github.com/C0dWiz/lensdb/issues>
- **Documentation**: <https://github.com/C0dWiz/lensdb/docs>
