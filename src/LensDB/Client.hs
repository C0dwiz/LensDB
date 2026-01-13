{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: LensDB.Client
-- Description: High-level client library for LensDB
-- Copyright: (c) 2026, C0dWiz
-- License: BSD-3-Clause
--
-- This module provides a high-level client interface for interacting with LensDB
-- servers. It handles connection management, protocol communication, and provides
-- a clean API for common operations.
module LensDB.Client
  ( -- * Client Types
    LensDBClient (..),
    ClientConfig (..),
    ClientError (..),
    OperationResult (..),
    defaultClientConfig,

    -- * Client Operations
    connect,
    disconnect,
    withClient,

    -- * Database Operations
    get,
    set,
    setEx,
    delete,
    exists,
    keys,
    size,
    clear,
    ping,

    -- * Batch Operations
    mget,
    mset,
    mdelete,

    -- * Advanced Operations
    transaction,
    scan,
    info,
  )
where

import Control.Concurrent (MVar, forkIO, newMVar, putMVar, readMVar, takeMVar, threadDelay, tryTakeMVar)
import Control.Exception (SomeException, bracket, catch, finally, throwIO, try)
import Control.Monad (forever, replicateM, unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft, isRight)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import LensDB.Protocol
  ( MessageType (..),
    ProtocolMessage (..),
    ProtocolResponse (..),
    ResponseStatus (..),
    createDelete,
    createErrorResponse,
    createGet,
    createPing,
    createSet,
    createSetEx,
    createSuccessResponse,
    decodeMessage,
    decodeResponse,
    encodeMessage,
    encodeResponse,
    magicNumber,
    protocolVersion,
  )
import Network.Socket (Family (AF_INET), SockAddr, Socket, SocketType (Stream))
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)

-- | Client configuration
data ClientConfig = ClientConfig
  { -- | Server host
    ccHost :: !String,
    -- | Server port
    ccPort :: !Word16,
    -- | Connection timeout in seconds
    ccTimeout :: !Int,
    -- | Maximum number of retries for failed operations
    ccMaxRetries :: !Int,
    -- | Buffer size for network operations
    ccBufferSize :: !Int
  }
  deriving (Show, Eq, Generic)

-- | Default client configuration
defaultClientConfig :: ClientConfig
defaultClientConfig =
  ClientConfig
    { ccHost = "127.0.0.1",
      ccPort = 8080,
      ccTimeout = 30,
      ccMaxRetries = 3,
      ccBufferSize = 4096
    }

-- | Client errors
data ClientError
  = -- | Connection-related error
    ConnectionError !String
  | -- | Protocol parsing error
    ProtocolError !String
  | -- | Server returned error
    ServerError !ResponseStatus !ByteString
  | -- | Operation timed out
    TimeoutError
  | -- | Maximum retries exceeded
    RetryExhausted !String
  deriving (Show, Eq, Generic)

-- | Operation result
data OperationResult a
  = -- | Operation succeeded
    Success !a
  | -- | Operation failed
    Failure !ClientError
  deriving (Show, Eq)

-- | LensDB client
data LensDBClient = LensDBClient
  { -- | Client configuration
    clientConfig :: !ClientConfig,
    -- | Network socket (Nothing if disconnected)
    clientSocket :: !(MVar (Maybe Socket)),
    -- | Connection status
    clientConnected :: !(MVar Bool),
    -- | Message sequence number
    clientSequence :: !(MVar Word32)
  }

-- | Create a new client
newClient :: ClientConfig -> IO LensDBClient
newClient config = do
  socketVar <- newMVar Nothing
  connectedVar <- newMVar False
  sequenceVar <- newMVar 0

  return
    LensDBClient
      { clientConfig = config,
        clientSocket = socketVar,
        clientConnected = connectedVar,
        clientSequence = sequenceVar
      }

-- | Connect to LensDB server
connect :: ClientConfig -> IO (Either ClientError LensDBClient)
connect config = do
  client <- newClient config

  result <- try $ do
    -- Get server address
    addrInfo <- NS.getAddrInfo Nothing (Just $ ccHost config) (Just $ show $ ccPort config)
    let serverAddr = head addrInfo

    -- Create and connect socket
    sock <- NS.socket (NS.addrFamily serverAddr) Stream NS.defaultProtocol
    NS.connect sock (NS.addrAddress serverAddr)

    -- Update client state
    takeMVar (clientSocket client)
    putMVar (clientSocket client) (Just sock)
    takeMVar (clientConnected client)
    putMVar (clientConnected client) True

    return client

  case result of
    Left err -> return $ Left $ ConnectionError $ show (err :: SomeException)
    Right client -> return $ Right client

-- | Disconnect from server
disconnect :: LensDBClient -> IO (Either ClientError ())
disconnect client = do
  result <- try $ do
    maybeSocket <- takeMVar (clientSocket client)
    case maybeSocket of
      Just sock -> do
        NS.close sock
        putMVar (clientSocket client) Nothing
      Nothing -> return ()

    takeMVar (clientConnected client)
    putMVar (clientConnected client) False

    return ()

  case result of
    Left err -> return $ Left $ ConnectionError $ show (err :: SomeException)
    Right _ -> return $ Right ()

-- | Execute operation with automatic connection management
withClient :: ClientConfig -> (LensDBClient -> IO a) -> IO (Either ClientError a)
withClient config action = do
  result <- LensDB.Client.connect config
  case result of
    Left err -> return $ Left err
    Right client -> do
      actionResult <- try $ action client
      disconnect client

      case actionResult of
        Left err -> return $ Left $ ConnectionError $ show (err :: SomeException)
        Right result -> return $ Right result

-- | Execute a single operation with retry logic
executeOperation :: LensDBClient -> ProtocolMessage -> IO (Either ClientError ProtocolResponse)
executeOperation client@LensDBClient {..} message = do
  let maxRetries = ccMaxRetries clientConfig
  executeWithRetry maxRetries message
  where
    executeWithRetry :: Int -> ProtocolMessage -> IO (Either ClientError ProtocolResponse)
    executeWithRetry 0 _ = return $ Left $ RetryExhausted "Maximum retries exceeded"
    executeWithRetry retries msg = do
      result <- tryExecuteOperation client msg
      case result of
        Right response -> return $ Right response
        Left err | retries > 0 -> do
          threadDelay 1000000 -- Wait 1 second before retry
          executeWithRetry (retries - 1) msg
        Left err -> return $ Left err

-- | Execute operation without retry
tryExecuteOperation :: LensDBClient -> ProtocolMessage -> IO (Either ClientError ProtocolResponse)
tryExecuteOperation client@LensDBClient {..} message = do
  -- Check if connected
  connected <- readMVar clientConnected
  if not connected
    then return $ Left $ ConnectionError "Not connected to server"
    else do
      maybeSocket <- readMVar clientSocket
      case maybeSocket of
        Nothing -> return $ Left $ ConnectionError "No socket available"
        Just sock -> do
          -- Send message
          let messageBytes = LBS.toStrict $ encodeMessage message
          sendResult <- try $ sendAll sock messageBytes

          case sendResult of
            Left err -> return $ Left $ ConnectionError $ show (err :: SomeException)
            Right _ -> do
              -- Receive response
              responseBytes <- try $ recv sock (ccBufferSize clientConfig)
              case responseBytes of
                Left err -> return $ Left $ ConnectionError $ show (err :: SomeException)
                Right "" -> return $ Left $ ConnectionError "Connection closed by server"
                Right bytes -> do
                  -- Parse response
                  case decodeResponse (LBS.fromStrict bytes) of
                    Left err -> return $ Left $ ProtocolError err
                    Right response -> return $ Right response

-- | Get value by key
get :: LensDBClient -> ByteString -> IO (OperationResult ByteString)
get client key = do
  let message = createGet key
  result <- executeOperation client message

  case result of
    Left err -> return $ Failure err
    Right response -> case respStatus response of
      StatusSuccess -> return $ Success $ respData response
      StatusKeyNotFound -> return $ Failure $ ServerError StatusKeyNotFound "Key not found"
      status -> return $ Failure $ ServerError status (respMessage response)

-- | Set key-value pair
set :: LensDBClient -> ByteString -> ByteString -> IO (OperationResult ())
set client key value = do
  let message = createSet key value
  result <- executeOperation client message

  case result of
    Left err -> return $ Failure err
    Right response -> case respStatus response of
      StatusSuccess -> return $ Success ()
      status -> return $ Failure $ ServerError status (respMessage response)

-- | Set key-value pair with expiration (SETEX)
setEx :: LensDBClient -> ByteString -> ByteString -> Word32 -> IO (OperationResult ())
setEx client key value ttl = do
  let message = createSetEx key value ttl
  result <- executeOperation client message

  case result of
    Left err -> return $ Failure err
    Right response -> case respStatus response of
      StatusSuccess -> return $ Success ()
      status -> return $ Failure $ ServerError status (respMessage response)

-- | Delete key
delete :: LensDBClient -> ByteString -> IO (OperationResult ())
delete client key = do
  let message = createDelete key
  result <- executeOperation client message

  case result of
    Left err -> return $ Failure err
    Right response -> case respStatus response of
      StatusSuccess -> return $ Success ()
      StatusKeyNotFound -> return $ Failure $ ServerError StatusKeyNotFound "Key not found"
      status -> return $ Failure $ ServerError status (respMessage response)

-- | Check if key exists
exists :: LensDBClient -> ByteString -> IO (OperationResult Bool)
exists client key = do
  let message =
        ProtocolMessage
          { msgMagic = magicNumber,
            msgVersion = protocolVersion,
            msgType = MsgExists,
            msgKey = key,
            msgValue = "",
            msgFlags = 0
          }

  result <- executeOperation client message

  case result of
    Left err -> return $ Failure err
    Right response -> case respStatus response of
      StatusSuccess ->
        case respData response of
          "true" -> return $ Success True
          "false" -> return $ Success False
          _ -> return $ Failure $ ProtocolError "Invalid exists response"
      status -> return $ Failure $ ServerError status (respMessage response)

-- | Get all keys
keys :: LensDBClient -> IO (OperationResult [ByteString])
keys client = do
  let message =
        ProtocolMessage
          { msgMagic = magicNumber,
            msgVersion = protocolVersion,
            msgType = MsgKeys,
            msgKey = "",
            msgValue = "",
            msgFlags = 0
          }

  result <- executeOperation client message

  case result of
    Left err -> return $ Failure err
    Right response -> case respStatus response of
      StatusSuccess ->
        let keysList = BS.lines $ respData response
         in return $ Success keysList
      status -> return $ Failure $ ServerError status (respMessage response)

-- | Get database size
size :: LensDBClient -> IO (OperationResult Int)
size client = do
  let message =
        ProtocolMessage
          { msgMagic = magicNumber,
            msgVersion = protocolVersion,
            msgType = MsgSize,
            msgKey = "",
            msgValue = "",
            msgFlags = 0
          }

  result <- executeOperation client message

  case result of
    Left err -> return $ Failure err
    Right response -> case respStatus response of
      StatusSuccess ->
        case reads $ BS.unpack $ respData response of
          [(size, "")] -> return $ Success size
          _ -> return $ Failure $ ProtocolError "Invalid size response"
      status -> return $ Failure $ ServerError status (respMessage response)

-- | Clear all data
clear :: LensDBClient -> IO (OperationResult ())
clear client = do
  let message =
        ProtocolMessage
          { msgMagic = magicNumber,
            msgVersion = protocolVersion,
            msgType = MsgClear,
            msgKey = "",
            msgValue = "",
            msgFlags = 0
          }

  result <- executeOperation client message

  case result of
    Left err -> return $ Failure err
    Right response -> case respStatus response of
      StatusSuccess -> return $ Success ()
      status -> return $ Failure $ ServerError status (respMessage response)

-- | Ping server
ping :: LensDBClient -> IO (OperationResult ByteString)
ping client = do
  let message = createPing
  result <- executeOperation client message

  case result of
    Left err -> return $ Failure err
    Right response -> case respStatus response of
      StatusSuccess -> return $ Success $ respData response
      status -> return $ Failure $ ServerError status (respMessage response)

-- | Multiple get operation
mget :: LensDBClient -> [ByteString] -> IO (OperationResult [(ByteString, ByteString)])
mget client keys = do
  results <- mapM (get client) keys
  let successes = [(k, v) | (k, Success v) <- zip keys results]
      failures = [err | Failure err <- results]

  if null failures
    then return $ Success successes
    else return $ Failure $ head failures -- Return first error

-- | Multiple set operation
mset :: LensDBClient -> [(ByteString, ByteString)] -> IO (OperationResult ())
mset client pairs = do
  results <- mapM (\(k, v) -> set client k v) pairs
  let failures = [err | Failure err <- results]

  if null failures
    then return $ Success ()
    else return $ Failure $ head failures -- Return first error

-- | Multiple delete operation
mdelete :: LensDBClient -> [ByteString] -> IO (OperationResult ())
mdelete client keys = do
  results <- mapM (delete client) keys
  let failures = [err | Failure err <- results]

  if null failures
    then return $ Success ()
    else return $ Failure $ head failures -- Return first error

-- | Transaction operation (placeholder for future implementation)
transaction :: LensDBClient -> [(ByteString, Maybe ByteString)] -> IO (OperationResult ())
transaction client operations = do
  -- This would implement transaction support in a real implementation
  -- For now, just execute operations sequentially
  results <- mapM executeOperation operations
  return $ Success ()
  where
    executeOperation (key, Just value) = set client key value
    executeOperation (key, Nothing) = delete client key

-- | Scan keys with pattern (placeholder for future implementation)
scan :: LensDBClient -> ByteString -> IO (OperationResult [ByteString])
scan client pattern = do
  -- This would implement pattern-based scanning in a real implementation
  allKeysResult <- keys client
  case allKeysResult of
    Success allKeys -> do
      let matchingKeys = filter (BS.isInfixOf pattern) allKeys
      return $ Success matchingKeys
    Failure err -> return $ Failure err

-- | Get server information (placeholder for future implementation)
info :: LensDBClient -> IO (OperationResult [(String, String)])
info client = do
  -- This would return server information in a real implementation
  return $ Success [("version", "0.1.0"), ("status", "running")]
