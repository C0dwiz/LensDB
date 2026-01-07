{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: LensDB.Network
-- Description: Network server and client handling
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
--
-- This module provides the network layer for LensDB, including the TCP server,
-- client connection handling, and request processing.
module LensDB.Network
  ( -- * Server Types
    Server (..),
    ServerConfig (..),
    ClientConnection (..),
    ConnectionStats (..),

    -- * Server Operations
    newServer,
    startServer,
    stopServer,
    isServerRunning,

    -- * Connection Management
    acceptClients,
    handleClient,
    closeConnection,

    -- * Request Processing
    processRequest,
    sendResponse,
    receiveMessage,

    -- * Network Utilities
    withSocket,
    gracefulShutdown,
  )
where

import Control.Concurrent (MVar, forkIO, killThread, newMVar, putMVar, takeMVar, threadDelay, tryTakeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, bracket, catch, finally, throwIO, try)
import Control.Monad (forever, unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word16)
import LensDB.Core (KVStore, StorageError (..), clear, delete, exists, get, keys, set, size)
import LensDB.Protocol
  ( MessageType (..),
    ProtocolMessage (..),
    ProtocolResponse (..),
    ResponseStatus (..),
    createErrorResponse,
    createPing,
    createSuccessResponse,
    decodeMessage,
    encodeResponse,
    validateMessage,
  )
import Network.Socket (AddrInfoFlag (AI_PASSIVE), Family (AF_INET), SockAddr, Socket, SocketOption (ReuseAddr), SocketType (Stream), accept, addrAddress, addrFamily, addrFlags, addrSocketType, bind, close, defaultHints, defaultProtocol, getAddrInfo, listen, setSocketOption, socket)
import Network.Socket.ByteString (recv, sendAll)
import System.IO (Handle, hClose, hFlush)
import System.Timeout (timeout)

-- | Server configuration
data ServerConfig = ServerConfig
  { -- | Server host address
    scHost :: !String,
    -- | Server port
    scPort :: !Word16,
    -- | Maximum number of concurrent connections
    scMaxConnections :: !Int,
    -- | Connection timeout in seconds
    scConnectionTimeout :: !Int,
    -- | Buffer size for network operations
    scBufferSize :: !Int
  }
  deriving (Show, Eq)

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { scHost = "127.0.0.1",
      scPort = 8080,
      scMaxConnections = 1000,
      scConnectionTimeout = 300,
      scBufferSize = 4096
    }

-- | Connection statistics
data ConnectionStats = ConnectionStats
  { -- | Total bytes received
    csBytesReceived :: !(TVar Int),
    -- | Total bytes sent
    csBytesSent :: !(TVar Int),
    -- | Total requests processed
    csRequestsProcessed :: !(TVar Int),
    -- | Total errors encountered
    csErrors :: !(TVar Int)
  }
  deriving (Eq)

-- | Client connection information
data ClientConnection = ClientConnection
  { -- | Client socket
    ccSocket :: !Socket,
    -- | Client address
    ccAddress :: !SockAddr,
    -- | Connection statistics
    ccStats :: !ConnectionStats,
    -- | Whether connection is active
    ccActive :: !(TVar Bool)
  }
  deriving (Eq)

-- | Server state
data Server = Server
  { -- | Server configuration
    svConfig :: !ServerConfig,
    -- | Key-value store
    kvStore :: !KVStore,
    -- | Server socket (Nothing if not running)
    svSocket :: !(MVar (Maybe Socket)),
    -- | Whether server is running
    svRunning :: !(TVar Bool),
    -- | Active connections
    svConnections :: !(TVar [ClientConnection]),
    -- | Global server statistics
    svStats :: !ConnectionStats
  }
  deriving (Eq)

-- | Create a new server instance
newServer :: ServerConfig -> KVStore -> IO Server
newServer config store = do
  serverSocket <- newMVar Nothing
  running <- newTVarIO False
  connections <- newTVarIO []

  -- Initialize statistics
  bytesReceived <- newTVarIO 0
  bytesSent <- newTVarIO 0
  requestsProcessed <- newTVarIO 0
  errors <- newTVarIO 0

  let stats =
        ConnectionStats
          { csBytesReceived = bytesReceived,
            csBytesSent = bytesSent,
            csRequestsProcessed = requestsProcessed,
            csErrors = errors
          }

  return
    Server
      { svConfig = config,
        kvStore = store,
        svSocket = serverSocket,
        svRunning = running,
        svConnections = connections,
        svStats = stats
      }

-- | Start the server
startServer :: Server -> IO ()
startServer server@Server {..} = do
  -- Check if server is already running
  running <- readTVarIO svRunning
  when running $ fail "Server is already running"

  -- Create and bind socket
  let ServerConfig {..} = svConfig
  addrInfo <- getAddrInfo (Just defaultHints {addrFlags = [AI_PASSIVE]}) (Just scHost) (Just $ show scPort)
  let serverAddr = head addrInfo

  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress serverAddr)
  listen sock scMaxConnections

  -- Update server state
  putMVar svSocket (Just sock)
  atomically $ writeTVar svRunning True

  -- Start accepting clients
  _ <- forkIO $ acceptClients server sock

  return ()

-- | Stop the server
stopServer :: Server -> IO ()
stopServer Server {..} = do
  -- Set running flag to False
  atomically $ writeTVar svRunning False

  -- Close server socket
  maybeSocket <- tryTakeMVar svSocket
  case maybeSocket of
    Just (Just sock) -> close sock
    _ -> return ()

  -- Close all client connections
  connections <- readTVarIO svConnections
  mapM_ closeConnection connections

  -- Clear connections list
  atomically $ writeTVar svConnections []

-- | Check if server is running
isServerRunning :: Server -> IO Bool
isServerRunning Server {..} = readTVarIO svRunning

-- | Accept client connections
acceptClients :: Server -> Socket -> IO ()
acceptClients server@Server {..} sock = do
  let ServerConfig {..} = svConfig

  forever $ do
    running <- readTVarIO svRunning
    unless running $ fail "Server stopped"

    -- Accept new connection with timeout
    result <- timeout (scConnectionTimeout * 1000000) $ accept sock
    case result of
      Nothing -> return () -- Timeout, continue
      Just (clientSock, clientAddr) -> do
        -- Check connection limit
        currentConnections <- readTVarIO svConnections
        if length currentConnections >= scMaxConnections
          then do
            close clientSock -- Reject connection
            atomically $ modifyTVar (csErrors svStats) (+ 1)
          else do
            -- Create connection stats
            bytesReceived <- newTVarIO 0
            bytesSent <- newTVarIO 0
            requestsProcessed <- newTVarIO 0
            errors <- newTVarIO 0

            let connStats =
                  ConnectionStats
                    { csBytesReceived = bytesReceived,
                      csBytesSent = bytesSent,
                      csRequestsProcessed = requestsProcessed,
                      csErrors = errors
                    }

            active <- newTVarIO True
            let connection =
                  ClientConnection
                    { ccSocket = clientSock,
                      ccAddress = clientAddr,
                      ccStats = connStats,
                      ccActive = active
                    }

            -- Add to connections list
            atomically $ modifyTVar svConnections (connection :)

            -- Handle client in separate thread
            _ <-
              forkIO $
                handleClient server connection `finally` do
                  -- Remove from connections list when done
                  atomically $ modifyTVar svConnections (filter (/= connection))

            return ()

-- | Handle a client connection
handleClient :: Server -> ClientConnection -> IO ()
handleClient server@Server {..} connection@ClientConnection {..} = do
  let ServerConfig {..} = svConfig

  -- Handle client until connection is closed or error occurs
  handleConnection
  where
    handleConnection = do
      active <- readTVarIO ccActive
      when active $ do
        -- Receive message with timeout
        let ServerConfig {..} = svConfig
        result <- timeout (scConnectionTimeout * 1000000) $ receiveMessage ccSocket scBufferSize
        case result of
          Nothing -> do
            -- Timeout, close connection
            atomically $ writeTVar ccActive False
          Just (Left err) -> do
            -- Error receiving message
            atomically $ modifyTVar (csErrors ccStats) (+ 1)
            atomically $ modifyTVar (csErrors svStats) (+ 1)
            atomically $ writeTVar ccActive False
          Just (Right message) -> do
            -- Update received bytes
            atomically $ modifyTVar (csBytesReceived ccStats) (+ BS.length message)
            atomically $ modifyTVar (csBytesReceived svStats) (+ BS.length message)

            -- Process message
            case decodeMessage (LBS.fromStrict message) of
              Left err -> do
                -- Invalid message
                let response = createErrorResponse StatusInvalidMsg (BSC.pack err)
                sendResponse ccSocket response
                atomically $ modifyTVar (csErrors ccStats) (+ 1)
                atomically $ modifyTVar (csErrors svStats) (+ 1)
                return ()
              Right protocolMsg -> do
                -- Validate message
                if validateMessage protocolMsg
                  then do
                    -- Process request
                    response <- processRequest server protocolMsg
                    sendResponse ccSocket response
                    atomically $ modifyTVar (csRequestsProcessed ccStats) (+ 1)
                    atomically $ modifyTVar (csRequestsProcessed svStats) (+ 1)
                  else do
                    -- Invalid message format
                    let response = createErrorResponse StatusInvalidMsg "Invalid message format"
                    sendResponse ccSocket response
                    atomically $ modifyTVar (csErrors ccStats) (+ 1)
                    atomically $ modifyTVar (csErrors svStats) (+ 1)

            -- Continue handling
            handleConnection

-- | Process a protocol message and return response
processRequest :: Server -> ProtocolMessage -> IO ProtocolResponse
processRequest Server {..} message = case msgType message of
  MsgPing -> return $ createSuccessResponse "pong"
  MsgGet -> do
    result <- get kvStore (msgKey message)
    case result of
      Left (KeyNotFound _) -> return $ createErrorResponse StatusKeyNotFound "Key not found"
      Left err -> return $ createErrorResponse StatusInternalError (BSC.pack $ show err)
      Right value -> return $ createSuccessResponse value
  MsgSet -> do
    result <- set kvStore (msgKey message) (msgValue message)
    case result of
      Left err -> return $ createErrorResponse StatusInternalError (BSC.pack $ show err)
      Right _ -> return $ createSuccessResponse "OK"
  MsgDelete -> do
    result <- delete kvStore (msgKey message)
    case result of
      Left (KeyNotFound _) -> return $ createErrorResponse StatusKeyNotFound "Key not found"
      Left err -> return $ createErrorResponse StatusInternalError (BSC.pack $ show err)
      Right _ -> return $ createSuccessResponse "OK"
  MsgExists -> do
    keyExists <- exists kvStore (msgKey message)
    let response = if keyExists then "true" else "false"
    return $ createSuccessResponse response
  MsgKeys -> do
    allKeys <- keys kvStore
    let keysStr = BS.intercalate (BSC.pack "\n") allKeys
    return $ createSuccessResponse keysStr
  MsgSize -> do
    currentSize <- size kvStore
    let sizeStr = BSC.pack $ show currentSize
    return $ createSuccessResponse sizeStr
  MsgClear -> do
    clear kvStore
    return $ createSuccessResponse "OK"
  MsgStats -> do
    -- Return basic stats (would be expanded in real implementation)
    let stats = "Basic statistics placeholder"
    return $ createSuccessResponse stats

-- | Send a response to client
sendResponse :: Socket -> ProtocolResponse -> IO ()
sendResponse sock response = do
  let responseBytes = LBS.toStrict $ encodeResponse response
  sendAll sock responseBytes

-- | Receive a message from client
receiveMessage :: Socket -> Int -> IO (Either String ByteString)
receiveMessage sock bufferSize = do
  result <- try $ recv sock bufferSize
  case result of
    Left err -> return $ Left $ show (err :: SomeException)
    Right "" -> return $ Left "Connection closed"
    Right bytes -> return $ Right bytes

-- | Close a client connection
closeConnection :: ClientConnection -> IO ()
closeConnection ClientConnection {..} = do
  atomically $ writeTVar ccActive False
  close ccSocket

-- | Graceful shutdown with timeout
gracefulShutdown :: Server -> Int -> IO ()
gracefulShutdown server timeoutSeconds = do
  -- Stop accepting new connections
  stopServer server

  -- Wait for existing connections to finish (with timeout)
  let maxWait = timeoutSeconds * 1000000 -- Convert to microseconds
  result <- timeout maxWait $ waitForConnections server

  case result of
    Nothing -> do
      -- Timeout reached, force close remaining connections
      connections <- readTVarIO (svConnections server)
      mapM_ closeConnection connections
    Just _ -> return ()
  where
    waitForConnections :: Server -> IO ()
    waitForConnections Server {..} = do
      connections <- readTVarIO svConnections
      if null connections
        then return ()
        else do
          threadDelay 100000 -- Wait 100ms
          waitForConnections server

-- | Utility for working with sockets
withSocket :: Family -> SocketType -> String -> String -> (Socket -> IO a) -> IO a
withSocket family sockType host port action = do
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)
  let serverAddr = head addrInfo

  bracket
    (socket (addrFamily serverAddr) sockType defaultProtocol)
    close
    action
