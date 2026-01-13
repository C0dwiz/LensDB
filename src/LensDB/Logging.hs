{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: LensDB.Logging
-- Description: Logging system for LensDB
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
--
-- This module provides a comprehensive logging system for LensDB with support
-- for multiple log levels, output destinations, and log formats.
module LensDB.Logging
  ( -- * Logging Types
    Logger (..),
    LogLevel (..),
    LogRecord (..),
    LoggingConfig (..),
    LogFormat (..),

    -- * Logger Operations
    defaultLoggingConfig,
    newLogger,
    logMessage,
    logDebug,
    logInfo,
    logWarn,
    logError,

    -- * Log Formatting
    formatJson,
    formatText,

    -- * Log Management
    rotateLogs,
    flushLogs,
    closeLogger,
  )
where

import Control.Concurrent (MVar, forkIO, newMVar, putMVar, takeMVar, threadDelay, tryTakeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, bracket, catch, try)
import Control.Monad (forever, unless, when)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Word (Word16)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName, (-<.>), (</>))
import System.IO (Handle, IOMode (AppendMode), hClose, hFlush, hPutStrLn, openFile, stderr, stdout)

-- | Log levels in order of severity
data LogLevel
  = LogLevelDebug
  | LogLevelInfo
  | LogLevelWarn
  | LogLevelError
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON LogLevel

-- | Convert log level to string
logLevelToString :: LogLevel -> String
logLevelToString LogLevelDebug = "DEBUG"
logLevelToString LogLevelInfo = "INFO"
logLevelToString LogLevelWarn = "WARN"
logLevelToString LogLevelError = "ERROR"

-- | Convert string to log level
stringToLogLevel :: String -> Maybe LogLevel
stringToLogLevel s = case map toLower s of
  "debug" -> Just LogLevelDebug
  "info" -> Just LogLevelInfo
  "warn" -> Just LogLevelWarn
  "error" -> Just LogLevelError
  _ -> Nothing

-- | Log record structure
data LogRecord = LogRecord
  { -- | Timestamp of the log entry
    lrTimestamp :: !UTCTime,
    -- | Log level
    lrLevel :: !LogLevel,
    -- | Log message
    lrMessage :: !String,
    -- | Module name (optional)
    lrModule :: !String,
    -- | Function name (optional)
    lrFunction :: !String,
    -- | Thread ID (optional)
    lrThread :: !String,
    -- | Additional metadata
    lrMetadata :: ![(String, String)]
  }
  deriving (Show, Eq, Generic)

instance ToJSON LogRecord

-- | Logger configuration
data LoggingConfig = LoggingConfig
  { -- | Minimum log level to output
    lcLevel :: !LogLevel,
    -- | Log file path (Nothing = stdout)
    lcFile :: !(Maybe FilePath),
    -- | Maximum log file size in bytes
    lcMaxSize :: !Int,
    -- | Number of backup files to keep
    lcBackupCount :: !Int,
    -- | Log format
    lcFormat :: !LogFormat,
    -- | Whether to use async logging
    lcAsync :: !Bool,
    -- | Buffer size for async logging
    lcBufferSize :: !Int
  }
  deriving (Show, Eq, Generic)

-- | Log format types
data LogFormat
  = LogFormatText
  | LogFormatJson
  deriving (Show, Eq, Generic)

-- | Logger state
data Logger = Logger
  { -- | Logger configuration
    lgConfig :: !LoggingConfig,
    -- | File handle (Nothing = stdout)
    lgHandle :: !(Maybe Handle),
    -- | Current buffer size
    lgBufferSize :: !(TVar Int),
    -- | Total bytes written
    lgTotalWritten :: !(TVar Int),
    -- | Async log queue
    lgQueue :: !(TVar [LogRecord]),
    -- | Trigger for async flush
    lgFlushTrigger :: !(MVar ())
  }
  deriving (Eq)

-- | Create a new logger
newLogger :: LoggingConfig -> IO Logger
newLogger config@LoggingConfig {..} = do
  -- Open log file or use stdout
  handle <- case lcFile of
    Just filePath -> do
      -- Create directory if it doesn't exist
      createDirectoryIfMissing True (takeDirectory filePath)
      openFile filePath AppendMode
    Nothing -> return stdout

  -- Initialize state
  bufferSize <- newTVarIO 0
  totalWritten <- newTVarIO 0
  queue <- newTVarIO []
  flushTrigger <- newMVar ()

  let logger =
        Logger
          { lgConfig = config,
            lgHandle = Just handle,
            lgBufferSize = bufferSize,
            lgTotalWritten = totalWritten,
            lgQueue = queue,
            lgFlushTrigger = flushTrigger
          }

  -- Start async logging worker if enabled
  when lcAsync $ do
    _ <- forkIO $ asyncLogWorker logger
    return ()

  return logger

-- | Log a message at the specified level
logMessage :: Logger -> LogLevel -> String -> IO ()
logMessage logger@Logger {..} level message = do
  now <- getCurrentTime
  let record =
        LogRecord
          { lrTimestamp = now,
            lrLevel = level,
            lrMessage = message,
            lrModule = "",
            lrFunction = "",
            lrThread = "",
            lrMetadata = []
          }

  writeLogRecord logger record

-- | Log a debug message
logDebug :: Logger -> String -> IO ()
logDebug logger = logMessage logger LogLevelDebug

-- | Log an info message
logInfo :: Logger -> String -> IO ()
logInfo logger = logMessage logger LogLevelInfo

-- | Log a warning message
logWarn :: Logger -> String -> IO ()
logWarn logger = logMessage logger LogLevelWarn

-- | Log an error message
logError :: Logger -> String -> IO ()
logError logger = logMessage logger LogLevelError

-- | Create default logging configuration
defaultLoggingConfig :: IO LoggingConfig
defaultLoggingConfig =
  return $
    LoggingConfig
      { lcLevel = LogLevelInfo,
        lcFile = Nothing,
        lcMaxSize = 100 * 1024 * 1024,
        lcBackupCount = 5,
        lcFormat = LogFormatText,
        lcAsync = True,
        lcBufferSize = 100
      }

-- | Write a log record
writeLogRecord :: Logger -> LogRecord -> IO ()
writeLogRecord logger@Logger {..} record
  | lrLevel record >= lcLevel lgConfig = do
      if lcAsync lgConfig
        then do
          -- Add to async queue
          atomically $ modifyTVar lgQueue (record :)
          -- Trigger flush if buffer is full
          currentSize <- atomically $ readTVar lgQueue
          when (length currentSize >= lcBufferSize lgConfig) $ do
            _ <- tryTakeMVar lgFlushTrigger
            putMVar lgFlushTrigger ()
        else do
          -- Synchronous logging
          writeRecordSync logger record
  | otherwise = return () -- Below log level threshold

-- | Synchronous record writing
writeRecordSync :: Logger -> LogRecord -> IO ()
writeRecordSync Logger {..} record = do
  let formatted = case lcFormat lgConfig of
        LogFormatText -> formatText record
        LogFormatJson -> map (toEnum . fromEnum) $ BS.unpack $ LBS.toStrict $ encode record

  case lgHandle of
    Just handle -> do
      hPutStrLn handle formatted
      hFlush handle
    Nothing -> putStrLn formatted

  -- Update statistics
  atomically $ modifyTVar lgTotalWritten (+ length formatted)

-- | Async log worker
asyncLogWorker :: Logger -> IO ()
asyncLogWorker logger@Logger {..} = forever $ do
  -- Wait for flush trigger or timeout
  result <- tryTakeMVar lgFlushTrigger
  case result of
    Just _ -> do
      -- Flush queue
      flushQueue logger
    Nothing -> do
      -- Wait a bit and check queue
      threadDelay 100000 -- 100ms
      queueSize <- atomically $ readTVar lgQueue
      unless (null queueSize) $ flushQueue logger

-- | Flush the async queue
flushQueue :: Logger -> IO ()
flushQueue logger = do
  let Logger {..} = logger
  -- Get all records from queue
  records <- atomically $ do
    records <- readTVar lgQueue
    writeTVar lgQueue []
    return records

  -- Write records in reverse order (newest first)
  mapM_ (writeRecordSync logger) (reverse records)

-- | Format a log record as text
formatText :: LogRecord -> String
formatText LogRecord {..} =
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lrTimestamp
      level = logLevelToString lrLevel
      moduleStr = if null lrModule then "" else "[" ++ lrModule ++ "]"
      functionStr = if null lrFunction then "" else lrFunction ++ ": "
   in timestamp ++ " " ++ level ++ " " ++ moduleStr ++ " " ++ functionStr ++ lrMessage

-- | Format a log record as JSON (returns ByteString)
formatJson :: LogRecord -> ByteString
formatJson = LBS.toStrict . encode

-- | Rotate log files when they exceed maximum size
rotateLogs :: Logger -> IO ()
rotateLogs Logger {..} = do
  case (lcFile lgConfig, lgHandle) of
    (Just filePath, Just handle) -> do
      -- Check current file size
      currentSize <- atomically $ readTVar lgTotalWritten

      when (currentSize > lcMaxSize lgConfig) $ do
        -- Close current file
        hClose handle

        -- Rotate files
        rotateFileChain filePath (lcBackupCount lgConfig)

        -- Open new file
        newHandle <- openFile filePath AppendMode

        -- Update logger state
        atomically $ writeTVar lgTotalWritten 0
    -- Note: In a real implementation, we'd update lgHandle atomically
    -- For now, this is simplified

    _ -> return () -- No file logging

-- | Rotate a chain of log files
rotateFileChain :: FilePath -> Int -> IO ()
rotateFileChain filePath backupCount = do
  -- Remove oldest backup if it exists
  let oldestBackup = filePath ++ "." ++ show backupCount
  exists <- doesFileExist oldestBackup
  when exists $ removeFile oldestBackup

  -- Rotate existing backups
  mapM_
    ( \i -> do
        let currentFile = filePath ++ "." ++ show i
            nextFile = filePath ++ "." ++ show (i + 1)
        exists <- doesFileExist currentFile
        when exists $ renameFile currentFile nextFile
    )
    [backupCount, backupCount - 1 .. 1]

  -- Move current file to .1
  exists <- doesFileExist filePath
  when exists $ renameFile filePath (filePath ++ ".1")

-- | Flush all pending log messages
flushLogs :: Logger -> IO ()
flushLogs logger@Logger {..} = do
  if lcAsync lgConfig
    then do
      -- Trigger async flush
      _ <- tryTakeMVar lgFlushTrigger
      putMVar lgFlushTrigger ()
      threadDelay 100000 -- Wait for flush to complete
    else do
      -- Synchronous - nothing to flush
      return ()

-- | Close the logger and flush all messages
closeLogger :: Logger -> IO ()
closeLogger logger@Logger {..} = do
  -- Flush any pending messages
  flushLogs logger

  -- Close file handle
  case lgHandle of
    Just handle | lcFile lgConfig /= Nothing -> hClose handle
    _ -> return ()

-- | Create logging configuration from string parameters
createLoggingConfig :: String -> Maybe FilePath -> String -> IO LoggingConfig
createLoggingConfig levelStr file formatStr = do
  let level = case stringToLogLevel levelStr of
        Just l -> l
        Nothing -> LogLevelInfo
      format = case map toLower formatStr of
        "json" -> LogFormatJson
        _ -> LogFormatText

  return
    LoggingConfig
      { lcLevel = level,
        lcFile = file,
        lcMaxSize = 100 * 1024 * 1024,
        lcBackupCount = 5,
        lcFormat = format,
        lcAsync = True,
        lcBufferSize = 100
      }
