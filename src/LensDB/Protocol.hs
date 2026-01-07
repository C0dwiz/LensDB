{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: LensDB.Protocol
-- Description: Binary protocol for client-server communication
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
--
-- This module implements the binary communication protocol used by LensDB
-- for client-server interactions. The protocol is designed to be simple,
-- efficient, and extensible.
module LensDB.Protocol
  ( -- * Protocol Types
    ProtocolMessage (..),
    MessageType (..),
    ProtocolResponse (..),
    ResponseStatus (..),

    -- * Protocol Constants
    magicNumber,
    protocolVersion,

    -- * Serialization
    encodeMessage,
    decodeMessage,
    encodeResponse,
    decodeResponse,

    -- * Protocol Utilities
    validateMessage,
    messageSize,
    createPing,
    createGet,
    createSet,
    createDelete,
    createSuccessResponse,
    createErrorResponse,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (unless, when)
import Data.Binary (Binary (..), Get, Put, decodeOrFail, encode)
import Data.Binary.Get (getByteString, getWord32be, getWord8, isEmpty)
import Data.Binary.Put (putByteString, putWord32be, putWord8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)

-- | Magic number for protocol identification (ASCII: "LENS")
magicNumber :: Word32
magicNumber = 0x4C454E53

-- | Current protocol version
protocolVersion :: Word8
protocolVersion = 1

-- | Message types in the protocol
data MessageType
  = -- | Health check/keepalive
    MsgPing
  | -- | Get a value by key
    MsgGet
  | -- | Set a key-value pair
    MsgSet
  | -- | Delete a key
    MsgDelete
  | -- | Check if key exists
    MsgExists
  | -- | Get all keys
    MsgKeys
  | -- | Get storage size
    MsgSize
  | -- | Clear all data
    MsgClear
  | -- | Get storage statistics
    MsgStats
  deriving (Show, Eq, Generic, NFData, Enum)

instance Binary MessageType where
  put = putWord8 . fromIntegral . fromEnum
  get = do
    word <- getWord8
    case word of
      0 -> return MsgPing
      1 -> return MsgGet
      2 -> return MsgSet
      3 -> return MsgDelete
      4 -> return MsgExists
      5 -> return MsgKeys
      6 -> return MsgSize
      7 -> return MsgClear
      8 -> return MsgStats
      _ -> fail $ "Invalid message type: " ++ show word

-- | Protocol message structure
data ProtocolMessage = ProtocolMessage
  { -- | Magic number for validation
    msgMagic :: !Word32,
    -- | Protocol version
    msgVersion :: !Word8,
    -- | Message type
    msgType :: !MessageType,
    -- | Key (empty for some message types)
    msgKey :: !ByteString,
    -- | Value (empty for most message types)
    msgValue :: !ByteString,
    -- | Message flags (reserved for future use)
    msgFlags :: !Word32
  }
  deriving (Show, Eq, Generic, NFData)

instance Binary ProtocolMessage where
  put ProtocolMessage {..} = do
    putWord32be msgMagic
    putWord8 msgVersion
    put msgType
    putWord32be $ fromIntegral $ BS.length msgKey
    putWord32be $ fromIntegral $ BS.length msgValue
    putWord32be msgFlags
    putByteString msgKey
    putByteString msgValue

  get = do
    magic <- getWord32be
    when (magic /= magicNumber) $
      fail $
        "Invalid magic number: " ++ show magic

    version <- getWord8
    when (version /= protocolVersion) $
      fail $
        "Unsupported protocol version: " ++ show version

    msgType <- get
    keyLen <- fromIntegral <$> getWord32be
    valLen <- fromIntegral <$> getWord32be
    flags <- getWord32be

    msgKey <- getByteString keyLen
    msgValue <- getByteString valLen

    return
      ProtocolMessage
        { msgMagic = magic,
          msgVersion = version,
          msgType = msgType,
          msgKey = msgKey,
          msgValue = msgValue,
          msgFlags = flags
        }

-- | Response status codes
data ResponseStatus
  = -- | Operation completed successfully
    StatusSuccess
  | -- | Key not found
    StatusKeyNotFound
  | -- | Invalid key format
    StatusInvalidKey
  | -- | Storage is full
    StatusStorageFull
  | -- | Invalid message format
    StatusInvalidMsg
  | -- | Internal server error
    StatusInternalError
  | -- | Unsupported operation
    StatusUnsupported
  deriving (Show, Eq, Generic, NFData, Enum)

instance Binary ResponseStatus where
  put = putWord8 . fromIntegral . fromEnum
  get = do
    word <- getWord8
    case word of
      0 -> return StatusSuccess
      1 -> return StatusKeyNotFound
      2 -> return StatusInvalidKey
      3 -> return StatusStorageFull
      4 -> return StatusInvalidMsg
      5 -> return StatusInternalError
      6 -> return StatusUnsupported
      _ -> fail $ "Invalid response status: " ++ show word

-- | Protocol response structure
data ProtocolResponse = ProtocolResponse
  { -- | Magic number (same as request)
    respMagic :: !Word32,
    -- | Protocol version
    respVersion :: !Word8,
    -- | Response status
    respStatus :: !ResponseStatus,
    -- | Response data (varies by message type)
    respData :: !ByteString,
    -- | Error message (if any)
    respMessage :: !ByteString
  }
  deriving (Show, Eq, Generic, NFData)

instance Binary ProtocolResponse where
  put ProtocolResponse {..} = do
    putWord32be respMagic
    putWord8 respVersion
    put respStatus
    putWord32be $ fromIntegral $ BS.length respData
    putWord32be $ fromIntegral $ BS.length respMessage
    putByteString respData
    putByteString respMessage

  get = do
    magic <- getWord32be
    when (magic /= magicNumber) $
      fail $
        "Invalid magic number in response: " ++ show magic

    version <- getWord8
    when (version /= protocolVersion) $
      fail $
        "Unsupported protocol version in response: " ++ show version

    respStatus <- get
    dataLen <- fromIntegral <$> getWord32be
    msgLen <- fromIntegral <$> getWord32be

    respData <- getByteString dataLen
    respMessage <- getByteString msgLen

    return
      ProtocolResponse
        { respMagic = magic,
          respVersion = version,
          respStatus = respStatus,
          respData = respData,
          respMessage = respMessage
        }

-- | Encode a protocol message to bytes
encodeMessage :: ProtocolMessage -> LBS.ByteString
encodeMessage = encode

-- | Decode a protocol message from bytes
decodeMessage :: LBS.ByteString -> Either String ProtocolMessage
decodeMessage bs = case decodeOrFail bs of
  Left (_, _, err) -> Left err
  Right (_, _, msg) -> Right msg

-- | Encode a protocol response to bytes
encodeResponse :: ProtocolResponse -> LBS.ByteString
encodeResponse = encode

-- | Decode a protocol response from bytes
decodeResponse :: LBS.ByteString -> Either String ProtocolResponse
decodeResponse bs = case decodeOrFail bs of
  Left (_, _, err) -> Left err
  Right (_, _, resp) -> Right resp

-- | Validate a protocol message
validateMessage :: ProtocolMessage -> Bool
validateMessage ProtocolMessage {..} =
  msgMagic == magicNumber
    && msgVersion == protocolVersion
    && BS.length msgKey <= 1024
    && BS.length msgValue <= 1024 * 1024 -- Max key length
    -- Max value length (1MB)

-- | Calculate the size of a message in bytes
messageSize :: ProtocolMessage -> Int
messageSize ProtocolMessage {..} =
  4
    + 1
    + 1
    + 4
    + 4
    + 4
    + BS.length msgKey -- Header (magic + version + type + keyLen + valLen + flags)
    + BS.length msgValue

-- | Create a ping message
createPing :: ProtocolMessage
createPing =
  ProtocolMessage
    { msgMagic = magicNumber,
      msgVersion = protocolVersion,
      msgType = MsgPing,
      msgKey = "",
      msgValue = "",
      msgFlags = 0
    }

-- | Create a GET message
createGet :: ByteString -> ProtocolMessage
createGet key =
  ProtocolMessage
    { msgMagic = magicNumber,
      msgVersion = protocolVersion,
      msgType = MsgGet,
      msgKey = key,
      msgValue = "",
      msgFlags = 0
    }

-- | Create a SET message
createSet :: ByteString -> ByteString -> ProtocolMessage
createSet key value =
  ProtocolMessage
    { msgMagic = magicNumber,
      msgVersion = protocolVersion,
      msgType = MsgSet,
      msgKey = key,
      msgValue = value,
      msgFlags = 0
    }

-- | Create a DELETE message
createDelete :: ByteString -> ProtocolMessage
createDelete key =
  ProtocolMessage
    { msgMagic = magicNumber,
      msgVersion = protocolVersion,
      msgType = MsgDelete,
      msgKey = key,
      msgValue = "",
      msgFlags = 0
    }

-- | Create a success response
createSuccessResponse :: ByteString -> ProtocolResponse
createSuccessResponse data_ =
  ProtocolResponse
    { respMagic = magicNumber,
      respVersion = protocolVersion,
      respStatus = StatusSuccess,
      respData = data_,
      respMessage = ""
    }

-- | Create an error response
createErrorResponse :: ResponseStatus -> ByteString -> ProtocolResponse
createErrorResponse status errMsg =
  ProtocolResponse
    { respMagic = magicNumber,
      respVersion = protocolVersion,
      respStatus = status,
      respData = "",
      respMessage = errMsg
    }
