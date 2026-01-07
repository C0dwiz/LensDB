{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: LensDB.ProtocolSpec
-- Description: Test suite for LensDB.Protocol module
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
module LensDB.ProtocolSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word32, Word8)
import LensDB.Protocol
import LensDB.Protocol (MessageType (..), ProtocolMessage (..), ProtocolResponse (..), ResponseStatus (..))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "ProtocolMessage" $ do
    describe "Serialization" $ do
      it "serializes and deserializes ping message" $ do
        let msg = createPing
        let encoded = encodeMessage msg
        let decoded = decodeMessage encoded

        decoded `shouldBe` Right msg

      it "serializes and deserializes get message" $ do
        let msg = createGet "test-key"
        let encoded = encodeMessage msg
        let decoded = decodeMessage encoded

        decoded `shouldBe` Right msg

      it "serializes and deserializes set message" $ do
        let msg = createSet "test-key" "test-value"
        let encoded = encodeMessage msg
        let decoded = decodeMessage encoded

        decoded `shouldBe` Right msg

      it "serializes and deserializes delete message" $ do
        let msg = createDelete "test-key"
        let encoded = encodeMessage msg
        let decoded = decodeMessage encoded

        decoded `shouldBe` Right msg

      it "handles messages with binary data" $ do
        let binaryData = BS.pack [0x00, 0x01, 0x02, 0xFF, 0xFE, 0xFD]
        let msg = createSet "binary-key" binaryData
        let encoded = encodeMessage msg
        let decoded = decodeMessage encoded

        decoded `shouldBe` Right msg

    describe "Validation" $ do
      it "validates correct messages" $ do
        let msg = createSet "valid-key" "valid-value"
        validateMessage msg `shouldBe` True

      it "rejects messages with invalid magic number" $ do
        let msg =
              ProtocolMessage
                { msgMagic = 0x12345678, -- Wrong magic
                  msgVersion = protocolVersion,
                  msgType = MsgSet,
                  msgKey = "key",
                  msgValue = "value",
                  msgFlags = 0
                }
        validateMessage msg `shouldBe` False

      it "rejects messages with invalid version" $ do
        let msg =
              ProtocolMessage
                { msgMagic = magicNumber,
                  msgVersion = 99, -- Wrong version
                  msgType = MsgSet,
                  msgKey = "key",
                  msgValue = "value",
                  msgFlags = 0
                }
        validateMessage msg `shouldBe` False

      it "rejects messages with overly long keys" $ do
        let longKey = BS.replicate 1025 'a'
        let msg = createSet longKey "value"
        validateMessage msg `shouldBe` False

      it "rejects messages with overly long values" $ do
        let longValue = BS.replicate (1024 * 1024 + 1) 'b' -- Over 1MB
        let msg = createSet "key" longValue
        validateMessage msg `shouldBe` False

    describe "Message creation" $ do
      it "creates ping message correctly" $ do
        let msg = createPing
        msgMagic msg `shouldBe` magicNumber
        msgVersion msg `shouldBe` protocolVersion
        msgType msg `shouldBe` MsgPing
        msgKey msg `shouldBe` ""
        msgValue msg `shouldBe` ""
        msgFlags msg `shouldBe` 0

      it "creates get message correctly" $ do
        let key = "test-key"
        let msg = createGet key
        msgMagic msg `shouldBe` magicNumber
        msgVersion msg `shouldBe` protocolVersion
        msgType msg `shouldBe` MsgGet
        msgKey msg `shouldBe` key
        msgValue msg `shouldBe` ""
        msgFlags msg `shouldBe` 0

      it "creates set message correctly" $ do
        let key = "test-key"
        let value = "test-value"
        let msg = createSet key value
        msgMagic msg `shouldBe` magicNumber
        msgVersion msg `shouldBe` protocolVersion
        msgType msg `shouldBe` MsgSet
        msgKey msg `shouldBe` key
        msgValue msg `shouldBe` value
        msgFlags msg `shouldBe` 0

      it "creates delete message correctly" $ do
        let key = "test-key"
        let msg = createDelete key
        msgMagic msg `shouldBe` magicNumber
        msgVersion msg `shouldBe` protocolVersion
        msgType msg `shouldBe` MsgDelete
        msgKey msg `shouldBe` key
        msgValue msg `shouldBe` ""
        msgFlags msg `shouldBe` 0

    describe "Message size calculation" $ do
      it "calculates correct size for ping message" $ do
        let msg = createPing
        messageSize msg `shouldBe` 4 + 1 + 1 + 4 + 4 + 4 -- Header only
      it "calculates correct size for set message" $ do
        let key = "key"
        let value = "value"
        let msg = createSet key value
        let expectedSize = 4 + 1 + 1 + 4 + 4 + 4 + BS.length key + BS.length value
        messageSize msg `shouldBe` expectedSize

  describe "ProtocolResponse" $ do
    describe "Serialization" $ do
      it "serializes and deserializes success response" $ do
        let response = createSuccessResponse "success-data"
        let encoded = encodeResponse response
        let decoded = decodeResponse encoded

        decoded `shouldBe` Right response

      it "serializes and deserializes error response" $ do
        let response = createErrorResponse StatusKeyNotFound "Key not found"
        let encoded = encodeResponse response
        let decoded = decodeResponse encoded

        decoded `shouldBe` Right response

      it "handles responses with binary data" $ do
        let binaryData = BS.pack [0x00, 0x01, 0x02, 0xFF]
        let response = createSuccessResponse binaryData
        let encoded = encodeResponse response
        let decoded = decodeResponse encoded

        decoded `shouldBe` Right response

    describe "Response creation" $ do
      it "creates success response correctly" $ do
        let data_ = "response-data"
        let response = createSuccessResponse data_
        respMagic response `shouldBe` magicNumber
        respVersion response `shouldBe` protocolVersion
        respStatus response `shouldBe` StatusSuccess
        respData response `shouldBe` data_
        respMessage response `shouldBe` ""

      it "creates error response correctly" $ do
        let status = StatusInternalError
        let errMsg = "Internal error occurred"
        let response = createErrorResponse status errMsg
        respMagic response `shouldBe` magicNumber
        respVersion response `shouldBe` protocolVersion
        respStatus response `shouldBe` status
        respData response `shouldBe` ""
        respMessage response `shouldBe` errMsg

  describe "Protocol constants" $ do
    it "has correct magic number" $ do
      magicNumber `shouldBe` 0x4C454E53 -- "LENS"
    it "has correct protocol version" $ do
      protocolVersion `shouldBe` 1

  describe "Round-trip tests" $ do
    it "handles complete round-trip for all message types" $ do
      let messages =
            [ createPing,
              createGet "test-key",
              createSet "key1" "value1",
              createSet "key2" "value2",
              createDelete "delete-key"
            ]

      let testRoundTrip msg = do
            let encoded = encodeMessage msg
            let decoded = decodeMessage encoded
            decoded `shouldBe` Right msg

      mapM_ testRoundTrip messages

    it "handles complete round-trip for all response types" $ do
      let responses =
            [ createSuccessResponse "success",
              createSuccessResponse "",
              createSuccessResponse (BS.pack [0x00, 0xFF]),
              createErrorResponse StatusKeyNotFound "Not found",
              createErrorResponse StatusInternalError "Error"
            ]

      let testRoundTrip response = do
            let encoded = encodeResponse response
            let decoded = decodeResponse encoded
            decoded `shouldBe` Right response

      mapM_ testRoundTrip responses

  describe "Error handling" $ do
    it "handles corrupted message data" $ do
      let corrupted = LBS.pack [0xFF, 0xFF, 0xFF, 0xFF] -- Invalid magic
      let decoded = decodeMessage corrupted

      case decoded of
        Left err -> err `shouldContain` "Invalid magic number"
        Right _ -> expectationFailure "Should have failed to decode corrupted data"

    it "handles truncated message data" $ do
      let truncated = LBS.pack [0x4C, 0x45, 0x4E, 0x53] -- Only magic, no more data
      let decoded = decodeMessage truncated

      case decoded of
        Left _ -> return () -- Expected to fail
        Right _ -> expectationFailure "Should have failed to decode truncated data"

    it "handles corrupted response data" $ do
      let corrupted = LBS.pack [0xFF, 0xFF, 0xFF, 0xFF] -- Invalid magic
      let decoded = decodeResponse corrupted

      case decoded of
        Left err -> err `shouldContain` "Invalid magic number"
        Right _ -> expectationFailure "Should have failed to decode corrupted data"

-- QuickCheck properties
prop_message_roundtrip :: ProtocolMessage -> Property
prop_message_roundtrip msg =
  validateMessage msg ==> property $ do
    let encoded = encodeMessage msg
    let decoded = decodeMessage encoded
    return $ decoded == Right msg

prop_response_roundtrip :: ProtocolResponse -> Property
prop_response_roundtrip response = property $ do
  let encoded = encodeResponse response
  let decoded = decodeResponse encoded
  return $ decoded == Right response

prop_set_message_roundtrip :: ByteString -> ByteString -> Property
prop_set_message_roundtrip key value =
  BS.length key <= 1024 && BS.length value <= 1024 * 1024 ==> property $ do
    let msg = createSet key value
    let encoded = encodeMessage msg
    let decoded = decodeMessage encoded
    return $ decoded == Right msg
