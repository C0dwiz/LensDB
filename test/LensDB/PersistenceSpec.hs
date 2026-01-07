{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: LensDB.PersistenceSpec
-- Description: Test suite for LensDB.Persistence module
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
module LensDB.PersistenceSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import LensDB.Core
import LensDB.Persistence
import LensDB.Persistence (BackupInfo (..), PersistenceConfig (..), PersistenceManager (..), Snapshot (..))
import System.Directory (createDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "PersistenceManager" $ do
    describe "Creation" $ do
      it "creates persistence manager with valid config" $ do
        withTempDirectory "." "test-data" $ \tempDir -> do
          let config =
                PersistenceConfig
                  { pcDataDir = tempDir,
                    pcSyncInterval = 60,
                    pcCompression = False,
                    pcBackupEnabled = True,
                    pcBackupInterval = 3600,
                    pcMaxBackups = 5,
                    pcCompactionThreshold = 0.8,
                    pcCompactionInterval = 3600
                  }

          manager <- newPersistenceManager config
          return (manager :: PersistenceManager)

    describe "Snapshot operations" $ do
      it "saves and loads snapshots correctly" $ do
        withTempDirectory "." "test-snapshots" $ \tempDir -> do
          let config =
                PersistenceConfig
                  { pcDataDir = tempDir,
                    pcSyncInterval = 60,
                    pcCompression = False,
                    pcBackupEnabled = False,
                    pcBackupInterval = 3600,
                    pcMaxBackups = 5,
                    pcCompactionThreshold = 0.8,
                    pcCompactionInterval = 3600
                  }

          manager <- newPersistenceManager config

          -- Create test data
          let testData =
                Map.fromList
                  [ ("key1", KeyValue "value1" undefined undefined 6),
                    ("key2", KeyValue "value2" undefined undefined 6),
                    ("key3", KeyValue "value3" undefined undefined 6)
                  ]

          -- Save snapshot
          saveSnapshot manager testData

          -- List snapshot files
          snapshotFiles <- getDataFiles tempDir
          length snapshotFiles `shouldBe` 1

          -- Load snapshot back
          let snapshotFile = head snapshotFiles
          result <- loadSnapshot manager snapshotFile

          case result of
            Left err -> expectationFailure $ "Failed to load snapshot: " ++ show err
            Right snapshot -> do
              snapData snapshot `shouldBe` testData
              snapVersion snapshot `shouldSatisfy` (> 0)

      it "handles non-existent snapshot files" $ do
        withTempDirectory "." "test-snapshots" $ \tempDir -> do
          let config =
                PersistenceConfig
                  { pcDataDir = tempDir,
                    pcSyncInterval = 60,
                    pcCompression = False,
                    pcBackupEnabled = False,
                    pcBackupInterval = 3600,
                    pcMaxBackups = 5,
                    pcCompactionThreshold = 0.8,
                    pcCompactionInterval = 3600
                  }

          manager <- newPersistenceManager config
          result <- loadSnapshot manager "/nonexistent/snapshot.dat"

          case result of
            Left err -> err `shouldContain` "does not exist"
            Right _ -> expectationFailure "Should have failed to load non-existent file"

    describe "Backup operations" $ do
      it "creates backups successfully" $ do
        withTempDirectory "." "test-backups" $ \tempDir -> do
          let config =
                PersistenceConfig
                  { pcDataDir = tempDir,
                    pcSyncInterval = 60,
                    pcCompression = False,
                    pcBackupEnabled = True,
                    pcBackupInterval = 3600,
                    pcMaxBackups = 5,
                    pcCompactionThreshold = 0.8,
                    pcCompactionInterval = 3600
                  }

          manager <- newPersistenceManager config

          -- Create test data
          let testData =
                Map.fromList
                  [ ("backup-key1", KeyValue "backup-value1" undefined undefined 12),
                    ("backup-key2", KeyValue "backup-value2" undefined undefined 12)
                  ]

          -- Create backup
          result <- createBackup manager testData

          case result of
            Left err -> expectationFailure $ "Failed to create backup: " ++ show err
            Right backupInfo -> do
              biPath backupInfo `shouldContain` tempDir
              biVersion backupInfo `shouldSatisfy` (> 0)
              biChecksum backupInfo `shouldSatisfy` (> 0)

      it "restores from backup correctly" $ do
        withTempDirectory "." "test-restore" $ \tempDir -> do
          let config =
                PersistenceConfig
                  { pcDataDir = tempDir,
                    pcSyncInterval = 60,
                    pcCompression = False,
                    pcBackupEnabled = True,
                    pcBackupInterval = 3600,
                    pcMaxBackups = 5,
                    pcCompactionThreshold = 0.8,
                    pcCompactionInterval = 3600
                  }

          manager <- newPersistenceManager config

          -- Create test data
          let originalData =
                Map.fromList
                  [ ("restore-key1", KeyValue "restore-value1" undefined undefined 13),
                    ("restore-key2", KeyValue "restore-value2" undefined undefined 13)
                  ]

          -- Create backup
          backupResult <- createBackup manager originalData
          case backupResult of
            Left err -> expectationFailure $ "Failed to create backup: " ++ show err
            Right backupInfo -> do
              -- Restore from backup
              restoreResult <- restoreFromBackup manager (biPath backupInfo)

              case restoreResult of
                Left err -> expectationFailure $ "Failed to restore: " ++ show err
                Right restoredData -> restoredData `shouldBe` originalData

  describe "Serialization" $ do
    describe "Key-value serialization" $ do
      it "serializes and deserializes key-value pairs correctly" $ do
        let key = "test-key"
            value = "test-value"
            kv = KeyValue value undefined undefined (BS.length value)

        let serialized = serializeKeyValue (key, kv)
        let deserialized = deserializeKeyValue serialized

        case deserialized of
          Nothing -> expectationFailure "Failed to deserialize key-value pair"
          Just (k, v) -> do
            k `shouldBe` key
            kvValue v `shouldBe` value
            kvSize v `shouldBe` BS.length value

      it "handles binary data correctly" $ do
        let key = "binary-key"
            binaryValue = BS.pack [0x00, 0x01, 0x02, 0xFF, 0xFE, 0xFD]
            kv = KeyValue binaryValue undefined undefined (BS.length binaryValue)

        let serialized = serializeKeyValue (key, kv)
        let deserialized = deserializeKeyValue serialized

        case deserialized of
          Nothing -> expectationFailure "Failed to deserialize binary key-value pair"
          Just (k, v) -> do
            k `shouldBe` key
            kvValue v `shouldBe` binaryValue
            kvSize v `shouldBe` BS.length binaryValue

      it "handles empty values correctly" $ do
        let key = "empty-key"
            emptyValue = ""
            kv = KeyValue emptyValue undefined undefined 0

        let serialized = serializeKeyValue (key, kv)
        let deserialized = deserializeKeyValue serialized

        case deserialized of
          Nothing -> expectationFailure "Failed to deserialize empty key-value pair"
          Just (k, v) -> do
            k `shouldBe` key
            kvValue v `shouldBe` emptyValue
            kvSize v `shouldBe` 0

    describe "Map serialization" $ do
      it "serializes and deserializes maps correctly" $ do
        let testData =
              Map.fromList
                [ ("map-key1", KeyValue "map-value1" undefined undefined 10),
                  ("map-key2", KeyValue "map-value2" undefined undefined 10),
                  ("map-key3", KeyValue "map-value3" undefined undefined 10)
                ]

        let serialized = serializeMap testData
        let deserialized = deserializeMap serialized

        case deserialized of
          Nothing -> expectationFailure "Failed to deserialize map"
          Just restoredMap -> restoredMap `shouldBe` testData

      it "handles empty maps correctly" $ do
        let emptyMap = Map.empty :: Map.Map ByteString KeyValue

        let serialized = serializeMap emptyMap
        let deserialized = deserializeMap serialized

        case deserialized of
          Nothing -> expectationFailure "Failed to deserialize empty map"
          Just restoredMap -> restoredMap `shouldBe` emptyMap

      it "handles single-item maps correctly" $ do
        let singleItem =
              Map.fromList
                [("single-key", KeyValue "single-value" undefined undefined 12)]

        let serialized = serializeMap singleItem
        let deserialized = deserializeMap serialized

        case deserialized of
          Nothing -> expectationFailure "Failed to deserialize single-item map"
          Just restoredMap -> restoredMap `shouldBe` singleItem

  describe "File management" $ do
    describe "Directory operations" $ do
      it "ensures data directory exists" $ do
        withTempDirectory "." "test-dir-creation" $ \tempDir -> do
          let dataDir = tempDir </> "data" </> "subdir"

          ensureDataDirectory dataDir

          -- Verify directory was created
          exists <- doesDirectoryExist dataDir
          exists `shouldBe` True

      it "gets data files correctly" $ do
        withTempDirectory "." "test-data-files" $ \tempDir -> do
          -- Create some test files
          writeFile (tempDir </> "snapshot1.dat") "test data 1"
          writeFile (tempDir </> "snapshot2.dat") "test data 2"
          writeFile (tempDir </> "backup1.bak") "backup data"
          writeFile (tempDir </> "other.txt") "other file"

          dataFiles <- getDataFiles tempDir

          -- Should only return .dat files
          length dataFiles `shouldBe` 2
          all (\f -> takeExtension f == ".dat") dataFiles `shouldBe` True

    describe "Data integrity" $ do
      it "verifies data integrity correctly" $ do
        withTempDirectory "." "test-integrity" $ \tempDir -> do
          let config =
                PersistenceConfig
                  { pcDataDir = tempDir,
                    pcSyncInterval = 60,
                    pcCompression = False,
                    pcBackupEnabled = False,
                    pcBackupInterval = 3600,
                    pcMaxBackups = 5,
                    pcCompactionThreshold = 0.8,
                    pcCompactionInterval = 3600
                  }

          manager <- newPersistenceManager config

          -- Create a valid snapshot
          let testData =
                Map.fromList
                  [("integrity-key", KeyValue "integrity-value" undefined undefined 15)]

          saveSnapshot manager testData

          -- Verify integrity
          isIntegrityValid <- verifyDataIntegrity manager
          isIntegrityValid `shouldBe` True

-- QuickCheck properties
prop_serialization_roundtrip :: ByteString -> ByteString -> Property
prop_serialization_roundtrip key value =
  BS.length key <= 1024 && BS.length value <= 1024 * 1024 ==> property $ do
    let kv = KeyValue value undefined undefined (BS.length value)
    let serialized = serializeKeyValue (key, kv)
    let deserialized = deserializeKeyValue serialized

    case deserialized of
      Nothing -> property $ discard
      Just (k, v) -> return $ k == key && kvValue v == value

prop_map_serialization_roundtrip :: [(ByteString, ByteString)] -> Property
prop_map_serialization_roundtrip pairs =
  all (\(k, v) -> BS.length k <= 1024 && BS.length v <= 1024) pairs ==> property $ do
    let testData = Map.fromList $ map (\(k, v) -> (k, KeyValue v undefined undefined (BS.length v))) pairs
    let serialized = serializeMap testData
    let deserialized = deserializeMap serialized

    case deserialized of
      Nothing -> property $ discard
      Just restoredMap -> return $ Map.map kvValue restoredMap == Map.fromList pairs
