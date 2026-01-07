{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Spec
-- Description: Main test suite for LensDB
-- Copyright: (c) 2026, CodWiz
-- License: BSD-3-Clause
--
-- This is the main test suite entry point for LensDB.
module Main where

import qualified LensDB.ConfigSpec as ConfigSpec
import qualified LensDB.CoreSpec as CoreSpec
import qualified LensDB.PersistenceSpec as PersistenceSpec
import qualified LensDB.ProtocolSpec as ProtocolSpec
import qualified LensDB.StorageSpec as StorageSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "LensDB Core" CoreSpec.spec
  describe "LensDB Storage" StorageSpec.spec
  describe "LensDB Protocol" ProtocolSpec.spec
  describe "LensDB Config" ConfigSpec.spec
  describe "LensDB Persistence" PersistenceSpec.spec
