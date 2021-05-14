{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HashForestTest where

import HashTreeTest
-- import Data.Gander.HashTree
import Data.Gander.HashForest

import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.ByteString.Char8            as B8
import System.IO (hClose, IOMode(..), withFile)
import System.IO.Temp

instance Arbitrary HashForest where
  arbitrary = HashForest <$> resize 3 arbitrary
  shrink (HashForest xs) = HashForest <$> shrink xs

-- TODO round-trip to binary files too

prop_roundtrip_hashforest_to_bytestring :: HashForest -> Bool
prop_roundtrip_hashforest_to_bytestring t = t' == t
  where
    bs = B8.unlines $ serializeForest t -- TODO why didn't it include the unlines part again?
    t' = deserializeForest Nothing bs

roundtrip_hashforest_to_file :: HashForest -> IO HashForest
roundtrip_hashforest_to_file t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeForest path t
  readForest Nothing path

prop_roundtrip_hashforest_to_file :: Property
prop_roundtrip_hashforest_to_file = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_hashforest_to_file t1
  assert $ t2 == t1

roundtrip_hashforest_to_binary_file :: HashForest -> IO HashForest
roundtrip_hashforest_to_binary_file t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeBinForest path t
  readForest Nothing path

prop_roundtrip_hashforest_to_binary_file :: Property
prop_roundtrip_hashforest_to_binary_file = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_hashforest_to_binary_file t1
  assert $ t2 == t1
