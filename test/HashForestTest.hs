{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HashForestTest where

import HashTreeTest
import Data.Gander.HashTree
import Data.Gander.HashForest

import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.ByteString.Char8            as B8
import System.IO (hClose, IOMode(..), withFile)
import System.IO.Temp

type TestForest = HashForest B8.ByteString

instance Arbitrary TestForest where
  arbitrary = HashForest <$> resize 3 arbitrary
  shrink (HashForest xs) = HashForest <$> shrink xs

instance Arbitrary ProdForest where
  arbitrary = HashForest <$> arbitrary
  shrink (HashForest ts) = HashForest <$> shrink ts

prop_roundtrip_hashforest_to_bytestring :: HashForest () -> Bool
prop_roundtrip_hashforest_to_bytestring t = t' == t
  where
    bs = B8.unlines $ serializeForest t -- TODO why didn't it include the unlines part again?
    t' = deserializeForest Nothing bs

roundtrip_hashforest_to_hashes :: HashForest () -> IO (HashForest ())
roundtrip_hashforest_to_hashes t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeForest path t
  readForest Nothing path

prop_roundtrip_hashforest_to_hashes :: Property
prop_roundtrip_hashforest_to_hashes = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_hashforest_to_hashes t1
  assert $ t2 == t1

roundtrip_hashforest_to_binary_hashes :: HashForest () -> IO (HashForest ())
roundtrip_hashforest_to_binary_hashes t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeBinForest path t
  readForest Nothing path

prop_roundtrip_hashforest_to_binary_hashes :: Property
prop_roundtrip_hashforest_to_binary_hashes = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_hashforest_to_binary_hashes t1
  assert $ t2 == t1
