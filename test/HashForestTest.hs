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
  arbitrary = HashForest <$> resize 5 arbitrary

-- TODO round-trip to binary files too

prop_roundtrip_hashforests_to_bytestring :: HashForest -> Bool
prop_roundtrip_hashforests_to_bytestring t = t' == t
  where
    bs = B8.unlines $ serializeForest t -- TODO why didn't it include the unlines part again?
    t' = deserializeForest Nothing bs

roundtrip_hashforests_to_file :: HashForest -> IO HashForest
roundtrip_hashforests_to_file t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeForest path t
  readForest Nothing path
