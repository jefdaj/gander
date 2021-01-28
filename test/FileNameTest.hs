{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FileNameTest where

import Data.Gander.HashTree
import Test.QuickCheck
import Util

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Char8            as B

import System.IO               (hClose)
import System.IO.Temp          (withSystemTempFile)
import Test.QuickCheck.Monadic (run, assert, pick, monadicIO)
import Test.QuickCheck.Unicode (list, char)

-- TODO this should fail on the empty string right?
parseFileName :: B.ByteString -> Either String FileName
parseFileName bs = A8.parseOnly nameP (B.append bs "\n")

-- TODO null and slash are the only chars not allowed in a filename, right?
-- TODO what about newlines?
reservedPathChars :: [Char]
reservedPathChars = ['\000', '\057']

-- TODO why is the not . null thing required to prevent empty strings? list1 should be enough
instance Arbitrary FileName where
  arbitrary = fmap p2n okList
    where
      okChar = char `suchThat` (\c -> not $ c `elem` reservedPathChars)
      okList = (list okChar) `suchThat` (not . null)

prop_roundtrip_filenames_to_bytestring :: FileName -> Bool
prop_roundtrip_filenames_to_bytestring n = p2n (n2p n) == n

roundtrip_filename_to_file :: FileName -> IO FileName
roundtrip_filename_to_file n = withSystemTempFile "roundtriptemp" $ \f hdl -> do
  hClose hdl
  B.writeFile f $ n2bs n
  bs <- B.readFile f
  return $ bs2n bs

prop_roundtrip_filenames_to_file :: Property
prop_roundtrip_filenames_to_file = monadicIO $ do
  d1 <- pick arbitrary
  d2 <- run $ roundtrip_filename_to_file d1
  assert $ d2 == d1
