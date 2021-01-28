{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FileNameTest where

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Char8            as B
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE

import Util
import Data.Gander.HashTree

import Test.QuickCheck
import Test.QuickCheck.Unicode

-- TODO this should fail on the empty string right?
parseFileName :: B.ByteString -> Either String FileName
parseFileName bs = A8.parseOnly nameP (B.append bs "\n")

-- TODO null and slash are the only chars not allowed in a filename, right?
-- TODO what about newlines?
reservedPathChars :: [Char]
reservedPathChars = ['\000', '\057']

-- TODO why is the not . null thing required to prevent empty strings? list1 should be enough
instance Arbitrary FileName where
  arbitrary = fmap T.pack okList
    where
      okChar = char `suchThat` (\c -> not $ c `elem` reservedPathChars)
      okList = (list okChar) `suchThat` (not . null)

prop_roundtrip_filename_to_bytestring :: FileName -> Bool
prop_roundtrip_filename_to_bytestring n = parseFileName (TE.encodeUtf8 n) == Right n
