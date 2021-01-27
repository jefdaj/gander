{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FileNameTest where

import Util
import Data.Gander.HashTree

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as DTE
import Test.QuickCheck
import Test.QuickCheck.Unicode
import qualified Data.Text as T
import Data.Attoparsec.ByteString.Char8 hiding (D, skipWhile, char)

-- TODO this should fail on the empty string right?
parseFileName :: B.ByteString -> Either String FileName
parseFileName bs = parseOnly nameP (B.append bs "\n")

-- TODO null and slash are the only chars not allowed in a filename, right?
-- TODO what about newlines?
reservedPathChars :: [Char]
reservedPathChars = ['\000', '\057']

instance Arbitrary FileName where
  arbitrary = fmap T.pack $ list1 $ char `suchThat` (\c -> not $ c `elem` reservedPathChars)

prop_roundtrip_filename_to_bytestring :: FileName -> Bool
prop_roundtrip_filename_to_bytestring n = parseFileName (DTE.encodeUtf8 n) == Right n
