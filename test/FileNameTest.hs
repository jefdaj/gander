{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FileNameTest where

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.UTF8             as BU
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import Test.HUnit
import System.IO
import System.IO.Temp

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

-- the smallest unicode test i could come up with
umlaut :: BU.ByteString
umlaut = BU.fromString "ä"

-- TODO wait, can you just use BU.ByteString for everything? maybe, it does look efficiency-minded
unit_roundtrip_umlaut_to_bytestring :: Assertion
unit_roundtrip_umlaut_to_bytestring =  umlaut' @=? umlaut
  where
    Right n = parseFileName (B.pack $ BU.toString umlaut)
    umlaut' = BU.fromString $ T.unpack n -- TODO should this be the new n2p?

-- TODO this works. but can it be done with the existing filename machinery too?
unit_roundtrip_umlaut_to_file :: Assertion
unit_roundtrip_umlaut_to_file = withSystemTempFile "roundtriptemp" $ \f hdl -> do
  hPutStr hdl $ BU.toString umlaut
  hClose hdl
  s <- readFile f
  let umlaut' = BU.fromString s
  umlaut' @=? umlaut

-- TODO why is the not . null thing required to prevent empty strings? list1 should be enough
instance Arbitrary FileName where
  arbitrary = fmap T.pack okList
    where
      okChar = char `suchThat` (\c -> not $ c `elem` reservedPathChars)
      okList = (list okChar) `suchThat` (not . null)

prop_roundtrip_filenames_to_bytestring :: FileName -> Bool
prop_roundtrip_filenames_to_bytestring n = parseFileName (TE.encodeUtf8 n) == Right n
