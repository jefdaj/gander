{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FileNameTest where

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.UTF8             as BU
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import Test.HUnit hiding (assert)
import System.IO
import System.IO.Temp

import Util
import Data.Gander.HashTree

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Unicode

-- TODO this should fail on the empty string right?
parseFileName :: B.ByteString -> Either String FileName
parseFileName bs = A8.parseOnly nameP (B.append bs "\n")

-- TODO null and slash are the only chars not allowed in a filename, right?
-- TODO what about newlines?
reservedPathChars :: [Char]
reservedPathChars = ['\000', '\057']

-- the smallest unicode test i could come up with
-- this one appears to be encoded right, if we write it to a file:
--   B8.writeFile "umlaut-hs.txt" umlaut
--   hexdump -C umlaut-hs.txt
--   c3 a4
umlaut :: BU.ByteString
umlaut = BU.fromString "ä"

-- this one also looks good, as long as we T.unpack it before writing:
--   writeFile "umlaut2-hs.txt" $ T.unpack umlaut2
--   hexdump -C umlaut2-hs.txt
--   c3 a4
umlaut2 :: FileName
umlaut2 = "ä"

-- TODO wait, can you just use BU.ByteString for everything? maybe, it does look efficiency-minded
unit_roundtrip_umlaut_to_bytestring :: Assertion
unit_roundtrip_umlaut_to_bytestring =  umlaut' @=? umlaut
  where

    -- TODO this seems important. the B.pack $ BU.toString part is required, but why?
    Right n = parseFileName $ B.pack $ BU.toString umlaut

    umlaut' = BU.fromString $ T.unpack n -- TODO should this be the new n2p?
 
unit_roundtrip_umlaut2_to_bytestring :: Assertion
unit_roundtrip_umlaut2_to_bytestring =  n @=? umlaut2
  where
    -- bs = TE.encodeUtf8 umlaut2
    bs = B.pack $ T.unpack umlaut2 -- TODO hey that works! but why?
    Right n = parseFileName bs
    -- umlaut' = BU.fromString $ T.unpack n -- TODO should this be the new n2p?

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

-- TODO is there any reason this wouldn't count as actually passing?
prop_roundtrip_filenames_to_bytestring :: FileName -> Bool
-- prop_roundtrip_filenames_to_bytestring n = parseFileName (BU.fromString $ T.unpack n) == Right n
prop_roundtrip_filenames_to_bytestring n = n == T.pack (BU.toString (BU.fromString (T.unpack n)))

roundtrip_filename_to_file :: FileName -> IO FileName
roundtrip_filename_to_file n = withSystemTempFile "roundtriptemp" $ \f hdl -> do
  hClose hdl
  B.writeFile f $ BU.fromString $ T.unpack n
  bs <- B.readFile f
  let n' = T.pack $ BU.toString bs
  return n'

prop_roundtrip_filenames_to_file :: Property
prop_roundtrip_filenames_to_file = monadicIO $ do
  d1 <- pick arbitrary
  d2 <- run $ roundtrip_filename_to_file d1
  assert $ d2 == d1
