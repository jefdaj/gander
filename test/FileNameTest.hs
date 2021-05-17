{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FileNameTest where

import Data.Gander.HashLine
import Test.QuickCheck
import Test.QuickCheck.Instances
import Util

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Char8            as B
import qualified Data.Text                        as T

import System.IO               (hClose)
import System.IO.Temp          (withSystemTempDirectory)
import Test.QuickCheck.Monadic (run, assert, pick, monadicIO)
import Test.QuickCheck.Unicode (list, char)
import System.FilePath ((</>))

import qualified Filesystem.Path.CurrentOS as OS

{- My `FileName` type is defined in `Util` as `Text` for efficiency, but
 - what it really means is "Text without slashes or null chars". So I have to
 - define my own Arbitrary instance here.
 -
 - TODO why is the not . null thing required to prevent empty strings? list1 should be enough
 - TODO wait, is the empty string also a valid filename?
 -}
instance Arbitrary FileName where
  arbitrary = FileName <$> (arbitrary :: Gen T.Text) `suchThat` validFileName
  shrink (FileName t) = FileName <$> filter validFileName (shrink t)

validFileName :: T.Text -> Bool
validFileName t = not (t `elem` ["", ".", ".."])
               && (not . T.any (== '/')) t -- no separators
               && (OS.valid . OS.fromText) t

prop_roundtrip_filename_to_bytestring :: FileName -> Bool
prop_roundtrip_filename_to_bytestring n = p2n (n2p n) == n

roundtrip_filename_to_name_of_tmpfile :: FileName -> IO ()
roundtrip_filename_to_name_of_tmpfile n = withSystemTempDirectory "roundtriptemp" $ \d -> do
  let f = d </> n2p n
  B.writeFile f "this is a test"
  _ <- B.readFile f
  return ()

prop_roundtrip_filename_to_name_of_tmpfile :: Property
prop_roundtrip_filename_to_name_of_tmpfile = monadicIO $ do
  n <- pick arbitrary
  run $ roundtrip_filename_to_name_of_tmpfile n
  assert True
