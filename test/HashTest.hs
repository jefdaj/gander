{-# LANGUAGE OverloadedStrings #-}

module HashTest where

import Data.Gander.Hash
import System.IO.Temp
import Test.HUnit

-- note: no need to explicitly match against sha256sum because the manual examples cover that
-- TODO but make sure they match manually! how to handle the base64 encoding part?

unit_hash_a_bytestring :: Assertion
unit_hash_a_bytestring = unHash (hashBytes "a bytestring") @=? "YTI3MDBmODFhZWE2ZjBm"

unit_hash_an_image :: Assertion
unit_hash_an_image = do
  h <- hashFile False "gander.png"
  unHash h @=? "NWMwYjNlN2FiZTQ5OWZj"

unit_hash_an_empty_file :: Assertion
unit_hash_an_empty_file = do
  f <- emptySystemTempFile "empty"
  h <- hashFile False f
  unHash h @=? "ZTNiMGM0NDI5OGZjMWMx"

-- TODO unit_hash_a_dir
-- TODO unit_hash_a_empty_dir
