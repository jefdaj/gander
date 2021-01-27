{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Gander.RoundTrip where

import Util
import Data.Gander.Hash
import Data.Gander.HashTree

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as DTE
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString
import Test.QuickCheck.Unicode
import qualified Data.Text as T
import Data.Attoparsec.ByteString.Char8 hiding (D, skipWhile, char)
import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.Combinator

---------------
-- utilities --
---------------

-- TODO move to HashTree.hs?
-- TODO this should fail on the empty string right?
parseFileName :: B.ByteString -> Either String FileName
parseFileName bs = parseOnly nameP (B.append bs "\n")

----------------------
-- random test data --
----------------------

-- TODO fix orphan instances?
-- TODO null and slash are the only chars not allowed in a filename, right?
-- TODO what about newlines?
-- TODO how to prevent empty strings after the invalid chars are removed?
-- ghci usage: generate (arbitrary :: Gen FileName)

-- taken from quickcheck-unicode source
excluding :: (a -> Bool) -> Gen a -> Gen a
excluding bad gen = loop
  where
    loop = do
      x <- gen
      if bad x
        then loop
        else return x

-- TODO why isn't this excluding ""?
instance Arbitrary FileName where
  arbitrary = fmap T.pack $ list1 $ excluding (\c -> c `elem` ['\000', '\057']) char

instance Arbitrary TreeType where
  arbitrary = do
    n <- choose (0,1 :: Int)
    return $ [F, D] !! n

instance Arbitrary IndentLevel where
  arbitrary = fmap IndentLevel $ ((arbitrary :: Gen Int) `suchThat` (>= 0))

instance Arbitrary Hash where
  arbitrary = fmap hashBytes (arbitrary :: Gen B.ByteString)

instance Arbitrary HashLine where
  arbitrary = do
    tt <- arbitrary :: Gen TreeType
    il <- arbitrary :: Gen IndentLevel
    h  <- arbitrary :: Gen Hash
    n  <- arbitrary :: Gen FileName
    return $ HashLine (tt, il, h, n)

----------------
-- properties --
----------------

prop_roundtrip_filename n = n' == (Right n)
  where
    bs = DTE.encodeUtf8 n -- (T.append n "\n") -- TODO maybe the \n was a bad idea then
    n' = parseFileName bs
