{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HashTreeTest where

import Data.Gander.HashTree

import FileNameTest

import Util
import Data.Gander.Hash

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as DTE
import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString
import Test.QuickCheck.Unicode
import Test.Tasty.Discover
import qualified Data.Text as T
import Data.Attoparsec.ByteString.Char8 hiding (D, skipWhile, char)
import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.Combinator


parseHashLine :: B.ByteString -> Either String (Maybe HashLine)
parseHashLine bs = parseOnly (lineP Nothing) (B.append bs "\n")

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


---------------------
-- manual examples --
---------------------

-- TODO test tree in haskell
-- TODO test dir
-- TODO test annex

----------------
-- unit tests --
----------------

-- TODO unit_build_tree_from_dir
-- TODO read_tree
-- TODO serialize_tree
-- TODO write_tree
-- TODO print_tree
-- TODO write_tree_binary?
-- TODO flatten_tree

-------------------------
-- round-trip to files --
-------------------------

-- prop_roundtrip_hashtree_to_file :: 

--     describe "HashTree" $ do
--       describe "HashTree" $ do
--         it "builds a tree from the test annex" $ pendingWith "need annex test harness"
--         it "can be round-tripped to a file" $ pendingWith "need annex test harness"
--         it "can handle unicode filenames" pending

prop_roundtrip_hashline_to_bytestring :: HashLine -> Bool
prop_roundtrip_hashline_to_bytestring l = l' == (Right $ Just l)
  where
    bs = prettyHashLine l -- (T.append n "\n") -- TODO maybe the \n was a bad idea then
    l' = parseHashLine bs
