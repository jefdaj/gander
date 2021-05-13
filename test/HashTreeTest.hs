{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HashTreeTest where

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Char8            as B8
-- import qualified Data.Text                        as T

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances.ByteString ()
import System.IO.Temp
import System.IO (hClose, IOMode(..), withFile)

import System.Directory.Tree (writeJustDirs)
import Util
import Data.Gander.Hash
import Data.Gander.HashLine
import Data.Gander.HashTree
import FileNameTest ()

parseHashLine :: B8.ByteString -> Either String (Maybe HashLine)
parseHashLine bs = A8.parseOnly (lineP Nothing) (B8.append bs "\n")

instance Arbitrary TreeType where
  arbitrary = do
    n <- choose (0,1 :: Int)
    return $ [F, D] !! n

instance Arbitrary IndentLevel where
  arbitrary = fmap IndentLevel $ ((arbitrary :: Gen Int) `suchThat` (>= 0))

instance Arbitrary Hash where
  arbitrary = fmap hashBytes (arbitrary :: Gen B8.ByteString)

instance Arbitrary HashLine where
  arbitrary = do
    tt <- arbitrary :: Gen TreeType
    il <- arbitrary :: Gen IndentLevel
    h  <- arbitrary :: Gen Hash
    n  <- arbitrary :: Gen FileName
    return $ HashLine (tt, il, h, n)

instance Arbitrary HashTree where
  arbitrary = do
    n <- arbitrary :: Gen FileName
    -- TODO there's got to be a better way, right?
    i <- choose (0,2 :: Int)
    if i == 0

      then do
        !cs <- resize 6 $ arbitrary :: Gen [HashTree] -- increase size to test RAM + CPU usage?
        return $ Dir { name     = n
                     , hash     = hashContents cs
                     , contents = cs
                     , nFiles   = sum $ map countFiles cs
                     }

      else do
        bs <- arbitrary :: Gen B8.ByteString
        return $ File { name = n
                      , hash = hashBytes bs
                      }

-- TODO test tree in haskell
-- TODO test dir
-- TODO test annex

-- TODO unit_build_tree_from_dir
-- TODO read_tree
-- TODO serialize_tree
-- TODO write_tree
-- TODO print_tree
-- TODO write_tree_binary?
-- TODO flatten_tree

-- prop_roundtrip_hashtrees_to_file :: 

--     describe "HashTree" $ do
--       describe "HashTree" $ do
--         it "builds a tree from the test annex" $ pendingWith "need annex test harness"

-- TODO what's right here but wrong in the roundtrip to bytestring ones?
prop_roundtrip_hashtrees_to_bytestring :: HashTree -> Bool
prop_roundtrip_hashtrees_to_bytestring t = t' == t
  where
    bs = B8.unlines $ serializeTree t -- TODO why didn't it include the unlines part again?
    t' = deserializeTree Nothing bs

-- TODO round-trip to binary files too

roundtrip_hashtree_to_file :: HashTree -> IO HashTree
roundtrip_hashtree_to_file t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeTree path t
  readTree Nothing path

-- prop_roundtrip_hashtrees_to_file :: Property
-- prop_roundtrip_hashtrees_to_file = monadicIO $ do
--   t1 <- pick arbitrary
--   t2 <- run $ roundtrip_hashtree_to_file t1
--   assert $ t2 == t1

-- prop_write_hashtrees_to_dirs :: Property
-- prop_write_hashtrees_to_dirs = monadicIO $ do
--   t <- pick (arbitrary :: Gen HashTree)
--   run $ withSystemTempDirectory "writedirtemp" $ \d -> writeJustDirs t
--   -- run $ putStrLn $ "t: " ++ show t
--   assert True
