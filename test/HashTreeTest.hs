{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

  -- you could shrink D -> F, but not without changing the rest of the hashline
  shrink _ = []

instance Arbitrary IndentLevel where
  arbitrary = fmap IndentLevel $ ((arbitrary :: Gen Int) `suchThat` (>= 0))
  shrink _ = []

instance Arbitrary Hash where
  arbitrary = fmap hashBytes (arbitrary :: Gen B8.ByteString)
  shrink _ = []

instance Arbitrary HashLine where

  arbitrary = do
    tt <- arbitrary :: Gen TreeType
    il <- arbitrary :: Gen IndentLevel
    h  <- arbitrary :: Gen Hash
    n  <- arbitrary :: Gen FileName
    return $ HashLine (tt, il, h, n)

  -- only shrinks the filename
  shrink (HashLine (tt, il, h, n)) = map (\n' -> HashLine (tt, il, h, n')) (shrink n)

instance Arbitrary a => Arbitrary (HashTree a) where

  arbitrary = do
    n <- arbitrary :: Gen FileName
    -- TODO there's got to be a better way, right?
    i <- choose (0,10 :: Int)
    if i == 0

      then do
        !cs <- resize 20 (arbitrary :: Gen [HashTree a])
        return $ Dir { name     = n
                     , hash     = hashContents cs
                     , contents = cs
                     , nFiles   = sum $ map countFiles cs
                     }

      else do
        bs <- arbitrary :: Gen B8.ByteString
        f  <- arbitrary
        return $ File { name = n
                      , hash = hashBytes bs
                      , file = f
                      }

  -- only shrinks the filename
  shrink f@(File {}) = map (\n -> f { name = n }) (shrink $ name f)

  -- shrinks either the name or the contents, and adjusts the rest to match
  shrink d@(Dir {}) = newNames ++ newContents
    where
      newNames = map (\n -> d { name = n }) (shrink $ name d)
      newContents = map (\cs -> d { contents = cs
                                  , hash = hashContents cs
                                  , nFiles = sum $ map countFiles cs})
                        (shrink $ contents d)

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

-- prop_roundtrip_hashtree_to_test_hashes_file :: 

--     describe "HashTree" $ do
--       describe "HashTree" $ do
--         it "builds a tree from the test annex" $ pendingWith "need annex test harness"

-- TODO what's right here but wrong in the roundtrip to bytestring ones?
prop_roundtrip_hashtree_to_bytestring :: HashTree () -> Bool
prop_roundtrip_hashtree_to_bytestring t = t' == t
  where
    bs = B8.unlines $ serializeTree t -- TODO why didn't it include the unlines part again?
    t' = deserializeTree Nothing bs

-- TODO round-trip to binary files too

roundtrip_hashtree_to_test_hashes_file :: HashTree () -> IO (HashTree ())
roundtrip_hashtree_to_test_hashes_file t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeTree path t
  readTree Nothing path

prop_roundtrip_hashtree_to_test_hashes_file :: Property
prop_roundtrip_hashtree_to_test_hashes_file = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_hashtree_to_test_hashes_file t1
  assert $ t2 == t1

roundtrip_hashtree_to_binary_hashes_file :: HashTree () -> IO (HashTree ())
roundtrip_hashtree_to_binary_hashes_file t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeBinTree path t
  readTree Nothing path

prop_roundtrip_hashtree_to_binary_hashes_file :: Property
prop_roundtrip_hashtree_to_binary_hashes_file = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_hashtree_to_binary_hashes_file t1
  assert $ t2 == t1

-- prop_write_hashtree_to_dirs :: Property
-- prop_write_hashtree_to_dirs = monadicIO $ do
--   t <- pick (arbitrary :: Gen HashTree)
--   run $ withSystemTempDirectory "writedirtemp" $ \d -> writeJustDirs t
--   -- run $ putStrLn $ "t: " ++ show t
--   assert True
