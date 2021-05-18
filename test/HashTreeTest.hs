{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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

import System.FilePath
import System.Directory (createDirectoryIfMissing, listDirectory, removePathForcibly, doesPathExist)
-- import System.Directory.Tree (writeJustDirs)
import Util
import Data.Gander.Hash
import Data.Gander.HashLine
import Data.Gander.HashTree
import FileNameTest ()
import System.FilePath.Glob (Pattern)

import Control.DeepSeq (force)

import Data.List (nubBy)
import System.Info (os)
import Data.Char (toLower)
import Control.Monad (when)

-- A HashTree where file contents are generated by QuickCheck and stored in
-- memory for round-trip tests
type TestTree = HashTree B8.ByteString

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

-- TODO can you really have an arbitrary hashline without the rest of a tree?
instance Arbitrary HashLine where

  arbitrary = do
    tt <- arbitrary :: Gen TreeType
    il <- arbitrary :: Gen IndentLevel
    h  <- arbitrary :: Gen Hash
    n  <- arbitrary :: Gen FileName
    return $ HashLine (tt, il, h, n)

  -- only shrinks the filename
  shrink (HashLine (tt, il, h, n)) = map (\n' -> HashLine (tt, il, h, n')) (shrink n)

-- for removing duplicate filenames using nubBy, taking into account
-- case-insensitivity on apple filesystem
duplicateFilenames :: HashTree a -> HashTree a -> Bool
duplicateFilenames = if os == "darwin" then macDupes else unixDupes
  where
    macDupes  a b = map toLower (n2p $ name a)
                 == map toLower (n2p $ name b)
    unixDupes a b = n2p (name a)
                 == n2p (name b)

-- This is specialized to (HashTree B8.ByteString) because it needs to use the
-- same arbitrary bytestring for the file content and its hash
instance Arbitrary TestTree where

  arbitrary = do
    n <- arbitrary :: Gen FileName
    -- TODO there's got to be a better way, right?
    i <- choose (0,5 :: Int)
    if i == 0

      then do
        !cs <- fmap (nubBy duplicateFilenames) $ resize 5 (arbitrary :: Gen [TestTree])
        return $ Dir { name     = n
                     , hash     = hashContents cs
                     , contents = cs
                     , nFiles   = sum $ map countFiles cs
                     }

      else do
        bs <- arbitrary :: Gen B8.ByteString
        return $ File { name = n
                      , hash = hashBytes bs
                      , fileData = bs
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

instance Arbitrary ProdTree where
  arbitrary = fmap dropFileData arbitrary

-- TODO rename the actual function file -> fileData to match future dirData
-- TODO rewrite this in terms of a generic map/fold so it works with other types
dropFileData :: TestTree -> ProdTree
dropFileData d@(Dir {contents = cs}) = d {contents = map dropFileData cs}
dropFileData f@(File {}) = f {fileData = ()}

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

-- prop_roundtrip_prodtree_to_hashes :: 

--     describe "HashTree" $ do
--       describe "HashTree" $ do
--         it "builds a tree from the test annex" $ pendingWith "need annex test harness"

confirmFileHashes :: TestTree -> Bool
confirmFileHashes (File {fileData = f, hash = h}) = hashBytes f == h
confirmFileHashes (Dir {contents = cs}) = all confirmFileHashes cs

prop_confirm_file_hashes :: TestTree -> Bool
prop_confirm_file_hashes = confirmFileHashes

-- TODO prop_confirm_dir_hashes too?

-- TODO what's right here but wrong in the roundtrip to bytestring ones?
prop_roundtrip_prodtree_to_bytestring :: ProdTree -> Bool
prop_roundtrip_prodtree_to_bytestring t = t' == t
  where
    bs = B8.unlines $ serializeTree t -- TODO why didn't it include the unlines part again?
    t' = deserializeTree Nothing bs

-- TODO round-trip to binary files too

roundtrip_prodtree_to_hashes :: ProdTree -> IO (ProdTree)
roundtrip_prodtree_to_hashes t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeTree path t
  readTree Nothing path

prop_roundtrip_prodtree_to_hashes :: Property
prop_roundtrip_prodtree_to_hashes = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_prodtree_to_hashes t1
  assert $ t2 == t1

-- TODO separate thing for test and production trees here?
roundtrip_prodtree_to_bin_hashes :: ProdTree -> IO (ProdTree)
roundtrip_prodtree_to_bin_hashes t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeBinTree path t
  readTree Nothing path

prop_roundtrip_prodtree_to_bin_hashes :: Property
prop_roundtrip_prodtree_to_bin_hashes = monadicIO $ do
  t1 <- pick (arbitrary :: Gen ProdTree)
  t2 <- run $ roundtrip_prodtree_to_bin_hashes t1
  assert $ t2 == t1

-- this is to catch the case where it tries to write the same file twice
-- (happened once because of macos filename case-insensitivity)
assertNoFile :: FilePath -> IO ()
assertNoFile path = do
  exists <- doesPathExist path
  when exists $ error $ "duplicate write to: '" ++ path ++ "'"

{- Take a generated `TestTree` and write it to a tree of tmpfiles.
 - Note that this calls itself recursively.
 -}
writeTestTreeDir :: FilePath -> TestTree -> IO ()
writeTestTreeDir root (File {name = n, fileData = bs}) = do
  let path = root </> n2p n
  assertNoFile path
  B8.writeFile path bs
writeTestTreeDir root (Dir {name = n, contents = cs}) = do
  let root' = root </> n2p n
  assertNoFile root'
  -- putStrLn $ "write test dir: " ++ root'
  createDirectoryIfMissing True root' -- TODO false here
  mapM_ (writeTestTreeDir root') cs

readTestTree :: Maybe Int -> Bool -> [Pattern] -> FilePath -> IO TestTree
readTestTree md verbose excludes path = buildTree B8.readFile verbose excludes path

-- the tests above round-trip to single files describing trees, whereas this
-- one round-trips to an actual directory tree on disk
-- note that you have to drop the bytestrings from the original testtree to compare them
roundtrip_testtree_to_dir :: TestTree -> IO TestTree
roundtrip_testtree_to_dir t = withSystemTempDirectory "roundtriptemp" $ \root -> do
  let tmpRoot = "/tmp/round-trip-tests" -- TODO replace with actual root
  createDirectoryIfMissing True tmpRoot -- TODO False?
  let treePath = tmpRoot </> n2p (name t)
  removePathForcibly treePath -- TODO remove
  writeTestTreeDir tmpRoot t
  -- putStrLn $ "treePath: " ++ treePath
  readTestTree Nothing False [] treePath

prop_roundtrip_testtree_to_dir :: Property
prop_roundtrip_testtree_to_dir = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_testtree_to_dir t1
  assert $ force t2 == t1 -- force evaluation to prevent any possible conflicts
