module UtilTest where

import Util

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.HUnit
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

-- describe "Util" $ do
--   describe "absolutize" $ do
--       it "is idempotent" pending
--       it "strips dots from paths" pending
--       it "does not follow symlinks" pending
-- 
--     describe "findAnnex" $ do
--       it "returns Nothing if given a nonexistent path" pending
--       it "returns Nothing if given a non-annex path" pending
--       it "returns (Just path) if path is an annex" pending
--       it "returns only absolute paths" pending
-- 
--     describe "isAnnexSymlink" $ do
--       it "returns False if given a non-symlink" pending
--       it "returns True if given a symlink pointing into a .git/annex/objects dir" pending
--       it "returns False if given a symlink pointing somewhere else" pending
-- 
--     describe "isNonAnnexSymlink" $ do
--       it "returns False if given a non-symlink" pending
--       it "returns False if given a symlink pointing into a .git/annex/objects dir" pending
--       it "returns True if given a symlink pointing somewhere else" pending

unit_expand_tilde_in_path :: Assertion
unit_expand_tilde_in_path = do
  home <- getHomeDirectory
  let explicit = home </> "xyz"
  implicit <- absolutize "~/xyz"
  implicit @=? explicit
