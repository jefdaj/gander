module UtilTest where

import Util

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.HUnit (Assertion, (@=?))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Control.Monad.IO.Class (liftIO)

-- describe "Util" $ do
--   describe "absolutize" $ do
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

unit_absolutize_tilde_expansion :: Assertion
unit_absolutize_tilde_expansion = do
  home <- getHomeDirectory
  let explicit = home </> "xyz"
  (Just implicit) <- absolutize "~/xyz"
  implicit @=? explicit

prop_absolutize_is_idempotent :: FilePath -> Property
prop_absolutize_is_idempotent path = monadicIO $ do
  (Just path' ) <- liftIO $ absolutize path
  (Just path'') <- liftIO $ absolutize path'
  assert $ path' == path''
