module UtilTest where

import Util
import FileNameTest

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.HUnit (Assertion, (@=?))
import Test.Hspec
import System.Directory (getHomeDirectory)
import System.FilePath ((</>), takeDirectory, joinPath)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (evaluate)
import qualified Data.Text as T

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

newtype ValidFilePath = ValidFilePath FilePath
  deriving (Eq, Ord, Read, Show)

instance Arbitrary ValidFilePath where
  arbitrary = do
    prefix <- oneof $ map pure ["", ".", "..", "~"]
    comps  <- (fmap . map) (\(FileName t) -> T.unpack t) $ listOf $ (arbitrary :: Gen FileName)
    let path = joinPath (prefix:comps)
    return $ ValidFilePath $ if null path then "/" else path

unit_absolutize_expands_tildes :: Assertion
unit_absolutize_expands_tildes = do
  home <- getHomeDirectory
  let explicit = home </> "xyz"
  (Just implicit) <- absolutize "~/xyz"
  implicit @=? explicit

unit_absolutize_rejects_the_null_path :: Assertion
unit_absolutize_rejects_the_null_path = do
  reject <- absolutize ""
  reject @=? Nothing

unit_absolutize_fixes_invalid_dotdot_path :: Assertion
unit_absolutize_fixes_invalid_dotdot_path = do
  fixed <- absolutize "/.." -- one level above / is invalid
  fixed @=? Just "/"

prop_absolutize_is_idempotent :: ValidFilePath -> Property
prop_absolutize_is_idempotent (ValidFilePath path) = monadicIO $ do
  (Just path' ) <- liftIO $ absolutize path
  (Just path'') <- liftIO $ absolutize path'
  assert $ path' == path''

prop_absolutize_strips_redundant_dotdot :: ValidFilePath -> Property
prop_absolutize_strips_redundant_dotdot (ValidFilePath path) = monadicIO $ do
  (Just abs ) <- fmap (fmap takeDirectory) $ liftIO $ absolutize path
  (Just abs') <- liftIO $ absolutize $ path </> ".."
  assert $ abs == abs'

prop_absolutize_strips_redundant_dot :: ValidFilePath -> Property
prop_absolutize_strips_redundant_dot (ValidFilePath path) = monadicIO $ do
  (Just abs ) <- liftIO $ absolutize path
  (Just abs') <- liftIO $ absolutize $ path </> "."
  assert $ abs == abs'
