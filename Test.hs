module Main where

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do

  -- describe "Prelude.head" $ do
  --   it "returns the first element of a list" $ do
  --     head [23 ..] `shouldBe` (23 :: Int)
  --
  --   it "returns the first element of an *arbitrary* list" $
  --     property $ \x xs -> head (x:xs) == (x :: Int)
  --
  --   it "throws an exception if used with an empty list" $ do
  --     evaluate (head []) `shouldThrow` anyException

  describe "Gander.Data" $ do
    describe "Hash"     $ it "behaves properly" pending
    describe "HashTree" $ it "behaves properly" pending
    describe "Delta"    $ it "behaves properly" pending
    describe "DupeSet" $ it "behaves properly" pending

  describe "Gander.Util" $ do

    describe "absolutize" $ do
      it "is idempotent" pending
      it "strips dots from paths" pending
      it "does not follow symlinks" pending
      it "expands tildes into home directories" pending

    describe "findAnnex" $ do
      it "returns Nothing if given a nonexistent path" pending
      it "returns Nothing if given a non-annex path" pending
      it "returns (Just path) if path is an annex" pending
      it "returns only absolute paths" pending

    describe "isAnnexSymlink" $ do
      it "returns False if given a non-symlink" pending
      it "returns True if given a symlink pointing into a .git/annex/objects dir" pending
      it "returns False if given a symlink pointing somewhere else" pending

    describe "isNonAnnexSymlink" $ do
      it "returns False if given a non-symlink" pending
      it "returns False if given a symlink pointing into a .git/annex/objects dir" pending
      it "returns True if given a symlink pointing somewhere else" pending

  describe "Gander.Run" $ do
    describe "runRsync"       $ it "behaves properly" pending
    describe "runGit"         $ it "behaves properly" pending
    describe "runGitMv"       $ it "behaves properly" pending
    describe "runGitAdd"      $ it "behaves properly" pending
    describe "runGitRm"       $ it "behaves properly" pending
    describe "runGitAnnexAdd" $ it "behaves properly" pending
    describe "runDelta"       $ it "behaves properly" pending
    describe "runSafeDeltas"  $ it "behaves properly" pending

  describe "Gander.Cmd" $ do
    describe "Add"    $ it "behaves properly" pending
    describe "Dedup"  $ it "behaves properly" pending
    describe "Diff"   $ it "behaves properly" pending
    describe "Dupes"  $ it "behaves properly" pending
    describe "Hash"   $ it "behaves properly" pending
    describe "Init"   $ it "behaves properly" pending
    describe "Mv"     $ it "behaves properly" pending
    describe "Rm"     $ it "behaves properly" pending
    describe "Test"   $ it "behaves properly" pending -- TODO is this part of the tests?
    describe "Update" $ it "behaves properly" pending
