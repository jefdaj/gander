{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Old
  (
  )
  where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as DTE

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString
import Test.QuickCheck.Unicode
import qualified Data.Text as T
-- import Test.QuickCheck.Monadic
-- import Control.Exception (evaluate)
import Data.Attoparsec.ByteString.Char8 hiding (D, skipWhile, char)
import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.Combinator

import Util
import Data.Gander.Hash
import Data.Gander.HashTree

----------
-- main --
----------

oldMain :: IO ()
oldMain = hspec $ do

  describe "Data.Gander" $ do

    -- TODO test filename handling here without actually generating all the messy filenames
    describe "HashTree" $ do
      describe "HashLine" $ do
        -- it "can serialize arbitrary filenames" $ pending
        -- it "can parse arbitrary filenames" $ pending
        it "can be round-tripped to a bytestring" $ do
          ls <- generate (arbitrary :: Gen [HashLine])
          let bs = B.unlines $ map prettyHashLine ls
              ls' = parseHashes Nothing bs
          ls' `shouldBe` ls
        -- TODO are prettyHashLine and lineP a good pair for round-tripping a single line?

      describe "HashTree" $ do
        it "builds a tree from the test annex" $ pendingWith "need annex test harness"
        it "can be round-tripped to a file" $ pendingWith "need annex test harness"
        it "can handle unicode filenames" pending

    describe "Delta"    $ it "behaves properly" pending
    describe "DupeSet" $ it "behaves properly" pending

  describe "Util" $ do

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
