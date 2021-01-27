{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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

import Gander.Util
import Gander.Data.Hash
import Gander.Data.HashTree

-------------------------------
-- generate random test data --
-------------------------------

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

-- TODO move to HashTree.hs?
-- TODO this should fail on the empty string right?
parseFileName :: B.ByteString -> Either String FileName
parseFileName bs = parseOnly nameP (B.append bs "\n")

-- TODO prop_roundTripBS toBsFn fromBsFn, for FileName + HashLine + HashTree

----------
-- main --
----------

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

    -- TODO is this actually all from Util? or HashTree?
    describe "FileName" $ do
      -- TODO convert to prop_ style with the \x :: thing
      it "can be round-tripped to bytestring" $ property $ \(n :: FileName) -> do
        let bs = DTE.encodeUtf8 n -- (T.append n "\n") -- TODO maybe the \n was a bad idea then
            n' = parseFileName bs
        n' `shouldBe` (Right n)

    describe "Hash" $ do
      describe "hashString" $ do
        it "hashes a simple test string" $
          (hashString "simple test string") `shouldBe` (Hash { unHash = "NTk2OGNmOWE3MmU1ZmYw" })
      describe "hashFile" $ do
        it "hashes an image from the source tree" $ do
          h <- hashFile False "gander.png"
          h `shouldBe` (Hash {unHash = "NWMwYjNlN2FiZTQ5OWZj"})
        it "matches the default git-annex sha256sum hashes" $
          pendingWith "need annex test harness"
          -- TODO hash files the regular way first
          -- TODO then annex them and use annex-aware hashing
          -- TODO there should be no difference in the hashes

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
