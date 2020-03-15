{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class     as H
import qualified Data.HashSet             as S
import Control.Monad.ST
import Data.Text

import Gander.Data
import System.FilePath      ((</>))

type HashTable s k v = C.HashTable s k v

-------------------
-- example usage --
-------------------

foo :: ST s (HashTable s Text Int)
foo = do
  ht <- H.newSized 1
  H.insert ht "abc" 1
  H.insert ht "dea" 3
  return ht

add1 :: HashTable s Text Int -> ST s (HashTable s Text Int)
add1 ht = do
  H.insert ht "abc" 2
  H.insert ht "dea" 4
  return ht

main :: IO ()
main = do
  let x = runST $ H.toList =<< foo
  print x
  let y = runST $ H.toList =<< add1 =<< foo
  print y

--------------
-- my usage --
--------------

dupeMap :: HashTree -> ST s (HashTable s Hash DupeSet)
dupeMap t = do
  ht <- H.newSized 1 -- TODO what's with the size thing? maybe use H.new instead
  addToDupeMap ht t
  return ht

-- inserts all nodes from a tree into an existing dupemap in ST s
addToDupeMap :: HashTable s Hash DupeSet -> HashTree -> ST s ()
addToDupeMap ht t = addToDupeMap' ht "" t

-- same, but start from a given root path
addToDupeMap' :: HashTable s Hash DupeSet -> FilePath -> HashTree -> ST s ()
addToDupeMap' ht dir (File n h      ) = H.insert ht h (1, F, S.singleton (dir </> n))
addToDupeMap' ht dir (Dir  n h cs fs) = do
  H.insert ht h (fs, D, S.singleton (dir </> n))
  mapM_ (addToDupeMap' ht (dir </> n)) cs

-- inserts one node into an existing dupemap in ST s
insertDupeSet :: HashTable s Hash DupeSet -> Hash -> DupeSet -> ST s ()
insertDupeSet ht h d2 = do
  existing <- H.lookup ht h
  case existing of
    Nothing -> H.insert ht h d2
    Just d1 -> H.insert ht h $ mergeDupeSets d1 d2
