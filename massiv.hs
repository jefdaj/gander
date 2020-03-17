{-# LANGUAGE OverloadedStrings #-}

module Main where

-- first attempt to get Massiv arrays working along with HashTables

import Data.Massiv.Array
import Control.Monad.ST

import qualified Data.HashTable.Class     as H
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashSet             as S
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as M
import Data.Hashable      (Hashable(..))

-- TODO any reason to retain F/D information at this stage?
-- data TreeType = F | D deriving (Eq, Read, Show)

newtype Hash = Hash { unHash :: B.ByteString }
  deriving (Eq, Read, Show, Ord)

-- This is unrelated to Gander's hashing. It's required to use Data.HashMap
instance Hashable Hash
  where
    hashWithSalt n h = hashWithSalt n (unHash h)

-- the Int is nfiles
type DupeSet  = (Int, S.HashSet B.ByteString)
type DupeList = (Int, [B.ByteString]) -- TODO remove?

-- in the actual program, bytestrings are wrapped with the Hash constuctor
type DupeMap     = M.HashMap     Hash DupeSet
type DupeTable s = C.HashTable s Hash DupeSet

testSets :: [DupeSet]
testSets =
  [ (1, S.singleton "file0")
  , (2, S.fromList ["test2/file1", "test3/file1"])
  ]

-- this compiles, but the table can't be pulled out of ST right?
testTable :: ST s (C.HashTable s Hash DupeSet)
testTable = H.fromList
  [ (Hash "hash0", testSets !! 0)
  , (Hash "hash1", testSets !! 1)
  ]

array1 :: Array D Ix1 DupeSet
array1 = makeArray Par (Sz1 $ length testSets) (testSets !!)

main :: IO ()
main = undefined
