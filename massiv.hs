{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

-- first attempt to get Massiv arrays working along with HashTables

import Data.Massiv.Array
import Control.Monad.ST

import qualified Data.HashTable.Class     as H
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashSet             as S
import qualified Data.ByteString.Char8 as B
-- import qualified Data.HashMap.Strict   as M
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
-- type DupeMap     = M.HashMap     Hash DupeSet
type DupeTable s = C.HashTable s Hash DupeSet

testHS :: [S.HashSet B.ByteString]
testHS =
  [ S.singleton "file0"
  , S.fromList ["test1/file1", "test1/file1"]
  , S.fromList ["test2/file1", "test3/file1"]
  ]

testDS :: [DupeSet]
testDS = Prelude.map (\s -> (S.size s, s)) testHS

testHT :: ST s (C.HashTable s Hash DupeSet)
testHT = H.fromList $ Prelude.map
           (\n -> (Hash $ B.pack $ "hash" ++ show n, testDS !! n))
           [0 .. (length testDS - 1)]

-- is this the only way to get testHT out of ST?
testHTL :: [(Hash, DupeSet)]
testHTL = runST $ H.toList =<< testHT

-- TODO is this reasonable?
type MyVector = Array N Ix1 DupeSet

vec1 :: MyVector
vec1 = makeArray Par (Sz1 $ length testDS) (testDS !!)

-- TODO why does it have to be DL?
vec2 :: Array DL Ix1 Int
vec2 = iterateN (Sz1 $ length testDS) succ 0

vec3 :: Array D Ix1 (Int, B.ByteString)
vec3 = makeArray Par (Sz1 $ length testDS) (tmp !!)
  where
   tmp = Prelude.map (\(Hash h, (n, _)) -> (n, h)) $ runST $ H.toList =<< testHT

vec3s :: Array N Ix1 (Int, B.ByteString)
vec3s = quicksort $ compute vec3

-- tests getting the indexes of a hash table

-- TODO any way to get the hashtable size directly?
mkVecFromHT :: Int -> C.HashTable s Hash DupeSet -> ST s MyVector
mkVecFromHT n ht = do
  let vec :: MyVector
      vec = makeArray Par (Sz1 n) (const (0, undefined))
  undefined

-- TODO vec2 from the testHT
-- vec :: ST s (Array N Ix1 DupeSet)
-- vec = undefined

main :: IO ()
main = undefined
