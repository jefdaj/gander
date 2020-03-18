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
import qualified Data.List as L

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
-- TODO does this need a forall s.?
type DupeTable s = C.HashTable s Hash DupeSet

testHS :: [S.HashSet B.ByteString]
testHS =
  [ S.singleton "file0"
  , S.fromList ["test1/file1", "test1/file1"]
  , S.fromList ["test2/file1", "test3/file1", "test4/file1"]
  , S.fromList ["test2/file1", "test3/file1"]
  ]

testDS :: [DupeSet]
testDS = Prelude.map (\s -> (negate $ S.size s, s)) testHS

testHT :: forall s. ST s (C.HashTable s Hash DupeSet)
testHT = H.fromList $ Prelude.map
           (\n -> (Hash $ B.pack $ "hash" ++ show n, testDS !! n))
           [0 .. (length testDS - 1)]

-- is this the only way to get testHT out of ST?
testHTL :: [(Hash, DupeSet)]
testHTL = runST $ H.toList =<< testHT

-- TODO is this reasonable?
type DupeSetVec   = Array N Ix1 DupeSet
type HashScoreVec = Array N Ix1 (Int, B.ByteString)

vec1 :: DupeSetVec
vec1 = makeArray Par (Sz1 $ length testDS) (testDS !!)

-- TODO why does it have to be DL?
vec2 :: Array DL Ix1 Int
vec2 = iterateN (Sz1 $ length testDS) succ 0

vec3 :: Array D Ix1 (Int, B.ByteString)
vec3 = makeArray Par (Sz1 $ length testDS) (tmp !!)
  where
   tmp = Prelude.map (\(Hash h, (n, _)) -> (-n, h)) testHTL

vec3s :: HashScoreVec
vec3s = quicksort $ compute vec3

-- TODO next try to get something like vec3 *inside* ST without using H.toList
--      first using keys, but then with just indexes if possible

mkVecFromHT :: forall s. C.HashTable s Hash DupeSet -> ST s [DupeSet]
mkVecFromHT ht = do
  sets <- H.foldM (\vs (_, v@(n,_)) -> return $ if n < (-1) then (v:vs) else vs) [] ht
  let unsorted = Data.Massiv.Array.fromList Par sets :: DupeSetVec
      sorted   = quicksort $ compute unsorted :: DupeSetVec
  return $ Data.Massiv.Array.toList sorted

dupes :: forall s. C.HashTable s Hash DupeSet -> ST s [DupeSet]
dupes = H.foldM (\vs (_, v@(n,_)) -> return $ if n < (-1) then (v:vs) else vs) []

-- TODO why is having this separate from the vector step so important?
testDS2 :: [DupeSet]
testDS2 = runST $ dupes =<< testHT

sortedDL :: [DupeSet]
sortedDL = Data.Massiv.Array.toList sorted
  where
    sets     = runST $ dupes =<< testHT
    unsorted = Data.Massiv.Array.fromList Par sets :: DupeSetVec
    sorted   = quicksort $ compute unsorted :: DupeSetVec

finished :: [DupeList]
finished = Prelude.map fixElem sortedDL
  where
    fixElem (n, fs) = (negate n, L.sort $ S.toList fs)

main :: IO ()
main = undefined
