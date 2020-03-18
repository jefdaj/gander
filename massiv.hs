{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RankNTypes #-}

module Main where

-- first attempt to get Massiv arrays working along with HashTables

import qualified Data.Massiv.Array as A
import Control.Monad.ST

import qualified Data.HashTable.Class     as H
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashSet             as S
import qualified Data.ByteString.Char8 as B
-- import qualified Data.HashMap.Strict   as M
import Data.Hashable      (Hashable(..))
import qualified Data.List as L
import Control.DeepSeq

-- TODO any reason to retain F/D information at this stage?
data TreeType = D | F deriving (Eq, Read, Show, Ord)

instance NFData TreeType
  where rnf = const ()

newtype Hash = Hash { unHash :: B.ByteString }
  deriving (Eq, Read, Show, Ord)

-- This is unrelated to Gander's hashing. It's required to use Data.HashMap
instance Hashable Hash
  where
    hashWithSalt n h = hashWithSalt n (unHash h)

-- the Int is nfiles
type DupeSet  = (Int, TreeType, S.HashSet B.ByteString)
type DupeList = (Int, TreeType, [B.ByteString]) -- TODO remove?

-- in the actual program, bytestrings are wrapped with the Hash constuctor
-- type DupeMap     = M.HashMap     Hash DupeSet
-- TODO does this need a forall s.?
type DupeTable s = C.HashTable s Hash DupeSet

testHS :: [(TreeType, S.HashSet B.ByteString)]
testHS =
  [ (F, S.singleton "file0")
  , (D, S.fromList ["test1/file1", "test1/file1"])
  , (D, S.fromList ["test2/file1", "test3/file1", "test4/file1"])
  , (D, S.fromList ["test2/file1", "test3/file1"])
  ]

testDS :: [DupeSet]
testDS = Prelude.map (\(t, s) -> (negate $ S.size s, t, s)) testHS

testHT :: forall s. ST s (C.HashTable s Hash DupeSet)
testHT = H.fromList $ Prelude.map
           (\n -> (Hash $ B.pack $ "hash" ++ show n, testDS !! n))
           [0 .. (length testDS - 1)]

-- is this the only way to get testHT out of ST?
testHTL :: [(Hash, DupeSet)]
testHTL = runST $ H.toList =<< testHT

testHTL2 :: (forall s. ST s (C.HashTable s Hash DupeSet)) -> [(Hash, DupeSet)]
testHTL2 ht = runST $ H.toList =<< ht

-- TODO is this reasonable?
type DupeSetVec   = A.Array A.N A.Ix1 DupeSet
type HashScoreVec = A.Array A.N A.Ix1 (Int, TreeType, B.ByteString)

vec1 :: DupeSetVec
vec1 = A.makeArray A.Par (A.Sz1 $ length testDS) (testDS !!)

-- TODO why does it have to be DL?
vec2 :: A.Array A.DL A.Ix1 Int
vec2 = A.iterateN (A.Sz1 $ length testDS) succ 0

vec3 :: A.Array A.D A.Ix1 (Int, TreeType, B.ByteString)
vec3 = A.makeArray A.Par (A.Sz1 $ length testDS) (tmp !!)
  where
   tmp = Prelude.map (\(Hash h, (n,t,_)) -> (-n, t, h)) testHTL

vec3s :: HashScoreVec
vec3s = A.quicksort $ A.compute vec3

-- TODO next try to get something like vec3 *inside* ST without using H.toList
--      first using keys, but then with just indexes if possible

mkVecFromHT :: forall s. C.HashTable s Hash DupeSet -> ST s [DupeSet]
mkVecFromHT ht = do
  sets <- H.foldM (\vs (_, v@(n,_,_)) -> return $ if n < (-1) then (v:vs) else vs) [] ht
  let unsorted = A.fromList A.Par sets :: DupeSetVec
      sorted   = A.quicksort $ A.compute unsorted :: DupeSetVec
  return $ A.toList sorted

dupes :: forall s. C.HashTable s Hash DupeSet -> ST s [DupeSet]
dupes = H.foldM (\vs (_, v@(n,_,_)) -> return $ if n < (-1) then (v:vs) else vs) []

-- TODO why is having this separate from the vector step so important?
testDS2 :: [DupeSet]
testDS2 = runST $ dupes =<< testHT

sortedDL :: [DupeSet]
sortedDL = A.toList sorted
  where
    sets     = runST $ dupes =<< testHT
    unsorted = A.fromList A.Par sets :: DupeSetVec
    sorted   = A.quicksort $ A.compute unsorted :: DupeSetVec

finished :: [DupeList]
finished = Prelude.map fixElem sortedDL
  where
    fixElem (n, t, fs) = (negate n, t, L.sort $ S.toList fs)

main :: IO ()
main = do
  print testHS
  print testDS
  print testDS2
  print finished
