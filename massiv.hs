{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.ST
import Data.Hashable (Hashable(..))
import Control.DeepSeq

import qualified Data.ByteString.Char8    as B
import qualified Data.HashSet             as S
import qualified Data.HashTable.Class     as H
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.List                as L
import qualified Data.Massiv.Array        as A

data TreeType = D | F
  deriving (Eq, Read, Show, Ord)

instance NFData TreeType
  where rnf = const () -- TODO is this valid?

newtype Hash = Hash { unHash :: B.ByteString }
  deriving (Eq, Read, Show, Ord)

-- This is unrelated to Gander's hashing. It's needed to use Hashes as H.HashTable keys
instance Hashable Hash
  where
    hashWithSalt n h = hashWithSalt n (unHash h)

-- the Int is nfiles
type DupeSet  = (Int, TreeType, S.HashSet B.ByteString)
type DupeList = (Int, TreeType, [B.ByteString]) -- TODO remove?
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

-- TODO is this reasonable?
type DupeSetVec   = A.Array A.N A.Ix1 DupeSet
type HashScoreVec = A.Array A.N A.Ix1 (Int, TreeType, B.ByteString)

dupesByNFiles :: (forall s. ST s (DupeTable s)) -> [DupeList]
dupesByNFiles ht = Prelude.map fixElem sortedL
  where
    dupes    = H.foldM (\vs (_, v@(n,_,_)) -> return $ if n < (-1) then (v:vs) else vs) []
    sets     = runST $ dupes =<< ht
    unsorted = A.fromList A.Par sets :: DupeSetVec
    sorted   = A.quicksort $ A.compute unsorted :: DupeSetVec
    sortedL  = A.toList sorted
    fixElem (n, t, fs) = (negate n, t, L.sort $ S.toList fs)

main :: IO ()
main = do
  print testHS
  print testDS
  print $ dupesByNFiles testHT
