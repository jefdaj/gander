{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SimTest where

import Data.Gander.Sim
import Data.Gander.HashTree
import Data.Gander.HashForest
import Data.Gander.Delta

import HashTreeTest
import HashForestTest
import DeltaTest

import qualified Data.ByteString.Char8 as B8
import Test.QuickCheck
import Data.Maybe (fromJust)

{- A Sim is a randomly generated initial state along with a list of
 - randomly generated Deltas to apply. We want to confirm that we get the same
 - final tree by:
 -
 - 1. applying the deltas with simDeltas
 - 2. running actual git operations on a matching git repo
 -
 - If the simulation matches the files in the git repo at each step, we can be
 - more confident both parts of the codebase are solid.
 -
 - TODO can we also re-generate the list of deltas from the list of states?
 -}
 
import Data.Gander.Sim

-- An initial tree along with a list of edits (Deltas) made to it
-- TODO include the simulated final state here too?
-- TODO should this go in the library Sim module instead? And maybe be renamed back to TreeHistory?
-- TODO should this be specialized to just the B8.ByteString one?
data TestSim = TestSim
  { simStart :: TestForest
  , simSteps :: [(TestDelta, TestForest)] -- TODO remove the forests here?
  }

-- TODO is there a way to pass data to the arbitrary fns?
--      arbitraryDelta :: TestForest -> Gen TestDelta maybe

-- TODO there's probably an existing quickcheck fn for this, right?
chooseFrom :: [a] -> Gen a
chooseFrom xs = do
  i <- choose (0, length xs - 1)
  return $ xs !! i

-- from https://stackoverflow.com/a/5852820
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

chooseTreePath :: HashTree a -> Gen FilePath
chooseTreePath tree = chooseFrom $ flattenTreePaths ("" :/ tree)

chooseForestTree :: HashForest a -> Gen (HashTree a)
chooseForestTree (HashForest trees) = chooseFrom trees

chooseForestPath :: HashForest a -> Gen FilePath
chooseForestPath f = chooseFrom $ flattenForestPaths ("" :// f)

arbitraryRm :: HashForest a -> Gen (Delta a)
arbitraryRm forest = do
  tree <- undefined -- TODO something like editTree :: (HashTree a -> HashTree a) -> Int -> HashForest a -> HashForest a
  Rm <$> chooseForestPath forest

-- TODO this one is a little trickier because it needs to pick a dir path, right?
arbitraryAdd :: Arbitrary (HashTree a) => HashForest a -> Gen (Delta a)
arbitraryAdd = undefined

arbitraryEdit :: Arbitrary (HashTree a) => HashForest a -> Gen (Delta a)
arbitraryEdit forest@(HashForest trees) = do
  index <- choose (0, length trees - 1) -- TODO is -1 right?
  let tree = trees !! index
  tree' <- arbitrary -- :: Gen (HashForest a)
  path <- chooseTreePath tree
  let subTree = fromJust $ dropTo tree path
  return $ Edit path tree tree'

  -- path <- chooseForestPath forest
  -- let old = fromJust $ dropTo forest path -- TODO is this safe?
  -- return undefined

arbitraryMv :: Arbitrary (HashTree a) => HashForest a -> Gen (Delta a)
arbitraryMv = undefined

arbitraryDelta :: Arbitrary (HashTree a) => HashForest a -> Gen (Delta a)
arbitraryDelta forest = do
  deltaFn <- chooseFrom [arbitraryAdd, arbitraryRm, arbitraryEdit, arbitraryMv]
  deltaFn forest
    -- paths = flattenForestPaths ("" :// forest)

-- Generate steps. This can't actually be an Arbitrary instance because it requires a starting forest.
-- TODO is IO the right idea to fit with QuickCheck here?
-- arbitraryDeltas :: Int -> TestForest -> Gen [(TestDelta, TestForest)]
-- arbitraryDeltas nSteps forest
--  | nSteps < 1 = return []
--  | otherwise = do
--      delta <- arbitraryDelta forest
--      let forest' = undefined $ simDelta forest delta
--      deltas <- arbitraryDeltas (nSteps - 1) forest'
--      return $ (delta, forest') : deltas
arbitraryDeltas = undefined

instance Arbitrary TestSim where

  arbitrary = do
    start <- arbitrary :: Gen TestForest
    nSteps <- choose (0, 10 :: Int)
    steps <- arbitraryDeltas nSteps start
    return $ TestSim { simStart = start, simSteps = steps }

  shrink = undefined -- TODO just shrink the deltas, not the tree for now
