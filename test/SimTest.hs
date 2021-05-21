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

-- TODO hey, are these arbitrary edits all on one tree? if so you should have
-- them be that and compose them with replacenth!

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

filterDirs :: [HashTree a] -> [HashTree a]
filterDirs [] = []
filterDirs (  (File {}):ts) =     filterDirs ts
filterDirs (d@(Dir  {}):ts) = d : filterDirs ts

-- same as chooseTreePath, but 
-- TODO wait, this doesn't have the tree itself, just the filepath. separate fn for that?
--      maybe rewrite flattenTree(') to return anchored trees and have those made into paths after
-- chooseTreeDir :: HashTree a -> Gen FilePath
-- chooseTreeDir 

chooseTree :: HashForest a -> Gen (HashTree a)
chooseTree (HashForest trees) = chooseFrom trees

-- chooseForestPath :: HashForest a -> Gen FilePath
-- chooseForestPath f = chooseFrom $ flattenForestPaths ("" :// f)

arbitraryRm :: HashTree a -> Gen (Delta a)
arbitraryRm tree = do
  -- tree <- undefined -- TODO something like editTree :: (HashTree a -> HashTree a) -> Int -> HashForest a -> HashForest a
  Rm <$> chooseTreePath tree

-- TODO this one is a little trickier because it needs to pick a dir path, right?
arbitraryAdd :: HashTree a -> Gen (Delta a)
arbitraryAdd = undefined

-- TODO is swapping a dir for a file or vice versa a valid edit? or should it be restricted to files only?
arbitraryEdit :: Arbitrary (HashTree a) => HashTree a -> Gen (Delta a)
arbitraryEdit tree = do
  path <- chooseTreePath tree
  let subTree  = fromJust $ dropTo tree path
  subTree' <- fmap (\t -> t {name = name tree}) arbitrary -- set the name to match the old one
  return $ Edit path subTree subTree'

arbitraryMv :: HashTree a -> Gen (Delta a)
arbitraryMv = undefined

arbitraryDelta :: Arbitrary (HashTree a) => HashTree a -> Gen (Delta a)
arbitraryDelta tree = do
  deltaFn <- chooseFrom [arbitraryAdd, arbitraryRm, arbitraryEdit, arbitraryMv]
  deltaFn tree

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
