{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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

{- A Sim is a randomly generated initial state along with a list of
 - randomly generated Deltas to apply. We want to confirm that we get the same
 - final tree by:
 -
 - 1. applying the deltas with simDeltas
 - 2. running actual git operations on a matching git repo
 -
 - If the simulation matches the files in the git repo at each step, we can be
 - more confident both parts of the codebase are solid.
 -}
 
import Data.Gander.Sim

-- An initial tree along with a list of edits (Deltas) made to it
-- TODO include the simulated final state here too?
-- TODO should this go in the library Sim module instead? And maybe be renamed back to TreeHistory?
-- TODO should this be specialized to just the B8.ByteString one?
data TestSim = TestSim
  { simStart :: TestForest
  , simSteps :: [(Delta B8.ByteString, TestForest)] -- TODO remove the forests here?
  }

-- TODO is there a way to pass data to the arbitrary fns?
--      arbitraryDelta :: TestForest -> Gen TestDelta maybe

-- TODO there's probably an existing quickcheck fn for this, right?
chooseFrom :: [a] -> Gen a
chooseFrom xs = do
  i <- choose (0, length xs - 1)
  return $ xs !! i

chooseForestPath :: HashForest a -> Gen FilePath
chooseForestPath f = chooseFrom $ flattenForestPaths ("" :// f)

arbitraryRm :: HashForest a -> Gen (Delta a)
arbitraryRm forest = Rm <$> chooseForestPath forest

-- TODO this one is a little trickier because it needs to pick a dir path, right?
arbitraryAdd :: HashForest a -> Gen (Delta a)
arbitraryAdd = undefined

arbitraryEdit :: HashForest a -> Gen (Delta a)
arbitraryEdit = undefined

arbitraryMv :: HashForest a -> Gen (Delta a)
arbitraryMv = undefined

arbitraryDelta :: TestForest -> Gen (Delta B8.ByteString)
arbitraryDelta forest = do
  deltaFn <- chooseFrom [arbitraryAdd, arbitraryRm, arbitraryEdit, arbitraryMv]
  deltaFn forest
    -- paths = flattenForestPaths ("" :// forest)

-- Generate steps. This can't actually be an Arbitrary instance because it requires a starting forest.
-- TODO is IO the right idea to fit with QuickCheck here?
-- arbitraryDeltas :: Int -> TestForest -> Gen [(Delta B8.ByteString, TestForest)]
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
