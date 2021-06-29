{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimTest where

-- import Debug.Trace

import Data.Gander.Sim
import Data.Gander.HashTree
import Data.Gander.HashForest
import Data.Gander.Delta
import Util (n2p, replaceNth)

import HashTreeTest
import HashForestTest
import DeltaTest

import qualified Data.ByteString.Char8 as B8
import Test.QuickCheck
import Data.Maybe (fromJust, listToMaybe, fromMaybe)
import Data.Either (Either(..), fromRight)
import System.FilePath ((</>))
import Control.Monad.IO.Class (liftIO)

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
  deriving (Eq, Read, Show)

simFinish :: TestSim -> TestForest
simFinish (TestSim {simStart = start, simSteps = []}) = start
simFinish s = snd $ last $ simSteps s

-- TODO is there a way to pass data to the arbitrary fns?
--      arbitraryDelta :: TestForest -> Gen TestDelta maybe

-- TODO hey, are these arbitrary edits all on one tree? if so you should have
-- them be that and compose them with replacenth!

-- TODO there's probably an existing quickcheck fn for this, right?
-- TODO what if hte list is empty?
chooseFrom :: [a] -> Gen a
chooseFrom [] = error "attempt to choose from empty list"
chooseFrom xs = do
  i <- choose (0, length xs - 1)
  return $ xs !! i

chooseTreePath :: HashTree a -> Gen FilePath
chooseTreePath = chooseFrom . map fst . listTreeNodePaths

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

-- if the forest contains any matching nodes, choose one randomly
-- TODO also include the path to it, like zipPaths
chooseNodeMatching :: (HashTree a -> Bool) -> HashTree a -> Gen (Maybe ((FilePath, HashTree a)))
chooseNodeMatching pred tree = if null dirs then return Nothing else fmap Just $ chooseFrom dirs
  where
    dirs = filter (pred . snd) $ listTreeNodePaths tree

chooseDir :: HashTree a -> Gen (Maybe ((FilePath, HashTree a)))
chooseDir = chooseNodeMatching isDir

-- chooseForestPath :: HashForest a -> Gen FilePath
-- chooseForestPath f = chooseFrom $ flattenForestPaths ("" :// f)

isDir :: HashTree a -> Bool
isDir (Dir _ _ _ _) = True
isDir (File _ _ _ ) = False

isFile :: HashTree a -> Bool
isFile = not . isDir

arbitraryRm :: TestTree -> Gen TestDelta
arbitraryRm t@(File {}) = error $ "attempt to rm top-level File: " ++ show t
arbitraryRm tree = Rm <$> chooseTreePath tree

-- TODO what happens if this is being added to the top level of a forest?
-- TODO this one is a little trickier because it needs to pick a dir path, right?
arbitraryAdd :: TestTree -> Gen TestDelta
arbitraryAdd tree = do
  dstDir <- fmap (fst . fromMaybe ("", tree)) $ chooseDir tree
  newTree <- arbitrary
  let dstPath = dstDir </> n2p (name newTree)
  return $ Add dstPath newTree

-- TODO is swapping a dir for a file or vice versa a valid edit? or should it be restricted to files only?
arbitraryEdit :: TestTree -> Gen TestDelta
arbitraryEdit f@(File {}) = do
  new <- arbitrary
  return $ Edit (n2p $ name f) f new
arbitraryEdit tree = do
  path <- chooseTreePath tree
  let subTree  = fromJust $ dropTo tree path -- TODO bug here
  subTree' <- arbitrary
  return $ Edit path subTree (subTree' { name = name subTree })

-- TODO is it any different if the new path happens to exist already?
-- TODO what if it's inside a new dir that doesn't exist already (requires mkdir)?
arbitraryMv :: TestTree -> Gen TestDelta
arbitraryMv tree = do
  oldPath <- chooseTreePath tree
  newDir  <- fmap (fst . fromMaybe ("", tree)) $ chooseDir tree
  newName <- arbitrary :: Gen FilePath
  let newPath = newDir </> newName
  return $ Mv oldPath newPath

-- TODO are all the operations always valid once you have a tree?
arbitraryDelta :: TestTree -> Gen TestDelta
arbitraryDelta tree = do
  deltaFn <- chooseFrom $ case tree of
                            (File {}) -> [arbitraryEdit] -- TODO any others?
                            (Dir  {}) -> [arbitraryAdd, arbitraryRm, arbitraryEdit, arbitraryMv]
  deltaFn tree

-- Generate steps. This can't actually be an Arbitrary instance because it
-- requires a starting forest.
arbitraryDeltas :: Int -> TestForest -> Gen [(TestDelta, TestForest)]

-- for an empty forest, the only valid delta is Add so we have to do that first
arbitraryDeltas nSteps f@(HashForest [])
  | nSteps < 1 = return []
  | otherwise = do
                  t <- arbitrary
                  let d = Add (n2p $ name t) t
                  arbitraryDeltas' (nSteps - 1) f d

arbitraryDeltas nSteps forest@(HashForest trees)
 | nSteps < 1 = return []
 | otherwise = do

     -- TODO does this still need to be done by tree rather than across the whole forest?
     -- TODO does this ignore the possibility of adding or deleting top-level trees?
     op <- choose (0 :: Int, 3)
     delta <- if op == 0
                then do
                  -- add a new tree
                  new <- arbitrary :: Gen TestTree
                  return $ Add (n2p $ name new) new
                else do
                  -- edit an existing tree
                  index <- choose (0 :: Int, length trees - 1)
                  let tree = trees !! index
                  arbitraryDelta tree
     arbitraryDeltas' (nSteps - 1) forest delta

arbitraryDeltas' nSteps forest delta = case simDeltaForest forest delta of
       Left  errMsg -> error errMsg
       Right forest' -> do
         deltas <- arbitraryDeltas nSteps forest' -- TODO something safer!
         return $ (delta, forest') : deltas


instance Arbitrary TestSim where

  arbitrary = do
    start <- arbitrary :: Gen TestForest
    nSteps <- choose (0, 10 :: Int)
    steps <- arbitraryDeltas nSteps start
    return $ TestSim { simStart = start, simSteps = steps }

  shrink s@(TestSim {}) = map (\ss -> s {simSteps = ss}) (shrink $ simSteps s)

-- TODO is there a better way to put this?
prop_sims_finish_without_errors :: TestSim -> Bool
prop_sims_finish_without_errors d = simFinish d `seq` True
