module SimTest where

import Data.Gander.Sim
import Data.Gander.HashTree
import Data.Gander.Delta

import Test.QuickCheck

{- A TreeSim is a randomly generated initial HashTree along with a list of
 - randomly generated Deltas to apply. We want to confirm that we get the same
 - final tree by:
 -
 - 1. applying the deltas with simDeltas
 - 2. running git operations on a matching git repo
 -
 - Then we can be confident that both parts of the codebase are solid.
 -}
 
import Data.Gander.Sim

-- An initial tree along with a list of edits (Deltas) made to it
-- TODO include the simulated final state here too?
-- TODO should this go in the library Sim module instead? And maybe be renamed back to TreeHistory?
data TreeSim a = TreeSim
  { tree :: HashTree a
  , deltas :: [(Delta a, HashTree a)]
  }

instance Arbitrary a => Arbitrary (TreeSim a) where
  arbitrary = undefined -- TODO generate a tree, then fold over 
  shrink = undefined -- TODO just shrink the deltas, not the tree for now
