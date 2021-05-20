module SimTest where

{- A Sim is a randomly generated initial HashTree along with a list of Deltas
 - to apply. We want to confirm that we get the same final tree by:
 -
 - 1. simulating git operations using simDeltas
 - 2. running git operations on a test git repo
 -
 - Then we can be confident that both parts of the codebase are solid.
 - See the SimTest module for most of the related code.
 -}
 
import Data.Gander.Sim
