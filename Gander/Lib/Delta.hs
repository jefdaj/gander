module Gander.Lib.Delta
  ( Delta(..)
  , diff
  , prettyDelta
  , printDeltas
  , safe
  , simDelta
  , simDeltas
  )
  where

{- This module calculates what a HashTree should look like after doing some git
 - operations, represented as Deltas. It's dramatically faster to update the
 - hashes based on those calculations than re-hash everything from the filesystem.
 - However, you can tell it to do that too and report any differences with the
 - --check flag. Code to actually run Deltas lives in the Run module.
 -}

import Gander.Config
import Gander.Lib.Hash (prettyHash)
import Gander.Lib.HashTree (HashTree(..), treeContainsPath, treeContainsHash, addSubTree, dropTo)

import System.FilePath     ((</>))
import Data.List           (find)
import Data.Maybe          (isJust)
-- import Data.Either         (fromRight)

-- TODO should these have embedded hashtrees? seems unneccesary but needed for findMoves
--      maybe only some of them are needed: add and edit. and edit only needs one.
data Delta
  = Add  FilePath HashTree
  | Rm   FilePath
  | Mv   FilePath FilePath
  | Edit FilePath HashTree
  deriving (Read, Show, Eq)

------------------------
-- diff two hashtrees --
------------------------

prettyDiff :: Delta -> String
prettyDiff (Add  f     t    ) = "added '"   ++ f  ++ "' (" ++ prettyHash (hash t ) ++ ")"
prettyDiff (Rm   f     t    ) = "removed '" ++ f  ++ "' (" ++ prettyHash (hash t ) ++ ")"
prettyDiff (Edit f     t1 t2) = "edited '"  ++ f  ++ "' (" ++ prettyHash (hash t1) ++ " -> " ++ prettyHash (hash t2) ++ ")"
prettyDiff (Mv   f1 f2 t    ) = "moved '"   ++ f1 ++ "' -> '" ++ f2 ++ "' (" ++ prettyHash (hash t) ++ ")"

printDiffs :: [Delta] -> IO ()
printDiffs = mapM_ (putStrLn . prettyDiff)

diff :: HashTree -> HashTree -> [Delta]
diff = diff' ""

diff' :: FilePath -> HashTree -> HashTree -> [Delta]
diff' a t1@(File f1 h1) t2@(File f2 h2)
  | f1 == f2 && h1 == h2 = []
  | f1 /= f2 && h1 == h2 = [Mv (a </> f1) (a </> f2)]
  | f1 == f2 && h1 /= h2 = [Edit (if a == f1 then f1 else a </> f1) t2]
  | otherwise = error $ "error in diff': " ++ show t1 ++ " " ++ show t2
diff' a (File _ _) t2@(Dir  d _ _ _) = [Rm a, Add (a </> d) t2]
-- TODO wait is this a Mv?
diff' a (Dir  d _ _ _) t2@(File _ _) = [Rm (a </> d), Add (a </> d) t2]
diff' a t1@(Dir _ h1 os _) (Dir _ h2 ns _)
  | h1 == h2 = []
  | otherwise = fixMoves t1 $ rms ++ adds ++ edits
  where
    adds  = [Add (a </> name x) x | x <- ns, not $ name x `elem` map name os]
    rms   = [Rm  (a </> name x)   | x <- os, not $ name x `elem` map name ns]
    edits = concat [diff' (a </> name o) o n | o <- os, n <- ns,
                                               o /= n, name o == name n]

-- given two Deltas, are they a matching Rm and Add that together make a Mv?
-- TODO need an initial tree too to check if the hashes match
findMv :: HashTree -> Delta -> Delta -> Bool
findMv t (Rm p) (Add _ t2) = case dropTo t p of
                               Nothing -> False
                               Just t3 -> t2 == t3
findMv _ _ _ = False

-- When a subtree with the same hash is removed and then re-added somewhere
-- else, that should be displayed as a single move operation. This will never
-- match 100% before and after actual operations, because the filesystem
-- version might be a move followed by editing files.
-- TODO to calculate that, need to store/get the hash of each subtree
-- TODO should edits be disallowed? that way all operations are hashable
fixMoves :: HashTree -> [Delta] -> [Delta]
fixMoves _ [] = []
fixMoves t (d1@(Rm f1):ds) = case find (findMv t d1) ds of
  Just d2@(Add f2 _) -> (Mv f1 f2) : let ds' = filter (/= d2) ds in fixMoves t ds'
  Just d2 -> error $ "findMv returned a non-add: " ++ show d2
  Nothing -> d1 : fixMoves t ds
fixMoves t (d:ds) = d : fixMoves t ds

-----------------------------
-- simulate git operations --
-----------------------------

simDelta :: HashTree -> Delta -> Either String HashTree
simDelta _ (Add  _ _) = undefined
simDelta _ (Mv   _ _) = undefined
simDelta _ (Rm   _  ) = undefined
simDelta _ (Edit _ _) = undefined

simDeltas :: HashTree -> [Delta] -> Either String HashTree
simDeltas = undefined

--------------------------------------------
-- check if simulated operations are safe --
--------------------------------------------

-- Can a delta be applied without losing anything?
-- TODO for efficiency, should this be part of a larger "applyIfSafe"?
--      (that would return the updated tree at the same time)
-- TODO in order to apply, need actual tree rather than just the hash!
safe :: HashTree -> Delta -> Bool
safe t (Add  p  _ ) = not $ treeContainsPath t p
safe t (Mv   _  p2) = not $ treeContainsPath t p2
safe _ (Rm   _  ) = undefined -- treeContainsHash (fromRight False $ simDelta t d) (hash s)
safe _ (Edit _ _) = undefined -- treeContainsHash (fromRight False $ simDelta t d) (hash s)

-- apply the delta if safe, otherwise return an explanation
-- applyPlan :: HashTree -> Delta -> Either String HashTree
-- applyPlan t (Add p _)
--   | treeContainsPath t p = Left $ "adding '" ++ p ++ "' would overwrite an existing path"
--   | otherwise = Right $ simDelta 
