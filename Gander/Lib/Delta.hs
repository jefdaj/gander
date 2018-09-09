module Gander.Lib.Delta
  ( Delta(..)
  , diff
  , prettyDiff
  , printDiffs
  , safe
  , runDelta
  , runDeltas
  )
  where

import Gander.Lib.Hash (Hash(..))
import Gander.Lib.HashTree (HashTree(..), treeContainsPath, treeContainsHash, addSubTree)
import System.FilePath ((</>))
import Data.List (find)

data Delta
  = Add  FilePath HashTree
  | Rm   FilePath HashTree
  | Mv   FilePath FilePath HashTree
  | Edit FilePath HashTree HashTree
  deriving (Read, Show, Eq)

---------------------------------
-- list changes after the fact --
---------------------------------

prettyHash :: HashTree -> String
prettyHash = take 8 . unHash . hash

prettyDiff :: Delta -> String
prettyDiff (Add  f     t    ) = "added '"   ++ f  ++ "' (" ++ prettyHash t ++ ")"
prettyDiff (Rm   f     t    ) = "removed '" ++ f  ++ "' (" ++ prettyHash t ++ ")"
prettyDiff (Edit f     t1 t2) = "edited '"  ++ f  ++ "' (" ++ prettyHash t1 ++ " -> " ++ prettyHash t2 ++ ")"
prettyDiff (Mv   f1 f2 t    ) = "moved '"   ++ f1 ++ "' -> '" ++ f2 ++ "' (" ++ prettyHash t ++ ")"

printDiffs :: [Delta] -> IO ()
printDiffs = mapM_ (putStrLn . prettyDiff)

diff :: HashTree -> HashTree -> [Delta]
diff = diff' ""

-- TODO need to make hashes take filenames into account too before this will work!
diff' :: FilePath -> HashTree -> HashTree -> [Delta]
diff' r t1@(File f1 h1) t2@(File f2 h2)
  | f1 == f2 && h1 == h2 = []
  | f1 /= f2 && h1 == h2 = [Mv (r </> f1) (r </> f2) t1]
  | f1 == f2 && h1 /= h2 = [Edit (if r == f1 then f1 else r </> f1) t1 t2]
  | otherwise = error $ "error in diff': " ++ show t1 ++ " " ++ show t2
diff' r t1@(File _ _) t2@(Dir  d _ _ _) = [Rm r t1, Add (r </> d) t2]
-- TODO wait is this a Mv?
diff' r t1@(Dir  d _ _ _) t2@(File _ _) = [Rm (r </> d) t1, Add (r </> d) t2]
diff' r (Dir _ h1 os _) (Dir _ h2 ns _)
  | h1 == h2 = []
  | otherwise = fixMoves $ rms ++ adds ++ edits
  where
    adds  = [Add (r </> name x) x | x <- ns, not $ name x `elem` map name os]
    rms   = [Rm  (r </> name x) x | x <- os, not $ name x `elem` map name ns]
    edits = concat [diff' (r </> name o) o n | o <- os, n <- ns,
                                               o /= n, name o == name n]

-- given two Deltas, are they a matching Rm and Add that together make a Mv?
findMv :: Delta -> Delta -> Bool
findMv (Rm _ h1) (Add _ h2) = h1 == h2
findMv _ _ = False

-- When a subtree with the same hash is removed and then re-added somewhere
-- else, that should be displayed as a single move operation. This will never
-- match 100% before and after actual operations, because the filesystem
-- version might be a move followed by editing files.
-- TODO to calculate that, need to store/get the hash of each subtree
-- TODO should edits be disallowed? that way all operations are hashable
fixMoves :: [Delta] -> [Delta]
fixMoves [] = []
fixMoves (d1@(Rm f1 h):ds) = case find (findMv d1) ds of
  Just d2@(Add f2 _) -> (Mv f1 f2 h) : let ds' = filter (/= d2) ds in fixMoves ds'
  Just d2 -> error $ "findMv returned a non-add: " ++ show d2
  Nothing -> d1 : fixMoves ds
fixMoves (d:ds) = d : fixMoves ds

----------------------------------
-- calculate changes beforehand --
----------------------------------

-- Takes a starting HashTree and a Delta, and applies the delta.
-- Useful for checking whether git operations will be safe,
-- and that the calculated diffs match actual changes afterward.
runDelta :: HashTree -> Delta -> HashTree
runDelta t1 (Add  f t2) = addSubTree t1 t2 f
runDelta _ (Rm   _ _  ) = undefined
runDelta _ (Mv   _ _ _) = undefined
runDelta _ (Edit _ _ _) = undefined

-- TODO do I want foldl' here instead??
runDeltas :: HashTree -> [Delta] -> HashTree
runDeltas = foldl runDelta

-- Can a delta be applied without losing anything?
-- TODO for efficiency, should this be part of a larger "applyIfSafe"?
--      (that would return the updated tree at the same time)
-- TODO in order to apply, need actual tree rather than just the hash!
safe :: HashTree -> Delta -> Bool
safe t (Add    p _  ) = not $ treeContainsPath t p
safe t (Mv   _ p _  ) = not $ treeContainsPath t p
safe t d@(Rm   _ s  ) = treeContainsHash (runDelta t d) (hash s)
safe t d@(Edit _ s _) = treeContainsHash (runDelta t d) (hash s)

-- apply the delta if safe, otherwise return an explanation
-- applyPlan :: HashTree -> Delta -> Either String HashTree
-- applyPlan t (Add p _)
--   | treeContainsPath t p = Left $ "adding '" ++ p ++ "' would overwrite an existing path"
--   | otherwise = Right $ runDelta 
