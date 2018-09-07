module Gander.Lib.Delta
  ( Delta(..)
  , diff
  , prettyDiff
  , printDiffs
  )
  where

import Gander.Lib.Hash (Hash(..))
import Gander.Lib.HashTree (HashTree(..))
import System.FilePath ((</>))
import Data.List (find)

data Delta
  = Add  FilePath Hash
  | Rm   FilePath Hash
  | Edit FilePath Hash Hash
  | Mv   FilePath FilePath Hash
  deriving (Read, Show, Eq)

---------------------------------
-- list changes after the fact --
---------------------------------

prettyDiff :: Delta -> String
prettyDiff (Add f (Hash h)) = "added '"   ++ f ++ "' (" ++ take 8 h ++ ")"
prettyDiff (Rm  f (Hash h)) = "removed '" ++ f ++ "' (" ++ take 8 h ++ ")"
prettyDiff (Mv f1 f2 (Hash h))
  = "moved '" ++ f1 ++ "' -> '" ++ f2 ++ "' (" ++ take 8 h ++ ")"
prettyDiff (Edit f (Hash h1) (Hash h2))
  = "edited '" ++ f ++ "' (" ++ take 8 h1 ++ " -> " ++ take 8 h2 ++ ")"

printDiffs :: [Delta] -> IO ()
printDiffs = mapM_ (putStrLn . prettyDiff)

diff :: HashTree -> HashTree -> [Delta]
diff = diff' ""

-- TODO need to make hashes take filenames into account too before this will work!
diff' :: FilePath -> HashTree -> HashTree -> [Delta]
diff' r t1@(File f1 h1) t2@(File f2 h2)
  | f1 == f2 && h1 == h2 = []
  | f1 /= f2 && h1 == h2 = [Mv (r </> f1) (r </> f2) h1]
  | f1 == f2 && h1 /= h2 = [Edit (if r == f1 then f1 else r </> f1) h1 h2]
  | otherwise = error $ "error in diff': " ++ show t1 ++ " " ++ show t2
diff' r (File _ h1    ) (Dir  d h2 _ _) = [Rm r h1, Add (r </> d) h2]
diff' r (Dir  d h1 _ _) (File _ h2    ) = [Rm (r </> d) h1, Add r h2]
diff' r (Dir _ h1 os _) (Dir _ h2 ns _)
  | h1 == h2 = []
  | otherwise = fixMoves $ rms ++ adds ++ edits
  where
    adds  = [Add (r </> name x) (hash x) | x <- ns, not $ name x `elem` map name os]
    rms   = [Rm  (r </> name x) (hash x) | x <- os, not $ name x `elem` map name ns]
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

-- TODO write this... but in a different module?
