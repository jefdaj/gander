{-# LANGUAGE OverloadedStrings #-}

module Gander.Data.Delta
  ( Delta(..)
  , diff
  , prettyDelta
  , printDeltas
  -- , safeDelta
  -- , safeDeltas
  , simDelta
  , simDeltas
  , assertSameTrees -- TODO rename to mention diff
  )
  where

{- This module calculates what a HashTree should look like after doing some git
 - operations, represented as Deltas. It's dramatically faster to update the
 - hashes based on those calculations than re-hash everything from the filesystem.
 - However, you can tell it to do that too and report any differences with the
 - --check flag. Code to actually run Deltas lives in the Run module.
 -}

-- import Gander.Config
import Gander.Data.HashTree
import qualified Data.ByteString.Char8 as B8

import Control.Monad       (when, foldM)
import Data.List           (find)
import Data.Maybe          (fromJust)
--import Gander.Data.DupeMap (listLostFiles)
import Gander.Data.Hash    (prettyHash)
import System.FilePath     ((</>))

-- TODO should these have embedded hashtrees? seems unneccesary but needed for findMoves
--      maybe only some of them are needed: add and edit. and edit only needs one.
data Delta
  = Add  FilePath HashTree
  | Rm   FilePath
  | Mv   FilePath FilePath
  | Edit FilePath HashTree -- TODO remove in favor of subtle use of Add?
  deriving (Read, Show, Eq)

------------------------
-- diff two hashtrees --
------------------------

-- TODO put the hashes back here?
prettyDelta :: Delta -> B8.ByteString
prettyDelta (Add  f  t ) = B8.pack $ "added '"   ++ f  ++ "' (" ++ B8.unpack (prettyHash (hash t)) ++ ")"
prettyDelta (Rm   f    ) = B8.pack $ "removed '" ++ f  ++ "'"
prettyDelta (Edit f  t ) = B8.pack $ "edited '"  ++ f  ++ "' (" ++ B8.unpack (prettyHash (hash t)) ++ ")"
prettyDelta (Mv   f1 f2) = B8.pack $ "moved '"   ++ f1 ++ "' -> '" ++ f2 ++ "'"

printDeltas :: [Delta] -> IO ()
printDeltas = mapM_ (putStrLn . B8.unpack . prettyDelta)

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
fixMoves :: HashTree -> [Delta] -> [Delta]
fixMoves _ [] = []
fixMoves t (d1@(Rm f1):ds) = case find (findMv t d1) ds of
  Just d2@(Add f2 _) -> (Mv f1 f2) : let ds' = filter (/= d2) ds in fixMoves t ds'
  Just d2 -> error $ "findMv returned a non-add: " ++ show d2
  Nothing -> d1 : fixMoves t ds
fixMoves t (d:ds) = d : fixMoves t ds

--------------------------------------------
-- check if simulated operations are safe --
--------------------------------------------

-- TODO think through how to report results more!
-- TODO can this whole thing be trivially written in runDeltaIfSafe?

-- Can a delta be applied without losing anything?
-- TODO for efficiency, should this be part of a larger "applyIfSafe"?
--      (that would return the updated tree at the same time)
-- TODO in order to apply, need actual tree rather than just the hash!
-- safeDelta :: HashTree -> Delta -> Bool
-- safeDelta t d = safeDeltas t [d]
-- 
-- safeDeltas :: HashTree -> [Delta] -> Bool
-- safeDeltas t ds = case simDeltas t ds of
--   Left  _  -> False
--   Right t2 -> null $ listLostFiles t t2

-----------------------------
-- simulate git operations --
-----------------------------

-- TODO think through how to report results more!
simDelta :: HashTree -> Delta -> Either String HashTree
simDelta t (Rm   p    ) = rmSubTree t p
simDelta t (Add  p  t2) = Right $ addSubTree t t2 p
simDelta t (Edit p  t2) = Right $ addSubTree t t2 p
simDelta t (Mv   p1 p2) = case simDelta t (Rm p1) of
  Left  e  -> Left e
  Right t2 -> simDelta t2 $ Add p2 $ fromJust $ dropTo t p1 -- TODO path error here?

simDeltas :: HashTree -> [Delta] -> Either String HashTree
simDeltas = foldM simDelta

-- seems like what we really want is runDeltaIfSafe, which does simDelta, checks safety, then runDelta

-- TODO be clearer on before/after and or expected/actual here
-- assertSameTrees :: FilePath -> HashTree -> HashTree -> IO ()
assertSameTrees :: (String, HashTree) -> (String, HashTree) -> IO ()
assertSameTrees (msg1, tree1) (msg2, tree2) = do
  let wrong = diff tree1 tree2
  when (not $ null wrong) $ do
    putStrLn $ unwords ["error!", msg1, "and", msg2, "should be identical, but aren't:"]
    printDeltas wrong
