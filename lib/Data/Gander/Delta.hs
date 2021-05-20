{-# LANGUAGE OverloadedStrings #-}

module Data.Gander.Delta
  ( Delta(..)
  , diff
  , prettyDelta
  , printDeltas
 )
  where

{- This module calculates what a HashTree should look like after doing some git
 - operations, represented as Deltas. It's dramatically faster to update the
 - hashes based on those calculations than re-hash everything from the filesystem.
 - However, you can tell it to do that too and report any differences with the
 - --check flag. Code to actually run Deltas lives in the Run module.
 -}

-- import Gander.Config
import Data.Gander.HashTree
import Util (n2p)
import qualified Data.ByteString.Char8 as B

import Control.Monad       (when, foldM)
import Data.List           (find)
import Data.Maybe          (fromJust)
--import Data.Gander.DupeMap (listLostFiles)
-- import Data.Gander.Hash    (prettyHash)
import System.FilePath     ((</>))

-- TODO should these have embedded hashtrees? seems unneccesary but needed for findMoves
--      maybe only some of them are needed: add and edit. and edit only needs one.
data Delta a
  = Add  FilePath (HashTree a)
  | Rm   FilePath
  | Mv   FilePath FilePath
  | Edit FilePath (HashTree a) (HashTree a) -- TODO remove in favor of subtle use of Add?
  deriving (Read, Show, Eq)

-- TODO put the hashes back here?
prettyDelta :: Delta () -> B.ByteString
prettyDelta (Add  f _  ) = B.pack $ "added '"   ++ f  ++ "'"
prettyDelta (Rm   f    ) = B.pack $ "removed '" ++ f  ++ "'"
prettyDelta (Edit f _ _) = B.pack $ "edited '"  ++ f  ++ "'"
prettyDelta (Mv   f1 f2) = B.pack $ "moved '"   ++ f1 ++ "' -> '" ++ f2 ++ "'"

printDeltas :: [Delta ()] -> IO ()
printDeltas = mapM_ (putStrLn . B.unpack . prettyDelta)

diff :: ProdTree -> ProdTree -> [Delta ()]
diff = diff' ""

diff' :: FilePath -> ProdTree -> ProdTree -> [Delta ()]
diff' a t1@(File f1 h1 ()) t2@(File f2 h2 ())
  | f1 == f2 && h1 == h2 = []
  | f1 /= f2 && h1 == h2 = [Mv (a </> n2p f1) (a </> n2p f2)]
  | f1 == f2 && h1 /= h2 = [Edit (if a == n2p f1 then n2p f1 else a </> n2p f1) t1 t2]
  | otherwise = error $ "error in diff': " ++ show t1 ++ " " ++ show t2
diff' a (File _ _ ()) t2@(Dir  d _ _ _) = [Rm a, Add (a </> n2p d) t2]
-- TODO wait is this a Mv?
diff' a (Dir  d _ _ _) t2@(File _ _ ()) = [Rm (a </> n2p d), Add (a </> n2p d) t2]
diff' a t1@(Dir _ h1 os _) (Dir _ h2 ns _)
  | h1 == h2 = []
  | otherwise = fixMoves t1 $ rms ++ adds ++ edits
  where
    adds  = [Add (a </> n2p (name x)) x | x <- ns, not $ name x `elem` map name os]
    rms   = [Rm  (a </> n2p (name x))   | x <- os, not $ name x `elem` map name ns]
    edits = concat [diff' (a </> n2p (name o)) o n | o <- os, n <- ns,
                                               o /= n, name o == name n]

-- given two Deltas, are they a matching Rm and Add that together make a Mv?
-- TODO need an initial tree too to check if the hashes match
findMv :: ProdTree -> Delta () -> Delta () -> Bool
findMv t (Rm p) (Add _ t2) = case dropTo t p of
                               Nothing -> False
                               Just t3 -> t2 == t3
findMv _ _ _ = False

-- When a subtree with the same hash is removed and then re-added somewhere
-- else, that should be displayed as a single move operation. This will never
-- match 100% before and after actual operations, because the filesystem
-- version might be a move followed by editing files.
fixMoves :: ProdTree -> [Delta ()] -> [Delta ()]
fixMoves _ [] = []
fixMoves t (d1@(Rm f1):ds) = case find (findMv t d1) ds of
  Just d2@(Add f2 _) -> (Mv f1 f2) : let ds' = filter (/= d2) ds in fixMoves t ds'
  Just d2 -> error $ "findMv returned a non-add: " ++ show d2
  Nothing -> d1 : fixMoves t ds
fixMoves t (d:ds) = d : fixMoves t ds
