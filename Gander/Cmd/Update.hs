module Gander.Cmd.Update where

-- TODO move most of this to Lib once it works

import Gander.Config (Config(..))
import Gander.Lib (HashTree(..), readTree, printHashes, hashContents, pathComponents)
import System.FilePath (joinPath)
import Data.List (find, sortBy)
import Data.Function (on)
import Data.Ord      (compare)

cmdUpdate :: Config -> FilePath -> FilePath -> FilePath -> IO ()
cmdUpdate _ root sub path = do
  tree1 <- readTree root
  tree2 <- readTree sub
  printHashes $ insertTreeInDir tree1 tree2 path

wrapInEmptyDir :: FilePath -> HashTree -> HashTree
wrapInEmptyDir n t = Dir { name = n, hash = h, contents = cs, nFiles = nFiles t }
  where
    cs = [t]
    h = hashContents cs

wrapInEmptyDirs :: FilePath -> HashTree -> HashTree
wrapInEmptyDirs p t = case pathComponents p of
  []     -> error "wrapInEmptyDirs needs at least one dir"
  (n:[]) -> wrapInEmptyDir n t
  (n:ns) -> wrapInEmptyDir n $ wrapInEmptyDirs (joinPath ns) t

insertTreeInDir :: HashTree -> HashTree -> FilePath -> HashTree
insertTreeInDir (File _ _) _ _ = error $ "attempt to insert tree into a file"
insertTreeInDir _ _ path | null (pathComponents path) = error "can't insert tree at null path"
insertTreeInDir main sub path = main { hash = h', contents = cs', nFiles = n' }
  where
    comps  = pathComponents path
    p1     = head comps
    path'  = joinPath $ tail comps
    h'     = hashContents cs'
    cs'    = sortBy (compare `on` name) $ filter (\c -> name c /= p1) (contents main) ++ [newSub]
    n'     = nFiles main + nFiles newSub - case oldSub of { Nothing -> 0; Just s -> nFiles s; }
    sub'   = sub { name = last comps }
    oldSub = find (\c -> name c == p1) (contents main)
    newSub = if length comps == 1
               then sub'
               else case oldSub of
                 Nothing -> wrapInEmptyDirs path sub'
                 Just d  -> insertTreeInDir d sub' path'
