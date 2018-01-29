module Gander.Cmd.Update where

-- TODO if this gets super complicated, might be worth making lenses?
-- TODO move most of this to Lib once it works
-- TODO rename update to insert?

import Debug.Trace

import Gander.Config (Config(..))
import Gander.Lib (HashTree(..), buildTree, printHashes, hashHashes)
import System.FilePath (pathSeparator, splitPath, joinPath)
import Data.List (find, sort)

-- import qualified System.Directory.Tree as DT

-- TODO read old and new hash files (not dirs for now)
-- TODO determine path of new one inside old (error if not)
-- TODO replace old subdir with whole new tree
-- TODO write... to stdout? outfile?

cmdUpdate :: Config -> FilePath -> FilePath -> FilePath -> IO ()
cmdUpdate cfg root sub path = do
  tree1 <- buildTree (verbose cfg) (exclude cfg) root
  tree2 <- buildTree (verbose cfg) (exclude cfg) sub
  printHashes $ insertTreeInDir path tree1 tree2

pathComponents :: FilePath -> [String]
pathComponents f = filter (not . null)
                 $ map (filter (/= pathSeparator))
                 $ splitPath f

wrapInEmptyDir :: FilePath -> HashTree -> HashTree
wrapInEmptyDir n t = Dir { name = n, hash = h, contents = cs, nFiles = nFiles t }
  where
    cs = [t]
    h = hashHashes [hash t]

wrapInEmptyDirs :: FilePath -> HashTree -> HashTree
wrapInEmptyDirs p t = case pathComponents p of
  []     -> error "wrapInEmptyDirs needs at least one dir"
  (n:[]) -> wrapInEmptyDir n t
  (n:ns) -> wrapInEmptyDir n $ wrapInEmptyDirs (joinPath ns) t

-- TODO should contents be sorted by name rather than current Ord instance
insertTreeInDir :: FilePath -> HashTree -> HashTree -> HashTree
insertTreeInDir _ (File _ _) _ = error "can't insert tree in file"
insertTreeInDir path _ _ | null (pathComponents path) = error "can't insert tree at null path"
insertTreeInDir path main sub = main { hash = h', contents = cs', nFiles = n' }
  where
    comps  = trace path $ pathComponents path
    dir    = head comps
    path'  = joinPath $ tail comps
    h'     = hashHashes $ map hash cs'
    cs'    = sort $ filter (\c -> name c /= dir) (contents main) ++ [newSub]
    n'     = nFiles main + nFiles newSub - case oldSub of { Nothing -> 0; Just s -> nFiles s; }
    sub'   = sub { name = last comps }
    oldSub = find (\c -> name c == dir) (contents main)
    newSub = if length comps == 1
               then sub'
               else case oldSub of
                 Nothing -> wrapInEmptyDirs path sub'
                 Just d  -> insertTreeInDir path' d sub'
