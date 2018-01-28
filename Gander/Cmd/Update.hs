module Gander.Cmd.Update where

-- TODO do the replace operation with lenses? or dropTo?

import Gander.Config (Config(..))
import Gander.Lib (HashTree(..), buildTree, printHashes, hashHashes)
import System.FilePath (pathSeparator, splitPath, joinPath)
import Data.List (find)

-- import qualified System.Directory.Tree as DT

-- TODO read old and new hash files (not dirs for now)
-- TODO determine path of new one inside old (error if not)
-- TODO replace old subdir with whole new tree
-- TODO write... to stdout? outfile?

cmdUpdate :: Config -> FilePath -> FilePath -> FilePath -> IO ()
cmdUpdate cfg root sub path = do
  tree1 <- buildTree (verbose cfg) (exclude cfg) root
  tree2 <- buildTree (verbose cfg) (exclude cfg) sub
  printHashes $ updateSubTree path tree1 tree2

pathComponents :: FilePath -> [String]
pathComponents f = filter (not . null)
                 $ map (filter (/= pathSeparator))
                 $ splitPath f

-- TODO remove once rewritten to update
subTreeExists :: FilePath -> HashTree -> Bool
subTreeExists p (File n _) = p == n
subTreeExists p (Dir n _ cs _) = case pathComponents p of
  []      -> True
  (n':p') -> case find (\c -> name c == n') cs of
               Nothing -> False
               Just d' -> subTreeExists (joinPath p') d'

wrapInEmptyDir :: FilePath -> HashTree -> HashTree
wrapInEmptyDir n t = Dir { name = n, hash = h, contents = cs, nFiles = nFiles t }
  where
    cs = [t]
    h = hashHashes [hash t]

wrapInEmptyDirs :: FilePath -> HashTree -> HashTree
wrapInEmptyDirs p t = case pathComponents p of
  [] -> error "went too far!"
  (n:ns) -> wrapInEmptyDir n $ wrapInEmptyDirs (joinPath ns) t

-- This inserts the sub tree into the main one at the path.
-- If the path doesn't exist, it makes empty dirs up to it first.
updateSubTree :: FilePath -> HashTree -> HashTree -> HashTree
updateSubTree path main sub = undefined
