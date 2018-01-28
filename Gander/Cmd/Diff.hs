module Gander.Cmd.Diff where

import Gander.Config (Config(..))
import Gander.Lib (buildTree, diff, printDiffs)

cmdDiff :: Config -> FilePath -> FilePath -> IO ()
cmdDiff cfg old new = do
  tree1 <- buildTree (verbose cfg) (exclude cfg) old
  tree2 <- buildTree (verbose cfg) (exclude cfg) new
  printDiffs $ diff tree1 tree2
