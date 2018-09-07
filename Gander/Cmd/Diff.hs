module Gander.Cmd.Diff where

import Gander.Config (Config(..))
import Gander.Lib (readOrBuildTree, renameRoot, diff, printDiffs)

cmdDiff :: Config -> FilePath -> FilePath -> IO ()
cmdDiff cfg old new = do
  tree1 <- fmap (renameRoot "old") $ readOrBuildTree (verbose cfg) (exclude cfg) old
  tree2 <- fmap (renameRoot "new") $ readOrBuildTree (verbose cfg) (exclude cfg) new
  printDiffs $ diff tree1 tree2
