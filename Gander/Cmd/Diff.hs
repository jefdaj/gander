module Gander.Cmd.Diff where

-- TODO guess and check hashes

import Gander.Config (Config(..))
import Gander.Data   (readOrBuildTree, renameRoot, diff, printDeltas)

cmdDiff :: Config -> FilePath -> FilePath -> IO ()
cmdDiff cfg old new = do
  tree1 <- fmap (renameRoot "old") $ readOrBuildTree (verbose cfg) (exclude cfg) old
  tree2 <- fmap (renameRoot "new") $ readOrBuildTree (verbose cfg) (exclude cfg) new
  printDeltas $ diff tree1 tree2
