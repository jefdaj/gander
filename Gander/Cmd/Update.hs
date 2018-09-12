module Gander.Cmd.Update where

-- TODO guess and check hashes? not sure if that makes sense here
-- TODO make an operation to replace a subtree (insert despite it existing, then diff)

import Gander.Config (Config(..))
import Gander.Lib    (readOrBuildTree, addSubTree, printTree)

cmdUpdate :: Config -> FilePath -> FilePath -> FilePath -> IO ()
cmdUpdate cfg root sub path = do
  tree1 <- readOrBuildTree (verbose cfg) (exclude cfg) root
  tree2 <- readOrBuildTree (verbose cfg) (exclude cfg) sub
  printTree $ addSubTree tree1 tree2 path
