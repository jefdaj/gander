module Gander.Cmd.Dupes where

-- TODO guess and check hashes

import Gander.Config (Config(..))
import Gander.Data   (readOrBuildTree, pathsByHash, dupesByNFiles, simplifyDupes, sortDupePaths, printDupes)
import Control.Monad.ST

cmdDupes :: Config -> FilePath -> IO ()
cmdDupes cfg path = do
  tree <- readOrBuildTree (verbose cfg) (exclude cfg) path
  let dupes = runST $ dupesByNFiles =<< pathsByHash tree
  printDupes $ map sortDupePaths $ simplifyDupes dupes
