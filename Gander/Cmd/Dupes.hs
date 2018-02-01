module Gander.Cmd.Dupes where

import Gander.Config (Config(..))
import Gander.Lib (readOrBuildTree, pathsByHash, dupesByNFiles, printDupes)

cmdDupes :: Config -> FilePath -> IO ()
cmdDupes cfg path = do
  tree <- readOrBuildTree (verbose cfg) (exclude cfg) path
  let pbyh = pathsByHash tree
      pdup = dupesByNFiles pbyh
  printDupes pdup
