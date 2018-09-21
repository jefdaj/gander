module Gander.Cmd.Dupes where

import Gander.Config (Config(..))
import Gander.Lib (readOrBuildTree, pathsByHash, dupesByNFiles, simplifyDupes, sortDupePaths, printDupes)

cmdDupes :: Config -> FilePath -> IO ()
cmdDupes cfg path = do
  tree <- readOrBuildTree (verbose cfg) (exclude cfg) path
  printDupes $ map sortDupePaths $ simplifyDupes $ map snd $ dupesByNFiles $ pathsByHash tree
