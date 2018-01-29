module Gander.Cmd.Dupes where

import Gander.Config (Config(..))
import Gander.Lib (readTree, pathsByHash, dupesByNFiles, printDupes)

cmdDupes :: Config -> FilePath -> IO ()
cmdDupes _ path = do
  tree <- readTree path
  let pbyh = pathsByHash tree
      pdup = dupesByNFiles pbyh
  printDupes pdup
