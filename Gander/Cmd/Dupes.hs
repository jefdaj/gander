module Gander.Cmd.Dupes where

import Gander.Config (Config(..))
import Gander.Lib (deserializeTree, pathsByHash, dupesByNFiles, printDupes)

cmdDupes :: Config -> FilePath -> IO ()
cmdDupes _ path = do
  tree <- fmap deserializeTree $ readFile path
  let pbyh = pathsByHash tree
      pdup = dupesByNFiles pbyh
  printDupes pdup
