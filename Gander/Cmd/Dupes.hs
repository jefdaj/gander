module Gander.Cmd.Dupes where

import Gander.Config
import Gander.Lib (deserializeTree)
import Gander.Lib.DupeMap

cmdDupes :: Config -> FilePath -> IO ()
cmdDupes _ path = do
  tree <- fmap deserializeTree $ readFile path
  let pbyh = pathsByHash tree
      pdup = dupesByNFiles pbyh
  printDupes pdup
