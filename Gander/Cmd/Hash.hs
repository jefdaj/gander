module Gander.Cmd.Hash where

import Gander.Config (Config(..))
import Gander.Lib (buildTree, printHashes)

cmdHash :: Config -> FilePath -> IO ()
cmdHash cfg path = do
  tree <- buildTree (verbose cfg) (exclude cfg) path
  printHashes tree
