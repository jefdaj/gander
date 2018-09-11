module Gander.Cmd.Hash where

import Gander.Config (Config(..))
import Gander.Lib    (buildTree, printHashes)

-- standalone command prints hashes only
cmdHash :: Config -> FilePath -> IO ()
cmdHash cfg target = do
  -- TODO test that the target exists?
  tree <- buildTree (verbose cfg) (exclude cfg) target
  printHashes tree
