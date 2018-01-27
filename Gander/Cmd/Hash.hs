module Gander.Cmd.Hash
  ( cmdHash
  )
  where

import Gander.Config
import Gander.Lib.HashTree (excludeGlobs, printHashes)
import System.Directory.Tree (readDirectoryWithL)

-- Note that you can't hash a folder while writing to a file inside it!
cmdHash :: Config -> FilePath -> IO ()
cmdHash cfg path = do
  tree <- readDirectoryWithL return path
  printHashes (verbose cfg) $ excludeGlobs (exclude cfg) tree
