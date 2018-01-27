module Gander.Cmd.Hash
  ( cmdHash
  )
  where

import Gander.Config
import Gander.Lib (buildTree, printHashes)

-- Note that you can't hash a folder while writing to a file inside it!
cmdHash :: Config -> FilePath -> IO ()
cmdHash cfg path = buildTree (exclude cfg) path >>= printHashes (verbose cfg)
