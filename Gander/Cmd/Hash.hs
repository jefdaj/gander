module Gander.Cmd.Hash where

import Gander.Config (Config(..))
import Gander.Lib (buildTree, printHashes, serializeTree)

cmdHash :: Config -> Maybe FilePath -> FilePath -> IO ()
cmdHash cfg mHashes target = do
  -- TODO test that the target exists
  -- TODO also test that the hashes file exists if in annex mode, and prompt for init if not
  --      (this guard should go on all the annex commands)
  tree <- buildTree (verbose cfg) (exclude cfg) target
  case mHashes of
    Nothing -> printHashes tree
    Just h  -> writeFile h $ serializeTree tree

guardStatus :: Config -> FilePath -> IO ()
guardStatus = undefined
  -- TODO check that git status is all clear
  -- TODO check that git-annex status is all clear too?

guardHash :: Config -> Maybe FilePath -> FilePath -> IO ()
guardHash = undefined
  -- TODO run guardInit here?
  -- TODO run guardStatus here?
  -- TODO check that hashes.txt exists
