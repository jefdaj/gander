{-# LANGUAGE BangPatterns #-}

module Gander.Cmd.Dupes where

-- TODO guess and check hashes

import Gander.Config (Config(..))
import Gander.Data   (readOrBuildTree, pathsByHash, dupesByNFiles, printDupes)

cmdDupes :: Config -> FilePath -> IO ()
cmdDupes cfg path = do
  tree <- readOrBuildTree (verbose cfg) (exclude cfg) path
  -- TODO rewrite sorting with lower memory usage
  -- let dupes = runST $ dupesByNFiles =<< pathsByHash tree
  -- printDupes $ map sortDupePaths $ simplifyDupes dupes
  printDupes $ dupesByNFiles $ pathsByHash tree
