{-# LANGUAGE BangPatterns #-}

module Gander.Cmd.Dupes where

-- TODO guess and check hashes

import Gander.Config (Config(..))
import Data.Gander   (HashForest(..), readOrBuildTrees, pathsByHash, dupesByNFiles, printDupes, writeDupes)

cmdDupes :: Config -> [FilePath] -> IO ()
cmdDupes cfg paths = do
  (HashForest trees) <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  -- TODO rewrite sorting with lower memory usage
  -- let dupes = runST $ dupesByNFiles =<< pathsByHash tree
  -- printDupes $ map sortDupePaths $ simplifyDupes dupes
  let ds = dupesByNFiles $ pathsByHash (head trees) -- TODO finish this
  case txt cfg of
    Nothing -> printDupes (maxdepth cfg) ds
    Just p  -> writeDupes (maxdepth cfg) p ds
