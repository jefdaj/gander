{-# LANGUAGE BangPatterns #-}

module Gander.Cmd.Dupes where

-- TODO guess and check hashes

import Gander.Config (Config(..))
import Gander.Data   (readOrBuildTree, pathsByHash, dupesByNFiles, simplifyDupes, sortDupePaths, printDupes)
import Control.Monad.ST
import qualified Data.HashTable.Class as H
import qualified Data.HashSet             as S

cmdDupes :: Config -> FilePath -> IO ()
cmdDupes cfg path = do
  tree <- readOrBuildTree (verbose cfg) (exclude cfg) path
  -- TODO rewrite sorting with lower memory usage
  -- let dupes = runST $ dupesByNFiles =<< pathsByHash tree
  -- printDupes $ map sortDupePaths $ simplifyDupes dupes
  let !dupes = runST $ H.toList =<< pathsByHash tree
      !dupes' = map (\(k, (a,b,c)) -> (k, (a, b, S.toList c))) dupes
  printDupes dupes'
