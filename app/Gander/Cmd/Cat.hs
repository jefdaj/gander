module Gander.Cmd.Cat where

import Gander.Config (Config(..))
import Data.Gander   (readOrBuildTrees, printForest, writeForest)

cmdCat :: Config -> [FilePath] -> IO ()
cmdCat cfg paths = do
  forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  case txt cfg of
    Nothing -> printForest   forest
    Just p  -> writeForest p forest
