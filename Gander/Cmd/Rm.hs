module Gander.Cmd.Rm where

import Text.Pretty.Simple (pPrint)
import Gander.Config (Config(..))
import Gander.Lib    (readOrBuildTree, gitRm, allDupes, userSaysYes)

-- TODO list files with no duplicates when confirming
-- TODO aha! ok to be missing folder hashes, just not files
cmdRm :: Config -> FilePath -> FilePath -> FilePath -> IO ()
cmdRm cfg target rootPath rmPath = do -- TODO correct toRm path using root!
  safe <- safeToRm cfg target rmPath
  let rm = gitRm (verbose cfg) rmPath
  if (safe || force cfg)
    then rm
    else do
      let msg = "ok to remove last copy of some files in '" ++ rmPath ++ "'?"
      confirm <- userSaysYes msg
      if confirm
        then rm
        else putStrLn $ "not removing '" ++ rmPath ++ "'"

safeToRm :: Config -> FilePath -> FilePath -> IO Bool
safeToRm cfg target toRm = do
  tree1 <- readOrBuildTree True (exclude cfg) target
  -- TODO this should be extracted from the target hashes right?
  -- tree2 <- readOrBuildTree True (exclude cfg) toRm
  pPrint tree1
  return $ allDupes tree1 tree2
