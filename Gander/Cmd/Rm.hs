module Gander.Cmd.Rm where

import Text.Pretty.Simple (pPrint)
import Gander.Config (Config(..))
import Gander.Lib    (readOrBuildTree, treeContainsPath, dropTo, gitRm, allDupes, userSaysYes)

-- TODO list files with no duplicates when confirming
-- TODO aha! ok to be missing folder hashes, just not files
cmdRm :: Config -> FilePath -> FilePath -> FilePath -> IO ()
cmdRm cfg target rootPath rmPath = do -- TODO correct toRm path using root!
  let rmPath' = "./" ++ rmPath -- TODO fix this of course
  safe <- safeToRm cfg target rmPath'
  let rm = gitRm (verbose cfg) rmPath'
  if (safe || force cfg)
    then rm
    else do
      -- TODO messages that distinguish file vs dir, or better say which files are last copy!
      let msg = "ok to remove last copy of some files in '" ++ rmPath' ++ "'?"
      confirm <- userSaysYes msg
      if confirm
        then rm
        else putStrLn $ "not removing '" ++ rmPath' ++ "'"

safeToRm :: Config -> FilePath -> FilePath -> IO Bool
safeToRm cfg target rmPath = do
  tree1 <- readOrBuildTree True (exclude cfg) target
  let exists = treeContainsPath tree1 rmPath
  if not exists
    then do
      putStrLn $ "target does not contain the path '" ++ rmPath ++ "'"
      return False
    else do
      putStrLn $ "found subtree: '" ++ rmPath ++ "'"
      -- pPrint tree1
      -- TODO this should be extracted from the target hashes right?
      -- tree2 <- readOrBuildTree True (exclude cfg) rmPath
      putStrLn $ "target: '" ++ target ++ "' rmPath: '" ++ rmPath ++ "'"
      return $ case dropTo tree1 rmPath of
        Nothing -> False
        Just t2 -> allDupes tree1 t2
