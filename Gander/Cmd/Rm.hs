module Gander.Cmd.Rm where

-- TODO next: fix relative paths thing, write a nice lost files warning, fix any last bugs... then good :D
-- TODO oh, write a couple other messages if it would help brian. lost files should be mentioned even when 0!

import Gander.Lib
import Gander.Util   (userSaysYes)
import Gander.Config (Config(..))
import Data.Maybe    (fromJust)

-- import qualified Data.ByteString as B

-- TODO list files with no duplicates when confirming
-- TODO aha! ok to be missing folder hashes, just not files
cmdRm :: Config -> FilePath -> FilePath -> FilePath -> IO ()
cmdRm cfg target _ rmPath = do -- TODO correct toRm path using root!
  let rmPath' = "./" ++ rmPath -- TODO fix this of course
  tree <- readOrBuildTree True (exclude cfg) target
  ok <- okToRm cfg tree rmPath'
  let rm = do
             gitRm cfg (fromJust $ annex cfg) rmPath'
             let mTree = rmSubTree tree rmPath'
             case mTree of
               Nothing -> putStrLn "failed to rmSubTree"
               Just t -> printHashes t
  if (ok || force cfg)
    then rm
    else do
      -- TODO messages that distinguish file vs dir, or better say which files are last copy!
      let msg = "ok to remove last copy of some files in '" ++ rmPath' ++ "'?"
      confirm <- userSaysYes msg
      if confirm
        then rm
        else putStrLn $ "not removing '" ++ rmPath' ++ "'"

-- Should this go in HashTree.hs?
okToRm :: Config -> HashTree -> FilePath -> IO Bool
okToRm _ tree rmPath = do
  let exists = treeContainsPath tree rmPath
  if not exists
    then do
      putStrLn $ "target does not contain the path '" ++ rmPath ++ "'"
      return False
    else do
      putStrLn $ "found subtree: '" ++ rmPath ++ "'"
      -- pPrint tree
      -- TODO this should be extracted from the target hashes right?
      -- tree2 <- readOrBuildTree True (exclude cfg) rmPath
      -- putStrLn $ "target: '" ++ tree ++ "' rmPath: '" ++ rmPath ++ "'"
      return $ case dropTo tree rmPath of
        Nothing -> False
        Just t2 -> allDupes tree t2
