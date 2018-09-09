module Gander.Cmd.Rm where

-- TODO next: fix relative paths thing, write a nice lost files warning, fix any last bugs... then good :D
-- TODO oh, write a couple other messages if it would help brian. lost files should be mentioned even when 0!

import Control.Monad (when)
-- import Text.Pretty.Simple (pPrint)
import Gander.Config (Config(..))
import System.FilePath ((</>))
import Gander.Lib   --  (HashTree, readOrBuildTree, treeContainsPath, dropTo, rmSubTree, printHashes, gitRm, allDupes, userSaysYes)

-- import qualified Data.ByteString as B

-- TODO list files with no duplicates when confirming
-- TODO aha! ok to be missing folder hashes, just not files
cmdRm :: Config -> FilePath -> FilePath -> FilePath -> IO ()
cmdRm cfg target _ rmPath = do -- TODO correct toRm path using root!
  let rmPath' = "./" ++ rmPath -- TODO fix this of course
  tree <- readOrBuildTree True (exclude cfg) target
  ok <- okToRm cfg tree rmPath'
  let rm = do
             gitRm (verbose cfg) rmPath'
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

-- assumes a lot of things:
-- you already put hashes in ANNEX_ROOT/.git/gander/hashes.txt
-- paths are specified from ANNEX_ROOT, both in the hashes file and on the command line
cmdTmpRm :: Config -> FilePath -> IO ()
cmdTmpRm cfg rmPath = withAnnex (verbose cfg) rmPath $ \dir -> do
  let hashPath = dir </> ".git" </> "gander" </> "hashes.txt"
  before <- readOrBuildTree True (exclude cfg) hashPath
  let exists = treeContainsPath before rmPath
  when (not exists) $ error $ "no hashes recorded for '" ++ rmPath ++ "'"
  let after = rmSubTree before rmPath
  case after of
    Nothing -> error $ "failed to simulate removing '" ++ rmPath ++ "' (coding issue...)"
    Just a  -> do
      gitRm True rmPath
      putStr $ "updating '" ++ hashPath ++ "'..."
      writeFile hashPath $ serializeTree a
      putStrLn " ok"
