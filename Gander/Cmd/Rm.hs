module Gander.Cmd.Rm where

import Control.Monad (when)
import Gander.Config (Config(..))
import Gander.Lib    (readTree, gitRm, allDupes)
import System.IO (hFlush, stdout)

-- TODO list files with no duplicates when confirming
cmdRm :: Config -> FilePath -> FilePath -> FilePath -> IO ()
cmdRm cfg root sub path = do
  safe <- safeToRm root sub
  let rm = gitRm (verbose cfg) path
  if safe || force cfg then rm else do
    let msg = "ok to remove last copy of some files in '" ++ path ++ "'?"
    confirm <- userSaysYes msg
    when confirm rm

userSaysYes :: String -> IO Bool
userSaysYes question = do
  putStr $ question ++ " (yes/no) "
  hFlush stdout
  let answers = [("yes", True), ("no", False)]
  answer <- getLine
  case lookup answer answers of
    Nothing -> userSaysYes question
    Just b  -> return b

safeToRm :: FilePath -> FilePath -> IO Bool
safeToRm main toRm = do
  tree1 <- readTree main
  tree2 <- readTree toRm
  return $ allDupes tree1 tree2
