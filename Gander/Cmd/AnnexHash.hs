module Gander.Cmd.AnnexHash where

import Gander.Lib       (runGit)
import Data.Maybe       (fromJust)
import Control.Monad    (when)
import Gander.Config    (Config(..))
import Gander.Lib       (buildTree, readTree, serializeTree, diff, printDiffs)
import System.Directory (doesFileExist)

-- TODO have buildTree accept a list of paths; should be doing unsorted + sorted at least

-- annex command prints messages and writes to hashes.txt
cmdAnnexHash :: Config -> FilePath -> FilePath -> IO ()
cmdAnnexHash cfg hashes target = do
  new    <- buildTree (verbose cfg) (exclude cfg) target
  exists <- doesFileExist hashes
  if exists then do
    when (verbose cfg) $ putStrLn "updating hashes.txt"
    old <- readTree hashes
    printDiffs $ diff old new
  else do
    when (verbose cfg) $ putStrLn "creating hashes.txt"
  writeFile hashes $ serializeTree new
  let dir = fromJust $ annex cfg
  out1 <- runGit dir ["add", "hashes.txt"]
  out2 <- runGit dir ["commit", "-m", "gander hash"]
  when (verbose cfg) $ putStr $ concat [out1, out2]
  -- TODO git add hashes.txt + git commit here
  -- putStrLn "done"

guardStatus :: Config -> FilePath -> IO ()
guardStatus = undefined
  -- TODO check that git status is all clear
  -- TODO check that git-annex status is all clear too?

guardHash :: Config -> Maybe FilePath -> FilePath -> IO ()
guardHash = undefined
  -- TODO run guardInit here?
  -- TODO run guardStatus here?
  -- TODO check that hashes.txt exists
