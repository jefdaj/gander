module Gander.Cmd.AnnexHash where

import Control.Monad    (when)
import Gander.Config    (Config(..))
import Gander.Lib       (buildTree, readTree, serializeTree, diff, printDiffs)
import System.Directory (doesFileExist)

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
