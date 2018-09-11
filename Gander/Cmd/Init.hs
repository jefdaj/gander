module Gander.Cmd.Init where

import Gander.Lib           (runGit)
import Gander.Cmd.AnnexHash (cmdAnnexHash)

import Control.Monad    (when)
import System.FilePath  (takeFileName, (</>))
import System.Directory (createDirectoryIfMissing)
import Gander.Config    (Config(..))

cmdInit :: Config -> FilePath -> IO ()
cmdInit cfg dir = do
  -- TODO check the root folder doesn't exist yet, or abort
  -- TODO guards: git, git-annex on path
  -- TODO abort if any exit code nonzero
  createDirectoryIfMissing True $ dir </> "unsorted"
  createDirectoryIfMissing True $ dir </> "sorted"
  out1 <- runGit dir ["init"]
  out2 <- runGit dir ["config", "user.name", "'gander user'"]
  out3 <- runGit dir ["config", "user.email", "f@ke.email"]
  out4 <- runGit dir ["annex", "init", takeFileName dir]
  when (verbose cfg) $ putStr $ concat [out1, out2, out3, out4]
  cmdAnnexHash cfg (dir </> "hashes.txt") (dir </> "unsorted") -- TODO option to do this later?

-- check that init has been run, or prompt user to do that first
guardInit :: Config -> FilePath -> IO ()
guardInit = undefined
  -- TODO check that git-annex is installed/on PATH
  -- TODO check the root folder exists
  -- TODO check that it's a git repository
  -- TODO check that it's a git-annex repository
  -- TODO check that hashes.txt, sorted, and unsorted exist
