module Gander.Cmd.Init where

import Gander.Config (Config(..))
-- import Gander.Lib

cmdInit :: Config -> FilePath -> IO ()
cmdInit = undefined
  -- TODO check the root folder doesn't exist yet, or abort
  -- TODO create the folder
  -- TODO git init
  -- TODO git annex init reponame
  -- TODO set name + email
  -- TODO at the top-level Gander.hs, run cmdHash after this to update + commit
  --      (need to --force so it doesn't complain about hashes.txt not existing?)

-- check that init has been run, or prompt user to do that first
guardInit :: Config -> FilePath -> IO ()
guardInit = undefined
  -- TODO check that git-annex is installed/on PATH
  -- TODO check the root folder exists
  -- TODO check that it's a git repository
  -- TODO check that it's a git-annex repository
  -- TODO check that hashes.txt, sorted, and unsorted exist
