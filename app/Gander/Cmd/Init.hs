module Gander.Cmd.Init where

-- TODO guess and check hashes

-- import Data.Gander
-- import Gander.Cmd.Hash (updateAnnexHashes)
-- import Util     (absolutize)
-- import Gander.Config (log)
-- import Gander.Run      (runGit, runGitCommit)

-- import Prelude hiding (log)

-- import System.FilePath  (takeFileName, (</>))
-- import System.Directory (createDirectoryIfMissing)
-- import Gander.Config    (Config(..))

-- import Data.Maybe (fromJust)

-- cmdInit :: Config -> FilePath -> IO ()
-- cmdInit cfg dir = do
--   dir' <- fmap fromJust $ absolutize dir -- TODO can this fail?
--   -- TODO check the root folder doesn't exist yet, or abort
--   -- TODO guards: git, git-annex on path
--   -- TODO abort if any exit code nonzero
--   createDirectoryIfMissing True $ dir' </> "unsorted"
--   createDirectoryIfMissing True $ dir' </> "sorted"
--   -- createDirectoryIfMissing True $ dir' </> ".git"
--   out1 <- runGit dir ["init"]
--   out2 <- runGit dir ["config", "user.name", "'gander user'"]
--   out3 <- runGit dir ["config", "user.email", "f@ke.email"]
--   out4 <- runGit dir ["annex", "init", takeFileName dir]
--   log cfg $ concat [out1, out2, out3, out4]
--   new <- buildProdTree (verbose cfg) (exclude cfg) dir
--   updateAnnexHashes cfg new
--   runGitCommit cfg dir $ "gander init " ++ takeFileName dir

-- check that init has been run, or prompt user to do that first
-- guardInit :: Config -> FilePath -> IO ()
-- guardInit = undefined
  -- TODO check that git-annex is installed/on PATH
  -- TODO check the root folder exists
  -- TODO check that it's a git repository
  -- TODO check that it's a git-annex repository
  -- TODO check that hashes.txt, sorted, and unsorted exist
