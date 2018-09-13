module Gander.Lib.Git
  ( annexAdd
  , findAnnex
  , gitAdd
  , gitCommit
  , gitMv
  , gitRm
  , inAnnex
  , noSlash
  , rsync
  , runGit
  , withAnnex
  )
  where

-- TODO rename Util? Files? System?
-- TODO add git-annex, rsync to nix dependencies

import Gander.Util
import Gander.Config

import Prelude hiding (log)

import System.Directory (createDirectoryIfMissing)
import System.FilePath  (dropFileName)
import System.FilePath  ((</>))
import System.Process   (readProcess, readCreateProcess, CreateProcess(..), proc)

rsync :: Config -> FilePath -> FilePath -> IO ()
rsync cfg src dest = do
  out <- readProcess "rsync" ["-aErvz", "--delete", noSlash src ++ "/", noSlash dest] ""
  log cfg out

runGit :: FilePath -> [String] -> IO String
runGit dir args = readCreateProcess (gitProc { cwd = Just dir }) ""
  where
    gitProc = proc "git" $ ["--git-dir=" ++ (dir </> ".git")] ++ args

-- TODO handle exit 1 when git-annex not installed
annexAdd :: Config -> FilePath -> IO ()
annexAdd cfg path = withAnnex cfg path $ \dir -> do
  out <- readProcess "git" ["-C", dir, "annex", "add", "--include-dotfiles", path] ""
  log cfg out

-- TODO get annex path from config! or pass explicitly
gitMv :: Config -> FilePath -> FilePath -> FilePath -> IO ()
gitMv cfg aPath src dst = withAnnex cfg aPath $ \dir -> do
  createDirectoryIfMissing True $ dir </> (dropFileName dst)
  out <- readProcess "git" ["-C", dir, "mv", src, dst] ""
  log cfg out

gitAdd :: Config -> FilePath -> [FilePath] -> IO ()
gitAdd cfg aPath paths = withAnnex cfg aPath $ \dir -> do
  out <- readProcess "git" (["-C", dir, "add"] ++ paths) ""
  log cfg out

gitRm :: Config -> FilePath -> FilePath -> IO ()
gitRm cfg aPath path = withAnnex cfg aPath $ \dir -> do
  out <- readProcess "git" ["-C", dir, "rm", "-rf", path] ""
  log cfg out

gitCommit :: Config -> FilePath -> String -> IO ()
gitCommit cfg aPath msg = withAnnex cfg aPath $ \dir -> do
  out <- readProcess "git" ["-C", dir, "commit", "-m", msg] ""
  log cfg out
