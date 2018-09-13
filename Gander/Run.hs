module Gander.Run
  ( runGitAnnexAdd
  , findAnnex
  , runGitAdd
  , runGitCommit
  , runGitMv
  , runGitRm
  , inAnnex
  , noSlash
  , runRsync
  , runGit
  , withAnnex
  , runDelta
  , runDeltas
  )
  where

-- TODO hide the individual system commands from export? should all go through runDelta*
-- TODO add git-annex, rsync to nix dependencies

import Gander.Config
import Gander.Lib
import Gander.Util

import Prelude hiding (log)

import System.Directory (createDirectoryIfMissing)
import System.FilePath  (dropFileName)
import System.FilePath  ((</>))
import System.Process   (readProcess, readCreateProcess, CreateProcess(..), proc)

runRsync :: Config -> FilePath -> FilePath -> IO ()
runRsync cfg src dest = do
  out <- readProcess "rsync" ["-aErvz", "--delete", noSlash src ++ "/", noSlash dest] ""
  log cfg out

runGit :: FilePath -> [String] -> IO String
runGit dir args = readCreateProcess (gitProc { cwd = Just dir }) ""
  where
    gitProc = proc "git" $ ["--git-dir=" ++ (dir </> ".git")] ++ args

-- TODO handle exit 1 when git-annex not installed
runGitAnnexAdd :: Config -> FilePath -> IO ()
runGitAnnexAdd cfg path = withAnnex cfg path $ \dir -> do
  out <- readProcess "git" ["-C", dir, "annex", "add", "--include-dotfiles", path] ""
  log cfg out

-- TODO get annex path from config! or pass explicitly
runGitMv :: Config -> FilePath -> FilePath -> FilePath -> IO ()
runGitMv cfg aPath src dst = withAnnex cfg aPath $ \dir -> do
  createDirectoryIfMissing True $ dir </> (dropFileName dst)
  out <- readProcess "git" ["-C", dir, "mv", src, dst] ""
  log cfg out

runGitAdd :: Config -> FilePath -> [FilePath] -> IO ()
runGitAdd cfg aPath paths = withAnnex cfg aPath $ \dir -> do
  out <- readProcess "git" (["-C", dir, "add"] ++ paths) ""
  log cfg out

runGitRm :: Config -> FilePath -> FilePath -> IO ()
runGitRm cfg aPath path = withAnnex cfg aPath $ \dir -> do
  out <- readProcess "git" ["-C", dir, "rm", "-rf", path] ""
  log cfg out

runGitCommit :: Config -> FilePath -> String -> IO ()
runGitCommit cfg aPath msg = withAnnex cfg aPath $ \dir -> do
  out <- readProcess "git" ["-C", dir, "commit", "-m", msg] ""
  log cfg out

-- Takes a starting HashTree and a Delta, and applies the delta.
-- Useful for checking whether git operations will be safe,
-- and that the calculated diffs match actual changes afterward.
-- TODO move to Run.hs
runDelta :: HashTree -> Delta -> HashTree -- TODO IO ()
runDelta t1 (Add  f t2) = addSubTree t1 t2 f
runDelta _ (Rm   _ _  ) = undefined
runDelta _ (Mv   _ _ _) = undefined
runDelta _ (Edit _ _ _) = undefined

-- TODO do I want foldl' here instead??
-- TODO move to Run.hs
runDeltas :: HashTree -> [Delta] -> HashTree -- TODO IO ()
runDeltas = foldl runDelta
