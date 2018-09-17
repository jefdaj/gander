module Gander.Run
  ( runGitAnnexAdd -- TODO don't export?
  , findAnnex -- TODO don't export?
  , runGitAdd -- TODO don't export
  , runGitCommit
  , runGitMv -- TODO don't export?
  , runGitRm -- TODO don't export?
  , inAnnex -- TODO don't export?
  , noSlash -- TODO don't export?
  , runRsync
  , runGit
  , withAnnex -- TODO don't export?
  , runDelta
  , runDeltas
  , safeRunDeltas
  )
  where

-- TODO add git-annex, rsync to nix dependencies

import Gander.Config
import Gander.Lib
import Gander.Util

import Prelude hiding (log)

import Data.Maybe       (fromJust)
import Control.Monad    (when, mapM_)
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

-- TODO handle exit 1 when git-annex not installed
-- TODO take a list of paths?
runGitAnnexAdd :: Config -> FilePath -> FilePath -> IO ()
runGitAnnexAdd cfg aPath path = withAnnex cfg aPath $ \dir -> do
  out <- readProcess "git" ["-C", dir, "annex", "add", "--include-dotfiles", path] ""
  log cfg out

runGitRm :: Config -> FilePath -> FilePath -> IO ()
runGitRm cfg aPath path = withAnnex cfg aPath $ \dir -> do
  out <- readProcess "git" ["-C", dir, "rm", "-rf", path] ""
  log cfg out

runGitCommit :: Config -> FilePath -> String -> IO ()
runGitCommit cfg aPath msg = withAnnex cfg aPath $ \dir -> do
  out <- readProcess "git" ["-C", dir, "commit", "-m", msg] ""
  log cfg out

-- Takes a starting HashTree and a Delta, and actually applies the delta.
-- Useful for checking whether git operations will be safe,
-- and that the calculated diffs match actual changes afterward.
runDelta :: Config -> FilePath -> Delta -> IO ()
runDelta c a   (Add p _  ) = runGitAnnexAdd c a p
runDelta c a   (Rm  p    ) = runGitRm       c a p
runDelta c a   (Mv  p1 p2) = runGitMv       c a p1 p2
runDelta c a e@(Edit _ _ ) = runDelta       c a e -- TODO are separate edits really needed?

runDeltas :: Config -> FilePath -> [Delta] -> IO ()
runDeltas c a = mapM_ (runDelta c a)

safeRunDeltas :: Config -> [Delta] -> String -> IO ()
safeRunDeltas cfg deltas msg = do
  let aPath  = fromJust $ annex cfg
      hashes = aPath </> "hashes.txt"
  before <- readTree hashes
  case simDeltas before deltas of
    Left e -> log cfg e
    Right expected -> do
      -- TODO check no lost files here and ask user to confirm if there are
      let lost = listLostFiles before expected
          run = do
            runDeltas cfg aPath deltas
            when (check cfg) $ do
              actual <- buildTree (verbose cfg) (exclude cfg) aPath
              assertSameTrees ("expected '" ++ aPath ++ "'", expected)
                              ("actual '"   ++ aPath ++ "'", actual)
            writeFile hashes $ serializeTree expected
            -- TODO should gitCommit be part of runDeltas?
            -- TODO sanitize dst for commit message
            runGitCommit cfg aPath msg
      case lost of
        [] -> do
          log cfg "deltas should be safe. running them..."
          run
        ls -> do
          let msg2 = unlines $
                      [ "Warning! The last copies of these files will be removed:" ]
                      ++ ls ++ [ "Is that what you want?"]
          confirm <- userSaysYes msg2
          if confirm
            then run
            else putStrLn "OK, aborting" -- TODO better message
