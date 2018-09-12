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
  , userSaysYes -- TODO not git related
  , withAnnex
  )
  where

-- TODO rename Util? Files? System?
-- TODO add git-annex, rsync to nix dependencies

import Gander.Util (pathComponents, absolutize, noSlash)

import Control.Monad    (when)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath  (dropFileName)
import System.FilePath  (takeDirectory, (</>))
import System.IO        (hFlush, stdout)
import System.Process   (readProcess, readCreateProcess, CreateProcess(..), proc)

findAnnex :: FilePath -> IO (Maybe FilePath)
findAnnex path = do
  absPath <- absolutize path
  let annex = absPath </> ".git" </> "annex"
  foundIt <- doesDirectoryExist annex
  if foundIt
    then return $ Just $ takeDirectory $ takeDirectory annex
    else if (null $ pathComponents absPath)
      then return Nothing
      else findAnnex $ takeDirectory absPath

inAnnex :: FilePath -> IO Bool
inAnnex = fmap (not . null) . findAnnex

rsync :: Bool -> FilePath -> FilePath -> IO ()
rsync verbose src dest = do
  out <- readProcess "rsync" ["-aErvz", "--delete", noSlash src ++ "/", noSlash dest] ""
  when verbose $ putStrLn out

withAnnex :: Bool -> FilePath -> (FilePath -> IO a) -> IO a
withAnnex verbose path fn = do
  annex <- findAnnex path
  case annex of
    Nothing -> error $ "'" ++ path ++ "' is not in a git-annex repo"
    Just dir -> do
      when verbose $ putStrLn $ "using git-annex repo '" ++ dir ++ "'"
      fn dir

runGit :: FilePath -> [String] -> IO String
runGit dir args = readCreateProcess (gitProc { cwd = Just dir }) ""
  where
    gitProc = proc "git" $ ["--git-dir=" ++ (dir </> ".git")] ++ args

-- TODO handle exit 1 when git-annex not installed
annexAdd :: Bool -> FilePath -> IO ()
annexAdd verbose path = withAnnex verbose path $ \dir -> do
  out <- readProcess "git" ["-C", dir, "annex", "add",
                            "--include-dotfiles", path] ""
  when verbose $ putStrLn out

-- TODO get annex path from config! or pass explicitly
gitMv :: Bool -> FilePath -> FilePath -> FilePath -> IO ()
gitMv verbose aPath src dst = withAnnex verbose aPath $ \dir -> do
  createDirectoryIfMissing True $ dir </> (dropFileName dst)
  out <- readProcess "git" ["-C", dir, "mv", src, dst] ""
  when verbose $ putStrLn out

gitAdd :: Bool -> FilePath -> [FilePath] -> IO ()
gitAdd verbose aPath paths = withAnnex verbose aPath $ \dir -> do
  out <- readProcess "git" (["-C", dir, "add"] ++ paths) ""
  when verbose $ putStrLn out

gitRm :: Bool -> FilePath -> FilePath -> IO ()
gitRm verbose aPath path = withAnnex verbose aPath $ \dir -> do
  out <- readProcess "git" ["-C", dir, "rm", "-rf", path] ""
  when verbose $ putStrLn out

gitCommit :: Bool -> FilePath -> String -> IO ()
gitCommit verbose aPath msg = withAnnex verbose aPath $ \dir -> do
  out <- readProcess "git" ["-C", dir, "commit", "-m", msg] ""
  when verbose $ putStrLn out

userSaysYes :: String -> IO Bool
userSaysYes question = do
  putStr $ question ++ " (yes/no) "
  hFlush stdout
  let answers = [("yes", True), ("no", False)]
  answer <- getLine
  case lookup answer answers of
    Nothing -> userSaysYes question
    Just b  -> return b
