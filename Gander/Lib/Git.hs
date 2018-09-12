module Gander.Lib.Git
  ( pathComponents
  , findAnnex
  , withAnnex
  , runGit
  , gitMv
  , gitRm
  , gitCommit
  , rsync
  , annexAdd
  , noSlash
  , inAnnex
  , absolutize
  , userSaysYes -- TODO not git related
  )
  where

-- TODO rename Util? Files? System?
-- TODO add git-annex, rsync to nix dependencies

import Control.Monad         (when)
import Data.List             (isPrefixOf)
import Data.Maybe            (fromJust)
import System.Directory      (getHomeDirectory, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath       (addTrailingPathSeparator, normalise, takeDirectory, (</>))
import System.FilePath       (pathSeparator, splitPath)
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.Process        (readProcess, readCreateProcess, CreateProcess(..), proc)
import System.IO (hFlush, stdout)

pathComponents :: FilePath -> [FilePath]
pathComponents f = filter (not . null)
                 $ map (filter (/= pathSeparator))
                 $ splitPath f

-- from schoolofhaskell.com/user/dshevchenko/cookbook
absolutize :: FilePath -> IO FilePath
absolutize aPath 
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath 
                             ++ tail aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots

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

noSlash :: FilePath -> FilePath
noSlash = reverse . dropWhile (== '/') . reverse

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
  createDirectoryIfMissing True $ dir </> dst
  out <- readProcess "git" ["-C", dir, "mv", src, dst] ""
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
