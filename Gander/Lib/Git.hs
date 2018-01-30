module Gander.Lib.Git where

-- TODO rename Util? Files? System?
-- TODO add git-annex, rsync to nix dependencies

import Control.Monad         (when)
import Data.List             (isPrefixOf)
import Data.Maybe            (fromJust)
import System.Directory      (getHomeDirectory, doesDirectoryExist)
import System.FilePath       (addTrailingPathSeparator, normalise, takeDirectory, (</>))
import System.FilePath       (pathSeparator, splitPath)
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.Process        (readProcess)
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

annexAdd :: Bool -> FilePath -> IO ()
annexAdd verbose path = withAnnex verbose path $ \dir -> do
  out <- readProcess "git" ["-C", dir, "annex", "add",
                            "--include-dotfiles", path] ""
  when verbose $ putStrLn out

gitRm :: Bool -> FilePath -> IO ()
gitRm verbose path = withAnnex verbose path $ \dir -> do
  out <- readProcess "git" ["-C", dir, "rm", "-rf", path] ""
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
