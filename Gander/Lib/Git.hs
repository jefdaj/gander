module Gander.Lib.Git where

-- TODO rename Util? Files? System?
-- TODO add git-annex, rsync to nix dependencies

import Data.List             (isPrefixOf)
import Data.Maybe            (fromJust)
import System.Directory      (getHomeDirectory, doesDirectoryExist)
import System.FilePath       (addTrailingPathSeparator, normalise, takeDirectory, (</>))
import System.FilePath       (pathSeparator, splitPath)
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.Process        (readProcess)

pathComponents :: FilePath -> [String]
pathComponents f = filter (not . null)
                 $ map (filter (/= pathSeparator))
                 $ splitPath f

-- from schoolofhaskell.com/user/dshevchenko/cookbook
absolutize :: String -> IO String
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

rsync :: FilePath -> FilePath -> IO ()
rsync src dest = do
  out <- readProcess "rsync" ["-aErvz", "--delete", noSlash src ++ "/", noSlash dest] ""
  mapM_ putStrLn $ lines out

-- TODO check that git is clean (no unstaged files etc) first? nah just don't commit
annexAdd :: FilePath -> IO ()
annexAdd path = do
  annex <- findAnnex path
  case annex of
    Nothing -> error $ "'" ++ path ++ "' is not in a git-annex repo"
    Just dir -> do
      out <- readProcess "git" ["-C", dir, "annex", "add", path] ""
      mapM_ putStrLn $ lines out
