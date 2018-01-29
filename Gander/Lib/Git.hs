module Gander.Lib.Git
  -- ( pathComponents
  -- , absolutize
  -- , findAnnex
  -- )
  where

-- TODO rename Util? Files? System?

import System.FilePath (pathSeparator, splitPath)

import Data.List             (isPrefixOf)
import Data.Maybe            (fromJust)
import System.Directory      (getHomeDirectory, doesDirectoryExist)
import System.FilePath       (addTrailingPathSeparator, normalise, takeDirectory, (</>))
import System.Path.NameManip (guess_dotdot, absolute_path)

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
    then return $ Just annex
    else if (null $ pathComponents absPath)
      then return Nothing
      else findAnnex $ takeDirectory absPath
