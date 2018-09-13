module Gander.Util
  ( absolutize
  , dropDir
  , findAnnex
  , inAnnex
  , log
  , noSlash
  , pathComponents
  , userSaysYes
  , withAnnex
  )
  where

import Gander.Config (Config(..))

import Prelude hiding (log)

import Control.Monad         (when)
import Data.List             (isPrefixOf)
import Data.Maybe            (fromJust)
import System.Directory      (getHomeDirectory, doesDirectoryExist)
import System.FilePath       (addTrailingPathSeparator, normalise, pathSeparator, splitPath, joinPath, takeDirectory, (</>))
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.IO        (hFlush, stdout)

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

dropDir :: FilePath -> FilePath
dropDir = joinPath . tail . splitPath

noSlash :: FilePath -> FilePath
noSlash = reverse . dropWhile (== '/') . reverse

log :: Config -> String -> IO ()
log cfg msg = when (verbose cfg) (putStrLn msg)

userSaysYes :: String -> IO Bool
userSaysYes question = do
  putStr $ question ++ " (yes/no) "
  hFlush stdout
  let answers = [("yes", True), ("no", False)]
  answer <- getLine
  case lookup answer answers of
    Nothing -> userSaysYes question
    Just b  -> return b

findAnnex :: FilePath -> IO (Maybe FilePath)
findAnnex path = do
  absPath <- absolutize path
  let aPath = absPath </> ".git" </> "annex"
  foundIt <- doesDirectoryExist aPath
  if foundIt
    then return $ Just $ takeDirectory $ takeDirectory aPath
    else if (null $ pathComponents absPath)
      then return Nothing
      else findAnnex $ takeDirectory absPath

inAnnex :: FilePath -> IO Bool
inAnnex = fmap (not . null) . findAnnex

withAnnex :: Config -> FilePath -> (FilePath -> IO a) -> IO a
withAnnex cfg path fn = do
  aPath <- findAnnex path
  case aPath of
    Nothing -> error $ "'" ++ path ++ "' is not in a git-annex repo"
    Just dir -> do
      log cfg $ "using git-annex repo '" ++ dir ++ "'"
      fn dir
