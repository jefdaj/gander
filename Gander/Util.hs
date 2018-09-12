module Gander.Util
  ( pathComponents
  , absolutize
  , dropDir
  , noSlash
  , log
  )
  where

import Gander.Config (Config(..))

import Prelude hiding (log)

import Control.Monad         (when)
import Data.List             (isPrefixOf)
import Data.Maybe            (fromJust)
import System.Directory      (getHomeDirectory)
import System.FilePath       (addTrailingPathSeparator, normalise, pathSeparator, splitPath, joinPath)
import System.Path.NameManip (guess_dotdot, absolute_path)

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


