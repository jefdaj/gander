module Gander.Util
  ( absolutize
  , dropDir
  , dropDir'
  , findAnnex
  , inAnnex
  , log
  , noSlash
  , pathComponents
  , userSaysYes
  , withAnnex
  , isAnnexSymlink
  , isNonAnnexSymlink
  , FileName
  , n2p
  , p2n
  )
  where

import Gander.Config (Config(..))

import Prelude hiding (log)

import Control.Monad         (when)
import Data.List             (isPrefixOf, isInfixOf)
-- import Data.Maybe            (fromJust)
import System.Directory      (getCurrentDirectory, doesDirectoryExist, canonicalizePath)
import System.FilePath       (pathSeparator, splitPath, joinPath, takeDirectory, (</>), takeBaseName)
-- import System.Path.NameManip (guess_dotdot, absolute_path)
import System.IO        (hFlush, stdout)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)

-- import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Short as BS
import qualified Data.Text as T

pathComponents :: FilePath -> [FilePath]
pathComponents f = filter (not . null)
                 $ map (filter (/= pathSeparator))
                 $ splitPath f

-- removed because MissingH is deprecated. now tilde expansion won't work?
-- from schoolofhaskell.com/user/dshevchenko/cookbook
-- absolutize :: FilePath -> IO FilePath
-- absolutize aPath
--     | "~" `isPrefixOf` aPath = do
--         homePath <- getHomeDirectory
--         return $ normalise $ addTrailingPathSeparator homePath
--                              ++ tail aPath
--     | otherwise = do
--         pathMaybeWithDots <- absolute_path aPath
--         return $ fromJust $ guess_dotdot pathMaybeWithDots

absolutize :: FilePath -> IO FilePath
absolutize p = do
  wd <- getCurrentDirectory
  canonicalizePath (wd </> p)

-- TODO this fails on the leading / in a full path?
dropDir :: FilePath -> FilePath
dropDir = joinPath . tail . splitPath

dropDir' :: FilePath -> FilePath
dropDir' path = case path of
  ('/':p) -> dropDir p
  p -> dropDir p

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

-- TODO should this return the main dir or .git/annex inside it?
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

-- We reuse the existing SHA256SUM from the link
-- TODO is this less efficient than putting all the logic in one function?
isAnnexSymlink :: FilePath -> IO Bool
isAnnexSymlink path = do
  status <- getSymbolicLinkStatus path
  if not (isSymbolicLink status)
    then return False
    else do
      l <- readSymbolicLink path
      return $ ".git/annex/objects/" `isInfixOf` l && "SHA256E-" `isPrefixOf` (takeBaseName l)


-- We treat these as files rather than following to avoid infinite cycles
isNonAnnexSymlink :: FilePath -> IO Bool
isNonAnnexSymlink path = do
  status <- getSymbolicLinkStatus path
  if not (isSymbolicLink status)
    then return False
    else do
      link <- readSymbolicLink path
      return $ not $ (".git/annex/objects/" `isInfixOf` link)
                  && ("SHA256E-" `isPrefixOf` (takeBaseName link))

-- from System.Directory.Tree --

-- | an element in a FilePath:
type FileName = T.Text

n2p :: FileName -> FilePath
n2p = T.unpack

p2n :: FilePath -> FileName
p2n = T.pack
