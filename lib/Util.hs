{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util
  ( absolutize
  , absolutize'
  , dropDir
  , dropDir'
  , findAnnex
  , inAnnex
  , noSlash
  -- , pathComponents
  , userSaysYes
  , withAnnex
  , isAnnexSymlink
  , isNonAnnexSymlink
  , FileName(..)
  , n2p
  , p2n
  , replaceNth
  -- , n2bs
  -- , bs2n
  )
  where

-- TODO remove this from Util
-- import Gander.Config (Config(..))

import Prelude hiding (log)

import Data.List             (isPrefixOf, isInfixOf)
import Data.Maybe            (fromJust)
import System.Directory      (getCurrentDirectory, getHomeDirectory, doesDirectoryExist, canonicalizePath)
import System.FilePath       (pathSeparator, joinPath, takeDirectory, (</>), takeBaseName, addTrailingPathSeparator, normalise)
import System.Path.NameManip (guess_dotdot_comps, absolute_path, slice_path, unslice_path)
import System.IO        (hFlush, stdout)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)

-- import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Short as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.UTF8             as BU
import qualified Data.ByteString.Char8 as B8

import qualified Filesystem.Path.CurrentOS as OS
import Data.Store             (encode, decodeIO, Store(..))
import TH.Derive
import System.Info (os)

import qualified Data.ByteString.Char8 as B8

import Control.DeepSeq
import GHC.Generics

-- TODO bug? converts both "" and "/" -> []
-- TODO why was this needed on top of splitPath?
-- pathComponents :: FilePath -> [FilePath]
-- pathComponents f = filter (not . null)
                 -- $ map (filter (/= pathSeparator))
                 -- $ splitPath f

-- TODO is there a potential for infinite recursion bugs here?
absolutize :: FilePath -> IO (Maybe FilePath)
absolutize path = do
  -- putStrLn $ "path: " ++ path
  path' <- absolutize' path
  case path' of
    Nothing -> return Nothing
    Just p' -> if p' == path
                 then fmap Just $ canonicalizePath p'
                 else canonicalizePath p' >>= absolutize

-- TODO explicitly prevent ~ except as the first path component?
-- based on: schoolofhaskell.com/user/dshevchenko/cookbook
absolutize' :: FilePath -> IO (Maybe FilePath)
absolutize' aPath
    | null aPath = return Nothing
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ Just $ normalise $ addTrailingPathSeparator homePath
                             ++ tail aPath
    -- TODO is this branch really, really slow? might be a bottleneck
    | otherwise = do
        aPath' <- absolute_path aPath
        let comps = slice_path aPath'
        return $ Just $ case guess_dotdot_comps comps of
          Nothing -> aPath'
          Just ps -> unslice_path ps

-- absolutize :: FilePath -> IO FilePath
-- absolutize p = do
--   wd <- getCurrentDirectory
--   canonicalizePath (wd </> p)

-- TODO this fails on the leading / in a full path?
dropDir :: FilePath -> FilePath
dropDir = joinPath . tail . slice_path

dropDir' :: FilePath -> FilePath
dropDir' path = case path of
  ('/':p) -> dropDir p
  p -> dropDir p

noSlash :: FilePath -> FilePath
noSlash = reverse . dropWhile (== '/') . reverse

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
  absPath <- fmap fromJust $ absolutize path -- TODO can this fail?
  let aPath = absPath </> ".git" </> "annex"
  foundIt <- doesDirectoryExist aPath
  if foundIt
    then return $ Just $ takeDirectory $ takeDirectory aPath
    else if (null $ slice_path absPath)
      then return Nothing
      else findAnnex $ takeDirectory absPath

inAnnex :: FilePath -> IO Bool
inAnnex = fmap (not . null) . findAnnex

withAnnex :: FilePath -> (FilePath -> IO a) -> IO a
withAnnex path fn = do
  aPath <- findAnnex path
  case aPath of
    Nothing -> error $ "'" ++ path ++ "' is not in a git-annex repo"
    Just dir -> do
      -- log cfg $ "using git-annex repo '" ++ dir ++ "'"
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
-- The newtype is needed to prevent overlapping with the standard Arbitrary
-- Text instance in the tests
newtype FileName = FileName T.Text
  deriving (Eq, Ord, Read, Show, Generic)

deriving instance NFData FileName

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
$($(derive [d|
  instance Deriving (Store FileName)
  |]))

n2p :: FileName -> FilePath
n2p (FileName t) = (if os == "darwin"
                      then B8.unpack . TE.encodeUtf8
                      else T.unpack) t

p2n :: FilePath -> FileName
p2n = FileName . (if os == "darwin"
                    then TE.decodeUtf8 . B8.pack
                    else T.pack)

-- n2bs :: FileName -> BU.ByteString
-- n2bs = BU.fromString . n2p

-- TODO should this have the option for a decoding error?
-- bs2n :: BU.ByteString -> FileName
-- bs2n = p2n . BU.toString

-- from https://stackoverflow.com/a/5852820
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs
