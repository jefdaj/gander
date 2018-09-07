{-# LANGUAGE BangPatterns #-}

-- TODO hashHashes should be hashDir
-- TODO should it also hash filenames?
-- TODO convert everything here to UTF-8?

module Gander.Lib.Hash
  ( Hash(..)
  , hashBytes
  , hashFile
  )
  where

import qualified Data.ByteString.Lazy as LB

import Control.Monad              (msum)
import Crypto.Hash                (Digest, SHA256, hashlazy)
import Data.List                  (isInfixOf)
import Data.Maybe                 (fromJust)
import System.FilePath            (takeBaseName)
import System.Posix.Files         (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)
import Data.Char                  (ord)

{- Checksum (sha256sum?) of a file or folder.
 - For files, should match the corresponding git-annex key.
 - TODO would storing it in a more efficient way help?
 - TODO would adding timestamps or number of files help?
 -}
newtype Hash = Hash String
  deriving (Eq, Read, Show, Ord)

hashBytes :: LB.ByteString -> String
hashBytes = show . (hashlazy :: LB.ByteString -> Digest SHA256)

hashAnnexSymlink :: FilePath -> IO (Maybe Hash)
hashAnnexSymlink path = do
  status <- getSymbolicLinkStatus path
  if not (isSymbolicLink status)
    then return Nothing
    else do
      link <- readSymbolicLink path
      return $ if ".git/annex/objects/" `isInfixOf` link
        then Just $ Hash $ drop 20 $ takeBaseName link
        else Nothing

hashSymlink :: FilePath -> IO (Maybe Hash)
hashSymlink path = do
  status <- getSymbolicLinkStatus path
  if not (isSymbolicLink status)
    then return Nothing
    else do
      -- TODO does this need to be unicode??
      link <- fmap (LB.pack . map (fromIntegral . ord)) (readSymbolicLink path)
      return $ Just $ Hash $ hashBytes link

-- see: https://stackoverflow.com/a/30537010
hashFileContents :: FilePath -> IO Hash
hashFileContents path = do -- TODO hashFileContents
  !sha256sum <- fmap hashBytes $ LB.readFile path
  -- when verbose (putStrLn $ sha256sum ++ " " ++ path)
  return $ Hash sha256sum

-- Hashes if necessary, but tries to read it from a link first
-- Note that this can only print file hashes, not the whole streaming trees format
hashFile :: Bool -> FilePath -> IO Hash
hashFile _ path = do
  aHash <- hashAnnexSymlink path
  sHash <- hashSymlink      path
  fHash <- fmap Just $ hashFileContents path
  return $ fromJust $ msum [aHash, sHash, fHash]
