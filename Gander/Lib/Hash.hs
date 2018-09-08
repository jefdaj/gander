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

import Crypto.Hash        (Digest, SHA256, hashlazy)
import Data.Char          (ord)
import Data.List          (isInfixOf)
import System.FilePath    (takeBaseName)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)

{- Checksum (sha256sum?) of a file or folder.
 - For files, should match the corresponding git-annex key.
 - TODO would storing it in a more efficient way help?
 - TODO would adding timestamps or number of files help?
 -}
newtype Hash = Hash { unHash :: String }
  deriving (Eq, Read, Show, Ord)

hashBytes :: LB.ByteString -> String
hashBytes = show . (hashlazy :: LB.ByteString -> Digest SHA256)

-- TODO warn in the readme that the annex is assumed to use the sha256 backend?
hashSymlink :: FilePath -> IO (Maybe Hash)
hashSymlink path = do
  status <- getSymbolicLinkStatus path
  if not (isSymbolicLink status)
    then return Nothing
    else do
      link <- readSymbolicLink path
      return $ Just $ Hash $ if ".git/annex/objects/" `isInfixOf` link
        then drop 20 $ takeBaseName link
        else hashBytes $ (LB.pack . map (fromIntegral . ord)) link

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
  -- aHash <- hashAnnexSymlink path
  sHash <- hashSymlink path
  fHash <- hashFileContents path
  return $ case sHash of
    Just h  -> h
    Nothing -> fHash
