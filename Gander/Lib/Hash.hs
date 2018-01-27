{-# LANGUAGE BangPatterns #-}

module Gander.Lib.Hash
  ( Hash(..)
  , hashFile
  , hashHashes
  )
  where

import qualified Data.ByteString.Lazy as LB

import Control.Monad              (when)
import Crypto.Hash                (Digest, SHA256, hashlazy)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List                  (sort, isInfixOf)
import System.FilePath            (takeBaseName)
import System.Posix.Files         (getFileStatus, isSymbolicLink, readSymbolicLink)

{- Checksum (sha256sum?) of a file or folder.
 - For files, should match the corresponding git-annex key.
 - TODO would storing it in a more efficient way help?
 - TODO would adding timestamps or number of files help?
 -}
newtype Hash = Hash String
  deriving (Eq, Read, Show, Ord)

hashBytes :: LB.ByteString -> String
hashBytes = show . (hashlazy :: LB.ByteString -> Digest SHA256)

annexLink :: FilePath -> IO (Maybe FilePath)
annexLink path = do
  status <- getFileStatus path
  if not (isSymbolicLink status)
    then return Nothing
    else do
      link <- readSymbolicLink path
      return $ if ".git/annex/objects/" `isInfixOf` link
        then Just link
        else Nothing

-- Hashes if necessary, but tries to read it from an annex link first
-- For the actual hashing see: https://stackoverflow.com/a/30537010
-- Note that this can only print file hashes, not the whole streaming trees format
hashFile :: Bool -> FilePath -> IO Hash
hashFile verbose path = do
  link <- annexLink path
  case link of
    Just l -> return $ Hash $ drop 20 $ takeBaseName l
    Nothing -> do
      !sha256sum <- fmap hashBytes $ LB.readFile path
      when verbose (putStrLn $ sha256sum ++ " " ++ path)
      return $ Hash sha256sum

-- TODO should the hashes include filenames? ie are two dirs with a different name different?
-- the "dir:" part prevents empty files and dirs from matching
-- TODO is there a more elegant way?
-- TODO remove the sort? not needed if tree order is reliable i suppose
hashHashes :: [Hash] -> Hash
hashHashes hs = Hash $ hashBytes $ pack txt
  where
    txt = show $ sort $ map (\(Hash h) -> h) hs