{-# LANGUAGE BangPatterns #-}

-- TODO hashHashes should be hashDir
-- TODO should it also hash filenames?
-- TODO convert everything here to UTF-8?

module Gander.Data.Hash
  ( Hash(..)
  , prettyHash
  , hashBytes
  , hashString
  , hashFile
  )
  where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as B8

import Crypto.Hash        (Digest, SHA256, hashlazy)
import Data.Char          (ord)
import Data.List          (isInfixOf, isPrefixOf)
import Data.List.Split    (splitOn)
import System.FilePath    (takeBaseName)
import System.Directory   (pathIsSymbolicLink)
import System.Posix.Files (readSymbolicLink)
import Data.Hashable      (Hashable(..))

{- Checksum (sha256sum?) of a file or folder.
 - For files, should match the corresponding git-annex key.
 - TODO would storing it in a more efficient way help?
 - TODO would adding timestamps or number of files help?
 -}
newtype Hash = Hash { unHash :: String }
  deriving (Eq, Read, Show, Ord)

-- This is unrelated to Gander's hashing. It's required to use Data.HashMap
instance Hashable Hash
  where
    hashWithSalt n h = hashWithSalt n (unHash h)

-- TODO actual Pretty instance
-- TODO how many chars to display? git uses two groups of 7 like this
-- prettyHash (Hash h) = firstN h ++ "..." ++ lastN h
prettyHash :: Hash -> String
prettyHash (Hash h) = firstN h
  where
    nChars = 8
    firstN = take nChars
    -- lastN  = reverse . take nChars . reverse

hashBytes :: LB.ByteString -> String
hashBytes = show . (hashlazy :: LB.ByteString -> Digest SHA256)

hashString :: String -> String
hashString = hashBytes . B8.pack

{- This applies to directories as well as files because when trying to traverse
 - non-annex symlinks there can be infinite cycles. For example it will fail on
 - /usr/bin/X11.
 -}
hashSymlink :: FilePath -> IO (Maybe Hash)
hashSymlink path = do
  isLink <- pathIsSymbolicLink path
  if not isLink
    then return Nothing
    else do
      link <- readSymbolicLink path
      return $ Just $ Hash $ if ".git/annex/objects/" `isInfixOf` link
                             && "SHA256E-" `isPrefixOf` (takeBaseName link)
        then last $ splitOn "--" $ head $ splitOn "." $ takeBaseName link
        else hashBytes $ (LB.pack . map (fromIntegral . ord)) link

-- see: https://stackoverflow.com/a/30537010
hashFileContents :: FilePath -> IO Hash
hashFileContents path = do -- TODO hashFileContents
  !sha256sum <- fmap hashBytes $ LB.readFile path
  -- when verbose (putStrLn $ sha256sum ++ " " ++ path)
  return $ Hash sha256sum

-- Hashes if necessary, but tries to read it from a link first
-- Note that this can only print file hashes, not the whole streaming trees format
-- TODO remove the unused verbose flag?
hashFile :: Bool -> FilePath -> IO Hash
hashFile _ path = do
  -- aHash <- hashAnnexSymlink path
  sHash <- hashSymlink path
  case sHash of
    Just h  -> return h
    Nothing -> do
      fHash <- hashFileContents path
      return fHash
