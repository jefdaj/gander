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

-- import qualified Data.ByteString.Lazy as LB
-- import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Char8 as B8

import Crypto.Hash        (Digest, SHA256, hash)
-- import Data.Byteable      (toBytes)
-- import Data.Char          (ord)
import Data.List          (isInfixOf, isPrefixOf)
import Data.List.Split    (splitOn)
import System.FilePath    (takeBaseName)
import System.Directory   (pathIsSymbolicLink)
import System.Posix.Files (readSymbolicLink)

{- Checksum (sha256sum?) of a file or folder.
 - For files, should match the corresponding git-annex key.
 - TODO would storing it in a more efficient way help?
 - TODO would adding timestamps or number of files help?
 -}
newtype Hash = Hash { unHash :: B8.ByteString }
  deriving (Eq, Read, Show, Ord)

-- TODO actual Pretty instance
-- TODO how many chars to display? git uses two groups of 7 like this
-- prettyHash (Hash h) = firstN h ++ "..." ++ lastN h
prettyHash :: Hash -> String
prettyHash = firstN . B8.unpack . unHash
  where
    nChars = 8
    firstN = take nChars
    -- lastN  = reverse . take nChars . reverse

-- this works, but can probably be made faster...
-- hashBytes :: B8.ByteString -> B8.ByteString
-- hashBytes = B8.pack . show . (hash :: B8.ByteString -> Digest SHA256)

-- TODO would digestFromByteString be faster?
-- TODO bug! digests come out unreadable :(
hashBytes :: B8.ByteString -> B8.ByteString
-- still unreadable: hashBytes = digestToByteString . (hash :: B8.ByteString -> Digest SHA256)
-- back to readable: hashBytes = B8.pack . show . (hash :: B8.ByteString -> Digest SHA256)
hashBytes = B8.pack . show . (hash :: B8.ByteString -> Digest SHA256)

hashString :: String -> B8.ByteString
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
        then B8.pack $ last $ splitOn "--" $ head $ splitOn "." $ takeBaseName link
        else hashString link -- TODO should this be a user-facing error instead?

-- see: https://stackoverflow.com/a/30537010
hashFileContents :: FilePath -> IO Hash
hashFileContents path = do -- TODO hashFileContents
  !sha256sum <- fmap hashBytes $ B8.readFile path
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
