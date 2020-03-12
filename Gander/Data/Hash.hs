{-# LANGUAGE BangPatterns #-}

-- TODO hashHashes should be hashDir
-- TODO should it also hash filenames?
-- TODO convert everything here to UTF-8?

module Gander.Data.Hash
  ( Hash(..)
  , prettyHash
  , hashBytes
  -- , hashBytesStreaming
  , hashString
  , hashFile
  )
  where

-- import qualified Data.ByteString.Lazy as LB
-- import qualified Data.ByteString.Lazy.Char8 as B
import qualified Crypto.Hash as CH

import Streaming (Stream, Of)
import qualified Streaming.Prelude as S
import Crypto.Hash.Algorithms
import Crypto.Hash.IO

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Streaming.Char8 as Q
-- import Data.Byteable      (toBytes)
-- import Data.Char          (ord)
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
newtype Hash = Hash { unHash :: B.ByteString }
  deriving (Eq, Read, Show, Ord)

-- This is unrelated to Gander's hashing. It's required to use Data.HashMap
instance Hashable Hash
  where
    hashWithSalt n h = hashWithSalt n (unHash h)

digestLength :: Int
digestLength = 20

-- TODO actual Pretty instance
-- TODO how many chars to display? git uses two groups of 7 like this
-- prettyHash (Hash h) = firstN h ++ "..." ++ lastN h
prettyHash :: Hash -> B.ByteString
prettyHash = B.take digestLength . unHash

-- this works, but can probably be made faster...
-- hashBytes :: B.ByteString -> B.ByteString
-- hashBytes = B.pack . show . (hash :: B.ByteString -> CH.Digest CH.SHA256)

compress :: B.ByteString -> B.ByteString
compress = B.take digestLength . B64.encode

hashBytes :: B.ByteString -> Hash
hashBytes = Hash . compress . B.pack . show . (CH.hash :: B.ByteString -> CH.Digest CH.SHA256)

-- TODO would digestFromByteString be faster?
-- TODO bug! digests come out unreadable :(
hashBytesStreaming :: L.ByteString -> IO Hash
hashBytesStreaming bs = do
  ctx <- hashMutableInitWith SHA256
  let chunked :: Stream (Of B.ByteString) IO ()
      chunked = Q.toChunks $ Q.fromLazy bs
  S.mapM_ (hashMutableUpdate ctx) chunked
  final <- hashMutableFinalize ctx
  return $ Hash $ compress $ B.pack $ show final

hashString :: String -> Hash
hashString = hashBytes . B.pack

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
      return $ Just $ if ".git/annex/objects/" `isInfixOf` link
                      && "SHA256E-" `isPrefixOf` (takeBaseName link)
        then Hash $ compress $ B.pack $ last $ splitOn "--" $ head $ splitOn "." $ takeBaseName link
        else hashString link -- TODO should this be a user-facing error instead?

-- see: https://stackoverflow.com/a/30537010
-- hashFileContents :: FilePath -> IO Hash
-- hashFileContents path = do -- TODO hashFileContents
--   !sha256sum <- fmap hashBytes $ B.readFile path
--   -- when verbose (putStrLn $ sha256sum ++ " " ++ path)
--   return $ Hash sha256sum

-- based on https://gist.github.com/michaelt/6c6843e6dd8030e95d58
-- TODO show when verbose?
hashFileContentsStreaming :: FilePath -> IO Hash
-- hashFileContentsStreaming path = withFile path ReadMode $ \h -> hashBytes Q.fromHandle h
hashFileContentsStreaming path = L.readFile path >>= hashBytesStreaming
--   ctx <- hashMutableInitWith SHA256
--   let chunked :: Stream (Of B.ByteString) IO ()
--       chunked = Q.toChunks $ Q.fromHandle h
--   S.mapM_ (hashMutableUpdate ctx) chunked
--   final <- hashMutableFinalize ctx
--   return $ B.pack $ show final

-- Hashes if necessary, but tries to read it from a link first
-- Note that this can only print file hashes, not the whole streaming trees format
-- TODO remove the unused verbose flag?
hashFile :: Bool -> FilePath -> IO Hash
hashFile _ path = do
  -- aHash <- hashAnnexSymlink path
  sHash <- hashSymlink path
  case sHash of
    Just h  -> return h
    Nothing -> hashFileContentsStreaming path
