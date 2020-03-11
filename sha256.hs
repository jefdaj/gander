{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

-- test streaming sha256sum to prevent huge memory usage

import Streaming
import qualified Streaming.Prelude as S
import Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming.Char8 as Q
import qualified Data.ByteString.Char8 as B
-- import qualified Crypto.Hash as CH
import System.IO (withFile, IOMode(..))

import Crypto.Hash.Algorithms
import Crypto.Hash.IO

-- hashBytes :: B.ByteString -> B.ByteString
-- hashBytes = B.pack . show . (CH.hash :: B.ByteString -> CH.Digest CH.SHA256)

-- hashFileContents :: FilePath -> IO B.ByteString
-- hashFileContents path = do -- TODO hashFileContents
--   !sha256sum <- fmap hashBytes $ B.readFile path
--   putStrLn $ (B.unpack sha256sum) ++ " " ++ path
--   return sha256sum

-- based on https://gist.github.com/michaelt/6c6843e6dd8030e95d58
hashFileContents2 :: FilePath -> IO B.ByteString
hashFileContents2 path = withFile path ReadMode $ \h -> do
  ctx <- hashMutableInitWith SHA256
  let raw :: ByteString IO () -- get raw bytes
      raw = Q.fromHandle h 
      segmented :: Stream (ByteString IO) IO () -- divide on "chunks"?
      segmented = Q.lines raw -- TODO how to segment?
      individualized :: Stream (Of B.ByteString) IO ()
      individualized = mapped Q.toStrict segmented -- danger: concatenate 'real' bytestrings!
  S.mapM_ (hashMutableUpdate ctx) individualized
  final <- hashMutableFinalize ctx
  return $ B.pack $ show final

smallfile :: FilePath
smallfile = "/mnt/data/phytozome12-protein.txt"

bigfile :: FilePath
bigfile = "/mnt/data/Pfam/database-files/alignment_and_tree.txt.gz"

-- sha256FromChunks :: [ByteString] -> Digest SHA256State
-- sha256FromChunks cs = completeSHA256Incremental $
--                           foldl pushChunk sha256Incremental cs

main :: IO ()
main = do
  h <- hashFileContents2 smallfile
  putStrLn $ (B.unpack h) ++ "  " ++ smallfile
