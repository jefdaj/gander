{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

-- test streaming sha256sum to prevent huge memory usage

import Streaming (Stream, Of)
import qualified Streaming.Prelude as S
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
hashFileContentsStreaming :: FilePath -> IO B.ByteString
hashFileContentsStreaming path = withFile path ReadMode $ \h -> do
  ctx <- hashMutableInitWith SHA256
  let chunked :: Stream (Of B.ByteString) IO ()
      chunked = Q.toChunks $ Q.fromHandle h
  S.mapM_ (hashMutableUpdate ctx) chunked
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
  h <- hashFileContentsStreaming smallfile
  putStrLn $ (B.unpack h) ++ "  " ++ smallfile
