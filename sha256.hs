{-# LANGUAGE BangPatterns #-}

module Main where

-- test streaming sha256sum to prevent huge memory usage

import qualified Data.ByteString.Char8 as B
import qualified Crypto.Hash as CH

hashBytes :: B.ByteString -> B.ByteString
hashBytes = B.pack . show . (CH.hash :: B.ByteString -> CH.Digest CH.SHA256)

hashFileContents :: FilePath -> IO B.ByteString
hashFileContents path = do -- TODO hashFileContents
  !sha256sum <- fmap hashBytes $ B.readFile path
  putStrLn $ (B.unpack sha256sum) ++ " " ++ path
  return sha256sum

bigfile :: FilePath
bigfile = "/mnt/data/Pfam/database-files/alignment_and_tree.txt.gz"

-- sha256FromChunks :: [ByteString] -> Digest SHA256State
-- sha256FromChunks cs = completeSHA256Incremental $
--                           foldl pushChunk sha256Incremental cs

main :: IO ()
main = do
  putStrLn $ "hashing '" ++ bigfile ++ "'..."
  h <- hashFileContents bigfile
  putStrLn $ B.unpack h
  putStrLn $ "done!"
