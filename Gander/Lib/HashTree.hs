{-# LANGUAGE BangPatterns #-}

module Gander.Lib.HashTree
  ( excludeGlobs -- TODO no need to export?
  , hashTree
  , printHashes
  , serializeTree
  , deserializeTree
  )
  where

import Gander.Types
import qualified Data.ByteString.Lazy  as LB
import qualified System.Directory.Tree as DT

import System.IO.Unsafe           (unsafeInterleaveIO)
import Control.Monad              (when, forM)
import Crypto.Hash                (Digest, SHA256, hashlazy)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List                  (sort, partition, isInfixOf, intersperse)
import System.FilePath            ((</>), takeBaseName, takeFileName, splitPath)
import System.FilePath.Glob       (compile, match)
import System.Posix.Files         (getFileStatus, isSymbolicLink, readSymbolicLink)

-- TODO add IO in order to say which files are being excluded?
excludeGlobs :: Options
             -> (DT.AnchoredDirTree FilePath -> DT.AnchoredDirTree FilePath)
excludeGlobs opts (a DT.:/ tree) = (a DT.:/ DT.filterDir keep tree)
  where
    noneMatch ps s = not $ any (\p -> match (compile p) s) ps
    keep (DT.Dir  n _) = noneMatch (exclude opts) n
    keep (DT.File n _) = noneMatch (exclude opts) n
    keep _ = True

hashTree :: Options -> DT.AnchoredDirTree FilePath -> IO [(Hash, String, FilePath)]
hashTree _ (a DT.:/ (DT.Failed n e )) = error $ (a </> n) ++ ": " ++ show e
hashTree os (_ DT.:/ (DT.File _ f)) = do
  h <- unsafeInterleaveIO $ hashFile os f
  return [(h, "file", f)]
hashTree os (a DT.:/ (DT.Dir n cs)) = do
  let root = a </> n
      hashSubtree t = unsafeInterleaveIO $ hashTree os $ root DT.:/ t
  subHashes <- fmap concat $ forM cs hashSubtree
  let rootHash = hashHashes $ map (\(h, _, _) -> h) subHashes
  return $ subHashes ++ [(rootHash, "dir ", root)]

-- TODO use serialize for this
printHashes :: Options -> DT.AnchoredDirTree FilePath -> IO ()
printHashes opts tree = do
  hashes <- hashTree opts tree
  mapM_ printHash hashes
  where
    printHash (Hash h, t, p) = putStrLn $ concat $ intersperse " " [h, t, p]

-------------------------------------
-- serialize and deserializeTree trees --
-------------------------------------

-- TODO can Foldable or Traversable simplify these?

serializeTree :: HashTree -> String
serializeTree = unlines . serialize' ""

serialize' :: FilePath -> HashTree -> [String]
serialize' dir (File n (Hash h)     ) = [unwords [h, "file", dir </> n]]
serialize' dir (Dir  n (Hash h) cs _)
  = concatMap (serialize' $ dir </> n) cs -- recurse on contents
  ++ [unwords [h, "dir ", dir </> n]] -- finish with hash of entire dir

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
deserializeTree :: String -> HashTree
deserializeTree = snd . head . foldr accTrees [] . map readLine . reverse . lines

countFiles :: HashTree -> Int
countFiles (File _ _    ) = 1
countFiles (Dir  _ _ _ n) = n

{- This one is confusing! It accumulates a list of trees and their indent
 - levels, and when it comes across a dir it uses the indents to determine
 - which files are children to put inside it vs which are siblings.
 -}
accTrees :: (Hash, String, Int, FilePath) -> [(Int, HashTree)] -> [(Int, HashTree)]
accTrees l@(h, t, indent, p) cs = case t of
  "file" -> cs ++ [(indent, File p h)]
  "dir"  -> let (children, siblings) = partition (\(i, _) -> i > indent) cs
                dir = Dir p h (map snd children)
                              (sum $ map (countFiles . snd) children)
            in siblings ++ [(indent, dir)]
  _ -> error $ "invalid line: '" ++ show l ++ "'" 

readLine :: String -> (Hash, String, Int, FilePath)
readLine line = (Hash h, t, i, takeFileName p)
  where
    [h, t]   = words tmp            -- first two words are hash and type
    (tmp, p) = splitAt 70 line      -- rest of the line is the path
    i        = length $ splitPath p -- get indent level from path

----------------------
-- build hash trees --
----------------------

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
hashFile :: Options -> FilePath -> IO Hash
hashFile opts path = do
  link <- annexLink path
  case link of
    Just l -> return $ Hash $ drop 20 $ takeBaseName l
    Nothing -> do
      !sha256sum <- fmap hashBytes $ LB.readFile path
      when (verbose opts) (putStrLn $ sha256sum ++ " " ++ path)
      return $ Hash sha256sum

-- TODO should the hashes include filenames? ie are two dirs with a different name different?
-- the "dir:" part prevents empty files and dirs from matching
-- TODO is there a more elegant way?
-- TODO remove the sort? not needed if tree order is reliable i suppose
hashHashes :: [Hash] -> Hash
hashHashes hs = Hash $ hashBytes $ pack txt
  where
    txt = show $ sort $ map (\(Hash h) -> h) hs
