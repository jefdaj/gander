module Gander.Lib.HashTree
  ( HashTree(..)
  , excludeGlobs -- TODO no need to export?
  , hashTree
  , printHashes
  , serializeTree
  , deserializeTree
  )
  where

import Gander.Lib.Hash
import qualified System.Directory.Tree as DT

import Control.Monad        (forM)
import Data.List            (partition, intersperse)
import System.FilePath      ((</>), takeFileName, splitPath)
import System.FilePath.Glob (compile, match)
import System.IO.Unsafe     (unsafeInterleaveIO)

{- A tree of file names matching (a subdirectory of) the annex,
 - where each dir and file node contains a hash of its contents.
 - TODO read and write files
 - TODO would also storing the number of files in each dir help, or timestamps?
 -}
-- data HashTree = DT.AnchoredDirTree Hash
--   deriving (Eq, Read, Show)
--   TODO rename name -> path?
data HashTree
  = File { name :: FilePath, hash :: Hash }
  | Dir  { name :: FilePath, hash :: Hash, contents :: [HashTree], nFiles :: Int }
  deriving (Read, Show)

-- TODO disable this while testing to ensure deep equality?
instance Eq HashTree where
  t1 == t2 = hash t1 == hash t2

excludeGlobs :: [String]
             -> (DT.AnchoredDirTree FilePath -> DT.AnchoredDirTree FilePath)
excludeGlobs excludes (a DT.:/ tree) = (a DT.:/ DT.filterDir keep tree)
  where
    noneMatch ps s = not $ any (\p -> match (compile p) s) ps
    keep (DT.Dir  n _) = noneMatch excludes n
    keep (DT.File n _) = noneMatch excludes n
    keep _ = True

hashTree :: Bool -> DT.AnchoredDirTree FilePath -> IO [(Hash, String, FilePath)]
hashTree _ (a DT.:/ (DT.Failed n e )) = error $ (a </> n) ++ ": " ++ show e
hashTree v (_ DT.:/ (DT.File _ f)) = do
  h <- unsafeInterleaveIO $ hashFile v f
  return [(h, "file", f)]
hashTree v (a DT.:/ (DT.Dir n cs)) = do
  let root = a </> n
      hashSubtree t = unsafeInterleaveIO $ hashTree v $ root DT.:/ t
  subHashes <- fmap concat $ forM cs hashSubtree
  let rootHash = hashHashes $ map (\(h, _, _) -> h) subHashes
  return $ subHashes ++ [(rootHash, "dir ", root)]

-- TODO use serialize for this
printHashes :: Bool -> DT.AnchoredDirTree FilePath -> IO ()
printHashes verbose tree = do
  hashes <- hashTree verbose tree
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
