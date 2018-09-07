module Gander.Lib.HashTree
  ( HashTree(..)
  , readTree
  , buildTree
  , readOrBuildTree
  , renameRoot
  , printHashes
  , prettyHashLine
  , serializeTree
  , flattenTree
  , deserializeTree
  , hashContents
  )
  where

import Gander.Lib.Hash
import qualified System.Directory.Tree as DT

import Data.ByteString.Lazy.Char8 (pack)
import Control.Monad        (forM)
import Data.List            (partition, sortBy)
import Data.Function        (on)
import Data.Ord             (compare)
import System.FilePath      ((</>), takeFileName, splitPath)
import System.FilePath.Glob (compile, match)
import System.IO.Unsafe     (unsafeInterleaveIO)
import System.Directory     (doesFileExist, doesDirectoryExist)

type HashLine = (Hash, String, FilePath)

-- TODO actual Pretty instance
prettyHashLine :: HashLine -> String
prettyHashLine (Hash h, t, p) = unwords [h, t, p]

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
  deriving (Read, Show, Eq, Ord) -- TODO switch to hash-based equality after testing

-- TODO disable this while testing to ensure deep equality?
-- instance Eq HashTree where
  -- t1 == t2 = hash t1 == hash t2

excludeGlobs :: [String]
             -> (DT.AnchoredDirTree FilePath -> DT.AnchoredDirTree FilePath)
excludeGlobs excludes (a DT.:/ tree) = (a DT.:/ DT.filterDir keep tree)
  where
    noneMatch ps s = not $ any (\p -> match (compile p) s) ps
    keep (DT.Dir  n _) = noneMatch excludes n
    keep (DT.File n _) = noneMatch excludes n
    keep _ = True

readTree :: FilePath -> IO HashTree
readTree = fmap deserializeTree . readFile

-- TODO are contents sorted? they probably should be for stable hashes
buildTree :: Bool -> [String] -> FilePath -> IO HashTree
buildTree beVerbose excludes path = do
  tree <- DT.readDirectoryWithL return path
  buildTree' beVerbose $ excludeGlobs excludes tree

buildTree' :: Bool -> DT.AnchoredDirTree FilePath -> IO HashTree
buildTree' _ (a DT.:/ (DT.Failed n e )) = error $ (a </> n) ++ ": " ++ show e
buildTree' v (_ DT.:/ (DT.File n f)) = do
  h <- unsafeInterleaveIO $ hashFile v f
  return File { name = n, hash = h }
buildTree' v (a DT.:/ (DT.Dir n cs)) = do
  let root = a </> n
      hashSubtree t = unsafeInterleaveIO $ buildTree' v $ root DT.:/ t
  subTrees <- forM cs hashSubtree
  let cs' = sortBy (compare `on` name) subTrees
  return Dir
    { name     = n
    , contents = cs'
    , hash     = hashContents cs'
    , nFiles   = sum $ map countFiles cs'
    }

-- TODO this should hash the same text you would get from serializing the tree
--      (looks like sha256sum output but with file/dir parts added)
hashContents :: [HashTree] -> Hash
hashContents ts = Hash $ hashBytes $ pack txt
  where
    ls  = map serializeTree ts
    txt = concat ls -- TODO why is this important vs unlines?

-- If passed a file this assumes it contains hashes and builds a tree of them;
-- If passed a dir it will scan it first and then build the tree.
readOrBuildTree :: Bool -> [String] -> FilePath -> IO HashTree
readOrBuildTree verbose exclude path = do
  isDir  <- doesDirectoryExist path
  isFile <- doesFileExist      path
  if      isFile then readTree path
  else if isDir then buildTree verbose exclude path
  else error $ "No such file: '" ++ path ++ "'"

-- for comparing two trees without getting hung up on different overall names
renameRoot :: String -> HashTree -> HashTree
renameRoot newName tree = tree { name = newName }

-- TODO use serialize for this
printHashes :: HashTree -> IO ()
printHashes = putStr . serializeTree

-------------------------------------
-- serialize and deserializeTree trees --
-------------------------------------

-- TODO can Foldable or Traversable simplify these?
serializeTree :: HashTree -> String
serializeTree = unlines . map prettyHashLine . flattenTree

flattenTree :: HashTree -> [HashLine]
flattenTree = flattenTree' ""

flattenTree' :: FilePath -> HashTree -> [(Hash, String, FilePath)]
flattenTree' dir (File n h     ) = [(h, "file", dir </> n)]
flattenTree' dir (Dir  n h cs _) = subtrees ++ [wholeDir]
  where
    subtrees = concatMap (flattenTree' $ dir </> n) cs
    wholeDir = (h, "dir ", dir </> n)

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
-- TODO refactor so there's a proper buildTree function and this uses it
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
