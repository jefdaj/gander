{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Gander.HashTree
  ( HashTree(..)
  , AnchoredHashTree(..)
  , ProdTree(..)
  , HashLine(..)
  , keepPath
  , readTree
  , buildTree
  , buildProdTree
  , readOrBuildTree
  , renameRoot
  , printTree
  , writeBinTree
  , serializeTree
  , splitPath'
  , writeTree
  , flattenTree
  , flattenTreePaths
  , flattenTree' -- TODO name this something better
  , listTreeNodes
  , listTreeNodePaths
  , zipPaths
  , deserializeTree
  , hashContents
  , dropTo
  , treeContainsPath
  , treeContainsHash
  , addSubTree
  , sortTreesByName
  , rmSubTree
  , accTrees -- TODO hide this better?
  -- for testing
  , countFiles
  , wrapInEmptyDir
  , wrapInEmptyDirs
  )
  where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff

-- import Debug.Trace

import Data.Gander.Hash
import Data.Gander.HashLine

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import qualified Data.Text as T

import Util (FileName(..), p2n, n2p)
import qualified System.Directory.Tree as DT

import Control.Monad        (msum)
import qualified Control.Monad.Parallel as P
import qualified Control.Monad          as M
import Data.List            (find, delete, sort)
import Data.Maybe           (isJust, catMaybes)
import Data.Function        (on)
import Data.List            (partition, sortBy)
import Data.Either          (fromRight)
import Data.Ord             (compare)
import System.Directory     (doesFileExist, doesDirectoryExist)
import System.FilePath      ((</>), splitPath, joinPath)
import System.FilePath.Glob (matchWith, Pattern, MatchOptions(..))
import System.IO            (hFlush, stdout, withFile, IOMode(..))
import System.IO.Unsafe     (unsafeInterleaveIO)

import Prelude hiding (take)
import Data.Attoparsec.ByteString.Char8 hiding (D, skipWhile)
import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.Combinator

import Data.Store             (encode, decodeIO, Store(..))
import Control.Exception.Safe (catchAny)
import TH.Derive

import Control.DeepSeq
import GHC.Generics (Generic)

{- A tree of file names matching (a subdirectory of) the annex,
 - where each dir and file node contains a hash of its contents.
 - TODO read and write files
 - TODO would also storing the number of files in each dir help, or timestamps?
 -}
-- data HashTree = DT.AnchoredDirTree Hash
--   deriving (Eq, Read, Show)
--   TODO rename name -> path?
data HashTree a
  = File { name :: !FileName, hash :: !Hash, fileData :: !a }
  | Dir  { name :: !FileName, hash :: Hash, contents :: [HashTree a], nFiles :: Int }
  deriving (Read, Ord, Generic) -- TODO switch to hash-based equality after testing

-- TODO does this fix the UTF8 decode error when showing trees in the Mac terminal?
instance Show a => Show (HashTree a) where
  show t@(File {name = (FileName n)}) = "File {name = FileName " ++ show n ++ ", hash = " ++ show (hash t) ++ ", fileData = " ++ show (fileData t) ++ "}"
  show t@(Dir  {name = (FileName n)}) =  "Dir {name = FileName " ++ show n ++ ", hash = " ++ show (hash t) ++ ", contents = " ++ show (contents t) ++ ", nFiles = " ++ show (nFiles t) ++ "}"

-- based on the DirTree code
data AnchoredHashTree a = (:/) { anchor :: !FilePath, hashTree :: HashTree a }
  deriving (Read, Show)

-- based on the DirTree code
instance Functor AnchoredHashTree where
  fmap f (b :/ d) = b :/ fmap f d

-- We only need the file decoration for testing, so we can leave it off the production types
type ProdTree = HashTree ()

-- TODO disable this while testing to ensure deep equality?
instance Eq (HashTree a) where
  t1 == t2 = hash t1 == hash t2

-- TODO once there's also a dirData, should this be BiFunctor instead?
-- TODO should this also re-hash the file, or is that not part of the fileData idea?
instance Functor HashTree where
  fmap fn f@(File {}) = f { fileData = fn (fileData f) }
  fmap fn d@(Dir  {}) = d { contents = map (fmap fn) (contents d) }

-- TODO test functor identity law

instance NFData a => NFData (HashTree a)

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
$($(derive [d|
    instance Store a => Deriving (Store (HashTree a))
    |]))

excludeGlobs :: [Pattern]
             -> (DT.AnchoredDirTree a -> DT.AnchoredDirTree a)
excludeGlobs excludes (a DT.:/ tree) = (a DT.:/ DT.filterDir (keep a) tree)
  where
    keep a (DT.Dir  n _) = keepPath excludes (a </> n2p n)
    keep a (DT.File n _) = keepPath excludes (a </> n2p n)
    keep a b = True

keepPath :: [Pattern] -> FilePath -> Bool
keepPath excludes path = not $ any (\ptn -> matchWith opts ptn path) excludes
  where
    opts = MatchOptions
             { matchDotsImplicitly = True
             , ignoreCase          = False
             , ignoreDotSlash      = True
             }

-- try to read as binary, and fall back to text if it fails
readTree :: Maybe Int -> FilePath -> IO (ProdTree)
readTree md path = catchAny
                    (B8.readFile path >>= decodeIO)
                    (\_ -> fmap (deserializeTree md) $ B8.readFile path)
--   (do
--      (hs :: [HashLine]) <- decodeIO =<< B8.readFile path
--      return $ snd $ head $ foldr accTrees [] hs)
--   (\_ -> fmap deserializeTree $ B8.readFile path)

-- see also `buildTestTree` in the `HashTreeTest` module
-- TODO remove this?
buildProdTree :: Bool -> [Pattern] -> FilePath -> IO ProdTree
buildProdTree = buildTree (return . const ())

-- TODO are contents sorted? they probably should be for stable hashes
buildTree :: Show a => (FilePath -> IO a) -> Bool -> [Pattern] -> FilePath -> IO (HashTree a)
buildTree readFileFn beVerbose excludes path = do
  -- putStrLn $ "buildTree path: '" ++ path ++ "'"
  -- TODO attempt building lazily only to a certain depth... 10?
  -- tree <- DT.readDirectoryWithLD 10 return path -- TODO need to rename root here?
  tree <- DT.readDirectoryWithL readFileFn path -- TODO need to rename root here?
  -- putStrLn $ show tree
  buildTree' readFileFn beVerbose 0 excludes tree

-- TODO take this as a command-line argument
lazyDirDepth :: Int
lazyDirDepth = 4

-- TODO oh no, does AnchoredDirTree fail on cyclic symlinks?
buildTree' :: Show a => (FilePath -> IO a) -> Bool -> Int -> [Pattern] -> DT.AnchoredDirTree a -> IO (HashTree a)
-- TODO catch and re-throw errors with better description and/or handle them here
buildTree' _ _ _ _  (a DT.:/ (DT.Failed n e )) = error $ (a </> n2p n) ++ ": " ++ show e
buildTree' readFileFn v depth es (a DT.:/ (DT.File n _)) = do
  -- TODO how to exclude these?
  let fPath = a </> n2p n
  !h  <- unsafeInterleaveIO $ hashFile v fPath
  !fd <- unsafeInterleaveIO $ readFileFn fPath -- TODO is this safe enough?
  -- seems not to help with memory usage?
  -- return $ (\x -> hash x `seq` name x `seq` x) $ File { name = n, hash = h }
  -- return File { name = n, hash = h }
  return $ (if depth < lazyDirDepth
              then id
              else (\x -> hash x `seq` name x `seq` x))
         $ File { name = n, hash = h, fileData = fd }

buildTree' readFileFn v depth es d@(a DT.:/ (DT.Dir n _)) = do
  let root = a </> n2p n
      -- bang t has no effect on memory usage
      hashSubtree t = unsafeInterleaveIO $ buildTree' readFileFn v (depth+1) es $ root DT.:/ t
      (_ DT.:/ (DT.Dir _ cs')) = excludeGlobs es d -- TODO operate on only the cs part

  -- this works, but doesn't affect memory usage:
  -- subTrees <- (if depth > 10 then M.forM else P.forM) cs' hashSubtree

  subTrees <- P.forM cs' hashSubtree

  -- sorting by hash is better in that it catches file renames,
  -- but sorting by name is better in that it lets you stream hashes to stdout.
  -- so we do both: name when building the tree, then hash when computing dir hashes
  let cs'' = sortBy (compare `on` name) subTrees
      -- csByH = sortBy (compare `on` hash) subTrees -- no memory difference

  -- use lazy evaluation up to 5 levels deep, then strict
  return $ (if depth < lazyDirDepth
              then id
              else (\r -> (hash r `seq` nFiles r `seq` hash r) `seq` r)) -- TODO nFiles bug here?
         $ Dir
            { name     = n
            , contents = cs''
            , hash     = hashContents cs''
            , nFiles   = sum $ map countFiles cs''
            }

hashContents :: [HashTree a] -> Hash
hashContents = hashBytes . B8.unlines . sort . map (BS.fromShort . unHash . hash)

-- If passed a file this assumes it contains hashes and builds a tree of them;
-- If passed a dir it will scan it first and then build the tree.
-- TODO don't assume??
readOrBuildTree :: Bool -> Maybe Int -> [Pattern] -> FilePath -> IO ProdTree
readOrBuildTree verbose mmaxdepth excludes path = do
  isDir  <- doesDirectoryExist path
  isFile <- doesFileExist      path
  if      isFile then readTree mmaxdepth path
  else if isDir then buildProdTree verbose excludes path
  else error $ "No such file: '" ++ path ++ "'"

-- for comparing two trees without getting hung up on different overall names
renameRoot :: FilePath -> HashTree a -> HashTree a
renameRoot newName tree = tree { name = p2n newName }

-------------------------------------
-- serialize and deserialize trees --
-------------------------------------

-- TODO can Foldable or Traversable simplify these?
-- TODO need to handle unicode here?
-- TODO does map evaluation influence memory usage?
serializeTree :: HashTree a -> [B8.ByteString]
serializeTree = map prettyHashLine . flattenTree

printTree :: HashTree a -> IO ()
printTree = mapM_ printLine . flattenTree
  where
    -- TODO don't flush every line
    printLine l = (putStrLn $ B8.unpack $ prettyHashLine l) >> hFlush stdout

-- this uses a handle for streaming output, which turns out to be important for memory usage
-- TODO rename writeHashes? this is a confusing way to say that
writeTree :: FilePath -> ProdTree -> IO ()
writeTree path tree = withFile path WriteMode $ \h ->
  mapM_ (B8.hPutStrLn h) (serializeTree tree)

writeBinTree :: FilePath -> ProdTree -> IO ()
writeBinTree path tree = B8.writeFile path $ encode tree

flattenTreePaths :: AnchoredHashTree a -> [FilePath]
flattenTreePaths (r :/ f@(File {})) = [r </> n2p (name f)]
flattenTreePaths (r :/ d@(Dir  {})) = sort $
  (r </> n2p (name d)) : concatMap (\c -> flattenTreePaths $ r' :/ c) (contents d)
  where
    r' = r </> n2p (name d)

flattenTree :: HashTree a -> [HashLine]
flattenTree = flattenTree' ""

-- TODO need to handle unicode here?
-- TODO does this affect memory usage?
flattenTree' :: FilePath -> HashTree a -> [HashLine]
flattenTree' dir (File n h _   ) = [HashLine (F, IndentLevel $ length (splitPath' dir), h, n)]
flattenTree' dir (Dir  n h cs _) = subtrees ++ [wholeDir]
  where
    subtrees = concatMap (flattenTree' $ dir </> n2p n) cs
    wholeDir = HashLine (D, IndentLevel $ length (splitPath' dir), h, n)

-- based on zipPaths in DirTree
zipPaths :: HashTree a -> HashTree (FilePath, a)
zipPaths tree = zipP "" tree
  where
    zipP p (File n h    fd) = File n h (p </> n2p n, fd)
    zipP p (Dir  n h cs dd) = Dir n h (map (zipP $ p </> n2p n) cs) dd

-- TODO better name? this is based on flattenDir in DirTree
-- Note that this will come out in dirs-first order, which doesn't work for streaming hashes.
-- It also doesn't remove the tree contents the way the original DirTree version does,
-- so it could use a lot of memory?
listTreeNodes :: HashTree a -> [HashTree a]
listTreeNodes (Dir n h cs dd) = Dir n h cs dd : concatMap listTreeNodes cs
listTreeNodes f               = [f]

listTreeNodePaths :: HashTree a -> [(FilePath, HashTree a)]
listTreeNodePaths tree = listP "" tree
  where
    listP p f@(File {name = n}) = [(p </> n2p n, f)]
    listP p d@(Dir  {name = n}) =  (p </> n2p n, d) : concatMap (listP $ p </> n2p n) (contents d)

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
-- TODO refactor so there's a proper buildTree function and this uses it
-- TODO what about files with newlines in them? might need to split at \n(file|dir)
deserializeTree :: Maybe Int -> B8.ByteString -> ProdTree
deserializeTree md = snd . head . foldr accTrees [] . reverse . parseHashes md

countFiles :: Show a => HashTree a -> Int
countFiles  (File _ _ _  ) = 1
countFiles d@(Dir _ _ _ _) = nFiles d

{- This one is confusing! It accumulates a list of trees and their indent
 - levels, and when it comes across a dir it uses the indents to determine
 - which files are children to put inside it vs which are siblings.
 -
 - If a value for d (max depth) is given, any line with an indent above that
 - will be dropped from the list to decrease memory usage.
 -}
-- accTrees :: Maybe Int -> HashLine -> [(Int, HashTree)] -> [(Int, HashTree)]
-- accTrees Nothing hl cs = accTrees' hl cs
-- accTrees (Just d) hl@(_, indent, _, _) cs
--   | indent > d = cs
--   | otherwise  = accTrees' hl cs

accTrees :: HashLine -> [(IndentLevel, ProdTree)] -> [(IndentLevel, ProdTree)]
accTrees (HashLine (t, (IndentLevel i), h, p)) cs = case t of
  F -> cs ++ [((IndentLevel i), File p h ())]
  D -> let (children, siblings) = partition (\(IndentLevel i2, _) -> i2 > i) cs
           dir = Dir p h (map snd children)
                         (sum $ map (countFiles . snd) children)
       in siblings ++ [(IndentLevel i, dir)]

-------------------
-- search a tree --
-------------------

-- treeContainsPath :: HashTree -> FilePath -> Bool
-- treeContainsPath (File f1 _     ) f2 = f1 == f2
-- treeContainsPath (Dir  f1 _ cs _) f2
--   | f1 == f2 = True
--   | length (splitPath f2) < 2 = False
--   | otherwise = let n   = head $ splitPath f2
--                     f2' = joinPath $ tail $ splitPath f2
--                 in if f1 /= n
--                   then False
--                   else any (\c -> treeContainsPath c f2') cs

treeContainsPath :: ProdTree -> FilePath -> Bool
treeContainsPath tree path = isJust $ dropTo tree path

-- TODO is there a reason the main fn doesn't work this way?
splitPath' :: FilePath -> [FilePath]
splitPath' = map (reverse . dropWhile (== '/') . reverse) . splitPath

dropTo :: HashTree a -> FilePath -> Maybe (HashTree a)
dropTo t@(File f1 _ _   ) f2 = if n2p f1 == f2 then Just t else Nothing
dropTo t@(Dir  f1 _ cs _) f2
  | n2p f1 == f2 = Just t
  | null (splitPath' f2) = Nothing
  | otherwise = let f2cs = splitPath' f2
                    f2'  = joinPath $ tail f2cs
                    n    = head f2cs
                in if n2p f1 /= n
                  then Nothing
                  else msum $ map (\c -> dropTo c f2') cs

treeContainsHash :: ProdTree -> Hash -> Bool
treeContainsHash (File _ h1 ()  ) h2 = h1 == h2
treeContainsHash (Dir  _ h1 cs _) h2
  | h1 == h2 = True
  | otherwise = any (\c -> treeContainsHash c h2) cs

-- TODO if tree contains path, be able to extract it! need for rm

-------------------
-- add a subtree --
-------------------

-- TODO use this to implement hashing multiple trees at once?
wrapInEmptyDir :: Show a => FilePath -> HashTree a -> HashTree a
wrapInEmptyDir n t = do
  Dir { name = p2n n, hash = h, contents = cs, nFiles = countFiles t }
  where
    cs = [t]
    h = hashContents cs

wrapInEmptyDirs :: Show a => FilePath -> HashTree a -> HashTree a
wrapInEmptyDirs p t = case splitPath' p of
  []     -> t
  (n:[]) -> wrapInEmptyDir n t
  (n:ns) -> wrapInEmptyDir n $ wrapInEmptyDirs (joinPath ns) t

-- TODO does the anchor here matter? maybe it's set to the full path accidentally
addSubTree :: Show a => HashTree a -> HashTree a -> FilePath -> HashTree a
addSubTree t@(File {}) t' p
  | p2n p == name t = t' -- completely replace it with the new tree
  | otherwise = wrapInEmptyDirs p t'
addSubTree _ _ path | null (splitPath' path) = error "can't insert tree at null path"
addSubTree main sub path = main { hash = h', contents = cs', nFiles = n' }
  where
    comps  = splitPath' path
    p1     = head comps
    path'  = joinPath $ tail comps
    h'     = hashContents cs'
    cs'    = sortTreesByName $ filter (\c -> name c /= p2n p1) (contents main) ++ [newSub]
    n'     = countFiles main + countFiles newSub - case oldSub of { Nothing -> 0; Just s -> countFiles s; }
    sub'   = sub { name = p2n $ last comps }
    oldSub = find (\c -> name c == p2n p1) (contents main)
    newSub = if length comps == 1
               then sub'
               else case oldSub of
                 Nothing -> wrapInEmptyDirs path sub'
                 Just d  -> addSubTree d sub' path'

sortTreesByName :: [HashTree a] -> [HashTree a]
sortTreesByName = sortBy (compare `on` name)

----------------------
-- remove a subtree --
----------------------

{- This one gets a little complicated because if the subtree exists
 - then after removing it we have to adjust parent nFiles back up to the root.
 - Also edits have to be done on the parent tree (so no File branch).
 - Buuuut for now can just ignore nFiles as it's not needed for the rm itself.
 - TODO does this actually solve nFiles too?
 -}
rmSubTree :: Show a => HashTree a -> FilePath -> Either String (HashTree a)
rmSubTree (File _ _ _) p = Left $ "rmSubTree on a File: '" ++ p ++ "'" -- TODO fix simtest error
rmSubTree d@(Dir _ _ cs n) p = case dropTo d p of
  Nothing -> Left $ "no such subtree: '" ++ p ++ "'"
  Just t -> Right $ if t `elem` cs
    then d { contents = delete t cs, nFiles = n - countFiles t }
    else d { contents = map (\c -> fromRight c $ rmSubTree c $ joinPath $ tail $ splitPath' p) cs
           , nFiles = n - countFiles t -- TODO would subtracting the removed files count be faster?
           }
