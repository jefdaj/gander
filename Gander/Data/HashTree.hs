{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gander.Data.HashTree
  ( HashTree(..)
  , TreeType(..)
  , keepPath
  , readTree
  , buildTree
  , readOrBuildTree
  , renameRoot
  , printTree
  , writeBinTree
  , prettyHashLine
  , serializeTree
  , writeTree
  , flattenTree
  , deserializeTree
  , hashContents
  , parseHashes
  , dropTo
  , treeContainsPath
  , treeContainsHash
  , addSubTree
  , rmSubTree
  )
  where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff

import Gander.Data.Hash

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as BS
import qualified Data.Text.Encoding as T
-- import qualified Data.ByteString.Streaming.Char8 as Q
-- import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.ByteString.Char8 as B

import Gander.Util (pathComponents, FileName, p2n, n2p)
import qualified System.Directory.Tree as DT

import Control.Monad        (msum)
-- import Control.Monad.Parallel (forM) -- no effect on memory usage (for annex links at least)
import qualified Control.Monad.Parallel as P
import qualified Control.Monad          as M
import Data.List            (find, delete, sort)
import Data.Maybe           (isJust)
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
import Data.Attoparsec.ByteString.Char8 hiding (D)
import Data.Attoparsec.Combinator

import Data.Store             (encode, decodeIO, Store(..))
import Control.Exception.Safe (catchAny)
import TH.Derive

import Control.DeepSeq

-- import Text.Regex.Do.Split  (split)
-- import Text.Regex.Do.TypeDo (Body(..), Pattern(..))

-- for distinguishing beween files and dirs
data TreeType = D | F
  deriving (Eq, Read, Show, Ord)

instance NFData TreeType
  where rnf = const () -- TODO is this valid?

$($(derive [d| instance Deriving (Store TreeType) |]))

type IndentLevel = Int

type HashLine = (TreeType, IndentLevel, Hash, FileName)

-- TODO actual Pretty instance
-- note: p can have weird characters, so it should be handled only as ByteString
prettyHashLine :: HashLine -> B.ByteString
prettyHashLine (t, n, h, p) = B.unwords
  [B.pack $ show t, B.pack $ show n, prettyHash h, T.encodeUtf8 p]

{- A tree of file names matching (a subdirectory of) the annex,
 - where each dir and file node contains a hash of its contents.
 - TODO read and write files
 - TODO would also storing the number of files in each dir help, or timestamps?
 -}
-- data HashTree = DT.AnchoredDirTree Hash
--   deriving (Eq, Read, Show)
--   TODO rename name -> path?
data HashTree
  = File { name :: !FileName, hash :: !Hash }
  | Dir  { name :: !FileName, hash :: Hash, contents :: [HashTree], nFiles :: Int }
  deriving (Read, Show, Ord) -- TODO switch to hash-based equality after testing

-- TODO disable this while testing to ensure deep equality?
instance Eq HashTree where
  t1 == t2 = hash t1 == hash t2

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
$($(derive [d|
    instance Deriving (Store HashTree)
    |]))

excludeGlobs :: [Pattern]
             -> (DT.AnchoredDirTree FileName -> DT.AnchoredDirTree FileName)
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
readTree :: FilePath -> IO HashTree
readTree path = catchAny
                  (B.readFile path >>= decodeIO)
                  (\_ -> fmap deserializeTree $ B.readFile path)
--   (do
--      (hs :: [HashLine]) <- decodeIO =<< B.readFile path
--      return $ snd $ head $ foldr accTrees [] hs)
--   (\_ -> fmap deserializeTree $ B.readFile path)

-- TODO are contents sorted? they probably should be for stable hashes
buildTree :: Bool -> [Pattern] -> FilePath -> IO HashTree
buildTree beVerbose excludes path = do
  -- putStrLn $ "buildTree path: '" ++ path ++ "'"
  -- TODO attempt building lazily only to a certain depth... 10?
  -- tree <- DT.readDirectoryWithLD 10 return path -- TODO need to rename root here?
  tree <- DT.readDirectoryWithL (return . p2n) path -- TODO need to rename root here?
  -- putStrLn $ show tree
  buildTree' beVerbose 0 excludes tree

-- TODO take this as a command-line argument
lazyDirDepth :: Int
lazyDirDepth = 4

-- TODO oh no, does AnchoredDirTree fail on cyclic symlinks?
buildTree' :: Bool -> Int -> [Pattern] -> DT.AnchoredDirTree FileName -> IO HashTree
-- TODO catch and re-throw errors with better description and/or handle them here
buildTree' _ _ _  (a DT.:/ (DT.Failed n e )) = error $ (a </> n2p n) ++ ": " ++ show e
buildTree' v depth es (_ DT.:/ (DT.File n f)) = do
  -- TODO how to exclude these?
  !h <- unsafeInterleaveIO $ hashFile v $ n2p f
  -- seems not to help with memory usage?
  -- return $ (\x -> hash x `seq` name x `seq` x) $ File { name = n, hash = h }
  -- return File { name = n, hash = h }
  return $ (if depth < lazyDirDepth
              then id
              else (\x -> hash x `seq` name x `seq` x))
         $ File { name = n, hash = h }

buildTree' v depth es d@(a DT.:/ (DT.Dir n _)) = do
  let root = a </> n2p n
      -- bang t has no effect on memory usage
      hashSubtree t = unsafeInterleaveIO $ buildTree' v (depth+1) es $ root DT.:/ t
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
              else (\r -> (hash r `seq` nFiles r `seq` hash r) `seq` r))
         $ Dir
            { name     = n
            , contents = cs''
            , hash     = hashContents cs''
            , nFiles   = sum $ map countFiles cs''
            }

hashContents :: [HashTree] -> Hash
hashContents = hashBytes . B.unlines . sort . map (BS.fromShort . unHash . hash)

-- If passed a file this assumes it contains hashes and builds a tree of them;
-- If passed a dir it will scan it first and then build the tree.
-- TODO don't assume??
readOrBuildTree :: Bool -> [Pattern] -> FilePath -> IO HashTree
readOrBuildTree verbose excludes path = do
  isDir  <- doesDirectoryExist path
  isFile <- doesFileExist      path
  if      isFile then readTree path
  else if isDir then buildTree verbose excludes path
  else error $ "No such file: '" ++ path ++ "'"

-- for comparing two trees without getting hung up on different overall names
renameRoot :: FilePath -> HashTree -> HashTree
renameRoot newName tree = tree { name = p2n newName }

-------------------------------------
-- serialize and deserialize trees --
-------------------------------------

-- TODO can Foldable or Traversable simplify these?
-- TODO need to handle unicode here?
-- TODO does map evaluation influence memory usage?
serializeTree :: HashTree -> [B.ByteString]
serializeTree = map prettyHashLine . flattenTree

printTree :: HashTree -> IO ()
printTree = mapM_ printLine . flattenTree
  where
    -- TODO don't flush every line
    printLine l = (putStrLn $ B.unpack $ prettyHashLine l) >> hFlush stdout

-- this uses a handle for streaming output, which turns out to be important for memory usage
-- TODO rename writeHashes? this is a confusing way to say that
writeTree :: FilePath -> HashTree -> IO ()
writeTree path tree = withFile path WriteMode $ \h ->
  mapM_ (B.hPutStrLn h) (serializeTree tree)

writeBinTree :: FilePath -> HashTree -> IO ()
writeBinTree path tree = B.writeFile path $ encode tree

flattenTree :: HashTree -> [HashLine]
flattenTree = flattenTree' ""

-- TODO need to handle unicode here?
-- TODO does this affect memory usage?
flattenTree' :: FilePath -> HashTree -> [HashLine]
flattenTree' dir (File n h     ) = [(F, length (splitPath dir), h, n)]
flattenTree' dir (Dir  n h cs _) = subtrees ++ [wholeDir]
  where
    subtrees = concatMap (flattenTree' $ dir </> n2p n) cs
    wholeDir = (D, length (splitPath dir), h, n)

typeP :: Parser TreeType
typeP = do
  t <- choice [char 'D', char 'F'] <* char ' '
  return $ read [t]

hashP :: Parser Hash
hashP = do
  h <- take digestLength -- TODO any need to sanitize these?
  _ <- char ' '
  return $ Hash $ BS.toShort h

-- like endOfLine, but make sure D/F comes next instead of the rest of a filename
-- TODO uh, what if the filename contains "\n(D|F)\ "? pretty pathological
breakP :: Parser ()
breakP = endOfLine >> choice [typeP >> return (), endOfInput]

nameP :: Parser FileName
nameP = fmap p2n $ manyTill anyChar $ lookAhead breakP

indentP :: Parser IndentLevel
indentP = do
  n <- manyTill digit $ char ' '
  -- TODO char ' ' here?
  return $ read n

lineP :: Parser HashLine
lineP = do
  t <- typeP
  i <- indentP
  h <- hashP
  p <- nameP
  return (t, i, h, p)

linesP :: Parser [HashLine]
linesP = sepBy' lineP endOfLine

fileP :: Parser [HashLine]
fileP = linesP <* endOfLine <* endOfInput

-- TODO use bytestring the whole time rather than converting
-- TODO should this propogate the Either?
-- TODO any more elegant way to make the parsing strict?
parseHashes :: B.ByteString -> [HashLine]
parseHashes = fromRight [] . parseOnly fileP

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
-- TODO refactor so there's a proper buildTree function and this uses it
-- TODO what about files with newlines in them? might need to split at \n(file|dir)
deserializeTree :: B.ByteString -> HashTree
deserializeTree = snd . head . foldr accTrees [] . reverse . parseHashes

countFiles :: HashTree -> Int
countFiles (File _ _    ) = 1
countFiles (Dir  _ _ _ n) = n

{- This one is confusing! It accumulates a list of trees and their indent
 - levels, and when it comes across a dir it uses the indents to determine
 - which files are children to put inside it vs which are siblings.
 -}
accTrees :: HashLine -> [(Int, HashTree)] -> [(Int, HashTree)]
accTrees (t, indent, h, p) cs = case t of
  F -> cs ++ [(indent, File p h)]
  D -> let (children, siblings) = partition (\(i, _) -> i > indent) cs
           dir = Dir p h (map snd children)
                         (sum $ map (countFiles . snd) children)
       in siblings ++ [(indent, dir)]

-------------------
-- search a tree --
-------------------

-- treeContainsPath :: HashTree -> FilePath -> Bool
-- treeContainsPath (File f1 _     ) f2 = f1 == f2
-- treeContainsPath (Dir  f1 _ cs _) f2
--   | f1 == f2 = True
--   | length (pathComponents f2) < 2 = False
--   | otherwise = let n   = head $ pathComponents f2
--                     f2' = joinPath $ tail $ pathComponents f2
--                 in if f1 /= n
--                   then False
--                   else any (\c -> treeContainsPath c f2') cs

treeContainsPath :: HashTree -> FilePath -> Bool
treeContainsPath tree path = isJust $ dropTo tree path

dropTo :: HashTree -> FilePath -> Maybe HashTree
dropTo t@(File f1 _     ) f2 = if n2p f1 == f2 then Just t else Nothing
dropTo t@(Dir  f1 _ cs _) f2
  | n2p f1 == f2 = Just t
  | length (pathComponents f2) < 2 = Nothing
  | otherwise = let n   = p2n $ head $ pathComponents f2
                    f2' = joinPath $ tail $ pathComponents f2
                in if f1 /= n
                  then Nothing
                  else msum $ map (\c -> dropTo c f2') cs

treeContainsHash :: HashTree -> Hash -> Bool
treeContainsHash (File _ h1     ) h2 = h1 == h2
treeContainsHash (Dir  _ h1 cs _) h2
  | h1 == h2 = True
  | otherwise = any (\c -> treeContainsHash c h2) cs

-- TODO if tree contains path, be able to extract it! need for rm

-------------------
-- add a subtree --
-------------------

wrapInEmptyDir :: FilePath -> HashTree -> HashTree
wrapInEmptyDir n t = do
  Dir { name = p2n n, hash = h, contents = cs, nFiles = nFiles t }
  where
    cs = [t]
    h = hashContents cs

wrapInEmptyDirs :: FilePath -> HashTree -> HashTree
wrapInEmptyDirs p t = case pathComponents p of
  []     -> error "wrapInEmptyDirs needs at least one dir"
  (n:[]) -> wrapInEmptyDir n t
  (n:ns) -> wrapInEmptyDir n $ wrapInEmptyDirs (joinPath ns) t

-- TODO does the anchor here matter? maybe it's set to the full path accidentally
addSubTree :: HashTree -> HashTree -> FilePath -> HashTree
addSubTree (File _ _) _ _ = error $ "attempt to insert tree into a file"
addSubTree _ _ path | null (pathComponents path) = error "can't insert tree at null path"
addSubTree main sub path = main { hash = h', contents = cs', nFiles = n' }
  where
    comps  = pathComponents path
    p1     = head comps
    path'  = joinPath $ tail comps
    h'     = hashContents cs'
    cs'    = sortBy (compare `on` name) $ filter (\c -> name c /= p2n p1) (contents main) ++ [newSub]
    n'     = nFiles main + nFiles newSub - case oldSub of { Nothing -> 0; Just s -> nFiles s; }
    sub'   = sub { name = p2n $ last comps }
    oldSub = find (\c -> name c == p2n p1) (contents main)
    newSub = if length comps == 1
               then sub'
               else case oldSub of
                 Nothing -> wrapInEmptyDirs path sub'
                 Just d  -> addSubTree d sub' path'

----------------------
-- remove a subtree --
----------------------

{- This one gets a little complicated because if the subtree exists
 - then after removing it we have to adjust parent nFiles back up to the root.
 - Also edits have to be done on the parent tree (so no File branch).
 - Buuuut for now can just ignore nFiles as it's not needed for the rm itself.
 - TODO does this actually solve nFiles too?
 -}
rmSubTree :: HashTree -> FilePath -> Either String HashTree
rmSubTree (File _ _) p = Left $ "no such subtree: '" ++ p ++ "'"
rmSubTree d@(Dir _ _ cs n) p = case dropTo d p of
  Nothing -> Left $ "no such subtree: '" ++ p ++ "'"
  Just t -> Right $ if t `elem` cs
    then d { contents = delete t cs, nFiles = n - countFiles t }
    else d { contents = map (\c -> fromRight c $ rmSubTree c $ joinPath $ tail $ splitPath p) cs
           , nFiles = n - countFiles t
           }
