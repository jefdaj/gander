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
  , parseHashes
  , getSubTree
  , treeContainsPath
  , treeContainsHash
  , addSubTree
  )
  where

import Debug.Trace

import Gander.Lib.Hash

-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.ByteString.Char8 as B

import Gander.Lib.Git (pathComponents) -- TODO not actually git related
import qualified System.Directory.Tree as DT

import Control.Monad        (forM, msum)
import Data.List            (find)
import Data.Function        (on)
import Data.List            (partition, sortBy)
import Data.Either          (fromRight)
import Data.Ord             (compare)
import System.Directory     (doesFileExist, doesDirectoryExist)
import System.FilePath      ((</>), takeFileName, splitPath, joinPath)
import System.FilePath.Glob (compile, match)
import System.IO.Unsafe     (unsafeInterleaveIO)

import Prelude hiding (take)
import Data.Attoparsec.ByteString.Char8 hiding (match)
import Data.Attoparsec.Combinator

-- import Text.Regex.Do.Split  (split)
-- import Text.Regex.Do.TypeDo (Body(..), Pattern(..))

type HashLine = (Char, Hash, FilePath)

-- TODO actual Pretty instance
-- TODO need to handle unicode here?
prettyHashLine :: HashLine -> String
prettyHashLine (t, Hash h, p) = unwords [[t], h, p]

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
  deriving (Read, Show, Ord) -- TODO switch to hash-based equality after testing

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

readTree :: FilePath -> IO HashTree
readTree = fmap deserializeTree . readFile

-- TODO are contents sorted? they probably should be for stable hashes
buildTree :: Bool -> [String] -> FilePath -> IO HashTree
buildTree beVerbose excludes path = do
  tree <- DT.readDirectoryWithL return path
  buildTree' beVerbose $ excludeGlobs excludes tree

-- TODO oh no, does AnchoredDirTree fail on cyclic symlinks?
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
hashContents ts = Hash $ hashBytes $ BL.pack txt
  where
    ls  = map serializeTree ts
    txt = concat ls -- TODO why is this important vs unlines?

-- If passed a file this assumes it contains hashes and builds a tree of them;
-- If passed a dir it will scan it first and then build the tree.
-- TODO don't assume??
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
-- TODO need to handle unicode here?
serializeTree :: HashTree -> String
serializeTree = unlines . map prettyHashLine . flattenTree

flattenTree :: HashTree -> [HashLine]
flattenTree = flattenTree' ""

-- TODO need to handle unicode here?
flattenTree' :: FilePath -> HashTree -> [HashLine]
flattenTree' dir (File n h     ) = [('F', h, dir </> n)]
flattenTree' dir (Dir  n h cs _) = subtrees ++ [wholeDir]
  where
    subtrees = concatMap (flattenTree' $ dir </> n) cs
    wholeDir = ('D', h, dir </> n)

typeP :: Parser Char
typeP = choice [char 'D', char 'F'] <* char ' '

hashP :: Parser Hash
hashP = do
  h <- take 64 -- TODO any need to sanitize these?
  _ <- char ' '
  return $ Hash $ B8.unpack h

-- line endOfLine, but make sure D/F comes next instead of the rest of a filename
-- TODO uh, what if the filename contains "\n(D|F)\ "? pretty pathological
breakP :: Parser ()
breakP = endOfLine >> typeP >> return ()

pathP :: Parser FilePath
pathP = manyTill anyChar $ lookAhead $ choice [breakP, endOfLine >> endOfInput]

lineP :: Parser (Char, Hash, Int, FilePath)
lineP = do
  t <- typeP
  h <- hashP
  p <- pathP
  let i = length $ splitPath p -- TODO this is ok with the linebreaks?
  return (t, h, i, takeFileName p)

linesP :: Parser [(Char, Hash, Int, FilePath)]
linesP = sepBy' lineP endOfLine

-- TODO use bytestring the whole time rather than converting
-- TODO should this propogate the Either?
parseHashes :: String -> [(Char, Hash, Int, FilePath)]
parseHashes = fromRight [] . parseOnly linesP . B8.pack

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
-- TODO refactor so there's a proper buildTree function and this uses it
deserializeTree :: String -> HashTree
deserializeTree = snd . head . foldr accTrees [] . reverse . parseHashes

countFiles :: HashTree -> Int
countFiles (File _ _    ) = 1
countFiles (Dir  _ _ _ n) = n

{- This one is confusing! It accumulates a list of trees and their indent
 - levels, and when it comes across a dir it uses the indents to determine
 - which files are children to put inside it vs which are siblings.
 -}
accTrees :: (Char, Hash, Int, FilePath) -> [(Int, HashTree)] -> [(Int, HashTree)]
accTrees l@(t, h, indent, p) cs = case t of
  'F' -> cs ++ [(indent, File p h)]
  'D' -> let (children, siblings) = partition (\(i, _) -> i > indent) cs
             dir = Dir p h (map snd children)
                           (sum $ map (countFiles . snd) children)
         in siblings ++ [(indent, dir)]
  _ -> error $ "invalid line: '" ++ show l ++ "'" 

-------------------
-- search a tree --
-------------------

treeContainsPath :: HashTree -> FilePath -> Bool
treeContainsPath (File f1 _     ) f2 = (trace ("traversing file: " ++ f1) f1) == f2
treeContainsPath (Dir  f1 _ cs _) f2
  | (trace ("traversing dir: " ++ f1) f1) == f2 = True
  | length (pathComponents f2) < 2 = False
  | otherwise = let n   = head $ pathComponents f2
                    f2' = joinPath $ tail $ pathComponents f2
                in if f1 /= n
                  then False
                  else any (\c -> treeContainsPath c f2') cs

getSubTree :: HashTree -> FilePath -> Maybe HashTree
getSubTree t@(File f1 _     ) f2 = if (trace ("traversing file: " ++ f1) f1) == f2 then Just t else Nothing
getSubTree t@(Dir  f1 _ cs _) f2
  | (trace ("traversing dir: " ++ f1) f1) == f2 = Just t
  | length (pathComponents f2) < 2 = Nothing
  | otherwise = let n   = head $ pathComponents f2
                    f2' = joinPath $ tail $ pathComponents f2
                in if f1 /= n
                  then Nothing
                  else msum $ map (\c -> getSubTree c f2') cs


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
wrapInEmptyDir n t = Dir { name = n, hash = h, contents = cs, nFiles = nFiles t }
  where
    cs = [t]
    h = hashContents cs

wrapInEmptyDirs :: FilePath -> HashTree -> HashTree
wrapInEmptyDirs p t = case pathComponents p of
  []     -> error "wrapInEmptyDirs needs at least one dir"
  (n:[]) -> wrapInEmptyDir n t
  (n:ns) -> wrapInEmptyDir n $ wrapInEmptyDirs (joinPath ns) t

addSubTree :: HashTree -> HashTree -> FilePath -> HashTree
addSubTree (File _ _) _ _ = error $ "attempt to insert tree into a file"
addSubTree _ _ path | null (pathComponents path) = error "can't insert tree at null path"
addSubTree main sub path = main { hash = h', contents = cs', nFiles = n' }
  where
    comps  = pathComponents path
    p1     = head comps
    path'  = joinPath $ tail comps
    h'     = hashContents cs'
    cs'    = sortBy (compare `on` name) $ filter (\c -> name c /= p1) (contents main) ++ [newSub]
    n'     = nFiles main + nFiles newSub - case oldSub of { Nothing -> 0; Just s -> nFiles s; }
    sub'   = sub { name = last comps }
    oldSub = find (\c -> name c == p1) (contents main)
    newSub = if length comps == 1
               then sub'
               else case oldSub of
                 Nothing -> wrapInEmptyDirs path sub'
                 Just d  -> addSubTree d sub' path'
