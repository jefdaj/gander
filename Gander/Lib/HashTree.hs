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

  , hashBreakP
  , hashLineP
  , hashLineP2
  , hashLinesP
  )
  where

import Debug.Trace

import Gander.Lib.Hash

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.ByteString.Char8 as B
import qualified System.Directory.Tree as DT

import Control.Monad        (forM)
import Data.Function        (on)
import Data.List            (partition, sortBy)
import Data.Ord             (compare)
import System.Directory     (doesFileExist, doesDirectoryExist)
import System.FilePath      ((</>), takeFileName, splitPath)
import System.FilePath.Glob (compile, match)
import System.IO.Unsafe     (unsafeInterleaveIO)

import Prelude hiding (take)
import Data.Attoparsec.Char8 hiding (match)
import Data.Attoparsec.Combinator

-- import Text.Regex.Do.Split  (split)
-- import Text.Regex.Do.TypeDo (Body(..), Pattern(..))

type HashLine = (String, Hash, FilePath)

-- TODO actual Pretty instance
-- TODO need to handle unicode here?
prettyHashLine :: HashLine -> String
prettyHashLine (t, Hash h, p) = unwords [t, h, p]

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
flattenTree' dir (File n h     ) = [("F", h, dir </> n)]
flattenTree' dir (Dir  n h cs _) = subtrees ++ [wholeDir]
  where
    subtrees = concatMap (flattenTree' $ dir </> n) cs
    wholeDir = ("D", h, dir </> n)

-- line endOfLine, but make sure D/F comes next instead of the rest of a filename
hashBreakP :: Parser ()
hashBreakP = do
  _ <- endOfLine
  _ <- choice [char 'D', char 'F']
  _ <- char ' '
  return ()

hashLineP :: Parser String
hashLineP = manyTill anyChar $ lookAhead $ choice [hashBreakP, endOfLine >> endOfInput]

hashLineP2 :: Parser (String, Hash, Int, FilePath)
hashLineP2 = undefined

-- TODO this looks right, so why doesn't it work? ask on stackoverflow
hashLinesP :: Parser [String]
hashLinesP = sepBy' hashLineP endOfLine

-- like `lines`, but works when the filenames themselves contain newlines
-- TODO use bytestrings
-- TODO why doesn't the pattern work?? fuck, "regex is treated as ordinary string" :(
diabolicalLines :: String -> [String]
diabolicalLines = undefined
-- diabolicalLines s = map B.unpack $ split (Pattern $ B.pack "\nF ") (Body $ B.pack s)

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
-- TODO refactor so there's a proper buildTree function and this uses it
deserializeTree :: String -> HashTree
deserializeTree = snd . head . foldr accTrees [] . map readHashLine . traceAll . diabolicalLines
  where
    traceAll ss = map (\s -> trace ("s: '" ++ s ++ "'") s) ss

countFiles :: HashTree -> Int
countFiles (File _ _    ) = 1
countFiles (Dir  _ _ _ n) = n

{- This one is confusing! It accumulates a list of trees and their indent
 - levels, and when it comes across a dir it uses the indents to determine
 - which files are children to put inside it vs which are siblings.
 -}
accTrees :: (String, Hash, Int, FilePath) -> [(Int, HashTree)] -> [(Int, HashTree)]
accTrees l@(t, h, indent, p) cs = case t of
  "F" -> cs ++ [(indent, File p h)]
  "D" -> let (children, siblings) = partition (\(i, _) -> i > indent) cs
             dir = Dir p h (map snd children)
                           (sum $ map (countFiles . snd) children)
         in siblings ++ [(indent, dir)]
  _ -> error $ "invalid line: '" ++ show l ++ "'" 

-- note that the filename can occasionally contain a newline!
readHashLine :: String -> (String, Hash, Int, FilePath)
readHashLine line = (t, Hash h, i, takeFileName p)
  where
    [t, h]   = words tmp            -- first two words are hash and type
    (tmp, p) = splitAt 67 line      -- rest of the line is the path
    i        = length $ splitPath p -- get indent level from path
