{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO break into modules? Main, Types, Config, Scan, Dedup
-- TODO is git-annex an actual dep, or just recommended to go with it?
-- TODO figure out how to read files + compute hashes in parallel

import qualified Data.Map              as Map
import qualified Data.ByteString.Lazy  as LB
import qualified System.Directory.Tree as DT

import Data.Ord                   (comparing)
import Control.Arrow              ((&&&))
import Control.Monad              (when, forM)
import Data.Map                   (Map)
import Data.Foldable              (toList)
import Crypto.Hash                (Digest, SHA256, hashlazy)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List                  (sort, sortBy, partition, isInfixOf, intersperse)
import System.Console.Docopt      (docoptFile, parseArgsOrExit,
                                   getArgOrExitWith, isPresent, longOption,
                                   shortOption, command, argument)
import System.Environment         (getArgs)
import System.FilePath            ((</>), takeBaseName, takeFileName, splitPath)
import System.IO.Unsafe           (unsafeInterleaveIO)
import System.Posix.Files         (getFileStatus, isSymbolicLink, readSymbolicLink)

-----------
-- types --
-----------

{- Parsed command line args
 - TODO add other stuff from usage.txt, or revise that
 -}
data Options = Options
  { verbose :: Bool
  , force   :: Bool
  }
  deriving (Read, Show)

{- Checksum (sha256sum?) of a file or folder.
 - For files, should match the corresponding git-annex key.
 - TODO would storing it in a more efficient way help?
 - TODO would adding timestamps or number of files help?
 -}
newtype Hash = Hash String
  deriving (Eq, Read, Show, Ord)

{- A tree of file names matching (a subdirectory of) the annex,
 - where each dir and file node contains a hash of its contents.
 - TODO read and write files
 - TODO would also storing the number of files in each dir help, or timestamps?
 -}
-- data HashTree = DT.AnchoredDirTree Hash
--   deriving (Eq, Read, Show)
data HashTree
  = Skip { name :: FilePath, hash :: Hash } -- for files + folders to ignore
  | File { name :: FilePath, hash :: Hash }
  | Dir  { name :: FilePath, hash :: Hash, contents :: [HashTree], nFiles :: Int }
  deriving (Read, Show)

-- TODO disable this while testing to ensure deep equality?
instance Eq HashTree where
  t1 == t2 = hash t1 == hash t2

type DupeList = (Int, [FilePath])

{- A map from file/dir hash to a list of duplicate file paths.
 - Could be rewritten to contain links to HashTrees if that helps.
 -}
type DupeMap = Map Hash DupeList

---------------------------
-- lazily hash and print --
---------------------------

-- Note that this part doesn't build any trees. Should it?

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
-- serialize and deserialize trees --
-------------------------------------

-- TODO can Foldable or Traversable simplify these?

serialize :: HashTree -> String
serialize = unlines . serialize' ""

serialize' :: FilePath -> HashTree -> [String]
serialize' _   (Skip  _ _           ) = []
serialize' dir (File n (Hash h)     ) = [unwords [h, "file", dir </> n]]
serialize' dir (Dir  n (Hash h) cs _)
  = concatMap (serialize' $ dir </> n) cs -- recurse on contents
  ++ [unwords [h, "dir ", dir </> n]] -- finish with hash of entire dir

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
deserialize :: String -> HashTree
deserialize = snd . head . foldr accTrees [] . map readLine . reverse . lines

countFiles :: HashTree -> Int
countFiles (Skip _ _    ) = 0
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

-- this is for hashing the "skip" nodes so they equate properly
-- TODO any good way to remove it?
hashName :: String -> Hash
hashName = Hash . hashBytes . pack

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

noSkips :: [HashTree] -> [HashTree]
noSkips [] = []
noSkips ((Skip _ _):xs) = noSkips xs
noSkips (x:xs) = x:noSkips xs

-- TODO write an equivalent that serializes as it goes rather than accumulating
-- hashTree :: DT.DirTree Hash -> Either String HashTree
-- hashTree (DT.Failed ".git" _) = Right $ Skip ".git" (hashName ".git") -- skip git (and annex) files
-- hashTree (DT.Dir    ".git" _) = Right $ Skip ".git" (hashName ".git") -- skip git (and annex) files
-- hashTree (DT.Failed n e ) = Left  $ n ++ " " ++ show e
-- hashTree (DT.File   n f ) = Right $ File n f
-- hashTree (DT.Dir    n ts) = case partitionEithers (map hashTree ts) of
--   ([], trees) -> let trees' = noSkips trees
--                  in Right $ Dir n (hashHashes $ map hash trees') trees'
--                                   (sum $ map countFiles trees)
--   (errors, _) -> Left  $ unlines errors

-- printHashes :: HashTree -> IO ()
-- printHashes = putStr . serialize

---------------------
-- build dupe maps --
---------------------

-- TODO can Foldable or Traversable simplify these?

pathsByHash :: HashTree -> DupeMap
pathsByHash = Map.fromListWith mergeDupeLists . pathsByHash' ""

mergeDupeLists :: DupeList -> DupeList -> DupeList
mergeDupeLists (n1, l1) (n2, l2) = (n1 + n2, l1 ++ l2)

pathsByHash' :: FilePath -> HashTree -> [(Hash, DupeList)]
pathsByHash' _   (Skip _ _      ) = []
pathsByHash' dir (File n h      ) = [(h, (1, [dir </> n]))]
pathsByHash' dir (Dir  n h cs fs) = cPaths ++ [(h, (fs, [dir </> n]))]
  where
    cPaths = concatMap (pathsByHash' $ dir </> n) cs

---------------
-- list dups --
---------------

-- TODO warning: so far it lists anything annexed as a dup

-- see https://mail.haskell.org/pipermail/beginners/2009-June/001867.html
sortDescLength :: [DupeList] -> [DupeList]
sortDescLength = map snd
               . sortBy (comparing $ negate . fst . snd)
               . map (length &&& id)

dupesByNFiles :: DupeMap -> [DupeList]
dupesByNFiles = sortDescLength . filter hasDupes . toList

hasDupes :: DupeList -> Bool
hasDupes (nfiles, paths) = length paths > 1 && nfiles > 0

printDupes :: [DupeList] -> IO ()
printDupes groups = mapM_ printGroup groups
  where
    explain fs ds = if fs == ds
      then "# " ++ show fs ++ " duplicate files"
      else "# " ++ show ds ++ " duplicate dirs with " ++ show (div fs ds)
                ++ " files each (" ++ show fs ++ " total)"
    printGroup (n, paths) = mapM_ putStrLn
                          $ [explain n (length paths)]
                          ++ sort paths ++ [""]

----------------
-- repl tests --
----------------

-- TODO rewrite with new hashTree
-- roundTripTree :: Bool -> FilePath -> IO Bool
-- roundTripTree beVerbose path = do
--   let opts = Options { verbose=beVerbose, force=False }
--   tree1 <- cmdHash opts path
--   let str1  = serialize   tree1
--       tree2 = deserialize str1
--       str2  = serialize   tree2
--   putStrLn str1
--   putStrLn $ show tree1
--   putStrLn $ "tree1 == tree2? " ++ show (tree1 == tree2)
--   putStrLn $ "show tree1 == show tree2? " ++ show (show tree1 == show tree2)
--   putStrLn $ "str1 == str2? "   ++ show (str1  == str2)
--   return $ tree1 == tree2

-- TODO rewrite with new hashTree
-- mapTree :: FilePath -> IO DupeMap
-- mapTree path = do
--   let opts = Options { verbose=True, force=False }
--   tree <- cmdHash opts path
--   let m = pathsByHash tree
--   return m

---------------
-- interface --
---------------

-- Note that you can't hash a folder while writing to a file inside it!
cmdHash :: Options -> FilePath -> IO ()
cmdHash opts path = DT.readDirectoryWithL return path >>= printHashes opts

cmdDupes :: Options -> FilePath -> IO [DupeList]
cmdDupes _ path = do
  tree <- fmap deserialize $ readFile path
  let pbyh = pathsByHash tree
      pdup = dupesByNFiles pbyh
  return pdup

main :: IO ()
main = do
  -- parse usage patterns, then use them to parse cli args
  let ptns = [docoptFile|usage.txt|]
  args <- parseArgsOrExit ptns =<< getArgs
  let cmd  n = isPresent args $ command n
      path n = getArgOrExitWith ptns args $ argument n
      flag s l  = isPresent args (shortOption s)
               || isPresent args (longOption  l)
      opts = Options
        { verbose = flag 'v' "verbose"
        , force   = flag 'f' "force"
        }
  -- dispatch on command
  if      cmd "hash"  then path "path"   >>= cmdHash opts
  else if cmd "dupes" then path "hashes" >>= cmdDupes opts >>= printDupes
  else print args >> print opts
