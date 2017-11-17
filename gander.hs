{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO break into modules? Main, Types, Config, Scan, Dedup
-- TODO is git-annex an actual dep, or just recommended to go with it?
-- TODO figure out how to read files + compute hashes in parallel
-- TODO take annexes into account when scanning:
--      * just use link basenames if files are annexed
--      * ignore any .git folders? yeah, assume one annex for now!

import qualified Data.Map              as Map
import qualified Data.ByteString.Lazy  as LB
import qualified System.Directory.Tree as DT

import Data.Ord                   (comparing)
import Control.Arrow              ((&&&))
import Data.Map                   (Map)
import Data.Foldable              (toList)
import Crypto.Hash                (Digest, SHA256, hashlazy)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either                (partitionEithers)
import Data.List                  (sort, sortBy, partition, isInfixOf)
import System.Console.Docopt      (docoptFile, parseArgsOrExit,
                                   getArgOrExitWith, isPresent, longOption,
                                   shortOption, command, argument)
import System.Environment         (getArgs)
import System.FilePath            ((</>), takeBaseName, takeFileName, splitPath)
import System.Posix.Files         (getFileStatus, isSymbolicLink, readSymbolicLink)

-----------
-- types --
-----------

{- Parsed command line args
 - TODO add other stuff from usage.txt, or revise that
 -}
data Options = Options
  { verbose :: Bool
  , confirm :: Bool
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
  = File { name :: FilePath, hash :: Hash }
  | Dir  { name :: FilePath, hash :: Hash, contents :: [HashTree], nFiles :: Int }
  | Skip { name :: FilePath } -- for files + folders to ignore
  deriving (Eq, Read, Show) -- TODO write Eq instance

-- TODO disable this while testing to ensure deep equality?
-- instance Eq HashTree where
--   t1 == t2 = hash t1 == hash t2

{- A map from file/dir hash to a list of duplicate file paths.
 - Could be rewritten to contain links to HashTrees if that helps.
 -}
type DupeMap = Map Hash (Int, [FilePath])

-------------------------------------
-- serialize and deserialize trees --
-------------------------------------

-- TODO can Foldable or Traversable simplify these?

serialize :: HashTree -> String
serialize = unlines . serialize' ""

serialize' :: FilePath -> HashTree -> [String]
serialize' _   (Skip  _             ) = []
serialize' dir (File n (Hash h)     ) = [unwords [h, "file", dir </> n]]
serialize' dir (Dir  n (Hash h) cs _)
  = concatMap (serialize' $ dir </> n) cs -- recurse on contents
  ++ [unwords [h, "dir ", dir </> n]] -- finish with hash of entire dir

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
deserialize :: String -> HashTree
deserialize = snd . head . foldr accTrees [] . map readLine . reverse . lines

countFiles :: HashTree -> Int
countFiles (Skip _      ) = 0
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
hashFile :: FilePath -> IO Hash
hashFile path = do
  link <- annexLink path
  case link of
    Just l -> return $ Hash $ drop 20 $ takeBaseName l
    Nothing -> do
      sha256sum <- fmap hashBytes $ LB.readFile path
      return $ Hash sha256sum

-- the "dir:" part prevents empty files and dirs from matching
-- TODO is there a more elegant way?
-- TODO remove the sort? not needed if tree order is reliable i suppose
hashHashes :: [Hash] -> Hash
hashHashes hs = Hash $ hashBytes $ pack txt
  where
    txt = unlines $ "dir:" : (sort $ map (\(Hash h) -> h) hs)

noSkips :: [HashTree] -> [HashTree]
noSkips [] = []
noSkips ((Skip _):xs) = noSkips xs
noSkips (x:xs) = x:noSkips xs

hashTree :: DT.DirTree Hash -> Either String HashTree
hashTree (DT.Failed ".git" _) = Right $ Skip ".git" -- skip git (and annex) files
hashTree (DT.Dir    ".git" _) = Right $ Skip ".git" -- skip git (and annex) files
hashTree (DT.Failed n e ) = Left  $ n ++ " " ++ show e
hashTree (DT.File   n f ) = Right $ File n f
hashTree (DT.Dir    n ts) = case partitionEithers (map hashTree ts) of
  ([], trees) -> let trees' = noSkips trees
                 in Right $ Dir n (hashHashes $ map hash trees') trees'
                                  (sum $ map countFiles trees)
  (errors, _) -> Left  $ unlines errors

-- TODO rename hash?
-- Note that you can't hash a folder while writing to a file inside it!
scan :: Options -> FilePath -> IO HashTree
scan _ path = do
  (_ DT.:/ tree) <- DT.readDirectoryWith hashFile path
  let tree' = hashTree tree
  case tree' of
    Left  e -> error e
    Right t -> return t

printScan :: HashTree -> IO ()
printScan = putStr . serialize

---------------------
-- build dupe maps --
---------------------

-- TODO can Foldable or Traversable simplify these?

pathsByHash :: HashTree -> DupeMap
pathsByHash = Map.fromListWith mergeDupeLists . pathsByHash' ""

mergeDupeLists :: (Int, [FilePath]) -> (Int, [FilePath]) -> (Int, [FilePath])
mergeDupeLists (n1, l1) (n2, l2) = (n1 + n2, l1 ++ l2)

pathsByHash' :: FilePath -> HashTree -> [(Hash, (Int, [FilePath]))]
pathsByHash' _   (Skip _        ) = []
pathsByHash' dir (File n h      ) = [(h, (1, [dir </> n]))]
pathsByHash' dir (Dir  n h cs fs) = cPaths ++ [(h, (fs, [dir </> n]))]
  where
    cPaths = concatMap (pathsByHash' $ dir </> n) cs

---------------
-- list dups --
---------------

-- TODO warning: so far it lists anything annexed as a dup

-- see https://mail.haskell.org/pipermail/beginners/2009-June/001867.html
sortDescLength :: [(Int, [FilePath])] -> [(Int, [FilePath])]
sortDescLength = map snd
               . sortBy (comparing $ negate . fst . snd)
               . map (length &&& id)

dupesByNFiles :: DupeMap -> [(Int, [FilePath])]
dupesByNFiles = sortDescLength . filter hasDupes . toList

hasDupes :: (Int, [FilePath]) -> Bool
hasDupes (nfiles, paths) = length paths > 1 && nfiles > 0

printDupes :: [(Int, [FilePath])] -> IO ()
printDupes groups = mapM_ printGroup groups
  where
    printGroup (n, paths) = mapM_ putStrLn
                          $ [show n ++ " duplicates:"] ++ sort paths ++ [""]

dupes :: Options -> FilePath -> IO [(Int, [FilePath])]
dupes _ path = do
  tree <- fmap deserialize $ readFile path
  let pbyh = pathsByHash tree
      pdup = dupesByNFiles pbyh
  return pdup

-----------
-- tests --
-----------

roundTripTree :: FilePath -> IO Bool
roundTripTree path = do
  let opts = Options False False False
  tree1 <- scan opts path
  let str1  = serialize   tree1
      tree2 = deserialize str1
      str2  = serialize   tree2
  putStrLn str1
  putStrLn $ show tree1
  putStrLn $ "tree1 == tree2? " ++ show (tree1 == tree2)
  putStrLn $ "str1 == str2? "   ++ show (str1  == str2)
  return $ tree1 == tree2

mapTree :: FilePath -> IO DupeMap
mapTree path = do
  let opts = Options False False False
  tree <- scan opts path
  let m = pathsByHash tree
  return m

----------
-- main --
----------

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
        , confirm = flag 'c' "confirm"
        , force   = flag 'f' "force"
        }
  -- dispatch on command
  if cmd "scan" then path "path" >>= scan opts >>= printScan
  else if cmd "dupes" then path "scan" >>= dupes opts >>= printDupes
  else print args >> print opts
