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
import Data.Map                   (Map)
import Data.Foldable              (toList)
import Crypto.Hash                (Digest, SHA256, hashlazy)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either                (partitionEithers)
import Data.List                  (sort, sortBy, partition)
import System.Console.Docopt      (docoptFile, parseArgsOrExit,
                                   getArgOrExitWith, isPresent, longOption,
                                   shortOption, command, argument)
import System.Environment         (getArgs)
import System.FilePath            ((</>), takeFileName, takeDirectory, splitPath)

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
  | Dir  { name :: FilePath, hash :: Hash, contents :: [HashTree] }
  deriving (Eq, Read, Show) -- TODO write Eq instance

-- TODO disable this while testing to ensure deep equality?
-- instance Eq HashTree where
--   t1 == t2 = hash t1 == hash t2

{- A map from file/dir hash to a list of duplicate file paths.
 - Could be rewritten to contain links to HashTrees if that helps.
 -}
type PathsByHash = Map Hash [FilePath]
  -- deriving (Read, Show)

-------------------------------------
-- serialize and deserialize trees --
-------------------------------------

-- TODO can Foldable or Traversable simplify these?

serialize :: HashTree -> String
serialize = unlines . serialize' ""

serialize' :: FilePath -> HashTree -> [String]
serialize' dir (File n (Hash h)   ) = [unwords [h, "file", dir </> n]]
serialize' dir (Dir  n (Hash h) cs)
  = concatMap (serialize' $ dir </> n) cs -- recurse on contents
  ++ [unwords [h, "dir ", dir </> n]] -- finish with hash of entire dir

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
deserialize :: String -> HashTree
deserialize = snd . head . foldr accTrees [] . map readLine . reverse . lines

{- This one is confusing! It accumulates a list of trees and their indent
 - levels, and when it comes across a dir it uses the indents to determine
 - which files are children to put inside it vs which are siblings.
 -}
accTrees :: (Hash, String, Int, FilePath) -> [(Int, HashTree)] -> [(Int, HashTree)]
accTrees l@(h, t, indent, p) cs = case t of
  "file" -> cs ++ [(indent, File p h)]
  "dir"  -> let (children, siblings) = partition (\(i, t) -> i > indent) cs
                dir = Dir p h $ map snd children
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

-- see https://stackoverflow.com/a/30537010
-- TODO this should match git-annex! maybe use its code directly?
hashFile :: FilePath -> IO Hash
hashFile path = do
  sha256sum <- fmap hashBytes $ LB.readFile path
  return $ Hash sha256sum

-- TODO remove the sort? not needed if tree order is reliable i suppose
hashHashes :: [Hash] -> Hash
hashHashes hs = Hash $ hashBytes $ pack txt
  where
    txt = unlines $ sort $ map (\(Hash h) -> h) hs

hashTree :: DT.DirTree Hash -> Either String HashTree
hashTree (DT.Failed n e ) = Left  $ n ++ " " ++ show e
hashTree (DT.File   n f ) = Right $ File n f
hashTree (DT.Dir    n ts) = case partitionEithers (map hashTree ts) of
  ([], trees) -> Right $ Dir n (hashHashes $ map hash trees) trees
  (errors, _) -> Left  $ unlines errors

-- TODO rename hashDir?
-- Note that you can't hash a folder while writing to a file inside it!
scan :: Options -> FilePath -> IO HashTree
scan opts path = do
  (_ DT.:/ tree) <- DT.readDirectoryWith hashFile path
  let tree' = hashTree tree
  case tree' of
    Left  e -> error e
    Right t -> return t

---------------------
-- build dupe maps --
---------------------

-- TODO can Foldable or Traversable simplify these?

pathsByHash :: HashTree -> PathsByHash
pathsByHash = Map.fromListWith (++) . pathsByHash' ""

pathsByHash' :: FilePath -> HashTree -> [(Hash, [FilePath])]
pathsByHash' dir (File n h   ) = [(h, [dir </> n])]
pathsByHash' dir (Dir  n h cs) = [(h, [dir </> n])]
                                 ++ concatMap (pathsByHash' $ dir </> n) cs

---------------
-- list dups --
---------------

-- TODO warning: so far it lists anything annexed as a dup

-- see https://mail.haskell.org/pipermail/beginners/2009-June/001867.html
sortDescLength :: [[FilePath]] -> [[FilePath]]
sortDescLength = map snd . sortBy (comparing $ negate . fst) . map (length &&& id)

dupesByNCopies :: PathsByHash -> [[FilePath]]
dupesByNCopies = sortDescLength . filter (\x -> length x > 1) . toList

printDupes :: [[FilePath]] -> IO ()
printDupes groups = mapM_ printGroup groups
  where
    printGroup paths = mapM_ putStrLn $ paths ++ [""]

dupes :: Options -> FilePath -> IO [[FilePath]]
dupes opts path = do
  -- tree <- scan opts path
  tree <- fmap deserialize $ readFile path
  -- putStrLn $ show tree
  let pbyh = pathsByHash tree
      pdup = dupesByNCopies pbyh
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

mapTree :: FilePath -> IO PathsByHash
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
  let cmd  name = isPresent args $ command name
      path name = getArgOrExitWith ptns args $ argument name
      flag s l  = isPresent args (shortOption s)
               || isPresent args (longOption  l)
      opts = Options
        { verbose = flag 'v' "verbose"
        , confirm = flag 'c' "confirm"
        , force   = flag 'f' "force"
        }
  -- dispatch on command
  if cmd "scan" then path "path" >>= scan opts >>= putStr . serialize
  else if cmd "dupes" then path "scan" >>= dupes opts >>= printDupes
  else print args >> print opts
