{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO break into modules? Main, Types, Config, Scan, Dedup
-- TODO is git-annex an actual dep, or just recommended to go with it?

import qualified Data.ByteString.Lazy  as LB
import qualified System.Directory.Tree as DT

import Crypto.Hash                (Digest, SHA256, hashlazy)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either                (partitionEithers)
import Data.List                  (sort)
import System.Console.Docopt      (docoptFile, parseArgsOrExit,
                                   getArgOrExitWith, isPresent, longOption,
                                   shortOption, command, argument)
import System.Environment         (getArgs)
import System.FilePath            ((</>))

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
newtype Hash = Hash (Digest SHA256)
  deriving (Eq, Show)

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
  deriving (Show) -- TODO write Eq instance

{- A map from file/dir hash to a list of duplicate file paths.
 - Could be rewritten to contain links to HashTrees if that helps.
 -}
data PathsByHash = Map Hash [FilePath]
  deriving Show

---------------------------
-- read/write hash trees --
---------------------------

serialize :: HashTree -> String
serialize = unlines . serialize' ""

serialize' :: FilePath -> HashTree -> [String]
serialize' dir (File n (Hash h)   ) = [unwords [show h, dir </> n]]
serialize' dir (Dir  n (Hash h) cs)
  = map (dir </>) (concatMap (serialize' $ dir </> n) cs) -- recurse on contents
  ++ [unwords [show h, n]] -- finish with hash of entire dir

----------
-- scan --
----------

hashBytes :: LB.ByteString -> Digest SHA256
hashBytes = hashlazy

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
    txt = unlines $ sort $ map (\(Hash h) -> show h) hs

hashTree :: DT.DirTree Hash -> Either String HashTree
hashTree (DT.Failed n e ) = Left  $ n ++ " " ++ show e
hashTree (DT.File   n f ) = Right $ File n f
hashTree (DT.Dir    n ts) = case partitionEithers (map hashTree ts) of
  ([], trees) -> Right $ Dir n (hashHashes $ map hash trees) trees
  (errors, _) -> Left  $ unlines errors

-- TODO rename hashDir?
scan :: Options -> FilePath -> IO (DT.DirTree Hash)
scan opts path = do
  (_ DT.:/ tree) <- DT.readDirectoryWithL hashFile path
  return tree

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
  if cmd "scan"
    then path "path" >>= scan opts >>= putStrLn . show
    else print args >> print opts
