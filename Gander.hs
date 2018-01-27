{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO sort by how many links deduping would save: files per dupe * (dupes - 1)
-- TODO break into modules? Main, Types, Config, Scan, Dedup
-- TODO is git-annex an actual dep, or just recommended to go with it?
-- TODO figure out how to read files + compute hashes in parallel

import Gander.Types
import Gander.Lib.HashTree
import Gander.Lib.DupeMap

import qualified System.Directory.Tree as DT

import System.Environment    (getArgs)
import System.Console.Docopt (docoptFile, parseArgsOrExit,
                              getArgOrExitWith, isPresent, longOption,
                              shortOption, command, argument)

-----------
-- types --
-----------

{- Parsed command line args
 - TODO add other stuff from usage.txt, or revise that
 -}
-- TODO remove from non-Cmd modules
data Options = Options
  { verbose :: Bool
  , force   :: Bool
  , exclude :: [String]
  }
  deriving (Read, Show)

----------------
-- repl tests --
----------------

-- TODO rewrite with new hashTree
-- roundTripTree :: Bool -> FilePath -> IO Bool
-- roundTripTree beVerbose path = do
--   let opts = Options { verbose=beVerbose, force=False }
--   tree1 <- cmdHash opts path
--   let str1  = serialize   tree1
--       tree2 = deserializeTree str1
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
cmdHash opts path = do
  tree <- DT.readDirectoryWithL return path
  printHashes (verbose opts) $ excludeGlobs (exclude opts) tree

cmdDupes :: Options -> FilePath -> IO [DupeList]
cmdDupes _ path = do
  tree <- fmap deserializeTree $ readFile path
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
      short n = getArgOrExitWith ptns args $ shortOption n
      flag s l  = isPresent args (shortOption s)
               || isPresent args (longOption  l)
  excludeList <- if (flag 'e' "exclude")
                   then short 'e' >>= readFile >>= return . lines
                   else return [".git*"]
  let opts = Options
        { verbose = flag 'v' "verbose"
        , force   = flag 'f' "force"
        , exclude = excludeList
        }
  -- dispatch on command
  if      cmd "hash"  then path "path"   >>= cmdHash opts
  else if cmd "dupes" then path "hashes" >>= cmdDupes opts >>= printDupes
  else print args >> print opts
