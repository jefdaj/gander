{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO sort by how many links deduping would save: files per dupe * (dupes - 1)
-- TODO break into modules? Main, Types, Config, Scan, Dedup
-- TODO is git-annex an actual dep, or just recommended to go with it?
-- TODO figure out how to read files + compute hashes in parallel

import Gander.Config    (Config(..))
import Gander.Cmd.Hash  (cmdHash)
import Gander.Cmd.Dupes (cmdDupes)

import System.Environment    (getArgs)
import System.Console.Docopt (docoptFile, parseArgsOrExit,
                              getArgOrExitWith, isPresent, longOption,
                              shortOption, command, argument)

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
  let cfg = Config
        { verbose = flag 'v' "verbose"
        , force   = flag 'f' "force"
        , exclude = excludeList
        }
  -- dispatch on command
  if      cmd "hash"  then path "path"   >>= cmdHash  cfg
  else if cmd "dupes" then path "hashes" >>= cmdDupes cfg
  else print args >> print cfg

-- TODO rewrite with new hashTree
-- roundTripTree :: Bool -> FilePath -> IO Bool
-- roundTripTree beVerbose path = do
--   let cfg = Config { verbose=beVerbose, force=False }
--   tree1 <- cmdHash cfg path
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
--   let cfg = Config { verbose=True, force=False }
--   tree <- cmdHash cfg path
--   let m = pathsByHash tree
--   return m
