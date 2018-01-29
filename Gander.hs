{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO sort by how many links deduping would save: files per dupe * (dupes - 1)
-- TODO break into modules? Main, Types, Config, Scan, Dedup
-- TODO is git-annex an actual dep, or just recommended to go with it?
-- TODO figure out how to read files + compute hashes in parallel

import Gander.Config         (Config(..))
import Gander.Cmd            (cmdHash, cmdDiff, cmdDupes, cmdTest,
                              cmdUpdate, cmdAnnex, cmdRm)
import System.Console.Docopt (docoptFile, parseArgsOrExit,
                              getArgOrExitWith, isPresent, longOption,
                              shortOption, command, argument)
import System.Environment    (getArgs)

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
  else if cmd "diff" then do
    old <- path "old"
    new <- path "new"
    cmdDiff cfg old new
  else if cmd "dupes" then path "hashes" >>= cmdDupes cfg
  else if cmd "test"  then path "path"   >>= cmdTest  cfg
  else if cmd "update" then do
    mainTree <- path "main"
    subTree  <- path "sub"
    subPath  <- path "path"
    cmdUpdate cfg mainTree subTree subPath
  else if cmd "annex" then do
    src  <- path "src"
    dest <- path "dest"
    cmdAnnex cfg src dest
  else if cmd "delete" then do
    hashes    <- path "root"
    delhashes <- path "sub"
    delpath   <- path "path"
    cmdRm cfg hashes delhashes delpath
  else print args >> print cfg
