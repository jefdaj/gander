{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO sort by how many links deduping would save: files per dupe * (dupes - 1)
-- TODO is git-annex an actual dep, or just recommended to go with it?
-- TODO figure out how to read files + compute hashes in parallel

import Gander.Cmd
import Gander.Config         (Config(..))
import System.Console.Docopt (docoptFile, parseArgsOrExit,
                              getArgOrExitWith, isPresent, longOption,
                              shortOption, command, argument)
import System.Environment    (getArgs)

main :: IO ()
main = do
  let ptns = [docoptFile|usage.txt|]
  args <- parseArgsOrExit ptns =<< getArgs
  let cmd    n = isPresent args $ command n
      arg    n = getArgOrExitWith ptns args $ argument n
      short  n = getArgOrExitWith ptns args $ shortOption n
      flag s l = isPresent args (shortOption s)
              || isPresent args (longOption  l)
  eList <- if (flag 'e' "exclude")
             then short 'e' >>= readFile >>= return . lines
             else return [".git*"]
  let cfg = Config
        { verbose = flag 'v' "verbose"
        , force   = flag 'f' "force"
        , exclude = eList
        }
  if cmd "hash" then do
    path <- arg "path"
    cmdHash cfg path
  else if cmd "diff" then do
    old <- arg "old"
    new <- arg "new"
    cmdDiff cfg old new
  else if cmd "dupes" then do
    hashes <- arg "hashes"
    cmdDupes cfg hashes
  else if cmd "test"  then do
    path <- arg "path"
    cmdTest cfg path
  else if cmd "update" then do
    mainTree <- arg "main"
    subTree  <- arg "sub"
    subPath  <- arg "path"
    cmdUpdate cfg mainTree subTree subPath
  else if cmd "annex" then do
    src  <- arg "src"
    dest <- arg "dest"
    cmdAnnex cfg src dest
  else if cmd "rm" then do
    target <- arg "target"
    rPath  <- arg "rootpath"
    dPath  <- arg "rmpath"
    cmdRm cfg target rPath dPath
  else if cmd "tmprm" then do
    rmPath <- arg "rmpath"
    cmdTmpRm cfg rmPath
  else if cmd "dedup" then do
    hashes <- arg "hashes"
    dpath  <- arg "path"
    cmdDedup cfg hashes dpath
  else do
    print args
    print cfg
