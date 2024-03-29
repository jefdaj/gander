{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO sort by how many links deduping would save: files per dupe * (dupes - 1)
-- TODO is git-annex an actual dep, or just recommended to go with it?
-- TODO figure out how to read files + compute hashes in parallel

import Gander.Cmd
import Util           (absolutize)
import Gander.Config         (Config(..), defaultConfig)
import System.Console.Docopt (docoptFile, parseArgsOrExit,
                              getArgOrExitWith, isPresent, getArg, getAllArgs,
                              shortOption, command, argument)
import System.Environment    (getArgs)
import System.FilePath       ((</>))
import System.FilePath.Glob (compile)

import System.Environment (setEnv)
import System.Locale.SetLocale

main :: IO ()
main = do

  -- TODO which is/are really needed?
  setEnv "LANG" "en_US.UTF-8"
  _ <- setLocale LC_ALL $ Just "en_US.UTF-8"

  let ptns = [docoptFile|app/usage.txt|]
  args <- parseArgsOrExit ptns =<< getArgs
  let cmd   n = isPresent args $ command n
      arg   n = getArgOrExitWith ptns args $ argument n
      lst   n = getAllArgs args $ argument n
      short n = getArgOrExitWith ptns args $ shortOption n
      flag  n = isPresent args $ shortOption n
  eList <- if (flag 'e')
             then short 'e' >>= readFile >>= return . map compile . lines
             else return $ exclude defaultConfig
  let cfg = Config
        { annex    = getArg args $ argument "annex"
        , bin      = getArg args $ shortOption 'b'
        , txt      = getArg args $ shortOption 't'
        , maxdepth = fmap (read :: String -> Int) $ getArg args $ shortOption 'm'
        , verbose  = flag 'v'
        , force    = flag 'f'
        , check    = flag 'c'
        , exclude  = eList
        }
  -- print cfg
  case annex cfg of

    -- annex-aware mode
    Just a -> do
      maPath <- absolutize a
      case maPath of
        Nothing -> do
          print args
          print cfg
        Just aPath -> do
          let hashes = aPath </> "hashes.txt"
          if      cmd "init"  then cmdInit  cfg aPath
          else if cmd "hash"  then cmdHash  cfg [aPath]
          else if cmd "dupes" then cmdDupes cfg [aPath]
          else if cmd "add" then do
            dst <- arg "dst"
            let src = getArg args $ argument "src"
            cmdAdd cfg dst src
          else if cmd "mv" then do
            src <- arg "src"
            dst <- arg "dst"
            cmdMv cfg src dst
          else if cmd "rm" then do
            rmPath <- arg "rmPath"
            cmdRm cfg hashes aPath rmPath
          else if cmd "dedup" then cmdDedup cfg
          else do
            print args
            print cfg

    -- standalone mode (note: still works on annexed files)
    Nothing -> do
      if cmd "hash" then do
        let paths = lst "path"
        cmdHash cfg paths
      else if cmd "diff" then do
        old <- arg "old"
        new <- arg "new"
        cmdDiff cfg old new
      else if cmd "dupes" then do
        let hashes = lst "hashes"
        cmdDupes cfg hashes
      else if cmd "test"  then do
        let paths = lst "path"
        cmdTest cfg paths
      else if cmd "update" then do
        mainTree <- arg "main"
        subTree  <- arg "sub"
        subPath  <- arg "path"
        cmdUpdate cfg mainTree subTree subPath
      else if cmd "rm" then do
        target <- arg "target"
        rPath  <- arg "rootpath"
        dPath  <- arg "rmpath"
        cmdRm cfg target rPath dPath
      else do
        print args
        print cfg
