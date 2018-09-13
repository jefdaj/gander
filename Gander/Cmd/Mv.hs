module Gander.Cmd.Mv where

-- TODO next: fix relative paths thing, write a nice lost files warning, fix any last bugs... then good :D
-- TODO oh, write a couple other messages if it would help brian. lost files should be mentioned even when 0!

import Gander.Lib
import Gander.Config (Config(..))
import Gander.Run    (runGitMv, runGitCommit)

import Control.Monad    (when)
import Data.Maybe       (fromJust)
import System.Directory (doesPathExist)
import System.FilePath  ((</>))

-- import qualified Data.ByteString as B

-- TODO what if they mean to move something *inside* something that exists already? that's ok but confusing here
-- TODO list files with no duplicates when confirming
-- TODO aha! ok to be missing folder hashes, just not files
cmdMv :: Config -> FilePath -> FilePath -> IO ()
cmdMv cfg src dst = do -- TODO correct toRm path using root!
  let aPath  = fromJust $ annex cfg
      hashes = aPath </> "hashes.txt" -- TODO check that this exists
      src'   = aPath </> src -- TODO add sorted?
      dst'   = aPath </> dst -- TODO add unsorted?
  before  <- readTree hashes
  seemsOk <- okToInsert cfg before dst'
  when (seemsOk || force cfg) $ do
    -- TODO write new hashes.txt here!
    runGitMv     cfg aPath src' dst' -- TODO check exit code
    runGitCommit cfg aPath "gander mv" -- TODO check exit code

-- TODO should this go in HashTree.hs?
okToInsert :: Config -> HashTree -> FilePath -> IO Bool
okToInsert cfg tree path = do
  let path' = (fromJust $ annex cfg) </> path
  exists <- doesPathExist path'
  if treeContainsPath tree path || exists
    then do
      putStrLn $ "path already exists: '" ++ path ++ "'"
      return False
    else return True
