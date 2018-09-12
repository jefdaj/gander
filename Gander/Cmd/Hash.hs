module Gander.Cmd.Hash where

import Gander.Lib
import Gander.Config (Config(..))

import Prelude hiding (log)

import Data.Maybe       (fromJust)
import Control.Monad    (when)
import System.Directory (doesFileExist)
import System.FilePath  ((</>))

log :: Config -> String -> IO ()
log cfg msg = when (verbose cfg) (putStrLn msg)

-- the maybe filepath controls standalone (print hashes)
-- vs annex mode (write to the filepath)...
-- TODO is there a better way to set that up?
cmdHash :: Config -> FilePath -> IO ()
cmdHash cfg target = do
  new <- buildTree (verbose cfg) (exclude cfg) target
  case annex cfg of
    Nothing -> printHashes new
    Just dir -> do
      updateAnnexHashes cfg new
      gitCommit (verbose cfg) dir "gander hash"
      -- out2 <- runGit dir ["commit", "-m", "gander hash"]

updateAnnexHashes :: Config -> HashTree -> IO ()
updateAnnexHashes cfg new = do
  let aPath  = fromJust $ annex cfg
      hashes = aPath </> "hashes.txt"
  log cfg "updating hashes.txt"
  exists <- doesFileExist hashes
  when exists $ do -- TODO only when verbose?
    old <- readTree hashes
    printDiffs $ diff old new -- just nice to verify this looks right
  writeFile hashes $ serializeTree new
  out <- runGit aPath ["add", "hashes.txt"]
  log cfg out

guardStatus :: Config -> FilePath -> IO ()
guardStatus = undefined
  -- TODO check that git status is all clear
  -- TODO check that git-annex status is all clear too?

guardHash :: Config -> Maybe FilePath -> FilePath -> IO ()
guardHash = undefined
  -- TODO run guardInit here?
  -- TODO run guardStatus here?
  -- TODO check that hashes.txt exists
