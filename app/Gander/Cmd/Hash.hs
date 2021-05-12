module Gander.Cmd.Hash where

-- TODO guess and check hashes

import Data.Gander
import Gander.Config (Config(..), log)
import Gander.Run    (runGit, runGitCommit)

import Prelude hiding (log)

import Control.Monad    (when)
import Data.Maybe       (fromJust)
import System.Directory (doesFileExist)
import System.FilePath  ((</>))

-- the maybe filepath controls standalone (print hashes)
-- vs annex mode (write to the filepath)...
-- TODO is there a better way to set that up?
-- TODO require that the path be either absolute + in the annex or relative and in the annex
-- this works, but add doesn't. so what changed?
cmdHash :: Config -> FilePath -> IO ()
cmdHash cfg target = case annex cfg of
  Nothing -> do
    t <- buildTree (verbose cfg) (exclude cfg) target
    case txt cfg of
      Nothing -> printTree t
      Just p  -> writeTree p t
    case bin cfg of
      Nothing -> return ()
      Just p -> writeBinTree p t
  Just dir -> do
    new <- buildTree (verbose cfg) (exclude cfg) target
    updateAnnexHashes cfg new
    runGitCommit cfg dir "gander hash" -- TODO only if hashes changed
    -- out2 <- runGit dir ["commit", "-m", "gander hash"]

updateAnnexHashes :: Config -> HashTree -> IO ()
updateAnnexHashes cfg new = do
  let aPath   = fromJust $ annex cfg
      hashes  = aPath </> "hashes.txt"
      bHashes = aPath </> "hashes.bin"
  log cfg "updating hashes.txt"
  exists <- doesFileExist hashes
  when exists $ do -- TODO only when verbose?
    old <- readTree (maxdepth cfg) hashes
    printDeltas $ diff old new -- just nice to verify this looks right
  -- B.writeFile hashes $ serializeTree new
  writeTree hashes new
  -- TODO listen to config here? or always do it?
  writeBinTree bHashes new
  out1 <- runGit aPath ["add", "hashes.txt"]
  log cfg out1
  out2 <- runGit aPath ["add", "hashes.bin"]
  log cfg out2

guardStatus :: Config -> FilePath -> IO ()
guardStatus = undefined
  -- TODO check that git status is all clear
  -- TODO check that git-annex status is all clear too?

guardHash :: Config -> Maybe FilePath -> FilePath -> IO ()
guardHash = undefined
  -- TODO run guardInit here?
  -- TODO run guardStatus here?
  -- TODO check that hashes.txt exists
