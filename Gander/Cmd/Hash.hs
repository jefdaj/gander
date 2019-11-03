module Gander.Cmd.Hash where

-- TODO guess and check hashes

import Gander.Data
import Gander.Config (Config(..))
import Gander.Util   (log)
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
  Nothing -> buildTree (verbose cfg) (exclude cfg) target >>= printTree
  Just dir -> do
    new <- buildTree (verbose cfg) (exclude cfg) target
    updateAnnexHashes cfg new
    runGitCommit cfg dir "gander hash" -- TODO only if hashes changed
    -- out2 <- runGit dir ["commit", "-m", "gander hash"]

updateAnnexHashes :: Config -> HashTree -> IO ()
updateAnnexHashes cfg new = do
  let aPath  = fromJust $ annex cfg
      hashes = aPath </> "hashes.txt"
  log cfg "updating hashes.txt"
  exists <- doesFileExist hashes
  when exists $ do -- TODO only when verbose?
    old <- readTree hashes
    printDeltas $ diff old new -- just nice to verify this looks right
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
