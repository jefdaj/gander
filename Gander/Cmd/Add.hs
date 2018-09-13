module Gander.Cmd.Add where

-- TODO guess and check hashes
-- TODO add rsync to nix dependencies

import Gander.Lib
import Gander.Config   (Config(..))
-- import Gander.Cmd.Hash (updateAnnexHashes)
import Gander.Run      (runDeltas, runGitCommit, runRsync)
-- import Gander.Run      (runGitAnnexAdd, runGitCommit, runRsync)

import Control.Monad   (when)
import Data.Maybe      (fromJust)
import System.FilePath ((</>))

-- TODO easier to split into two completely different cases? add or rsync+add
-- both start by assembling an Add
-- then apply it (consider rsync + apply it here later)
-- then calculate after hashes
-- if check flag, check those against actual hashes
-- write after hashes

cmdAdd :: Config -> FilePath -> Maybe FilePath -> IO ()
cmdAdd cfg dst mSrc = do
  let aPath = fromJust $ annex cfg
      dst'  = aPath </> "unsorted" </> dst
  dstTree <- case mSrc of
    Nothing -> buildTree (verbose cfg) (exclude cfg) dst
    Just s  -> rsyncAndHash cfg s dst'
  let ds  = [Add dst' dstTree]
      msg = unwords ["add", dst]
  safeRunDeltas cfg ds msg

  -- annexed <- inAnnex $ trace ("dst': " ++ dst') dst'
  -- when (not annexed) (error "destination should be in a git-annex repo") -- TODO remove?
  -- old version, which totally works but has to re-hash:
  -- msgDst <- case mSrc of
    -- Nothing -> return dst -- files should have been manually added to the annex
    -- Just s -> do
      -- _ <- runRsync cfg s dst' -- TODO control verbosity
      -- before  <- buildTree (verbose cfg) (exclude cfg) s
      -- after   <- buildTree (verbose cfg) (exclude cfg) dst'
      -- let missing = diff before after
      -- when (not $ null missing) $ do
        -- putStrLn $ "error! final files differ from '" ++ s ++ "':"
        -- printDeltas missing
      -- return s
  -- runGitAnnexAdd cfg dst'
  -- new <- buildTree (verbose cfg) (exclude cfg) aPath
  -- updateAnnexHashes cfg new

-- TODO move to Run.hs
-- TODO extend to run list of deltas
safeRunDeltas :: Config -> [Delta] -> String -> IO ()
safeRunDeltas cfg deltas msg = do
  let aPath  = fromJust $ annex cfg
      hashes = aPath </> "hashes.txt"
  before <- readTree hashes
  case simDeltas before deltas of
    Nothing -> undefined -- use Either and print error here
    Just expected -> do
      runDeltas cfg aPath deltas
      when (check cfg) $ do
        actual <- buildTree (verbose cfg) (exclude cfg) aPath
        assertSameTrees aPath expected actual
        writeFile hashes $ serializeTree expected
      -- TODO should gitCommit be part of runDeltas?
      -- TODO sanitize dst for commit message
      runGitCommit cfg aPath msg

rsyncAndHash :: Config -> FilePath -> FilePath -> IO HashTree
rsyncAndHash cfg s dst' = do
  _ <- runRsync cfg s dst' -- TODO control verbosity
  before <- buildTree (verbose cfg) (exclude cfg) s
  after  <- buildTree (verbose cfg) (exclude cfg) dst'
  assertSameTrees s before after
  return after

-- TODO move to HashTree.hs
assertSameTrees :: FilePath -> HashTree -> HashTree -> IO ()
assertSameTrees path before after = do
  let missing = diff before after
  when (not $ null missing) $ do
    putStrLn $ "error! final files differ from '" ++ path ++ "':"
    printDeltas missing
