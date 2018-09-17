module Gander.Cmd.Add where

-- TODO add rsync to nix dependencies

import Gander.Lib
import Gander.Config (Config(..))
import Gander.Run    (safeRunDeltas, runRsync)

import Data.Maybe      (fromJust)
import System.FilePath ((</>))

cmdAdd :: Config -> FilePath -> Maybe FilePath -> IO ()
cmdAdd cfg dst mSrc = do
  let aPath = fromJust $ annex cfg
      dst'  = aPath </> "unsorted" </> dst
  dstTree <- case mSrc of
    Nothing -> buildTree (verbose cfg) (exclude cfg) dst
    Just s  -> rsyncAndHash cfg s dst'
  let ds  = [Add dst' dstTree]
      msg = unwords ["add", dst] -- TODO sanitize!
  safeRunDeltas cfg ds msg

rsyncAndHash :: Config -> FilePath -> FilePath -> IO HashTree
rsyncAndHash cfg s dst' = do
  _ <- runRsync cfg s dst' -- TODO control verbosity
  before <- buildTree (verbose cfg) (exclude cfg) s
  after  <- buildTree (verbose cfg) (exclude cfg) dst'
  assertSameTrees s before after
  return after
