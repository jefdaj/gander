module Gander.Cmd.Add where

-- TODO add rsync to nix dependencies
-- TODO is the dst path being duplicated a bit? like unsorted/u1/u1?

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
    Just s  -> rsyncAndHash cfg s dst' -- TODO aha! safeRunDeltas doesn't take this into account
    -- is there any need to --check when adding anyway? maybe need some extra logic for it
  let ds  = [Add dst' dstTree]
      msg = unwords ["add", dst] -- TODO sanitize!
  safeRunDeltas cfg ds msg

rsyncAndHash :: Config -> FilePath -> FilePath -> IO HashTree
rsyncAndHash cfg s dst' = do
  _ <- runRsync cfg s dst' -- TODO control verbosity
  before <- buildTree (verbose cfg) (exclude cfg) s
  after  <- buildTree (verbose cfg) (exclude cfg) dst'
  assertSameTrees ("original files ('" ++ s    ++ "')", before)
                  ("annexed  files ('" ++ dst' ++ "')", after)
  return after
