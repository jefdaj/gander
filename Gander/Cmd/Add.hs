module Gander.Cmd.Add where

-- TODO also annex add it, and rename back to add... or annex? import?
-- TODO add rsync to nix dependencies

import Control.Monad (when)
import Gander.Config (Config(..))
import Gander.Lib    (diff, printDiffs, buildTree, rsync, annexAdd, renameRoot)
import Gander.Cmd.Hash (cmdHash)
import System.FilePath ((</>))
import Data.Maybe (fromJust)

-- import Debug.Trace

cmdAdd :: Config -> FilePath -> Maybe FilePath -> IO ()
cmdAdd cfg dst mSrc = do
  let aPath = fromJust $ annex cfg
      dst'  = aPath </> "unsorted" </> dst
  -- annexed <- inAnnex $ trace ("dst': " ++ dst') dst'
  -- when (not annexed) (error "destination should be in a git-annex repo") -- TODO remove?
  case mSrc of
    Nothing -> return () -- files should have been manually added to the annex
    Just s -> do
      _ <- rsync (verbose cfg) s dst' -- TODO control verbosity
      before  <- fmap (renameRoot "old") $ buildTree (verbose cfg) (exclude cfg) s
      after   <- fmap (renameRoot "new") $ buildTree (verbose cfg) (exclude cfg) dst'
      let missing = diff before after
      when (not $ null missing) $ do
        _ <- error $ "final files differ from '" ++ s ++ "':"
        printDiffs missing
  annexAdd (verbose cfg) dst'
  cmdHash cfg aPath
