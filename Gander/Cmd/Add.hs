module Gander.Cmd.Add where

-- TODO add rsync to nix dependencies

import Gander.Lib
import Gander.Config   (Config(..))
import Gander.Cmd.Hash (updateAnnexHashes)
import Gander.Run      (runGitAnnexAdd, runGitCommit, runRsync)

import Control.Monad   (when)
import Data.Maybe      (fromJust)
import System.FilePath ((</>), takeFileName)

cmdAdd :: Config -> FilePath -> Maybe FilePath -> IO ()
cmdAdd cfg dst mSrc = do
  let aPath  = fromJust $ annex cfg
      dst'   = aPath </> "unsorted" </> dst
  -- annexed <- inAnnex $ trace ("dst': " ++ dst') dst'
  -- when (not annexed) (error "destination should be in a git-annex repo") -- TODO remove?
  msgDst <- case mSrc of
    Nothing -> return dst -- files should have been manually added to the annex
    Just s -> do
      _ <- runRsync cfg s dst' -- TODO control verbosity
      before  <- buildTree (verbose cfg) (exclude cfg) s
      after   <- buildTree (verbose cfg) (exclude cfg) dst'
      let missing = diff before after
      when (not $ null missing) $ do
        putStrLn $ "error! final files differ from '" ++ s ++ "':"
        printDiffs missing
      return s
  runGitAnnexAdd cfg dst'
  new <- buildTree (verbose cfg) (exclude cfg) aPath
  updateAnnexHashes cfg new
  let msg = unwords ["add", takeFileName msgDst]
  runGitCommit cfg aPath msg -- TODO sanitize the commit msg!
