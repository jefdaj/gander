module Gander.Cmd.Annex where

-- TODO also annex add it, and rename back to add... or annex? import?
-- TODO add rsync to nix dependencies

import Control.Monad (when)
import Gander.Config (Config(..))
import Gander.Lib    (HashTree(..), diff, printDiffs, buildTree, rsync,
                      inAnnex, annexAdd, absolutize)

cmdAnnex :: Config -> FilePath -> FilePath -> IO ()
cmdAnnex cfg src dest = do
  dest' <- absolutize dest
  annexed <- inAnnex dest'
  when (not annexed) (error "destination should be in a git-annex repo")
  _ <- rsync (verbose cfg) src dest' -- TODO control verbosity
  _ <- annexAdd (verbose cfg) dest'
  before <- buildTree (verbose cfg) (exclude cfg) src
  after  <- buildTree (verbose cfg) (exclude cfg) dest'
  -- diff contents rather than top-level trees because the name probably changed
  when ((length $ contents before) /= (length $ contents after))
    (error "something went wrong during copy!")
  let diffs = concatMap (\(a,b) -> diff a b)
            $ zip (contents before) (contents after)
  when (not $ null diffs) $ do
    printDiffs diffs
    error "something went wrong during copy!"
