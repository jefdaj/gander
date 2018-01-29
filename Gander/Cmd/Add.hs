module Gander.Cmd.Add where

-- TODO rename to copy
-- TODO add rsync to nix dependencies

import Control.Monad  (when)
import Gander.Config  (Config(..))
import Gander.Lib     (HashTree(..), diff, printDiffs, buildTree)
import System.Process (readProcess)

noSlash :: FilePath -> FilePath
noSlash = reverse . dropWhile (== '/') . reverse

cmdAdd :: Config -> FilePath -> FilePath -> IO ()
cmdAdd cfg src dest = do
  out <- readProcess "rsync" ["-aErvz", "--delete", noSlash src ++ "/", noSlash dest] ""
  mapM_ putStrLn $ lines out
  before <- buildTree (verbose cfg) (exclude cfg) src
  after  <- buildTree (verbose cfg) (exclude cfg) dest
  -- diff contents rather than top-level trees because the name probably changed
  let diffs = concatMap (\(a,b) -> diff a b) $ zip (contents before) (contents after)
  when (not $ null diffs) $ do
    printDiffs diffs
    error "something went wrong during copy!"
