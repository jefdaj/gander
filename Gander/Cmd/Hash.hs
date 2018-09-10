module Gander.Cmd.Hash where

import Gander.Config    (Config(..))
import Gander.Lib       (buildTree, printHashes, readTree, serializeTree, diff, printDiffs)
import System.Directory (doesFileExist)

cmdHash :: Config -> Maybe FilePath -> FilePath -> IO ()
cmdHash cfg mHashes target = do
  -- TODO test that the target exists
  tree <- buildTree (verbose cfg) (exclude cfg) target
  case mHashes of
    Nothing -> printHashes tree -- standalone command
    Just h  -> do
      exists <- doesFileExist h
      if exists then do
        putStrLn $ "updating '" ++ h ++ "'..."
        before <- readTree h
        printDiffs $ diff before tree
      else do
        putStrLn $ "creating '" ++ h ++ "'..."
      writeFile h $ serializeTree tree
      putStrLn "done"

guardStatus :: Config -> FilePath -> IO ()
guardStatus = undefined
  -- TODO check that git status is all clear
  -- TODO check that git-annex status is all clear too?

guardHash :: Config -> Maybe FilePath -> FilePath -> IO ()
guardHash = undefined
  -- TODO run guardInit here?
  -- TODO run guardStatus here?
  -- TODO check that hashes.txt exists
