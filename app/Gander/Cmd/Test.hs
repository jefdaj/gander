module Gander.Cmd.Test where

import Data.Gander
import Prelude hiding (log)

import Text.Pretty.Simple (pPrint)

-- TODO remove because this shouldn't depend on the app target?
import Gander.Config      (Config(..), log)

import qualified Data.ByteString.Char8 as B

cmdTest :: Config -> [FilePath] -> IO ()
cmdTest cfg paths = do
  putStrLn "loading config: "; pPrint cfg; putStrLn ""
  forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  -- let forest = HashForest trees
  testSerialization cfg forest
  testDupes cfg forest
  -- testRm cfg forest

explain :: String -> IO () -> IO ()
explain msg fn = putStrLn msg >> fn >> putStrLn ""

testSerialization :: Config -> HashForest () -> IO ()
testSerialization cfg forest1 = do
  explain "making hashforest:" $ pPrint forest1
  let string1 = B.unlines $ serializeForest forest1
      forest2 = deserializeForest (maxdepth cfg) string1
      string2 = B.unlines $ serializeForest forest2
  let tests = [forest1 == forest2, show forest1 == show forest2, string1 == string2]
  if (all id tests) then do
    explain "round-tripped hashforest to string:" $ printForest forest1
  else do
    putStrLn "failed to round-trip hashforest to string!"
    print string1
    print string2
    putStrLn "failed to round-trip the tree!"

testDupes :: Config -> HashForest () -> IO ()
testDupes cfg forest = do
  let ds = dupesByNFiles $ pathsByHash forest
  -- explain "making dupemap from hashforest:" $ pPrint m
  explain "using dupemap to report duplicates:" $ printDupes (maxdepth cfg) ds

testRm :: Config -> HashTree () -> IO ()
testRm cfg tree = case rmSubTree tree "./demo/backup" of
  Left e -> log cfg e
  Right t -> do
    explain "before rmSubTree:" $ printTree tree
    explain "after  rmSubTree:" $ printTree t
