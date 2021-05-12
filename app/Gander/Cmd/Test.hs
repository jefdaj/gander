module Gander.Cmd.Test where

import Data.Gander
import Prelude hiding (log)

import Text.Pretty.Simple (pPrint)

-- TODO remove because this shouldn't depend on the app target?
import Gander.Config      (Config(..), log)

import qualified Data.ByteString.Char8 as B

cmdTest :: Config -> FilePath -> IO ()
cmdTest cfg path = do
  putStrLn "loading config: "; pPrint cfg; putStrLn ""
  tree <- readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) path
  testSerialization cfg tree
  testDupes cfg tree
  -- testRm cfg tree

explain :: String -> IO () -> IO ()
explain msg fn = putStrLn msg >> fn >> putStrLn ""

testSerialization :: Config -> HashTree -> IO ()
testSerialization cfg tree1 = do
  explain "making hashtree:" $ pPrint tree1
  let str1  = B.unlines $ serializeTree tree1
      tree2 = deserializeTree (maxdepth cfg) str1
      str2  = B.unlines $ serializeTree tree2
  let tests = [tree1 == tree2, show tree1 == show tree1, str1 == str2]
  if (all id tests) then do
    explain "round-tripped hashtree to string:" $ printTree tree1
  else do
    putStrLn "failed to round-trip hashtree to string!"
    print str1
    print str2
    putStrLn "failed to round-trip the tree!"

testDupes :: Config -> HashTree -> IO ()
testDupes cfg tree = do
  let ds = dupesByNFiles $ pathsByHash tree
  -- explain "making dupemap from hashtree:" $ pPrint m
  explain "using dupemap to report duplicates:" $ printDupes (maxdepth cfg) ds

testRm :: Config -> HashTree -> IO ()
testRm cfg tree = case rmSubTree tree "./demo/backup" of
  Left e -> log cfg e
  Right t -> do
    explain "before rmSubTree:" $ printTree tree
    explain "after  rmSubTree:" $ printTree t
