module Gander.Cmd.Test where

import Gander.Data
import Prelude hiding (log)

import Gander.Util        (log)
import Text.Pretty.Simple (pPrint)
import Gander.Config      (Config(..))

cmdTest :: Config -> FilePath -> IO ()
cmdTest cfg path = do
  putStrLn "loading config: "; pPrint cfg; putStrLn ""
  tree <- readOrBuildTree (verbose cfg) (exclude cfg) path
  testSerialization cfg tree
  testDupes cfg tree
  -- testRm cfg tree

explain :: String -> IO () -> IO ()
explain msg fn = putStrLn msg >> fn >> putStrLn ""

testSerialization :: Config -> HashTree -> IO ()
testSerialization _ tree1 = do
  explain "making hashtree:" $ pPrint tree1
  let str1  = serializeTree tree1
      tree2 = deserializeTree str1
      str2  = serializeTree tree2
  let tests = [tree1 == tree2, show tree1 == show tree1, str1 == str2]
  if (all id tests) then do
    explain "round-tripped hashtree to string:" $ printTree tree1
  else do
    putStrLn "failed to round-trip hashtree to string!"
    print str1
    print str2
    putStrLn "failed to round-trip the tree!"

testDupes :: Config -> HashTree -> IO ()
testDupes _ tree = do
  let m  = pathsByHash tree
      ds = simplifyDupes $ dupesByNFiles m
  explain "making dupemap from hashtree:" $ pPrint m
  explain "using dupemap to report duplicates:" $ printDupes ds

testRm :: Config -> HashTree -> IO ()
testRm cfg tree = case rmSubTree tree "./demo/backup" of
  Left e -> log cfg e
  Right t -> do
    explain "before rmSubTree:" $ printTree tree
    explain "after  rmSubTree:" $ printTree t
