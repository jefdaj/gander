module Gander.Cmd.Test where

import Gander.Lib

import Text.Pretty.Simple (pPrint)
import Gander.Config      (Config(..))

cmdTest :: Config -> FilePath -> IO ()
cmdTest cfg path = do
  putStrLn "loading config: "; pPrint cfg; putStrLn ""
  -- tree <- testSerialization cfg path
  tree <- readOrBuildTree (verbose cfg) (exclude cfg) path
  -- testDupes cfg tree
  testRm cfg tree

testSerialization :: Config -> FilePath -> IO HashTree
testSerialization cfg path = do
  tree1 <- readOrBuildTree (verbose cfg) (exclude cfg) path
  putStrLn "making hashtree:"; pPrint tree1
  putStrLn ""
  let str1  = serializeTree tree1
      tree2 = deserializeTree str1
      str2  = serializeTree tree2
  let tests = [tree1 == tree2, show tree1 == show tree1, str1 == str2]
  if (all id tests) then do
    putStrLn "round-tripped hashtree to string:"
    printHashes tree1
    putStrLn ""
    return tree1
  else do
    putStrLn "failed to round-trip hashtree to string!"
    print str1
    print str2
    putStrLn "failed to round-trip the tree!"
    return tree1

testDupes :: Config -> HashTree -> IO ()
testDupes _ tree = do
  let m  = pathsByHash tree
      ds = simplifyDupes $ map snd $ dupesByNFiles m
  putStrLn "making dupemap from hashtree:"   ; pPrint m
  putStrLn ""
  putStrLn "using dupemap to report duplicates:"; printDupes ds

testRm :: Config -> HashTree -> IO ()
testRm _ tree = case rmSubTree tree "./demo/backup" of
  Just t -> do
    putStrLn "before rmSubTree:"
    printHashes tree
    putStrLn "after rmSubTree:"
    printHashes t
  Nothing -> putStrLn "failed to rmSubTree"
