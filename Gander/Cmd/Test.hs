module Gander.Cmd.Test where

import Gander.Lib

import Text.Pretty.Simple (pPrint)
import Control.Monad      (when)
import Gander.Config      (Config(..))

cmdTest :: Config -> FilePath -> IO ()
cmdTest cfg path = do
  putStrLn "loading config: "; pPrint cfg; putStrLn ""
  tree <- testSerialization cfg path
  testDupes cfg tree

testSerialization :: Config -> FilePath -> IO HashTree
testSerialization cfg path = do
  tree1 <- buildTree (verbose cfg) (exclude cfg) path
  putStrLn "making hashtree:"; pPrint tree1
  putStrLn ""
  let str1  = serializeTree tree1
      tree2 = deserializeTree str1
      str2  = serializeTree tree2
  let tests = [tree1 == tree2, show tree1 == show tree1, str1 == str2]
  when (not $ all id tests) (error "failed to round-trip the tree!")
  putStrLn "round-tripping hashtree to string:"
  printHashes tree1
  putStrLn ""
  return tree1

testDupes :: Config -> HashTree -> IO ()
testDupes _ tree = do
  let m  = pathsByHash tree
      ds = simplifyDupes $ dupesByNFiles m
  putStrLn "making dupemap from hashtree:"   ; pPrint m
  putStrLn ""
  putStrLn "using dupemap to report duplicates:"; printDupes ds
