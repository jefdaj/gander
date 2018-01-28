module Gander.Cmd.Test where

import Gander.Config (Config(..))
import Gander.Lib (buildTree, printHashes, serializeTree, deserializeTree,
                   pathsByHash)

cmdTest :: Config -> FilePath -> IO ()
cmdTest cfg path = testSerialization cfg path >> testDupes cfg path

testSerialization :: Config -> FilePath -> IO ()
testSerialization cfg path = do
  tree1 <- buildTree (verbose cfg) (exclude cfg) path
  printHashes tree1
  let str1  = serializeTree tree1
      tree2 = deserializeTree str1
      str2  = serializeTree tree2
  putStrLn $ "tree1 == tree2? " ++ show (tree1 == tree2)
  putStrLn $ "show tree1 == show tree2? " ++ show (show tree1 == show tree2)
  putStrLn $ "str1 == str2? "   ++ show (str1  == str2)

testDupes :: Config -> FilePath -> IO ()
testDupes cfg path = do
  tree <- buildTree (verbose cfg) (exclude cfg) path
  let m = pathsByHash tree
  print m
