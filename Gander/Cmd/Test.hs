module Gander.Cmd.Test
  ( cmdTest
  , roundTripTree
  )
  where

import Gander.Config (Config(..))
import Gander.Lib (buildTree, printHashes)

cmdTest :: Config -> FilePath -> IO ()
cmdTest = roundTripTree

roundTripTree :: Config -> FilePath -> IO ()
roundTripTree cfg path = do
  tree1 <- buildTree (verbose cfg) (exclude cfg) path
  printHashes tree1
  -- print $ flattenTree tree1

-- TODO rewrite with new hashTree
-- roundTripTree :: Bool -> FilePath -> IO Bool
-- roundTripTree beVerbose path = do
--   let cfg = Config { verbose=beVerbose, force=False }
--   tree1 <- cmdHash cfg path
--   let str1  = serialize   tree1
--       tree2 = deserializeTree str1
--       str2  = serialize   tree2
--   putStrLn str1
--   putStrLn $ show tree1
--   putStrLn $ "tree1 == tree2? " ++ show (tree1 == tree2)
--   putStrLn $ "show tree1 == show tree2? " ++ show (show tree1 == show tree2)
--   putStrLn $ "str1 == str2? "   ++ show (str1  == str2)
--   return $ tree1 == tree2

-- TODO rewrite with new hashTree
-- mapTree :: FilePath -> IO DupeMap
-- mapTree path = do
--   let cfg = Config { verbose=True, force=False }
--   tree <- cmdHash cfg path
--   let m = pathsByHash tree
--   return m
