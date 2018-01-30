module Gander.Cmd.Dedup where

import Control.Monad (when)
import Gander.Config (Config(..))
import Gander.Lib (DupeList, readTree, userSaysYes, pathsByHash,
                   dupesByNFiles, withAnnex, absolutize, noSlash)
import System.IO (hFlush, stdout)
import System.Console.ANSI (clearScreen)
import System.Exit (exitSuccess)
import System.Process        (readProcess)

cmdDedup :: Config -> FilePath -> FilePath -> IO ()
cmdDedup cfg hashes _ = do
  tree  <- readTree hashes
  let dupes = dupesByNFiles $ pathsByHash tree
  mapM_ (\dl -> userPicks dl >>= dedupGroup cfg dl) dupes

-- TODO check that they share the same annex?
-- TODO check that dupes is longer than 2 (1?)
-- TODO current code is wrong whenever picking a number besides 1!
dedupGroup :: Config -> DupeList  -> Maybe FilePath -> IO ()
dedupGroup cfg (_, _, dupes) dest = case dest of
  Nothing -> return ()
  Just path -> do
    path' <- fmap noSlash $ absolutize path
    dupes' <- fmap (map noSlash) (mapM absolutize dupes)
    let absDupes = filter (/= path') (tail dupes')
    withAnnex (verbose cfg) path' $ \dir -> do
      when (path' /= head dupes') $ do
        -- TODO also capture exit code and don't clear screen if error
        out <- readProcess "git" ["-C", dir, "mv", head dupes', path'] ""
        when (verbose cfg) $ putStrLn out
      outs <- mapM (\f -> readProcess "git" ["-C", dir, "rm", "-rf", f] "") absDupes
      when (verbose cfg) $ mapM_ putStrLn outs

-- Prompt the user where to put the one duplicate from each group we want to keep.
-- The choice could be one of the existing paths or a new one they enter.
-- It could also be Nothing if they choose to skip the group.
-- TODO have a default save dir for custom paths?
userPicks :: DupeList -> IO (Maybe FilePath)
userPicks (n, t, paths) = do
  clearScreen
  let nDupes = length paths :: Int
  putStrLn $ "These " ++ show nDupes ++ " are duplicates:"
  listDupes 20 paths
  listOptions
  putStr "What do you pick? "
  hFlush stdout
  answer <- getLine
  if answer == "skip" then return Nothing
  else if answer == "quit" then exitSuccess
  else if answer `elem` map show [1..length paths+1] then do
    let index = read answer :: Int
    return $ Just $ paths !! (index - 1)
  else do
    confirm <- userSaysYes $ "Save to '" ++ answer ++ "'?"
    if confirm
      then return $ Just answer
      else userPicks (n, t, paths)
 
listDupes :: Int -> [FilePath] -> IO ()
listDupes howMany paths = putStrLn $ unlines numbered
  where
    paths' = take howMany paths ++ ellipses
    numbered = map (\(n,p) -> "  " ++ show n ++ ". '" ++ p ++ "'")
                   (zip [(1 :: Int)..] paths')
    ellipses = if length paths > howMany then ["..."] else []

listOptions :: IO ()
listOptions = putStrLn $ unlines
  [ "To dedup them you can:"
  , "  type the number of one of the existing copies to keep, and delete the rest"
  , "  type a new path to save the content, and delete all of these"
  , "\nYou can also type 'skip' to leave them alone for now, or 'quit' to quit"
  ]

-- TODO explain to user:
--   list (first N) duplicates
--   what would you like to do?
--     skip for now
--     skip always (ignore)
--     save one of them by number
--     save to a new path you enter
--     quit
-- TODO "collapse" fn:
--   rm all but the one picked, double-checking they all still have the same hash
--   (error if not)
--   move the one picked if not at final destination

-- TODO getSubTree :: HashTree -> FilePath -> HashTree

-- TODO cmdDedup :: Config -> FilePath -> Maybe FilePath -> IO ()
--      cmdDedup cfg root msub = do
--        tree <- buildTree (verbose cfg) (exclude cfg) root
--        let tree' = case msub of
--                      Nothing -> tree
--                      Just sub -> getSubTree tree sub
--        ...
