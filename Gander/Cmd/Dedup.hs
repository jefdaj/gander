module Gander.Cmd.Dedup where

import Gander.Config (Config(..))
import Gander.Lib (HashTree(..), readTree, userSaysYes)
import System.IO (hFlush, stdout)
import System.Console.ANSI (clearScreen)
import System.Exit (exitSuccess)

cmdDedup :: Config -> FilePath -> FilePath -> IO ()
cmdDedup cfg hashes dir = do
  tree <- readTree hashes
  -- TODO finish it here!
  return ()

-- Prompt the user where to put the one duplicate from each group we want to keep.
-- The choice could be one of the existing paths or a new one they enter.
-- It could also be Nothing if they choose to skip the group.
-- TODO have a default save dir for custom paths?
userPicks :: [FilePath] -> IO (Maybe FilePath)
userPicks paths = do
  -- TODO unify this with promptToPick since they seem to be the same...
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
      else userPicks paths

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
