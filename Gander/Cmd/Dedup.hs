module Gander.Cmd.Dedup where

import Gander.Config (Config(..))
import Gander.Lib (HashTree(..), readTree, userSaysYes)
import System.IO (hFlush, stdout)
import System.Console.ANSI (clearScreen)

cmdDedup :: Config -> FilePath -> FilePath -> IO ()
cmdDedup cfg hashes dir = do
  tree <- readTree hashes
  return ()

-- Prompt the user where to put the one duplicate from each group we want to keep.
-- The choice could be one of the existing paths or a new one they enter.
-- It could also be Nothing if they choose to skip the group.
userPicks :: [FilePath] -> IO (Maybe FilePath)
userPicks dupePaths = do
  clearScreen
  let nDupes = length dupePaths :: Int
  putStrLn $ "These " ++ show nDupes ++ " are duplicates:"
  promptToPick dupePaths

listDupes :: Int -> [FilePath] -> IO ()
listDupes howMany paths = putStrLn $ unlines numbered
  where
    paths' = take howMany paths ++ ellipses
    numbered = map (\(n,p) -> "  " ++ show n ++ ". " ++ p) (zip [(1 :: Int)..] paths')
    ellipses = if length paths > howMany then ["..."] else []

promptToPick :: [FilePath] -> IO (Maybe FilePath)
promptToPick paths = do
  listDupes 8 paths
  listOptions
  putStr "What do you pick? "
  hFlush stdout
  answer <- getLine
  if answer == "skip" then return Nothing
  else if answer `elem` map show [1..length paths+1] then do
    let index = read answer :: Int
    return $ Just $ paths !! (index - 1)
  else do
    -- TODO allow custom path here, but ask to confirm if it's weird?
    -- putStrLn ""
    -- putStrLn "That's not one of the options."
    confirm <- userSaysYes $ "Save to '" ++ answer ++ "'?"
    if confirm
      then return $ Just answer
      else userPicks paths

listOptions :: IO ()
listOptions = putStrLn $ unlines
  [ "To dedup them you can:"
  , "  type the number of one of the existing copy to keep, and delete the rest"
  , "  type a new path to save the content, and delete all of these"
  , "You can also type 'skip' to leave them alone for now"
  ]

-- TODO no haskeline unless needed! just use simple IO prompts (make Prompt module)

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
