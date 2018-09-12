module Gander.Cmd.Dedup where

-- TODO have a separate dedup command that only does the rm part without moving stuff?
-- TODO need to start a new file ignore.txt or something for hashes to ignore

import Gander.Lib
import Gander.Cmd.Hash (updateAnnexHashes)

import Control.Monad       (when)
import Data.Foldable       (toList)
import Data.List           (delete)
import Data.Maybe          (fromJust)
import Gander.Config       (Config(..))
import System.Console.ANSI (clearScreen)
import System.Exit         (exitSuccess)
import System.FilePath     ((</>), splitPath, joinPath)
import System.IO           (hFlush, stdout)

cmdDedup :: Config -> IO ()
cmdDedup cfg = do
  let aPath    = fromJust $ annex cfg
      hashes   = aPath </> "hashes.txt"
      unsorted = aPath </> "unsorted"
  tree <- readTree hashes
  dedupLoop cfg unsorted [] tree

-- TODO how to properly thread the changed tree through each step?
-- TODO check exit codes!
-- TODO sanitize commit message
dedupLoop :: Config -> FilePath -> [Hash] -> HashTree -> IO ()
dedupLoop cfg path ignored tree = do
  let aPath       = fromJust $ annex cfg
      dupes       = dupesByNFiles $ pathsByHash tree -- TODO toList here?
      dupesToSort = filter (\(h,_) -> not $ h `elem` ignored) (toList dupes)
  when (null dupesToSort) (clearScreen >> putStrLn "no duplicates. congrats!" >> exitSuccess)
  let (h1, ds)    = head dupesToSort -- TODO should these be just the plain paths?
      (_,_,paths) = ds
      ignored'    = h1:ignored
      sorted      = aPath </> "sorted"
  copyToKeep <- userPicks sorted ds
  case copyToKeep of
    Nothing -> dedupLoop cfg path ignored' tree
    Just keep -> do
      -- let keep'  = dropDir keep
      let paths' = map dropDir paths
      dedupGroup cfg aPath paths' keep -- at this point everything is relative to annex
      -- let tree' = tree -- TODO need to update tree to remove non-keepers!
      -- TODO use filename as part of commit? have to shorten/sanitize
      new <- buildTree (verbose cfg) (exclude cfg) aPath
      updateAnnexHashes cfg new
      let msg = unwords ["dedup", keep]
      gitCommit (verbose cfg) aPath msg
      dedupLoop cfg path ignored' tree

dropDir :: FilePath -> FilePath
dropDir = joinPath . tail . splitPath

-- TODO check that they share the same annex?
-- TODO check that dupes is longer than 2 (1?)
-- TODO current code is wrong whenever picking a number besides 1!
dedupGroup :: Config -> FilePath -> [FilePath] -> FilePath -> IO ()
dedupGroup cfg aPath dupes dest = do
    -- TODO ok do the easier to think through way: two branches
  if dest `elem` dupes
    then mapM_ (gitRm (verbose cfg) aPath) (delete dest dupes)
    else do
      -- move the first one to dest, then delete the rest (always at least 2)
      let src    = head dupes
          dupes' = tail dupes
      gitMv (verbose cfg) aPath src dest -- TODO or just dupes'?
      mapM_ (gitRm (verbose cfg) aPath) dupes'

-- Prompt the user where to put the one duplicate from each group we want to keep.
-- The choice could be one of the existing paths or a new one they enter.
-- It could also be Nothing if they choose to skip the group.
-- TODO have a default save dir for custom paths?
userPicks :: FilePath -> DupeList -> IO (Maybe FilePath)
userPicks sorted (n, t, paths) = do
  clearScreen
  let nDupes = length paths :: Int
  putStrLn $ "These " ++ show nDupes ++ " are duplicates:"
  listDupes 20 paths
  listOptions
  putStr "What do you think? "
  hFlush stdout
  answer <- getLine
  if answer == "skip" then return Nothing
  else if answer == "quit" then exitSuccess
  else if answer `elem` map show [1..length paths+1] then do
    let index = read answer :: Int
    return $ Just $ paths !! (index - 1)
  -- TODO if user inputs a path, makedirs up to it before trying to move
  else do -- TODO this whole branch is an infinite loop somehow?
    -- let answer' = sorted </> answer -- TODO why does this cause <<loop>>??
    confirm <- userSaysYes $ "Save to 'sorted/" ++ answer ++ "'?"
    if confirm
      then return $ Just $ "sorted" </> answer
      else do
        userPicks sorted (n, t, paths)
 
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
  , "  type the number of one of the existing copies to keep, and remove the rest"
  , "  type a new path to save the content, and remove all of these"
  , "\nYou can also type 'skip' to leave them alone for now, or 'quit' to quit"
  ]
