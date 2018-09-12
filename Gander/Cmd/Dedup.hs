module Gander.Cmd.Dedup where

-- TODO have a separate dedup command that only does the rm part without moving stuff?
-- TODO need to start a new file ignore.txt or something for hashes to ignore
-- TODO fix <<loop>> bug when answering wrong!

import Gander.Lib
import Gander.Cmd.Hash (cmdHash)

import Data.Maybe (fromJust)
import Data.Foldable (toList)
import Control.Monad (when)
import Gander.Config (Config(..))
import System.IO (hFlush, stdout)
import System.Console.ANSI (clearScreen)
import System.Exit (exitSuccess)
import System.FilePath ((</>), splitPath, joinPath)
import Data.List (delete)

cmdDedup :: Config -> IO ()
cmdDedup cfg = do
  let aPath    = fromJust $ annex cfg
      hashes   = aPath </> "hashes.txt"
      unsorted = aPath </> "unsorted"
  tree <- readTree hashes
  dedupLoop cfg unsorted [] tree

-- cmdDedup cfg hashes _ = do
--   tree  <- readTree hashes
--   let dupes = dupesByNFiles $ pathsByHash tree
--   -- TODO state monad or some kind of accumulator for tree here
--   -- TODO and then recalculate dupes after each step
--   mapM_ (\dl -> userPicks dl >>= dedupGroup cfg dl) dupes

-- TODO how to properly thread the changed tree through each step?
dedupLoop :: Config -> FilePath -> [Hash] -> HashTree -> IO ()
dedupLoop cfg path ignored tree = do
  let aPath       = fromJust $ annex cfg
      dupes       = dupesByNFiles $ pathsByHash tree -- TODO toList here?
      dupesToSort = filter (\(h,_) -> not $ h `elem` ignored) (toList dupes)
      (h1, ds)    = head dupesToSort -- TODO should these be just the plain paths?
      (_,_,paths) = ds
      ignored'    = h1:ignored
      sorted      = aPath </> "sorted"
  when (null dupes) (putStrLn "no duplicates. congrats!" >> exitSuccess)
  copyToKeep <- userPicks sorted ds
  case copyToKeep of
    Nothing -> dedupLoop cfg path ignored' tree
    Just keep -> do
      -- let keep'  = dropDir keep
      let paths' = map dropDir paths
      dedupGroup cfg aPath paths' keep -- at this point everything is relative to annex
      -- let tree' = tree -- TODO need to update tree to remove non-keepers!
      -- TODO use filename as part of commit? have to shorten/sanitize
      cmdHash cfg aPath
      gitCommit (verbose cfg) aPath "gander mv" -- TODO check exit code
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


--     let aPath   = fromJust $ annex cfg
--         dupes'  = delete dest dupes
-- 
--     -- path' <- fmap noSlash $ absolutize path
--     -- dupes' <- fmap (map noSlash) (mapM absolutize dupes)
--     -- let absDupes = filter (/= path) (tail dupes')
--     let aPath   = fromJust $ annex cfg
--         -- path'   = aPath </> path
--         -- dupes'  = map (aPath </>) dupes
--         dupes' = filter (/= path) (tail dupes) -- TODO is this right?
--     withAnnex (verbose cfg) aPath $ \dir -> do
--       when (path /= head dupes) $ do
--         -- TODO also capture exit code and don't clear screen if error
--         -- out <- readProcess "git" ["-C", dir, "mv", head dupes', path'] ""
--         -- when (verbose cfg) $ putStrLn out
--         gitMv (verbose cfg) dir (head dupes') (dropDir path) -- TODO or just dupes'?
--       -- outs <- mapM (\f -> readProcess "git" ["-C", dir, "rm", "-rf", f] "") absDupes
--       mapM_ (gitRm (verbose cfg) dir) dupes'
--       return ()
--       -- when (verbose cfg) $ mapM_ putStrLn outs


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
