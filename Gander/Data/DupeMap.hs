{-# LANGUAGE OverloadedStrings #-}

{-
 - But regardless of data structure, one of the most crucial things to do is to
 - prune your bytestrings! By default, ByteStrings seek to share the underlying
 - byte array no matter how you slice and dice them; and by default that's a
 - good thing. However, when you're reading hundreds of Mbs, chopping them into
 - little pieces —most of which are equal— and storing them in a data
 - structure, you don't want to accidentally keep holding on to every byte
 - array you ever touched! If you use bytestring-trie, it does this pruning for
 - you automatically. If you use HashMap or something, then you'll want to
 - write a thin wrapper around the API in order to ByteString.copy keys before
 - inserting them into the map.
 -}

module Gander.Data.DupeMap
  ( DupeList
  , DupeMap
  , allDupes
  , dupesByNFiles
  , hasDupes
  , listAllFiles
  , listLostFiles
  , mergeDupeLists
  , pathsByHash
  , printDupes
  , simplifyDupes
  , sortDupePaths
  , sortDescLength
  )
  where

-- TODO are the paths getting messed up somewhere in here?
-- like this: myfirstdedup/home/user/gander/demo/myfirstdedup/unsorted/backup/backup

import Prelude hiding (lookup)
import qualified Data.ByteString.Char8 as B8

import Gander.Data.Hash
import Gander.Data.HashTree
import Gander.Util (dropDir)

-- import Data.Foldable   (toList)
import Data.List       (nub, sort, sortBy, isPrefixOf)
import Data.HashMap.Strict (HashMap, difference, toList, fromListWith, keys, lookup)
import Data.Ord        (comparing)
import System.FilePath ((</>), splitDirectories)

-- TODO can Foldable or Traversable simplify these?

-- note that most of the functions use (Hash, DupeList) instead of plain DupeList
type DupeList = (Int, TreeType, [FilePath])

{- A map from file/dir hash to a list of duplicate file paths.
 - Could be rewritten to contain links to HashTrees if that helps.
 -}
type DupeMap = HashMap Hash DupeList
-- type DupeMap = :ashMap Hash (Int, TreeType, [FilePath])
-- toList that to get :: [(Hash, (Int, TreeType, [FilePath]))]

-- TODO does this need to drop the top component?
pathsByHash :: HashTree -> DupeMap
pathsByHash = fromListWith mergeDupeLists . map dropDirs' . pathsByHash' ""
  where
    dropDirs (i, t, ps) = (i, t, map dropDir ps)
    dropDirs' (h, l) = (h, dropDirs l)

mergeDupeLists :: DupeList -> DupeList -> DupeList
mergeDupeLists (n1, t, l1) (n2, _, l2) = (n1 + n2, t, l1 ++ l2)

pathsByHash' :: FilePath -> HashTree -> [(Hash, DupeList)]
pathsByHash' dir (File n h      ) = [(h, (1, F, [dir </> n]))]
pathsByHash' dir (Dir  n h cs fs) = cPaths ++ [(h, (fs, D, [dir </> n]))]
  where
    cPaths = concatMap (pathsByHash' $ dir </> n) cs

-- TODO warning: so far it lists anything annexed as a dup

-- see https://mail.haskell.org/pipermail/beginners/2009-June/001867.html
sortDescLength :: [(Hash, DupeList)] -> [(Hash, DupeList)]
sortDescLength = map unDecorate . sortBy (comparing score) . map decorate
  where
    decorate (h, l@(n, _, _)) = (n, (h, l))
    unDecorate (_, (h, l)) = (h, l)
    score (n, _) = negate n -- sorts by descending number of files

dupesByNFiles :: DupeMap -> [(Hash, DupeList)]
dupesByNFiles = sortDescLength . filter hasDupes . toList

{- Assumes a pre-sorted list as provided by dupesByNFiles.
 - Removes lists whose elements are all inside elements of the first list.
 - For example if the first is dir1, dir2, dir3
 - and the next is dir1/file.txt, dir2/file.txt, dir3/file.txt
 - ... then the second set is redundant and confusing to show.
 -}
simplifyDupes :: [(Hash, DupeList)] -> [(Hash, DupeList)]
simplifyDupes [] = []
simplifyDupes (d@(_, (_,_,fs)):ds) = d : filter (not . redundantList) ds
  where
    redundantList (_, (_,_,fs')) = all redundantElem fs'
    redundantElem e'  = any id [(splitDirectories e) `isPrefixOf` (splitDirectories e') | e <- fs]

-- TODO orderDupePaths :: [FilePath] -> [FilePath]
--      should order by least path components, then shortest name, then maybe alphabetical

-- sorts paths by shallowest (fewest dirs down), then length of filename,
-- then finally alphabetical
-- TODO is it inefficient enough to slow down the dupes command? rewrite if so
sortDupePaths :: (Hash, DupeList) -> (Hash, DupeList)
sortDupePaths (h, (i, t, ps)) = (h, (i, t, sortBy myCompare ps))
  where
    myCompare p1 p2 = let d1 = length $ splitDirectories p1
                          d2 = length $ splitDirectories p2
                          l1 = length p1
                          l2 = length p2
                      in if      d1 > d2 then GT
                         else if d1 < d2 then LT
                         else if l1 > l2 then GT
                         else if l1 < l2 then LT
                         else compare p1 p2

hasDupes :: (Hash, DupeList) -> Bool
hasDupes (_, (nfiles, _, paths)) = length paths > 1 && nfiles > 0

-- TODO use this as the basis for the dedup repl
printDupes :: [(Hash, DupeList)] -> IO ()
printDupes groups = mapM_ printGroup groups
  where
    explain t h fs ds = (if t == F
      then "# " ++ show fs ++ " files"
      else "# " ++ show ds ++ " dirs (" ++ show (div fs ds) ++ " files each, " ++ show fs ++ " total)")
      ++ " have hash " ++ B8.unpack (prettyHash h) ++ ":"
    printGroup (h, (n, t, paths)) = mapM_ putStrLn
                             $ [explain t h n (length paths)]
                             ++ sort paths ++ [""]

-----------------------------
-- info about copy numbers --
-----------------------------

-- TODO is this actually helpful?
listAllFiles :: FilePath -> HashTree -> [(Hash, FilePath)]
listAllFiles anchor (File n h     ) = [(h, anchor </> n)]
listAllFiles anchor (Dir  n _ cs _) = concatMap (listAllFiles $ anchor </> n) cs


-- TODO rewrite allDupes by removing the subtree first then testing membership
--      (that way can use the removing part separately in cmdDedup)

-- helper for allDupes
-- TODO how to make the lookups safe?
anotherCopy :: Hash -> DupeMap -> DupeMap -> Bool
anotherCopy h mainMap subMap = nMain > nSub
  where
    (Just nMain) = fmap (\(n,_,_) -> n) $ lookup h mainMap
    (Just nSub ) = fmap (\(n,_,_) -> n) $ lookup h subMap

allDupes :: HashTree -> HashTree -> Bool
allDupes mainTree subTree = all safeToRmHash $ keys subDupes
  where
    mainDupes = pathsByHash mainTree
    subDupes  = pathsByHash subTree
    safeToRmHash h = anotherCopy h mainDupes subDupes

-- for warning the user when their action will delete the last copy of a file
-- TODO also warn about directories, because sometimes they might care (Garageband files for example)
listLostFiles :: HashTree -> HashTree -> [FilePath]
listLostFiles before after = filesLost
  where
    hashesBefore = pathsByHash before
    hashesAfter  = pathsByHash after
    hashesLost   = difference hashesBefore hashesAfter
    filesLost    = nub $ sort $ concatMap (\(_,t,fs) -> if t == F then fs else []) hashesLost
