{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Gander.Data.DupeMap
  ( DupeSet
  , DupeMap
  , allDupes
  , dupesByNFiles
  -- , hasDupes
  , listAllFiles
  , listLostFiles
  , mergeDupeSets
  , pathsByHash
  , printDupes
  -- , simplifyDupes
  -- , sortDupePaths
  , sortDescLength
  )
  where

import Control.Monad.ST
-- import Data.Hashable (Hashable(..))
-- import Control.DeepSeq

-- import qualified Data.ByteString.Char8    as B
import qualified Data.HashSet             as S
import qualified Data.HashTable.Class     as H
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.List                as L
import qualified Data.Massiv.Array        as A

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

-- TODO are the paths getting messed up somewhere in here?
-- like this: myfirstdedup/home/user/gander/demo/myfirstdedup/unsorted/backup/backup

import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as M
-- import qualified Data.HashSet          as S

import Gander.Data.Hash
import Gander.Data.HashTree
-- import Gander.Util (dropDir)

import Data.List       (sort, sortBy, isPrefixOf)
import Data.Ord        (comparing)
import System.FilePath ((</>), splitDirectories)

-- TODO can Foldable or Traversable simplify these?

-- note that most of the functions use (Hash, DupeSet) instead of plain DupeSet
type DupeSet  = (Int, TreeType, S.HashSet B.ByteString)
type DupeList = (Int, TreeType, [B.ByteString]) -- TODO move to Cmd/Dupes.hs?

{- A map from file/dir hash to a list of duplicate file paths.
 - Could be rewritten to contain links to HashTrees if that helps.
 -}
-- type DupeMap = M.HashMap Hash DupeSet

-- type DupeMap = :ashMap Hash (Int, TreeType, [FilePath])
-- toList that to get :: [(Hash, (Int, TreeType, [FilePath]))]

-- TODO does this need to drop the top component?
-- pathsByHash :: HashTree -> DupeMap
-- pathsByHash = M.fromListWith mergeDupeSets . map dropDirs' . pathsByHash' ""
--   where
--     dropDirs (i, t, ps) = (i, t, S.map dropDir ps)
--     dropDirs' (h, l) = (h, dropDirs l)

-- new ST-based mutable maps --

-- first attempt: use old DupeMap for everything but create it using ST
-- TODO then try to use *only* the ST hashmaps

-- TODO remove DupeMap type?
type DupeMap     = M.HashMap Hash DupeSet
type DupeTable s = C.HashTable s Hash DupeSet

-- TODO would this be a lot more efficient if we remove the H.toList M.fromList stuff?
-- TODO what about if we guess the approximate size first?
-- TODO what about if we make it from the serialized hashes instead of a tree?
pathsByHash :: HashTree -> ST s (DupeTable s)
pathsByHash t = do
  ht <- H.newSized 1 -- TODO what's with the size thing? maybe use H.new instead
  addToDupeMap ht t
  -- TODO try putting it back and compare overall speed
  -- H.mapM_ (\(k,_) -> H.mutate ht k removeNonDupes) ht
  return ht

-- inserts all nodes from a tree into an existing dupemap in ST s
addToDupeMap :: DupeTable s -> HashTree -> ST s ()
addToDupeMap ht t = addToDupeMap' ht "" t

-- same, but start from a given root path
addToDupeMap' :: DupeTable s -> FilePath -> HashTree -> ST s ()
addToDupeMap' ht dir (File n h      ) = insertDupeSet ht h (1, F, S.singleton (B.pack (dir </> n)))
addToDupeMap' ht dir (Dir  n h cs fs) = do
  insertDupeSet ht h (fs, D, S.singleton (B.pack (dir </> n)))
  mapM_ (addToDupeMap' ht (dir </> n)) cs

-- inserts one node into an existing dupemap in ST s
insertDupeSet :: DupeTable s -> Hash -> DupeSet -> ST s ()
insertDupeSet ht h d2 = do
  existing <- H.lookup ht h
  case existing of
    Nothing -> H.insert ht h d2
    Just d1 -> H.insert ht h $ mergeDupeSets d1 d2

-- pathsByHash t = runST $ 

-- TODO make maps immediately instead of intermediate lists here?
mergeDupeSets :: DupeSet -> DupeSet -> DupeSet
mergeDupeSets (n1, t, l1) (n2, _, l2) = (n1 + n2, t, S.union l1 l2)

-- pathsByHash' :: FilePath -> HashTree -> [(Hash, DupeSet)]
-- pathsByHash' dir (File n h      ) = [(h, (1, F, S.singleton (dir </> n)))]
-- pathsByHash' dir (Dir  n h cs fs) = cPaths ++ [(h, (fs, D, S.singleton (dir </> n)))]
--   where
--     cPaths = concatMap (pathsByHash' $ dir </> n) cs

-- TODO warning: so far it lists anything annexed as a dup

-- see https://mail.haskell.org/pipermail/beginners/2009-June/001867.html
-- TODO or is this the bottleneck?
--      could use mapM_ to perform this in place
sortDescLength :: [(Hash, DupeSet)] -> [(Hash, DupeSet)]
sortDescLength = map unDecorate . sortBy (comparing score) . map decorate
  where
    decorate (h, l@(n, _, _)) = (n, (h, l))
    unDecorate (_, (h, l)) = (h, l)
    score (n, _) = negate n -- sorts by descending number of files

-- dupes :: forall s. C.HashTable s Hash DupeSet -> ST s [DupeSet]
-- dupes = H.foldM (\vs (_, v@(n,_,_)) -> return $ if n < (-1) then (v:vs) else vs) []

-- dupesTmp :: forall s. C.HashTable s Hash DupeSet -> [DupeSet]
-- dupesTmp ht = runST $ dupes =<< ht

-- TODO include hashes too, just to print them?
-- dupesByNFiles :: DupeTable s -> [DupeSet]
-- dupesByNFiles dt = undefined

-- TODO is this reasonable?
type DupeSetVec   = A.Array A.N A.Ix1 DupeSet
-- type HashScoreVec = A.Array A.N A.Ix1 (Int, TreeType, B.ByteString)

dupesByNFiles :: (forall s. ST s (DupeTable s)) -> [DupeList]
dupesByNFiles ht = simplifyDupes $ Prelude.map fixElem sortedL
  where
    dupes    = H.foldM (\vs (_, v@(n,_,_)) -> return $ if n < (-1) then (v:vs) else vs) []
    sets     = runST $ dupes =<< ht
    unsorted = A.fromList A.Par sets :: DupeSetVec
    sorted   = A.quicksort $ A.compute unsorted :: DupeSetVec
    sortedL  = A.toList sorted
    fixElem (n, t, fs) = (negate n, t, L.sort $ S.toList fs)

-- TODO could this be faster than quicksorting everything even though single threaded?
-- usage: H.mapM_ (\(k,_) -> H.mutate dt k removeNonDupes) dt
-- rewrite of `filter hasDupes` for use with H.mutate
-- removeNonDupes :: Maybe DupeSet -> (Maybe DupeSet, ())
-- removeNonDupes Nothing = (Nothing, ())
-- removeNonDupes (Just v@(nfiles, _, paths)) = (if S.size paths > 1 && nfiles > 0
--                                                 then Just v
--                                                 else Nothing, ())

{- Assumes a pre-sorted list as provided by dupesByNFiles.
 - Removes lists whose elements are all inside elements of the first list.
 - For example if the first is dir1, dir2, dir3
 - and the next is dir1/file.txt, dir2/file.txt, dir3/file.txt
 - ... then the second set is redundant and confusing to show.
 -}
simplifyDupes :: [DupeList] -> [DupeList]
simplifyDupes [] = []
simplifyDupes (d@((_,_,fs)):ds) = d : filter (not . redundantSet) ds
  where
    redundantSet ((_,_,fs')) = all redundant fs'
    redundant e' = any id [(splitDirectories e)
                           `isPrefixOf`
                           (splitDirectories $ B.unpack e') | e <- map B.unpack fs]

-- TODO orderDupePaths :: [FilePath] -> [FilePath]
--      should order by least path components, then shortest name, then maybe alphabetical

-- sorts paths by shallowest (fewest dirs down), then length of filename,
-- then finally alphabetical
-- TODO is it inefficient enough to slow down the dupes command? rewrite if so
-- sortDupePaths :: (Hash, DupeSet) -> (Hash, DupeList)
-- sortDupePaths (h, (i, t, ps)) = (h, (i, t, sortBy myCompare $ S.toList ps))
--   where
--     myCompare p1 p2 = let d1 = length $ splitDirectories $ B.unpack p1
--                           d2 = length $ splitDirectories $ B.unpack p2
--                           l1 = length $ B.unpack p1
--                           l2 = length $ B.unpack p2
--                       in if      d1 > d2 then GT
--                          else if d1 < d2 then LT
--                          else if l1 > l2 then GT
--                          else if l1 < l2 then LT
--                          else compare p1 p2

-- hasDupes :: (Hash, DupeSet) -> Bool
-- hasDupes (_, (nfiles, _, paths)) = S.size paths > 1 && nfiles > 0

-- TODO use this as the basis for the dedup repl
printDupes :: [DupeList] -> IO ()
printDupes groups = mapM_ printGroup $ groups
  where
    explain t fs ds = (if t == F
      then "# " ++ show fs ++ " files"
      else "# " ++ show ds ++ " dirs ("
                ++ show (div fs ds) ++ " files each, "
                ++ show fs ++ " total)") ++ ":"
    printGroup (n, t, paths) = mapM_ putStrLn
                             $ [explain t n (length paths)]
                             ++ sort (map B.unpack paths) ++ [""]

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
    (Just nMain) = fmap (\(n,_,_) -> n) $ M.lookup h mainMap
    (Just nSub ) = fmap (\(n,_,_) -> n) $ M.lookup h subMap

allDupes :: HashTree -> HashTree -> Bool
-- allDupes mainTree subTree = all safeToRmHash $ undefined subDupes
allDupes mainTree subTree = undefined safeToRmHash $ undefined subDupes
  where
    mainDupes = undefined $ pathsByHash mainTree
    subDupes  = undefined $ pathsByHash subTree
    safeToRmHash h = anotherCopy h mainDupes subDupes

-- for warning the user when their action will delete the last copy of a file
-- TODO also warn about directories, because sometimes they might care (Garageband files for example)
-- TODO make more efficient by restricting to hashes found in the removed subtree! (only used for Rm right?)
listLostFiles :: HashTree -> HashTree -> [FilePath]
listLostFiles before after = filesLost
  where
    hashesBefore = pathsByHash before
    hashesAfter  = pathsByHash after
    hashesLost   = undefined hashesBefore hashesAfter
    filesLost    = sort $ S.toList $ S.unions $ M.elems
                 $ M.map (\(_,_,fs) -> fs)
                 $ M.filter (\(_,t,_) -> t == F) hashesLost
