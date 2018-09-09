module Gander.Lib.DupeMap
  ( DupeList
  , DupeMap
  , pathsByHash
  , mergeDupeLists
  , sortDescLength
  , dupesByNFiles
  , simplifyDupes
  , hasDupes
  , printDupes
  , allDupes

  , listAllFiles
  , listLostFiles
  )
  where

import Prelude hiding (lookup)
import Gander.Lib.Hash
import Gander.Lib.HashTree

import Data.Foldable   (toList)
import Data.List       (nub, sort, sortBy, isPrefixOf)
import Data.Map        (Map, (\\))
import Data.Map        (fromListWith, keys, lookup)
import Data.Ord        (comparing)
import System.FilePath ((</>), splitDirectories)

-- TODO can Foldable or Traversable simplify these?

-- TODO add the hash here too?
type DupeList = (Int, TreeType, [FilePath])

{- A map from file/dir hash to a list of duplicate file paths.
 - Could be rewritten to contain links to HashTrees if that helps.
 -}
type DupeMap = Map Hash DupeList

pathsByHash :: HashTree -> DupeMap
pathsByHash = fromListWith mergeDupeLists . pathsByHash' ""

mergeDupeLists :: DupeList -> DupeList -> DupeList
mergeDupeLists (n1, t, l1) (n2, _, l2) = (n1 + n2, t, l1 ++ l2)

pathsByHash' :: FilePath -> HashTree -> [(Hash, DupeList)]
pathsByHash' dir (File n h      ) = [(h, (1, F, [dir </> n]))]
pathsByHash' dir (Dir  n h cs fs) = cPaths ++ [(h, (fs, D, [dir </> n]))]
  where
    cPaths = concatMap (pathsByHash' $ dir </> n) cs

-- TODO warning: so far it lists anything annexed as a dup

-- see https://mail.haskell.org/pipermail/beginners/2009-June/001867.html
sortDescLength :: [DupeList] -> [DupeList]
sortDescLength = map snd
               . sortBy (comparing $ negate . (\(n,_,_) -> n) . snd)
               . map (\t@(n,_,_) -> (n, t))
               -- . map (length &&& id)

dupesByNFiles :: DupeMap -> [DupeList]
dupesByNFiles = sortDescLength . filter hasDupes . toList

{- Assumes a pre-sorted list as provided by dupesByNFiles.
 - Removes lists whose elements are all inside elements of the first list.
 - For example if the first is dir1, dir2, dir3
 - and the next is dir1/file.txt, dir2/file.txt, dir3/file.txt
 - ... then the second set is redundant and confusing to show.
 -}
simplifyDupes :: [DupeList] -> [DupeList]
simplifyDupes [] = []
simplifyDupes (d@(_,_,fs):ds) = d : filter (not . redundantList) ds
  where
    redundantList (_,_,fs') = all redundantElem fs'
    redundantElem e'  = any id [(splitDirectories e) `isPrefixOf` (splitDirectories e') | e <- fs]

hasDupes :: DupeList -> Bool
hasDupes (nfiles, _, paths) = length paths > 1 && nfiles > 0

-- TODO use this as the basis for the dedup repl
printDupes :: [DupeList] -> IO ()
printDupes groups = mapM_ printGroup groups
  where
    explain fs ds = if fs == ds
      then "# " ++ show fs ++ " duplicate files"
      else "# " ++ show ds ++ " duplicate dirs with " ++ show (div fs ds)
                ++ " files each (" ++ show fs ++ " total)"
    printGroup (n, _, paths) = mapM_ putStrLn
                             $ [explain n (length paths)]
                             ++ sort paths ++ [""]

-----------------------------
-- info about copy numbers --
-----------------------------

-- TODO is this actually helpful?
listAllFiles :: FilePath -> HashTree -> [(Hash, FilePath)]
listAllFiles anchor (File n h     ) = [(h, anchor </> n)]
listAllFiles anchor (Dir  n _ cs _) = concatMap (listAllFiles $ anchor </> n) cs


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
    hashesLost   = hashesBefore \\ hashesAfter
    filesLost    = nub $ sort $ concatMap (\(_,t,fs) -> if t == F then fs else []) hashesLost
