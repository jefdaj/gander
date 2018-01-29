module Gander.Lib.DupeMap
  ( DupeList
  , DupeMap
  , pathsByHash
  , mergeDupeLists
  , sortDescLength
  , dupesByNFiles
  , hasDupes
  , printDupes
  , allDupes
  )
  where

import Prelude hiding (lookup)
import Gander.Lib.Hash
import Gander.Lib.HashTree

import Control.Arrow   ((&&&))
import Data.Foldable   (toList)
import Data.List       (sort, sortBy)
import Data.Map        (Map)
import Data.Map        (fromListWith, keys, lookup)
import Data.Ord        (comparing)
import System.FilePath ((</>))

-- TODO can Foldable or Traversable simplify these?

type DupeList = (Int, [FilePath])

{- A map from file/dir hash to a list of duplicate file paths.
 - Could be rewritten to contain links to HashTrees if that helps.
 -}
type DupeMap = Map Hash DupeList

pathsByHash :: HashTree -> DupeMap
pathsByHash = fromListWith mergeDupeLists . pathsByHash' ""

mergeDupeLists :: DupeList -> DupeList -> DupeList
mergeDupeLists (n1, l1) (n2, l2) = (n1 + n2, l1 ++ l2)

pathsByHash' :: FilePath -> HashTree -> [(Hash, DupeList)]
pathsByHash' dir (File n h      ) = [(h, (1, [dir </> n]))]
pathsByHash' dir (Dir  n h cs fs) = cPaths ++ [(h, (fs, [dir </> n]))]
  where
    cPaths = concatMap (pathsByHash' $ dir </> n) cs

-- TODO warning: so far it lists anything annexed as a dup

-- see https://mail.haskell.org/pipermail/beginners/2009-June/001867.html
sortDescLength :: [DupeList] -> [DupeList]
sortDescLength = map snd
               . sortBy (comparing $ negate . fst . snd)
               . map (length &&& id)

dupesByNFiles :: DupeMap -> [DupeList]
dupesByNFiles = sortDescLength . filter hasDupes . toList

hasDupes :: DupeList -> Bool
hasDupes (nfiles, paths) = length paths > 1 && nfiles > 0

printDupes :: [DupeList] -> IO ()
printDupes groups = mapM_ printGroup groups
  where
    explain fs ds = if fs == ds
      then "# " ++ show fs ++ " duplicate files"
      else "# " ++ show ds ++ " duplicate dirs with " ++ show (div fs ds)
                ++ " files each (" ++ show fs ++ " total)"
    printGroup (n, paths) = mapM_ putStrLn
                          $ [explain n (length paths)]
                          ++ sort paths ++ [""]

-- helper for allDupes
-- TODO how to make the lookups safe?
anotherCopy :: Hash -> DupeMap -> DupeMap -> Bool
anotherCopy h mainMap subMap = nMain > nSub
  where
    (Just nMain) = fmap fst $ lookup h mainMap
    (Just nSub ) = fmap fst $ lookup h subMap

allDupes :: HashTree -> HashTree -> Bool
allDupes mainTree subTree = all safeToRmHash $ keys subDupes
  where
    mainDupes = pathsByHash mainTree
    subDupes  = pathsByHash subTree
    safeToRmHash h = anotherCopy h mainDupes subDupes
