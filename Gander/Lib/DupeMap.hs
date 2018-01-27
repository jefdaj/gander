module Gander.Lib.DupeMap
  ( pathsByHash
  , mergeDupeLists
  , sortDescLength
  , dupesByNFiles
  , hasDupes
  , printDupes
  )
  where

import Gander.Types

import Control.Arrow   ((&&&))
import Data.Foldable   (toList)
import Data.List       (sort, sortBy)
import Data.Map        (fromListWith)
import Data.Ord        (comparing)
import System.FilePath ((</>))

-- TODO can Foldable or Traversable simplify these?

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
