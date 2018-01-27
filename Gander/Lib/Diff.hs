module Gander.Lib.Diff
  ( Diff(..)
  , diff
  )
  where

import Gander.Lib.HashTree (HashTree(..))
import System.FilePath ((</>))

data Diff
  = Added   FilePath
  | Removed FilePath
  | Changed FilePath
  deriving (Read, Show)

diff :: HashTree -> HashTree -> [Diff]
diff = diff' ""

diff' :: FilePath -> HashTree -> HashTree -> [Diff]
diff' _ old new | old == new = []
diff' r (File _ _    ) (File _ _    ) = [Changed r]
diff' r (File _ _    ) (Dir  d _ _ _) = [Removed r, Added $ r </> d]
diff' r (Dir  d _ _ _) (File _ _    ) = [Removed $ r </> d, Added r]
diff' r (Dir _ _ os _) (Dir _ _ ns _) = added ++ removed ++ changed
  where
    added   = [Added   $ r </> name x | x <- ns, not $ name x `elem` map name os]
    removed = [Removed $ r </> name x | x <- os, not $ name x `elem` map name ns]
    changed = concat [diff' (r </> name o) o n | o <- os, n <- ns,
                                                 o /= n, name o == name n]
