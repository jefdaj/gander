module Data.Gander.HashForest
  ( HashForest(..)
  )
  where

import Data.Gander.HashTree

{- A forest is just a list of trees without an overall content hash. It's used
 - at the top level when reading potentially more than one tree from the
 - command line.
 -}
data HashForest = HashForest [HashTree]
  deriving (Eq, Show, Read)
