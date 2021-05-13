{-# LANGUAGE TemplateHaskell #-}

module Data.Gander.HashForest
  ( HashForest(..)
  , readForest
  )
  where

-- import Data.Gander.Hash
-- import Data.Gander.HashLine
import Data.Gander.HashTree

import TH.Derive
import Data.Store             (Store(..))

{- A forest is just a list of trees without an overall content hash. It's used
 - at the top level when reading potentially more than one tree from the
 - command line.
 -}
data HashForest = HashForest [HashTree]
  deriving (Eq, Show, Read)

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
$($(derive [d|
    instance Deriving (Store HashForest)
    |]))

readForest :: Maybe Int -> [FilePath] -> IO HashForest
readForest md paths = HashForest <$> mapM (readTree md) paths
