{-# LANGUAGE TemplateHaskell #-}

module Data.Gander.HashForest
  ( HashForest(..)
  , readForest
  , buildForest
  , readOrBuildForest
  , serializeForest
  )
  where

-- import Data.Gander.Hash
-- import Data.Gander.HashLine
import Data.Gander.HashTree

import TH.Derive
import Data.Store             (Store(..))
import System.FilePath.Glob (Pattern)
import qualified Data.ByteString.Char8 as B8

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

-- TODO how should errors propagate?
readForest :: Maybe Int -> [FilePath] -> IO HashForest
readForest md paths = HashForest <$> mapM (readTree md) paths

-- TODO how should errors propagate?
buildForest :: Bool -> [Pattern] -> [FilePath] -> IO HashForest
buildForest beVerbose excludes paths = HashForest <$> mapM (buildTree beVerbose excludes) paths

readOrBuildForest :: Bool -> Maybe Int -> [Pattern] -> [FilePath] -> IO HashForest
readOrBuildForest verbose mmaxdepth excludes paths = HashForest <$> mapM (readOrBuildTree verbose mmaxdepth excludes) paths

serializeForest :: HashForest -> [B8.ByteString]
serializeForest (HashForest ts) = concatMap serializeTree ts 
