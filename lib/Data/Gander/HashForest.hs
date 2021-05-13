{-# LANGUAGE TemplateHaskell #-}

module Data.Gander.HashForest
  ( HashForest(..)
  , readTrees
  , buildForest
  , readForest
  , readOrBuildForest
  , serializeForest
  , deserializeForest
  , printForest
  , writeForest
  , writeBinForest
  )
  where

-- import Data.Gander.Hash
import Data.Gander.HashLine (parseHashes)
import Data.Gander.HashTree

import TH.Derive
import Data.Store             (Store(..), encode, decodeIO)
import System.FilePath.Glob (Pattern)
import qualified Data.ByteString.Char8 as B8
import System.IO            (withFile, IOMode(..))
import Control.Exception.Safe (catchAny)

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
readTrees :: Maybe Int -> [FilePath] -> IO HashForest
readTrees md paths = HashForest <$> mapM (readTree md) paths

readForest :: Maybe Int -> FilePath -> IO HashForest
readForest md path = catchAny
                      (B8.readFile path >>= decodeIO)
                      (\_ -> fmap (deserializeForest md) $ B8.readFile path)

-- TODO how should errors propagate?
buildForest :: Bool -> [Pattern] -> [FilePath] -> IO HashForest
buildForest beVerbose excludes paths = HashForest <$> mapM (buildTree beVerbose excludes) paths

-- TODO be clearer: this works on trees, but you could also read a forest directly
readOrBuildForest :: Bool -> Maybe Int -> [Pattern] -> [FilePath] -> IO HashForest
readOrBuildForest verbose mmaxdepth excludes paths = HashForest <$> mapM (readOrBuildTree verbose mmaxdepth excludes) paths

serializeForest :: HashForest -> [B8.ByteString]
serializeForest (HashForest ts) = concatMap serializeTree ts 

deserializeForest :: Maybe Int -> B8.ByteString -> HashForest
deserializeForest md = HashForest <$> fmap snd . foldr accTrees [] . reverse . parseHashes md

printForest :: HashForest -> IO ()
printForest (HashForest ts) = mapM_ printTree ts

-- this uses a handle for streaming output, which turns out to be important for memory usage
-- TODO rename writeHashes? this is a confusing way to say that
writeForest :: FilePath -> HashForest -> IO ()
writeForest path forest = withFile path WriteMode $ \h ->
  mapM_ (B8.hPutStrLn h) (serializeForest forest)

writeBinForest :: FilePath -> HashForest -> IO ()
writeBinForest path forest = B8.writeFile path $ encode forest
