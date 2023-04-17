{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Gander.HashForest
  ( HashForest(..)
  , ProdForest
  , readTrees
  , buildForest
  , readForest
  , readOrBuildTrees
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
import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO            (withFile, IOMode(..))
import Control.Exception.Safe (catchAny)

-- TODO figure lazy out to avoid huge RAM usage
import Codec.Compression.Zstd.Lazy as ZL
import Codec.Compression.Zstd      as Z

{- A forest is just a list of trees without an overall content hash. It's used
 - at the top level when reading potentially more than one tree from the
 - command line.
 -}
newtype HashForest a = HashForest [HashTree a]
  -- deriving (Eq, Show, Read)

type ProdForest = HashForest ()

deriving instance Eq   a => Eq   (HashForest a)
deriving instance Show a => Show (HashForest a)
deriving instance Read a => Read (HashForest a)

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
-- TODO why can't you extend () to and type Store a => a here?
$($(derive [d|
    instance Deriving (Store (HashForest ()))
    |]))

-- TODO how should errors propagate?
readTrees :: Maybe Int -> [FilePath] -> IO (HashForest ())
readTrees md paths = HashForest <$> mapM (readTree md) paths

-- TODO remove existing bin case and only handle "txt" hashes now? (which will be partly binary)
readForest :: Maybe Int -> FilePath -> IO (HashForest ())
readForest md path = catchAny
                      (B8.readFile path >>= decodeIO)
                      (\_ -> fmap (deserializeForest md) $ B8.readFile path)

-- TODO how should errors propagate?
buildForest :: Bool -> [Pattern] -> [FilePath] -> IO (HashForest ())
buildForest beVerbose excludes paths = HashForest <$> mapM (buildProdTree beVerbose excludes) paths

-- TODO be clearer: this works on trees, but you could also read a forest directly
readOrBuildTrees :: Bool -> Maybe Int -> [Pattern] -> [FilePath] -> IO (HashForest ())
readOrBuildTrees verbose mmaxdepth excludes paths = HashForest <$> mapM (readOrBuildTree verbose mmaxdepth excludes) paths

-- TODO is there a reason this doesn't join lines?
serializeForest :: HashForest () -> [B8.ByteString]
serializeForest (HashForest ts) = concatMap serializeTree ts 

serializeForestZstd :: HashForest () -> B8.ByteString
serializeForestZstd = Z.compress Z.maxCLevel . B8.intercalate (B8.pack "\n") . serializeForest

deserializeForest :: Maybe Int -> B8.ByteString -> HashForest ()
deserializeForest md = HashForest <$> fmap snd . foldr accTrees [] . reverse . parseHashes' md . ZL.decompress . BL.fromStrict
  where
    parseHashes' x = parseHashes x . BL.toStrict

printForest :: HashForest () -> IO ()
printForest (HashForest ts) = mapM_ printTree ts

-- this uses a handle for streaming output, which turns out to be important for memory usage
-- TODO rename writeHashes? this is a confusing way to say that
writeForest :: FilePath -> HashForest () -> IO ()
writeForest path forest = withFile path WriteMode $ \h ->
  -- mapM_ (B8.hPutStrLn h) (serializeForest forest)
  B8.hPutStrLn h $ serializeForestZstd forest

writeBinForest :: FilePath -> HashForest () -> IO ()
writeBinForest path forest = B8.writeFile path $ encode forest
