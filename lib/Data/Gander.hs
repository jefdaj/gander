module Data.Gander
  -- hash
  ( Hash(..)
  , prettyHash
  , hashBytes
  , hashFile

  -- hashtree
  , HashTree(..)
  , AnchoredHashTree(..)
  , TreeType(..)
  , addSubTree
  , buildTree
  , buildProdTree
  , deserializeTree
  , dropTo
  , flattenTree
  , flattenTreePaths
  , flattenTree' -- TODO name this something better
  , hashContents
  , listAllFiles
  , listLostFiles
  , printTree
  , writeBinTree
  , writeTree
  , readOrBuildTree
  , readTree
  , renameRoot
  , rmSubTree
  , serializeTree
  , treeContainsHash
  , treeContainsPath

  -- hashforest
  , HashForest(..)
  , buildForest
  , readForest
  , readTrees
  , readOrBuildTrees
  , flattenForestPaths
  , printForest
  , writeForest
  , writeBinForest
  , serializeForest
  , deserializeForest

  -- delta
  , Delta(..)
  , diff
  , prettyDelta
  , printDeltas

  -- sim
  , simDelta
  , simDeltas
  -- , safeDelta
  -- , safeDeltas
  , assertSameTrees

  -- dupemap
  , DupeSet
  , DupeMap
  , allDupes
  , dupesByNFiles
  , pathsByHash
  , mergeDupeSets
  , printDupes
  , writeDupes
  -- , simplifyDupes
  -- , sortDupePaths
  )
  where

import Data.Gander.Delta
import Data.Gander.Sim
import Data.Gander.DupeMap
import Data.Gander.Hash
import Data.Gander.HashLine
import Data.Gander.HashTree
import Data.Gander.HashForest
