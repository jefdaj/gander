module Gander.Data
  -- hash
  ( Hash(..)
  , prettyHash
  , hashBytes
  , hashFile

  -- hashtree
  , HashTree(..)
  , TreeType(..)
  , addSubTree
  , buildTree
  , deserializeTree
  , dropTo
  , flattenTree
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

  -- delta
  , Delta(..)
  , diff
  , prettyDelta
  , printDeltas
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

import Gander.Data.Delta
import Gander.Data.DupeMap
import Gander.Data.Hash
import Gander.Data.HashTree
