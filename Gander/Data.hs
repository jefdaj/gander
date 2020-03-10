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
  , DupeList
  , DupeMap
  , allDupes
  , dupesByNFiles
  , pathsByHash
  , printDupes
  , simplifyDupes
  , sortDupePaths
  )
  where

import Gander.Data.Delta
import Gander.Data.DupeMap
import Gander.Data.Hash
import Gander.Data.HashTree