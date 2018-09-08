module Gander.Lib
  -- hash
  ( Hash
  , hashBytes
  , hashFile

  -- hashtree
  , HashTree(..)
  , readTree
  , buildTree
  , readOrBuildTree
  , flattenTree
  , serializeTree
  , deserializeTree
  , hashContents
  , renameRoot
  , printHashes
  , getSubTree
  , treeContainsPath
  , treeContainsHash
  , addSubTree

  -- delta
  , Delta(..)
  , diff
  , prettyDiff
  , printDiffs
  , runDelta
  , runDeltas
  , safe

  -- dupemap
  , DupeList
  , DupeMap
  , dupesByNFiles
  , simplifyDupes
  , printDupes
  , pathsByHash
  , allDupes

  -- git
  , pathComponents -- TODO not really git related right?
  , findAnnex
  , inAnnex
  , rsync
  , annexAdd
  , absolutize
  , gitRm
  , withAnnex
  , userSaysYes
  )
  where

import Gander.Lib.Hash
import Gander.Lib.HashTree
import Gander.Lib.Delta
import Gander.Lib.DupeMap
import Gander.Lib.Git
