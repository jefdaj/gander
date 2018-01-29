module Gander.Lib
  -- hash
  ( Hash

  -- hashtree
  , HashTree(..)
  , readTree
  , buildTree
  , flattenTree
  , serializeTree
  , deserializeTree
  , hashHashes
  , printHashes

  -- diff
  , Diff(..)
  , diff
  , prettyDiff
  , printDiffs

  -- dupemap
  , DupeList
  , DupeMap
  , dupesByNFiles
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
  )
  where

import Gander.Lib.Hash
import Gander.Lib.HashTree
import Gander.Lib.Diff
import Gander.Lib.DupeMap
import Gander.Lib.Git
