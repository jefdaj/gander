module Gander.Config
  ( Config(..)
  , defaultConfig
  )
  where

import System.FilePath.Glob (compile, Pattern)

{- Parsed command line args
 - TODO add other stuff from usage.txt, or revise that
 -}
-- TODO remove from non-Cmd modules
data Config = Config
  { annex   :: Maybe FilePath
  , bin     :: Maybe FilePath
  , txt     :: Maybe FilePath
  , verbose :: Bool
  , force   :: Bool
  , check   :: Bool
  , exclude :: [Pattern]
  }
  deriving (Read, Show)

-- TODO separate defaults for annex and standalone?
defaultConfig :: Config
defaultConfig = Config
  { annex   = Nothing
  , bin     = Nothing
  , txt     = Nothing
  , verbose = True
  , force   = False
  , check   = True
  , exclude = map compile ["hashes.*", ".git*", ".*.sw*", "._DS_Store", "*.plist"]
  }
