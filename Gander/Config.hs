module Gander.Config
  ( Config(..)
  , defaultConfig
  )
  where

{- Parsed command line args
 - TODO add other stuff from usage.txt, or revise that
 -}
-- TODO remove from non-Cmd modules
data Config = Config
  { annex   :: Maybe FilePath
  , verbose :: Bool
  , force   :: Bool
  , check   :: Bool
  , exclude :: [String]
  }
  deriving (Read, Show)

-- TODO separate defaults for annex and standalone?
defaultConfig :: Config
defaultConfig = Config
  { annex   = Nothing
  , verbose = True
  , force   = False
  , check   = True
  , exclude = ["hashes.txt", ".git*", ".*.sw*", "._DS_Store", "*.plist"]
  }
