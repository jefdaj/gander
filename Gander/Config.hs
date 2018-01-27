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
  { verbose :: Bool
  , force   :: Bool
  , exclude :: [String]
  }
  deriving (Read, Show)

defaultConfig :: Config
defaultConfig = Config
  { verbose = True
  , force = False
  , exclude = [".git*"]
  }
