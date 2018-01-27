module Gander.Config
  ( Config(..)
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
