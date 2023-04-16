module Gander.Config
  ( Config(..)
  , defaultConfig
  , log
  )
  where

import Prelude hiding (log)
import System.FilePath.Glob (compile, Pattern)
import Control.Monad (when)

{- Parsed command line args
 - TODO add other stuff from usage.txt, or revise that
 -}
-- TODO remove from non-Cmd modules
data Config = Config
  { bin      :: Maybe FilePath
  , txt      :: Maybe FilePath
  , maxdepth :: Maybe Int
  , verbose  :: Bool
  , force    :: Bool
  , check    :: Bool
  , exclude  :: [Pattern]
  }
  deriving (Read, Show)

defaultConfig :: Config
defaultConfig = Config
  { bin      = Nothing
  , txt      = Nothing
  , maxdepth = Nothing
  , verbose  = True
  , force    = False
  , check    = True
  , exclude  = map compile ["hashes.*", ".git*", ".*.sw*", "._DS_Store", "*.plist"]
  }

-- TODO remove this from Util
log :: Config -> String -> IO ()
log cfg msg = when (verbose cfg) (putStrLn msg)
