module Main where

-- TODO break into modules? Main, Types, Config, Scan, Dedup
-- TODO also expose a library so you can test stuff in the REPL!
--      (or would loading Main be enough?)

import System.Console.Docopt hiding (command)
import System.Directory.Tree

-----------
-- types --
-----------

{- Parsed command line args
 - TODO add other stuff from usage.txt, or revise that
 -}
data Config = Config
  { command  :: String
  , annexDir :: FilePath
  , dedupDir :: FilePath
  , verbose  :: Bool
  , confirm  :: Bool
  }

{- Checksum (sha256sum?) of a file or folder.
 - For files, should match the corresponding git-annex key.
 - TODO would storing it in a more efficient way help?
 - TODO would adding timestamps or number of files help?
 -}
type Hash = String

{- A tree of file names matching (a subdirectory of) the annex,
 - where each dir and file node contains a hash of its contents.
 - TODO read and write files
 - TODO would also storing the number of files in each dir help, or timestamps?
 -}
data HashTree = AnchoredDirTree Hash
  deriving (Eq, Read, Show)

{- A map from file/dir hash to a list of duplicate file paths.
 - Could be rewritten to contain links to HashTrees if that helps.
 -}
data PathsByHash = Map Hash [FilePath]

----------------
-- scan files --
----------------

scan :: Config -> IO HashTree
scan cfg = undefined

----------
-- main --
----------

-- patterns :: Docopt
-- patterns = [docoptFile|USAGE|]

parseConfig :: FilePath -> IO Config
parseConfig path = undefined

-- TODO use a case statement instead so types can be different?
commands :: [(String, Config -> IO ())]
commands =
  [ ("scan"   , \c -> scan c >> return ())
  , ("add"    , undefined)
  , ("verify" , undefined)
  , ("fsck"   , undefined)
  , ("gc"     , undefined)
  ]

runCommand :: Config -> IO ()
runCommand cfg = case lookup (command cfg) commands of
  Nothing -> putStrLn $ "no such command: " ++ command cfg
  Just fn -> fn cfg

main :: IO ()
main = putStrLn "Hello, Haskell!"
