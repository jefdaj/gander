{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO break into modules? Main, Types, Config, Scan, Dedup
-- TODO also expose a library so you can test stuff in the REPL!
--      (or would loading Main be enough?)
-- TODO is git-annex an actual dep, or just recommended to go with it?
-- TODO shit, need to add a different data structure to put hashes in the dirs huh?
--      guess the easy/safe way is to start with one of these and transform it?
--      can do that without any additional IO so not so bad I guess
--      Oh, can stop on failures that way too

import System.Console.Docopt hiding (command)
import System.Directory.Tree
import System.Environment (getArgs)
import System.FilePath ((</>))

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
newtype Hash = Hash String
  deriving (Eq, Read, Show)

{- A tree of file names matching (a subdirectory of) the annex,
 - where each dir and file node contains a hash of its contents.
 - TODO read and write files
 - TODO would also storing the number of files in each dir help, or timestamps?
 -}
-- data HashTree = AnchoredDirTree Hash
--   deriving (Eq, Read, Show)
data HashTree
  = File { name :: FileName, hash :: Hash }
  | Dir  { name :: FileName, hash :: Hash, contents :: [HashTree] }
  deriving (Read, Show) -- TODO write Eq instance

{- A map from file/dir hash to a list of duplicate file paths.
 - Could be rewritten to contain links to HashTrees if that helps.
 -}
data PathsByHash = Map Hash [FilePath]
  deriving (Eq, Read, Show)

----------------
-- scan files --
----------------

hashFile :: FilePath -> IO Hash
hashFile path = undefined

hashTree :: AnchoredDirTree Hash -> HashTree
hashTree = undefined

scan :: FilePath -> IO HashTree
scan path = readDirectoryWith hashFile path >>= return . hashTree

----------
-- main --
----------

patterns :: Docopt
patterns = [docoptFile|usage.txt|]

parseConfig :: FilePath -> IO Config
parseConfig path = undefined

runCommand :: Config -> IO ()
runCommand cfg = case command cfg of
  "scan"   -> scan ((annexDir cfg) </> (dedupDir cfg)) >> return ()
  "add"    -> undefined
  "verify" -> undefined
  "fsck"   -> undefined
  "gc"     -> undefined
  _        -> putStrLn $ "no such command: " ++ command cfg

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  print args
  putStrLn "Hello, Haskell!"
