{-# LANGUAGE OverloadedStrings #-}

-- this looks super useful, but interestingly:
-- > on Windows or OSX platformFormat will be Text, and on Linux it will be ByteString.
import qualified Filesystem.Path.CurrentOS as OS

{- The idea is that we'll have to read and write to the filesystem in the
 - currentOS format (either Text or ByteString, but we won't know which), and
 - convert that to Text internally (or bytestring UTF8 later?).
 -
 - Probably the best way forward is to make n2os and os2n functions.
 -}

-- import qualified Data.ByteString.Unicode as BU

import Test.QuickCheck
import Util
import FileNameTest
import Control.Monad

-- import qualified Data.ByteString.Unicode as BU
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  filenames <- generate (arbitrary :: Gen [FileName])
  forM_ filenames $ \f -> do

    let humanReadable = OS.fromText f
    putStrLn $ "humanReadable: " ++ show humanReadable
    return ()

    -- putStrLn $ "f' " ++ show f'

    -- if OS.valid f'
      -- then do
        -- putStrLn $ "valid: " ++ show f'
        -- writeFile (OS.encode f') "this is a test"
      -- else putStrLn $ "INVALID: " ++ show f'
  -- putStrLn $ "toText /tmp: '" ++ (OS.encodeString "/tmp") ++ "'"

  return ()

-- writeDiabolicalFile :: FilePath -> IO FilePath
-- writeDiabolicalFile dir = do
--   basename <- fmap n2p $ generate (arbitrary :: Gen FileName)
--   let path = dir </> basename
--   -- putStrLn $ "writing diabolical file: '" ++ path ++ "'"
--   writeFile path "this is a test"
--   return path

n2os :: FileName -> FilePath
n2os = fromText

os2n :: FilePath -> FileName
os2n = toText
