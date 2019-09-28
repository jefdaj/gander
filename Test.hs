module Main where

import Test.Hspec
import System.Process
import System.Exit

main :: IO ()
main = hspec $ do
  describe "dependencies installed?" $ do
    testDep "git"       ["--help"]
    testDep "rsync"     ["-h"]
    testDep "git-annex" ["-h"]

testDep :: FilePath -> [String] -> Spec
testDep name args = it name $ do
  (code, _, _) <- readProcessWithExitCode name args ""
  code `shouldBe` ExitSuccess
