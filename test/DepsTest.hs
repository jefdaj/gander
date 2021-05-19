module DepsTest  where

import Test.Hspec
import System.Process
import System.Exit

testDep :: FilePath -> [String] -> Spec
testDep name args = it (unwords $ name:args) $ do
  (code, _, _) <- readProcessWithExitCode name args ""
  code `shouldBe` ExitSuccess

spec_make_sure_git_is_installed :: Spec
spec_make_sure_git_is_installed = testDep "git" ["--help"]

spec_make_sure_rsync_is_installed :: Spec
spec_make_sure_rsync_is_installed = testDep "rsync" ["-h"]

spec_make_sure_git_annex_is_installed :: Spec
spec_make_sure_git_annex_is_installed = testDep "git-annex" ["-h"]
