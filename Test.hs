module Main where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Ingredients.Rerun

main :: IO ()
main = defaultMainWithIngredients [rerunningTests [consoleTestReporter]] tests

tests :: TestTree
tests = testGroup "Tests" []
