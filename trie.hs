{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Trie as T
import qualified Data.ByteString.Char8 as B

test :: T.Trie B.ByteString
test = T.fromList
  [ ("key1", "value1")
  ]

main :: IO ()
main = do
  print test
