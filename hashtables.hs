module Main where

import qualified Data.HashTable.IO as H

-- looks like any of them should work:
-- type HashTable k v = H.BasicHashTable k v
type HashTable k v = H.CuckooHashTable k v

foo :: IO (HashTable Int Int)
foo = do
    ht <- H.new
    H.insert ht 1 1
    return ht

main :: IO ()
main = do
  f <- foo
  l <- H.toList f
  print l
