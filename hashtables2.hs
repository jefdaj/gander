{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Control.Monad.ST
import Data.Text

type HashTable s k v = C.HashTable s k v

foo :: ST s (HashTable s Text Int)
foo = do
  ht <- H.newSized 1 -- TODO calculate size from tree beforehand? efficient, but no streaming :(
  H.insert ht "abc" 1
  H.insert ht "dea" 3
  return ht

add1 :: HashTable s Text Int -> ST s (HashTable s Text Int)
add1 ht = do
  H.insert ht "abc" 2
  H.insert ht "dea" 4
  return ht

main :: IO ()
main = do
  let x = runST $ H.toList =<< foo
  print x
  let y = runST $ H.toList =<< add1 =<< foo
  print y
