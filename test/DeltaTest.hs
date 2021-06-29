{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module DeltaTest where

-- I seem to have accidentally written something like an "HashTreeAction",
-- as described here:
--
-- https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html
--
-- So I might as well add the rest of the scaffolding to use it in the
-- HashTree Arbitrary instance.
--
-- TODO also rename it to HashTreeAction?

import Data.Gander.Delta
import Data.Gander.HashTree
import HashTreeTest -- TODO oh no, is this a cyclic dependency?

import Test.QuickCheck
-- import Test.QuickCheck.Monadic
import           Control.Applicative     ((<$>), (<*>))

import Data.ByteString.Char8 as B8

type TestDelta = Delta B8.ByteString

deriving instance Arbitrary TestDelta
