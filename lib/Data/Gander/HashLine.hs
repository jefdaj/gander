{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Gander.HashLine
  ( TreeType(..)
  , IndentLevel(..)
  , HashLine(..)
  , prettyHashLine
  , parseHashes
  -- for testing
  , nameP
  , lineP
  , linesP
  )
  where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff

-- import Debug.Trace

import Data.Gander.Hash

import qualified Data.ByteString.Char8 as B8 -- TODO switch to Word8?
import qualified Data.ByteString.Short as BS
import qualified Data.Text.Encoding as T

import Util (pathComponents, FileName, p2n, n2p, FileName(..))
import qualified System.Directory.Tree as DT

import Control.Monad        (msum)
import qualified Control.Monad.Parallel as P
import qualified Control.Monad          as M
import Data.List            (find, delete, sort)
import Data.Maybe           (isJust, catMaybes)
import Data.Function        (on)
import Data.List            (partition, sortBy)
import Data.Either          (fromRight)
import Data.Ord             (compare)
import System.Directory     (doesFileExist, doesDirectoryExist)
import System.FilePath      ((</>), splitPath, joinPath)
import System.FilePath.Glob (matchWith, Pattern, MatchOptions(..))
import System.IO            (hFlush, stdout, withFile, IOMode(..))
import System.IO.Unsafe     (unsafeInterleaveIO)

import Prelude hiding (take)
import Data.Attoparsec.ByteString.Char8 hiding (D, skipWhile)
import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.Combinator

import Data.Store             (encode, decodeIO, Store(..))
import Control.Exception.Safe (catchAny)
import TH.Derive

import Control.DeepSeq

-- for distinguishing beween files and dirs
data TreeType = D | F
  deriving (Eq, Read, Show, Ord)

instance NFData TreeType
  where rnf = const () -- TODO is this valid?

$($(derive [d| instance Deriving (Store TreeType) |]))

newtype IndentLevel = IndentLevel Int
  deriving (Read, Show, Eq, Ord)

-- TODO make a skip type here, or in hashtree?
-- TODO remove the tuple part now?
newtype HashLine = HashLine (TreeType, IndentLevel, Hash, FileName)
  deriving (Read, Show, Eq, Ord)

-- TODO actual Pretty instance
-- TODO avoid encoding as UTF-8 if possible; use actual bytestring directly
-- note: p can have weird characters, so it should be handled only as ByteString
prettyHashLine :: HashLine -> B8.ByteString
prettyHashLine (HashLine (t, (IndentLevel n), h, FileName p)) = B8.unwords
  [B8.pack $ show t, B8.pack $ show n, prettyHash h, T.encodeUtf8 p] -- TODO mismatch with n2p, p2n?

typeP :: Parser TreeType
typeP = do
  t <- choice [char 'D', char 'F'] <* char ' '
  return $ read [t]

hashP :: Parser Hash
hashP = do
  h <- take digestLength -- TODO any need to sanitize these?
  _ <- char ' '
  return $ Hash $ BS.toShort h

{- Like endOfLine, but make sure D/F comes next followed by a valid hash digest
 - instead of the rest of a filename. This catches the rare case where a
 - filename contains a newline followed by D or F. You could still construct a
 - filename that would fool it, but it would be extremely unlikely to happen by
 - chance.
 - TODO can it use null-separated lines instead like -print0?
 -}
breakP :: Parser ()
breakP = endOfLine >> choice [typeP >> indentP >> hashP >> return (), endOfInput]

-- TODO should anyChar be anything except forward slash and the null char?
nameP :: Parser FileName
nameP = fmap p2n $ do
  c  <- anyChar
  cs <- manyTill anyChar $ lookAhead breakP
  return (c:cs)

indentP :: Parser IndentLevel
indentP = do
  n <- manyTill digit $ char ' '
  -- TODO char ' ' here?
  return $ IndentLevel $ read n

-- TODO is there a cleaner syntax for this?
-- TODO this should still count up total files when given a max depth
lineP :: Maybe Int -> Parser (Maybe HashLine)
lineP md = do
  t <- typeP
  (IndentLevel i) <- indentP
  case md of
    Nothing -> parseTheRest t (IndentLevel i)
    Just d -> do
      if i > d
        then do
          skipWhile (not . isEndOfLine)
          lookAhead breakP
          return Nothing
        else parseTheRest t (IndentLevel i)
  where
    parseTheRest t i = do
      h <- hashP
      p <- nameP
      -- return $ trace ("finished: " ++ show (t, i, h, p)) $ Just (t, i, h, p)
      return $ Just (HashLine (t, i, h, p))

linesP :: Maybe Int -> Parser [HashLine]
linesP md = do
  hls <- sepBy' (lineP md) endOfLine
  return $ catMaybes hls -- TODO count skipped lines here?

fileP :: Maybe Int -> Parser [HashLine]
fileP md = linesP md <* endOfLine <* endOfInput

-- TODO use bytestring the whole time rather than converting
-- TODO should this propogate the Either?
-- TODO any more elegant way to make the parsing strict?
parseHashes :: Maybe Int -> B8.ByteString -> [HashLine]
parseHashes md = fromRight [] . parseOnly (fileP md)
