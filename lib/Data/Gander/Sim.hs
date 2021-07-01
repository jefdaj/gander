module Data.Gander.Sim
  -- , safeDelta
  -- , safeDeltas
  ( simDelta
  , simDeltaForest
  , simDeltas
  , simDeltasForest
  , assertSameTrees -- TODO rename to mention diff
  , findMatchingTreeIndex
  )
  where

-- import Debug.Trace

import Data.Gander.HashTree
import Data.Gander.HashForest
import Data.Gander.Delta
import Control.Monad (when, foldM)
import Data.Maybe (fromJust)
import Util (n2p, replaceNth)
import Data.List (findIndex, delete)

-- TODO remove Deltas from the name? Run doesn't have them. Or add it like RunDeltas

--------------------------------------------
-- check if simulated operations are safe --
--------------------------------------------

-- TODO think through how to report results more!
-- TODO can this whole thing be trivially written in runDeltaIfSafe?

-- Can a delta be applied without losing anything?
-- TODO for efficiency, should this be part of a larger "applyIfSafe"?
--      (that would return the updated tree at the same time)
-- TODO in order to apply, need actual tree rather than just the hash!
-- safeDelta :: HashTree -> Delta -> Bool
-- safeDelta t d = safeDeltas t [d]
-- 
-- safeDeltas :: HashTree -> [Delta] -> Bool
-- safeDeltas t ds = case simDeltas t ds of
--   Left  _  -> False
--   Right t2 -> null $ listLostFiles t t2

-----------------------------
-- simulate git operations --
-----------------------------

findMatchingTreeIndex :: Show a => HashForest a -> FilePath -> Maybe Int
findMatchingTreeIndex (HashForest trees) path = findIndex (\t -> n2p (name t) == topDir path) trees

simDelta :: Show a => HashTree a -> Delta a -> Either String (HashTree a)
simDelta t d@(Rm   p     ) = rmSubTree t p
simDelta t (Edit p t1 t2)
  | t == t1 = Right t2
  | otherwise = Right $ addSubTree t t2 p
simDelta t (Add  p   t2) = Right $ addSubTree t t2 p
simDelta t (Mv   p1  p2) = case simDelta t (Rm p1) of
  Left  e  -> Left e
  Right t2 -> simDelta t2 $ Add p2 $ fromJust $ dropTo t p1 -- TODO path error here?

simDeltas :: Show a => HashTree a -> [Delta a] -> Either String (HashTree a)
simDeltas = foldM simDelta

-- TODO remove Show constraint once done debugging?
simDeltaForest :: Show a => HashForest a -> Delta a -> Either String (HashForest a)
simDeltaForest f@(HashForest ts) d = case findMatchingTreeIndex f (deltaName d) of
  Nothing -> case d of
               (Add p t) -> let t' = if n2p (name t) == p then t else wrapInEmptyDirs p t
                            in Right $ HashForest (t':ts) -- TODO order doesn't matter?
               _ -> Left $ "no such tree: '" ++ deltaName d ++ "'"
  Just i -> case d of
              (Rm p) -> let t = ts !! i
                        in if p == n2p (name t)
                             then Right $ HashForest $ delete t ts
                             else simDeltaForest' ts d i
              _ -> simDeltaForest' ts d i

simDeltaForest' :: Show a => [HashTree a] -> Delta a -> Int -> Either String (HashForest a)
simDeltaForest' ts d i = do
  t' <- simDelta (ts !! i) d
  return $ HashForest $ replaceNth i t' ts

simDeltasForest :: Show a => HashForest a -> [Delta a] -> Either String (HashForest a)
simDeltasForest = foldM simDeltaForest

-- seems like what we really want is runDeltaIfSafe, which does simDelta, checks safety, then runDelta

-- TODO be clearer on before/after and or expected/actual here
-- assertSameTrees :: FilePath -> HashTree -> HashTree -> IO ()
assertSameTrees :: Show a => (String, HashTree a) -> (String, HashTree a) -> IO ()
assertSameTrees (msg1, tree1) (msg2, tree2) = do
  let wrong = diff tree1 tree2
  when (not $ null wrong) $ do
    putStrLn $ unwords ["error!", msg1, "and", msg2, "should be identical, but aren't:"]
    printDeltas wrong
