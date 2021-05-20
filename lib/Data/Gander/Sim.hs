module Data.Gander.Sim
  -- , safeDelta
  -- , safeDeltas
  ( simDelta
  , simDeltas
  , assertSameTrees -- TODO rename to mention diff
  )
  where

import Data.Gander.HashTree
import Data.Gander.Delta
import Control.Monad (when, foldM)
import Data.Maybe (fromJust)

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

-- TODO think through how to report results more!
simDelta :: ProdTree -> Delta () -> Either String (ProdTree)
simDelta t (Rm   p    ) = rmSubTree t p
simDelta t (Add  p  t2) = Right $ addSubTree t t2 p
simDelta t (Edit p _ t2) = Right $ addSubTree t t2 p
simDelta t (Mv   p1 p2) = case simDelta t (Rm p1) of
  Left  e  -> Left e
  Right t2 -> simDelta t2 $ Add p2 $ fromJust $ dropTo t p1 -- TODO path error here?

simDeltas :: ProdTree -> [Delta ()] -> Either String (ProdTree)
simDeltas = foldM simDelta

-- seems like what we really want is runDeltaIfSafe, which does simDelta, checks safety, then runDelta

-- TODO be clearer on before/after and or expected/actual here
-- assertSameTrees :: FilePath -> HashTree -> HashTree -> IO ()
assertSameTrees :: (String, ProdTree) -> (String, ProdTree) -> IO ()
assertSameTrees (msg1, tree1) (msg2, tree2) = do
  let wrong = diff tree1 tree2
  when (not $ null wrong) $ do
    putStrLn $ unwords ["error!", msg1, "and", msg2, "should be identical, but aren't:"]
    printDeltas wrong
