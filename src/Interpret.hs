module Interpret
  ( interpret
  ) where

import Compose
import Control.Lens
import Control.Monad.Reader
-------------------------------------------------------------------------------
-- | Prepare a song and it's applyTempos to be played.
interpret :: Tempo -> Sequence a -> [Hit]
interpret t = toHits . runSequenceR t . applyTempo

par :: [Hit] -> [Hit] -> [Hit]
par [] ys = ys
par xs [] = xs
par (x:xs) (y:ys)
  | (x ^. dur) <= (y ^. dur) = x : par xs (y:ys)
  | otherwise                = y : par (x:xs) ys

totalDur :: Command -> Int
totalDur (Prim hit)    = hit ^. dur
totalDur (Chain b1 b2) = totalDur b1 + totalDur b2
totalDur (Par b1 b2)   = max (totalDur b1) (totalDur b2)
totalDur _             = 0

toHits :: Sequence a -> [Hit]
toHits comp = go 0 (execSequence comp)
  where
    go d (Prim h)      = [h & dur .~ d]
    go d (Chain b1 b2) = go d b1 ++ go (d + totalDur b1) b2
    go d (Par   b1 b2) = go d b1 `par` go d b2
    go _ _             = []

applyTempo :: Sequence a -> SequenceR a
applyTempo sm = go $ unSequence sm
  where
    go (Prim h, a) = do
      d <- ask
      lift $ seque (Prim (h & dur %~ (`div` d))) a
    go (Chain b1 b2, a) = do
      go (b1, a)
      go (b2, a)
    go (Par b1 b2, a) = do
      d <- ask
      let (Sequence (t1, b)) = runSequenceR d (go (b1, a))
      let (Sequence (t2, c)) = runSequenceR d (go (b2, a))
      lift $ seque (Par t1 t2) c
    go (None, a) = return a
