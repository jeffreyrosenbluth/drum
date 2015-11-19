module Interpret
  ( interpret
  ) where

import Compose
import Control.Lens
import Control.Monad.State
-------------------------------------------------------------------------------
interpret :: Tempo -> Song a -> [Hit]
interpret t = toHits . runDrumMachine t . adjustDurs

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
totalDur (BPM _)       = 0
totalDur None          = 0

toHits :: Song a -> [Hit]
toHits comp = go 0 (execSong comp)
  where
    go d (Prim h)      = [h & dur .~ d]
    go d (Chain b1 b2) = go d b1 ++ go (d + totalDur b1) b2
    go d (Par   b1 b2) = go d b1 `par` go d b2
    go d (BPM _)       = []
    go d None          = []

adjustDurs :: Song a -> DrumMachine a
adjustDurs sm = go $ unSong sm
  where
    go (Prim h, a) = do
      d <- get
      lift $ song (Prim (h & dur %~ (`div` d))) a
    go (Chain b1 b2, a) = do
      go (b1, a)
      go (b2, a)
    go (Par b1 b2, a) = do
      d <- get
      let (Song (t1, b)) = runDrumMachine d (go (b1, a))
      let (Song (t2, c)) = runDrumMachine d (go (b2, a))
      lift $ song (Par t1 t2) c
    go (BPM n, a)  = do
       put n
       return a
    go (None, a) = return a
