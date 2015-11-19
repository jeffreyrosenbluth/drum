module Interpret
  ( interpret
  ) where

import Compose
import Control.Lens
import Control.Monad.Reader
-------------------------------------------------------------------------------
interpret :: Tempo -> SongM a -> [Hit]
interpret t = toHits . runSongMonad t . adjustDurs

par :: [Hit] -> [Hit] -> [Hit]
par [] ys = ys
par xs [] = xs
par (x:xs) (y:ys)
  | (x ^. dur) <= (y ^. dur) = x : par xs (y:ys)
  | otherwise                = y : par (x:xs) ys

totalDur :: Beat -> Int
totalDur (Prim hit)    = hit ^. dur
totalDur (Chain b1 b2) = totalDur b1 + totalDur b2
totalDur (Par b1 b2)   = max (totalDur b1) (totalDur b2)
totalDur BPM           = 0
totalDur None          = 0

toHits :: SongM a -> [Hit]
toHits comp = go 0 (execSongM comp)
  where
    go d (Prim h)      = [h & dur .~ d]
    go d (Chain b1 b2) = go d b1 ++ go (d + totalDur b1) b2
    go d (Par   b1 b2) = go d b1 `par` go d b2
    go d BPM           = []
    go d None          = []

adjustDurs :: SongM a -> SongMonad a
adjustDurs sm = go $ unSongM sm
  where
    go (Prim h, a) = do
      d <- ask
      lift $ songM (Prim (h & dur %~ (`div` d))) a
    go (Chain b1 b2, a) = do
      go (b1, a)
      go (b2, a)
    go (Par b1 b2, a) = do
      d <- ask
      let (SongM (t1, b)) = runSongMonad d (go (b1, a))
      let (SongM (t2, c)) = runSongMonad d (go (b2, a))
      lift $ songM (Par t1 t2) c
    go (BPM, a)  = do
      local (const 250) (return a)
    go (None, a) = return a
