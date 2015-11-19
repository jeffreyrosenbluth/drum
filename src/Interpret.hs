module Interpret
  ( interpret
  , mkSM
  ) where

import Compose
import Control.Lens
import Control.Monad.Reader
-------------------------------------------------------------------------------

par :: [Hit] -> [Hit] -> [Hit]
par [] ys = ys
par xs [] = xs
par (x:xs) (y:ys)
  | (x ^. dur) <= (y ^. dur) = x : par xs (y:ys)
  | otherwise                = y : par (x:xs) ys

totalDur :: Beat -> Int
totalDur (Prim hit)    = hit ^. dur
totalDur (Chain c1 c2) = totalDur c1 + totalDur c2
totalDur (Par c1 c2)   = max (totalDur c1) (totalDur c2)
totalDur None          = 0

interpret :: SongM a -> [Hit]
interpret comp = go 0 (execSongM comp)
  where
    go d (Prim   h)    = [h & dur .~ d]
    go d (Chain c1 c2) = go d c1 ++ go (d + totalDur c1) c2
    go d (Par   c1 c2) = go d c1 `par` go d c2
    go d None          = []

mkSM :: SongM a -> SongMonad a
mkSM sm = go $ unSongM sm
  where
    go (Prim h, a) = do
      d <- ask
      lift $ SongM (Prim (h & dur %~ (`div` d)), a)
    go (Chain s1 s2, a) = do
      go (s1, a)
      go (s2, a)
    go (Par s1 s2, a) = do
      d <- ask
      let (SongM (t1,_)) = runReaderT (go (s1, a)) d
      let (SongM (t2,_)) = runReaderT (go (s2, a)) d
      lift $ SongM (Par t1 t2, a)
    go (None, a) = return a
