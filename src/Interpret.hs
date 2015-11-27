--------------------------------------------------------------------------------
-- |
-- Module      :  Interpret
-- Author      : Jeffrey Rosenbluth
-- Copyright   : (c) 2015, Jeffrey Rosenbluth
--
-- Convert 'drum' to Midi.
--
-------------------------------------------------------------------------------

module Interpret
  ( interpret
  ) where

import Core
import Control.Lens
import Control.Monad.Reader
-------------------------------------------------------------------------------
-- | Prepare a song and it's applyControls to be played.
interpret :: Control -> Beat a -> [Hit]
interpret t = toHits . runSequenceR t . applyControl

par :: [Hit] -> [Hit] -> [Hit]
par [] ys = ys
par xs [] = xs
par (x:xs) (y:ys)
  | (x ^. duration) <= (y ^. duration) = x : par xs (y:ys)
  | otherwise                = y : par (x:xs) ys

totalDur :: Command -> Rational
totalDur (Prim hit)    = hit ^. duration
totalDur (Chain b1 b2) = totalDur b1 + totalDur b2
totalDur (Par b1 b2)   = max (totalDur b1) (totalDur b2)
totalDur _             = 0

toHits :: Beat a -> [Hit]
toHits comp = go 0 (execBeat comp)
  where
    go d (Prim h)      = [h & duration .~ d]
    go d (Chain b1 b2) = go d b1 ++ go (d + totalDur b1) b2
    go d (Par   b1 b2) = go d b1 `par` go d b2
    go _ _             = []

applyControl :: Beat a -> SequenceR a
applyControl sm = go $ unBeat sm
  where
    go (Prim h, a) = do
      d <- ask
      let m = d ^. bpm * d ^. tempo
          v = h ^. velocity * d ^. level
          v' = max 0 (min v 127)
      lift $ beat (Prim (h & duration *~ (60000 / m)
                           & velocity .~  v)) a
    go (Chain b1 b2, a) = do
      go (b1, a)
      go (b2, a)
    go (Par b1 b2, a) = do
      d <- ask
      let (Beat (t1, b)) = runSequenceR d (go (b1, a))
      let (Beat (t2, c)) = runSequenceR d (go (b2, a))
      lift $ beat (Par t1 t2) c
    go (Tempo x b, a) = do
      local (\c -> c & tempo *~ x) (go (b, a))
    go (Level x b, a) =
      local (\c -> c & level *~ x) (go (b, a))
    go (None, a) = return a
