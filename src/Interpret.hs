module Interpret
  ( interpret
  ) where

import Compose
import Control.Lens
-------------------------------------------------------------------------------

par :: [Hit] -> [Hit] -> [Hit]
par [] ys = ys
par xs [] = xs
par (x:xs) (y:ys)
  | (x ^. dur) <= (y ^. dur) = x : par xs (y:ys)
  | otherwise                = y : par (x:xs) ys

totalDur :: Composition -> Int
totalDur (Prim hit)    = hit ^. dur
totalDur (Chain c1 c2) = totalDur c1 + totalDur c2
totalDur (Par c1 c2)   = max (totalDur c1) (totalDur c2)

interpret :: Compose a -> [Hit]
interpret comp = go 0 (execCompose comp)
  where
    go d (Prim   h)    = [h & dur .~ d]
    go d (Chain c1 c2) = go d c1 ++ go (d + totalDur c1) c2
    go d (Par   c1 c2) = go d c1 `par` go d c2
