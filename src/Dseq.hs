module Dseq
  ( dseq
  ) where

import Core
import Drum
import Control.Monad (zipWithM_)

dseq :: Sound -> Rational -> [Char] -> Song
dseq s n cs = zipWithM_ velocity vs ts
  where
    vs = map toVol (filter (`elem` ".0123456789") cs)
    ts = repeat $ note n (atom s)

toVol :: Char -> Rational
toVol '.' = 0
toVol '0' = 10
toVol '1' = 20
toVol '2' = 35
toVol '3' = 50
toVol '4' = 66
toVol '5' = 80
toVol '6' = 92
toVol '7' = 104
toVol '8' = 116
toVol '9' = 127
toVol  _  = error "The sky is falling"
