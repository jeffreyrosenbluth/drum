--------------------------------------------------------------------------------
-- |
-- Module      :  DSeq
-- Author      : Jeffrey Rosenbluth
-- Copyright   : (c) 2015, Jeffrey Rosenbluth
--
-- A Drum Maching Micro-Language, base on
-- http://www.csounds.com/journal/issue8/dseq.html
-- by Jacob Joaquin
-- CSOUND JOURNAL: Home | issue8
--------------------------------------------------------------------------------

module Dseq
  ( dseq
  ) where

import Core
import Drum
import Control.Monad (zipWithM_)

dseq :: Sound -> Rational -> [Char] -> Song
dseq s n cs = zipWithM_ vel vs ts
  where
    vs = map toVol (filter (`elem` ".0123456789") cs)
    ts = repeat $ note n (atom s)

toVol :: Char -> Rational
toVol '.' = 0
toVol '0' = 20
toVol '1' = 40
toVol '2' = 50
toVol '3' = 60
toVol '4' = 70
toVol '5' = 80
toVol '6' = 90
toVol '7' = 100
toVol '8' = 110
toVol '9' = 120
toVol  _  = error "The sky is falling"
