module Example where

import Data.Monoid

import Types
import Interpret
import Drum
import Play

-- Use >> to sequence, <> to play in parallel.
beat1 = (hiHat >> hiHat >> hiHat >> hiHat) <> (bass >> snare >> bass >> snare)

-- Demonstrate do notation for sequencing.
-- 'orbit' make an infinite sequence.
bs = orbit $ do
   bass
   snare
   bass
   snare
   bass
   n8 snare
   dot bass
   snare

h8 = clone 8 (n8 hiHat)

h12 = clone 12 (n8 hiHat)

trill = clone 8 (n16 hiHat)

hats = do
  h8
  trill
  h12
  trill
  hiHat
  hiHat
  hiHat

trap = hats <> (n1 bass >> n1 snare >> n1 bass >> n1 snare) >> crash >> ride

house = mconcat [ orbit (dot $ n8 rest >> hiHat)
                , orbit (n8 rest >> hiHat >> hiHat >> hiHat >> hiHat)
                , orbit bass
                ]
