module Example where

import Data.Monoid

import Compose
import Interpret
import Drum
import Play

-- Use >> to sequence
-- Use <> to play in parallel.
beat1 :: Song
beat1 = (hiHat >> hiHat >> hiHat >> hiHat) <> (bass >> snare >> bass >> snare)

-- Demonstrate do notation for sequencing.
-- 'orbit' make an infinite sequence.
bs :: Song
bs = do
   bass
   snare
   bass
   snare
   bass
   n8 snare
   dot bass
   snare

test :: Song
test = crash >> crash

h8, h12, trill, hats :: Song
h8    = clone 8 (n8 hiHat)
h12   = clone 12 (n8 hiHat)
trill = clone 8 (n16 hiHat)
hats  = do
  h8
  trill
  h12
  trill
  hiHat
  hiHat
  hiHat

trap :: Song
trap = hats <> (n1 bass >> n1 snare >> n1 bass >> n1 snare) >> crash >> ride

house :: Song
house = mconcat [ orbit (dot $ n8 rest >> hiHat)
                , orbit (n8 rest >> hiHat >> hiHat >> hiHat >> hiHat)
                , orbit bass
                ]

sample :: Song
sample = do
    bass2
    bass
    stick
    snare
    snare2
    hiHat
    crash
    ride
    cow
    hiTom
    tamb
    clap
    loTom2
    clHat
    loTom
    pedal
    midTom
    opHat
    midTom2
    hiTom2
    chinese
    rideBl
    splash
    crash2
    slap
    ride2
    hiBongo
    loBongo
    muConga
    opConga
    loConga
    hiTimb
    loTimb
    hiAgogo
    loAgogo
    cabasa
    maracas
    sWhistl
    lWhistl
    sGuiro
    lGuiro
    claves
    hiWdBlk
    loWdBlk
    muCuica
    opCuica
    muTrngl
    opTrngl
