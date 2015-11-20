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
   note 8 $ snare
   dot bass
   snare

ftb :: Song
ftb = bpm 89 >> level 64 >>
     (s16 >> s16 >> s16 >> s32 >> s32
   >> r16 >> s32 >> r32 >> r16 >> s32 >> s32
   >> s16 >> r16 >> s32 >> r32 >> s32 >> s32
   >> r16 >> s32 >> r32 >> s32 >> s32 >> r16)
   <> (rest
   >> t16 >> r32 >> t32 >> r16 >> t32 >> t32
   >> r16 >> t16 >> r32 >> t32 >> r16
   >> t16 >> r32 >> t32 >> r16 >> t32 >> t32)
   <> clone 2 (note 2 $ bass)
  where
    s16 = note 16 $ hiHat
    s32 = note 32 $ hiHat
    r16 = note 16 $ rest
    r32 = note 32 $ rest
    t16 = note 16 $ snare
    t32 = note 32 $ snare

test :: Song
test = level 50 >> crash >> level 127 >> crash

h8, h12, trill, hats :: Song
h8    = clone 8 (note 8 $ hiHat)
h12   = clone 12 (note 8 $ hiHat)
trill = clone 8 (note 16 $ hiHat)
hats  = do
  h8
  trill
  h12
  trill
  clone 3 hiHat

trap :: Song
trap = hats <> (b >> s >> b >> s)
            >> crash
            >> ride
  where
    s = note 1 $ snare
    b = note 1 $ bass

house :: Song
house = mconcat [ orbit (dot $ note 8 $ rest >> hiHat)
                , orbit (note 8 $ rest >> hiHat >> hiHat >> hiHat >> hiHat)
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
    bpm 500
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
    level 32
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
    bpm 120
    maracas
    sWhistl
    lWhistl
    sGuiro
    lGuiro
    claves
    level 100
    hiWdBlk
    loWdBlk
    muCuica
    opCuica
    muTrngl
    opTrngl
