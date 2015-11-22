module Main where

import Data.Monoid

import Core
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
ftb = velocity 80 (s16 >> s16 >> s16 >> s32 >> s32
   >> r16 >> s32 >> r32 >> r16 >> s32 >> s32
   >> s16 >> r16 >> s32 >> r32 >> s32 >> s32
   >> r16 >> s32 >> r32 >> s32 >> s32 >> r16)
   <> velocity 80 (rest
   >> t16 >> r32 >> t32 >> r16 >> t32 >> t32
   >> r16 >> t16 >> r32 >> t32 >> r16
   >> t16 >> r32 >> t32 >> r16 >> t32 >> t32)
   <> clone 2 (note 2 $ (velocity 127 bass2))
  where
    s16 = note 16 $ hiHat
    s32 = note 32 $ hiHat
    r16 = note 16 $ rest
    r32 = note 32 $ rest
    t16 = note 16 $ snare
    t32 = note 32 $ snare

dec :: Song -> Song
dec instr = do
  velocity 125 instr
  velocity 110 instr
  velocity 95  instr
  velocity 80  instr
  velocity 65  instr
  velocity 50  instr
  velocity 35  instr
  velocity 20  instr
  velocity 35  instr
  velocity 50  instr
  velocity 65  instr
  velocity 80  instr
  velocity 95  instr
  velocity 110 instr
  velocity 125 instr

cresc_decresc :: Song
cresc_decresc = do
  sequence_ $ zipWith velocity [20..110] (repeat $ note 32 snare)
  sequence_ $ zipWith velocity [110,109..20] (repeat $ note 32 snare)

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
    b = note 1 $ bass2

house :: Song
house = mconcat [ orbit (dot $ note 8 $ rest >> hiHat)
                , orbit (note 8 $ rest >> hiHat >> hiHat >> hiHat >> hiHat)
                , orbit bass2
                ]

sample :: Song
sample = sequence_ $ map (scaleBPM 4 . dec)
    [ bass2, bass, stick, snare, snare2, hiHat, crash, ride, cow
    , hiTom, tamb, clap, loTom2, clHat, loTom, pedal, midTom
    , opHat, midTom2, hiTom2, chinese, rideBl, splash, crash2
    , slap , ride2, hiBongo, loBongo, muConga, opConga, loConga
    , hiTimb, loTimb, hiAgogo, loAgogo, cabasa, maracas, sWhistl
    , lWhistl, sGuiro, lGuiro, claves, hiWdBlk, loWdBlk, muCuica
    , opCuica, muTrngl, opTrngl
    ]

-- | Intro to 'Toxicity' by System of a Down.
--   Demonstrates use of do, vs '>>' vs sequence_ [...]
toxicityIntro :: Song
toxicityIntro = do
  note 8 (velocity 127 bass2)
  note 16 $ do
    sh >> bass2 >> rest >> bass2
    -- Instead of:
    -- sh >> rest >> bass2 >> rest
    -- We can always use sequence_ if we want to create a list of songs
    -- so that we can perhaps map over it.
    sequence_ [sh, rest, bass2, rest]
    sh >> rest
    riff1
  clone 4 (note 32 snare)
  note 16 $ do
    riff1
    cd >> rest >> hiHat >> snare
    riff2
    hiHat >> snare >> bc >> rest
    bc >> rest >> hiHat >> sh
    riff2
    bass2 >> rest >> sh >> rest
  where
    sh = snare <> hiHat
    bc = bass2  <> hiHat
    cd = bass2  <> crash
    riff1 = do
      clone 2 snare
      clone 2 hiTom
      clone 2 hiTom2
    riff2 = hiHat >> bass2 >> sh >> rest

main :: IO ()
main = play trap 220
