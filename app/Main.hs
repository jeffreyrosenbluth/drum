--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      : Jeffrey Rosenbluth
-- Copyright   : (c) 2015, Jeffrey Rosenbluth
--
-- Example drum beats.
--
-------------------------------------------------------------------------------
module Main where

import Data.Monoid

import Core
import Interpret
import Drum
import Dseq
import Play

-- Use >> to sequence
-- Use <> to play in parallel.
beat1 :: Song
beat1 = clone 4 . note 8
      $ (hiHat >> hiHat >> hiHat >> hiHat)
     <> (bass >> snare >> bass >> snare)

-- Demonstrate do notation for sequencing.
-- 'orbit' make an infinite sequence.
bs :: Song
bs = scaleBPM 1.5 . clone 3 $ do
   bass
   snare
   bass
   snare
   bass
   n8 $ snare
   dot bass
   snare

simple :: Song
simple = clone 8 . scaleBPM 2
       $ (clone 4 clHat) <> (bass >> r4 >> snare >> r4)

ftb :: Song
ftb = scaleBPM (1/2) . clone 3 $ velocity 80 (s16 >> s16 >> s16 >> s32 >> s32
   >> r16 >> s32 >> r32 >> r16 >> s32 >> s32
   >> s16 >> r16 >> s32 >> r32 >> s32 >> s32
   >> r16 >> s32 >> r32 >> s32 >> s32 >> r16)
   <> velocity 80 (r4
   >> t16 >> r32 >> t32 >> r16 >> t32 >> t32
   >> r16 >> t16 >> r32 >> t32 >> r16
   >> t16 >> r32 >> t32 >> r16 >> t32 >> t32)
   <> clone 2 (n2 $ (velocity 127 bass))
  where
    s16 = n16 $ sWhistl
    s32 = n32 $ opConga
    t16 = n16 $ snare
    t32 = n32 $ cow

wdm :: Song
wdm = clone 4 . scaleBPM 4 $
      (accent bass >> bass >> clone 6 r4 >> clone 2 (accent bass >> bass) >> r4)
   <> (r1 >> accent snare >> r1 >> snare >> snare >> r4 >> accent snare >> clone 3 r4)
   <> (clone 4 (r4 >> hiHat >> accent hiHat >> hiHat))
   <> (clone 7 r4 >> accent loTom >> loTom >> accent loTom >> loTom >> clone 5 r4)
   <> (clone 14 r4 >> accent loTom2 >> r4)
   <> (clone 6 r4 >> accent hiTom >> hiTom >> clone 8 r4)
  --  where
  --    accent = velocity 127

dec :: Song -> Song
dec instr = do
  velocity 120 instr
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
  velocity 120 instr

cresc_decresc :: Song
cresc_decresc = do
  sequence_ $ zipWith velocity [20..110] (repeat $ n32 snare)
  sequence_ $ zipWith velocity [110,109..20] (repeat $ n32 snare)

hats :: Song
hats  = do
  h8
  trill
  h12
  trill
  clone 3 hiHat
  where
    h8    = clone 8 (n8 $ hiHat)
    h12   = clone 12 (n8 $ hiHat)
    trill = clone 8 (n16 $ hiHat)

trap :: Song
trap = clone 2 $ hats <> (b >> s >> b >> s)
  where
    s = n1 $ snare
    b = n1 $ bass

sample :: Song
sample = sequence_ $ map (scaleBPM 4 . dec)
    [ bass, bass, stick, snare, snare2, hiHat, crash, ride, cow
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
toxicityIntro = clone 3 $ do
  n8 (velocity 127 bass)
  n16 $ do
    sh >> bass >> r4 >> bass
    -- Instead of:
    -- sh >> r4 >> bass >> r4
    -- We can always use sequence_ if we want to create a list of songs
    -- so that we can perhaps map over it.
    sequence_ [sh, r4, bass, r4]
    sh >> r4
    riff1
  clone 4 (n32 snare)
  n16 $ do
    riff1
    cd >> r4 >> hiHat >> snare
    riff2
    hiHat >> snare >> bc >> r4
    bc >> r4 >> hiHat >> sh
    riff2
    bass >> r4 >> sh >> r4
  where
    sh = snare <> hiHat
    bc = bass  <> hiHat
    cd = bass  <> crash
    riff1 = do
      clone 2 snare
      clone 2 hiTom
      clone 2 hiTom2
    riff2 = hiHat >> bass >> sh >> r4


-- | Examples using dseq language.
icecube :: Song
icecube = scaleBPM 1.75 . clone 2
        $ dseq BassDrum1   8 "7... .... 7... .... 7... .... 7.77 .7.."
       <> dseq BassDrum2   8 ".... 7... .... 7... .... 7... .... 7..."
       <> dseq SnareDrum2  8 ".... 4... .... 4... .... 4... .... 4..."
       <> dseq ClosedHihat 8 "7.7. 7.77 .77. 7.77 7.7. 7.77 .77. ...."
       <> dseq OpenHihat   8 ".... .... .... .... .... .... .... .7.."

reed :: Song
reed = scaleBPM 1.75 . clone 4
     $ dseq BassDrum1   8 "7... 7... 7... 7..."
    <> dseq ClosedHihat 8 "..7. ..7. ..7. ..77"
    <> dseq RideCymbal1 8 "...5 .... .... ...."
    <> dseq HandClap    8 ".... .... .... ...5"

trips :: Song
trips = clone 2
      $ dseq BassDrum1    8 "7...   7...   7...   7..."
     <> dseq ClosedHihat 12 "955955 955955 955955 955955"

funkyDrummer :: Song
funkyDrummer = clone 4
             $ dseq BassDrum1   8 "5.5...5...5..5.."
            <> dseq SnareDrum1  8 "....3..3.3.33..3"
            <> dseq ClosedHihat 8 "5555555.55555.55"
            <> dseq OpenHihat   8 ".......7.....7.."

hiphop :: Song
hiphop = scaleBPM 1.5 . clone 2
       $ dseq BassDrum1   12 "9.5..7 ...... 9..9.. .....7 ...9.. ...7.. ..7..5 ......"
      <> dseq SnareDrum1  12 "...... 9..... ...... 9..... ...... 9..... ...... 9..7.7"
      <> dseq PedalHihat  12 "9..9.. 9.9... 9..... 9....9 ...9.. 9....9 ...9.. 9..9.."
      <> dseq ClosedHihat 12 "9.9... 9..5.. 9..5.9 ...5.. 9..5.. 9..5.. 9.5... 9..5.."
      <> dseq Tambourine  12 "7.59.5 7.59.5 7.59.5 7.59.5 7.59.5 7.59.5 7.59.5 7.59.5"


main :: IO ()
main = play $ do
  beat1         >> sep
  bs            >> sep
  simple        >> sep
  ftb           >> sep
  wdm           >> sep
  cresc_decresc >> sep
  trap          >> sep
  toxicityIntro >> sep
  icecube       >> sep
  reed          >> sep
  funkyDrummer  >> sep
  hiphop
  where
    sep = clone 4 stick
