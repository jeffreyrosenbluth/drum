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
bs = do
   bass
   snare
   bass
   snare
   bass
   n8 $ snare
   dot bass
   snare

simple :: Song
simple = clone 4 . scaleBPM 2
       $ (clone 4 clHat) <> (bass2 >> r4 >> snare >> r4)

doubleBass :: Song
doubleBass = clone 6 . scaleBPM 6 $ note 3 ( clHat <> (clone 3 bass2) )

ftb :: Song
ftb = scaleBPM (1/2) $ velocity 80 (s16 >> s16 >> s16 >> s32 >> s32
   >> r16 >> s32 >> r32 >> r16 >> s32 >> s32
   >> s16 >> r16 >> s32 >> r32 >> s32 >> s32
   >> r16 >> s32 >> r32 >> s32 >> s32 >> r16)
   <> velocity 80 (r4
   >> t16 >> r32 >> t32 >> r16 >> t32 >> t32
   >> r16 >> t16 >> r32 >> t32 >> r16
   >> t16 >> r32 >> t32 >> r16 >> t32 >> t32)
   <> clone 2 (n2 $ (velocity 127 bass2))
  where
    s16 = n16 $ sWhistl
    s32 = n32 $ opConga
    t16 = n16 $ snare
    t32 = n32 $ cow

wdm :: Song
wdm = clone 4 . scaleBPM 4 $
      (accent bass2 >> bass2 >> clone 6 r4 >> clone 2 (accent bass2 >> bass2) >> r4)
   <> (r1 >> accent snare >> r1 >> snare >> snare >> r4 >> accent snare >> clone 3 r4)
   <> (clone 4 (r4 >> hiHat >> accent hiHat >> hiHat))
   <> (clone 7 r4 >> accent loTom >> loTom >> accent loTom >> loTom >> clone 5 r4)
   <> (clone 14 r4 >> accent loTom2 >> r4)
   <> (clone 6 r4 >> accent hiTom >> hiTom >> clone 8 r4)
  --  where
  --    accent = velocity 127

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
  sequence_ $ zipWith velocity [20..110] (repeat $ n32 snare)
  sequence_ $ zipWith velocity [110,109..20] (repeat $ n32 snare)

h8, h12, trill, hats :: Song
h8    = clone 8 (n8 $ hiHat)
h12   = clone 12 (n8 $ hiHat)
trill = clone 8 (n16 $ hiHat)
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
    s = n1 $ snare
    b = n1 $ bass2

house :: Song
house = mconcat [ (dot $ n8 $ r4 >> hiHat)
                , (n8 $ r4 >> hiHat >> hiHat >> hiHat >> hiHat)
                , bass2
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
toxicityIntro = clone 3 $ do
  n8 (velocity 127 bass2)
  n16 $ do
    sh >> bass2 >> r4 >> bass2
    -- Instead of:
    -- sh >> r4 >> bass2 >> r4
    -- We can always use sequence_ if we want to create a list of songs
    -- so that we can perhaps map over it.
    sequence_ [sh, r4, bass2, r4]
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
    bass2 >> r4 >> sh >> r4
  where
    sh = snare <> hiHat
    bc = bass2  <> hiHat
    cd = bass2  <> crash
    riff1 = do
      clone 2 snare
      clone 2 hiTom
      clone 2 hiTom2
    riff2 = hiHat >> bass2 >> sh >> r4

icecube :: Song
icecube = dseq BassDrum1   8 "7... .... 7... .... 7... .... 7.77 .7.."
       <> dseq BassDrum2   8 ".... 7... .... 7... .... 7... .... 7..."
       <> dseq SnareDrum2  8 ".... 4... .... 4... .... 4... .... 4..."
       <> dseq ClosedHihat 8 "7.7. 7.77 .77. 7.77 7.7. 7.77 .77. ...."
       <> dseq OpenHihat   8 ".... .... .... .... .... .... .... .7.."

reed :: Song
reed = dseq BassDrum1   8 "7... 7... 7... 7..."
    <> dseq ClosedHihat 8 "..7. ..7. ..7. ..77"
    <> dseq RideCymbal1 8 "...5 .... .... ...."
    <> dseq HandClap    8 ".... .... .... ...5"

main :: IO ()
main = play' $ do
  beat1
  bs
  simple
  doubleBass
  ftb
  wdm
  cresc_decresc
  scaleBPM 2 $ trap
  house
  sample
  toxicityIntro
