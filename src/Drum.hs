module Drum
  ( orbit
  , clone
  , scaleBPM
  , scaleVel
  , atom
  , note
  , dot
  , velocity
  , rest

  , bass2
  , bass
  , stick
  , snare
  , snare2
  , hiHat
  , crash
  , ride
  , cow
  , hiTom
  , tamb
  , clap
  , loTom2
  , clHat
  , loTom
  , pedal
  , midTom
  , opHat
  , midTom2
  , hiTom2
  , chinese
  , rideBl
  , splash
  , crash2
  , slap
  , ride2
  , hiBongo
  , loBongo
  , muConga
  , opConga
  , loConga
  , hiTimb, loTimb
  , hiAgogo
  , loAgogo
  , cabasa
  , maracas
  , sWhistl
  , lWhistl
  , sGuiro
  , lGuiro
  , claves
  , hiWdBlk
  , loWdBlk
  , muCuica
  , opCuica
  , muTrngl
  , opTrngl

  ) where

import Core
import Control.Lens
import Data.Ratio

quarter :: Rational
quarter = 60000

volume :: Rational
volume = 100

-- | Loop a song forever.
orbit :: Beat a -> Beat a
orbit b = b >> orbit b

-- | Replicate a song n times.
clone :: Rational -> Beat a -> Beat a
clone 1 b = b
clone n b = b >> clone (n-1) b

scaleBPM :: Rational -> Beat a -> Beat a
scaleBPM x b = beat (Tempo x c) a
  where
    (c, a) = unBeat b

scaleVel :: Rational -> Beat a -> Beat a
scaleVel x b = beat (Level x c) a
  where
    (c, a) = unBeat b

-- | Make an instrumet that plays for 0 seconds at 0 volumes.
--   Conenient when used with other commads to set them.
atom :: Sound -> Song
atom t = note 4 . strike $ hit t 0 volume

-- | Convenience function for setting the duration of a note.
note :: Rational -> Song -> Song
note n = beatMap (\h -> h & dur .~ 4 * quarter / n)

-- | Make a dotted rhythm.
dot :: Song -> Song
dot = beatMap (\h -> h & dur %~ (* (3 % 2)))

-- | Set the velocity of a Song
velocity :: Rational -> Song -> Song
velocity n = beatMap (\h -> h & vol .~ (max 0 (min 127 n)))

-- | A quarter note rest
rest :: Song
rest = note 4 . strike $ hit BassDrum1 0 0

-- | Quarter notes for all instruments in kit.
--   Abbreviations: hi = high, lo = low, cl = close, op = open,
--                  mu = mute, s = short, l = long.
bass2, bass, stick, snare, snare2, hiHat, crash, ride, cow, hiTom    :: Song
tamb, clap, loTom2, clHat, loTom, pedal, midTom, opHat, midTom2      :: Song
hiTom2, chinese, rideBl, splash, crash2, slap, ride2, hiBongo        :: Song
loBongo, muConga, opConga, loConga, hiTimb, loTimb, hiAgogo, loAgogo :: Song
cabasa, maracas, sWhistl, lWhistl, sGuiro, lGuiro, claves, hiWdBlk   :: Song
loWdBlk, muCuica, opCuica, muTrngl, opTrngl                          :: Song
bass2   = atom BassDrum2
bass    = atom BassDrum1
stick   = atom SideStick
snare   = atom SnareDrum1
snare2  = atom SnareDrum2
hiHat   = atom ClosedHihat
crash   = atom CrashCymbal1
ride    = atom RideCymbal1
cow     = atom Cowbell
hiTom   = atom HighTom1
tamb    = atom Tambourine
clap    = atom HandClap
loTom2  = atom LowTom2
clHat   = atom ClosedHihat
loTom   = atom LowTom1
pedal   = atom PedalHihat
midTom  = atom MidTom1
opHat   = atom OpenHihat
midTom2 = atom MidTom2
hiTom2  = atom HighTom2
chinese = atom ChineseCymbal
rideBl  = atom RideBell
splash  = atom SplashCymbal
crash2  = atom CrashCymbal2
slap    = atom VibraSlap
ride2   = atom RideCymbal2
hiBongo = atom HighBongo
loBongo = atom LowBongo
muConga = atom MuteHighConga
opConga = atom OpenHighConga
loConga = atom LowConga
hiTimb  = atom HighTimbale
loTimb  = atom LowTimbale
hiAgogo = atom HighAgogo
loAgogo = atom LowAgogo
cabasa  = atom Cabasa
maracas = atom Maracas
sWhistl = atom ShortWhistle
lWhistl = atom LongWhistle
sGuiro  = atom ShortGuiro
lGuiro  = atom LongGuiro
claves  = atom Claves
hiWdBlk = atom HighWoodBlock
loWdBlk = atom LowWoodBlock
muCuica = atom MuteCuica
opCuica = atom OpenCuica
muTrngl = atom MuteTriangle
opTrngl = atom OpenTriangle
