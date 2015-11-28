--------------------------------------------------------------------------------
-- |
-- Module      :  Drum
-- Author      : Jeffrey Rosenbluth
-- Copyright   : (c) 2015, Jeffrey Rosenbluth
--
-- Intruments and Convenience functions for 'drum'.
--
-------------------------------------------------------------------------------

module Drum
  ( orbit
  , clone
  , scaleBPM
  , scaleVel
  , atom
  , note
  , dot
  , vel
  , rest

  , n1, n2, n4, n8, n16, n32, n64
  , r1, r2, r4, r8, r16, r32, r64

  , accent
  , backwards

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
import Control.Lens hiding (backwards)
import Data.Ratio

volume :: Rational
volume = 92

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
note n = beatMap (\h -> h & duration .~ 4 / n)

-- | Make a dotted rhythm.
dot :: Song -> Song
dot = beatMap (\h -> h & duration *~ (3 / 2))

-- | Set the velocity of a Song
vel :: Rational -> Song -> Song
vel n = beatMap (\h -> h & velocity .~ max 0 (min 127 n))

-- | A quarter note rest
rest :: Rational -> Song
rest n = note n . strike $ hit BassDrum1 0 0

-- | Convenience functions to create notes of various durations.
n1, n2, n4, n8, n16, n32, n64 :: Song -> Song
n1  = note 1
n2  = note 2
n4  = note 4
n8  = note 8
n16 = note 16
n32 = note 32
n64 = note 164

-- | Rests
r1, r2, r4, r8, r16, r32, r64 :: Song
r1  = rest 1
r2  = rest 2
r4  = rest 4
r8  = rest 8
r16 = rest 16
r32 = rest 32
r64 = rest 64

accent :: Song -> Song
accent = vel 120

backwards :: Beat a -> Beat a
backwards (Beat (c, a)) = beat (go c) a
  where
    go p@(Prim h) = p
    go (Chain c1 c2) = Chain (go c2) (go c1)
    go (Par   c1 c2)
      | dur1 < dur2 = Par (Chain space (go c1)) (go c2)
      | dur2 < dur1 = Par (go c1) (Chain space (go c2))
      | otherwise   = Par (go c1) (go c2)
      where
        dur1 = totalDur c1
        dur2 = totalDur c2
        diff = abs (dur2 - dur1)
        space =  Prim $ hit BassDrum1 diff 0
    go (Tempo x c) = Tempo x (go c)
    go (Level x c) = Level x (go c)
    go None        = None
    
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
