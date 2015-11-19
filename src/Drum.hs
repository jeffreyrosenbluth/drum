module Drum where

import Compose
import Control.Lens

quarter :: Int
quarter = 60000

-- | Make an instrumet that plays for 0 seconds at 0 volumes.
--   Conenient when used with other commads to set them.
atom :: Sound -> Song'
atom t = note 4 . strike $ hit t 0 0

-- | Convenience function for setting the duration of a note.
note :: Int -> Song' -> Song'
note n = songMap (\h -> h & dur .~ 4 * quarter `div` n)

-- | Make a dotted rhythm.
dot :: Song' -> Song'
dot = songMap (\h -> h & dur %~ (\d -> 3 * d `div` 2 ))

-- | Quarter notes for all instruments in kit.
--   Abbreviations: hi = high, lo = low, cl = close, op = open,
--                  mu = mute, s = short, l = long.
bass2, bass, stick, snare, snare2, hiHat, crash, ride, cow, hiTom    :: Song'
tamb, clap, loTom2, clHat, loTom, pedal, midTom, opHat, midTom2      :: Song'
hiTom2, chinese, rideBl, splash, crash2, slap, ride2, hiBongo        :: Song'
loBongo, muConga, opConga, loConga, hiTimb, loTimb, hiAgogo, loAgogo :: Song'
cabasa, maracas, sWhistl, lWhistl, sGuiro, lGuiro, claves, hiWdBlk   :: Song'
loWdBlk, muCuica, opCuica, muTrngl, opTrngl                          :: Song'
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

-- | A rest
rest :: Song'
rest = note 4 . strike $ hit BassDrum1 0 0
