module Drum where

import Compose
import Control.Lens

volume :: Int
volume = 100

quarter :: Int
quarter = 60000

atom :: Sound -> Song
atom t = n4 . strike $ hit t 0 volume

n16, n8, n4, n2, n1, dot :: Song -> Song
n16 = songMap (\h -> (h & dur .~ round (fromIntegral quarter / 4)))
n8  = songMap (\h -> (h & dur .~ round (fromIntegral quarter / 2)))
n4  = songMap (\h -> (h & dur .~ quarter))
n2  = songMap (\h -> (h & dur .~ (2 * quarter)))
n1  = songMap (\h -> (h & dur .~ (4 * quarter)))
dot = songMap (\h -> h & dur %~ (\d -> round (fromIntegral d * 1.5)))

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

rest :: Song
rest = n4 . strike $ hit BassDrum1 0 0
