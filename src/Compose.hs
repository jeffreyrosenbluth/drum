{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

module Compose
  ( Sound(..)
  , Hit
  , hit
  , tone
  , vol
  , dur
  , Command(..)
  , Beat(..)
  , Song
  , Control(..)
  , bpm
  , tempo
  , SequenceR
  , beat
  , song
  , runSequenceR
  , beatMap
  , execBeat
  , strike
  , orbit
  , clone
  , scaleBPM

  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import Control.Lens
-------------------------------------------------------------------------------
-- | The available instruments.
data Sound =
     BassDrum2     | BassDrum1     | SideStick    | SnareDrum1
   | HandClap      | SnareDrum2    | LowTom2      | ClosedHihat
   | LowTom1       | PedalHihat    | MidTom2      | OpenHihat
   | MidTom1       | HighTom2      | CrashCymbal1 | HighTom1
   | RideCymbal1   | ChineseCymbal | RideBell     | Tambourine
   | SplashCymbal  | Cowbell       | CrashCymbal2 | VibraSlap
   | RideCymbal2   | HighBongo     | LowBongo     | MuteHighConga
   | OpenHighConga | LowConga      | HighTimbale  | LowTimbale
   | HighAgogo     | LowAgogo      | Cabasa       | Maracas
   | ShortWhistle  | LongWhistle   | ShortGuiro   | LongGuiro
   | Claves        | HighWoodBlock | LowWoodBlock | MuteCuica
   | OpenCuica     | MuteTriangle  | OpenTriangle
   deriving (Show,Eq,Ord,Enum)

-- | A single drum strike, 'tone' is the instrument, dur the duration, and
--  vol the volume.
data Hit = Hit
    { _tone :: Sound
    , _dur  :: Int
    , _vol  :: Int
    } deriving (Show)

makeLenses ''Hit

-- | Constructor fo 'Hit'.
hit :: Sound -> Int -> Int -> Hit
hit t d v = Hit t d v

-- | The commands the comprixe a song. Prim for a single drum hit. Chain for
--   a sequence of hits. Par for making to sequences run in parallel. And BPM,
--   Vol, for tempo and volume.
data Command =
    Prim  Hit
  | Chain Command Command
  | Par   Command Command
  | Tempo Double  Command
  | None
  deriving (Show)

newtype Beat a = Beat {unBeat :: (Command, a)}

type Song = Beat ()


data Control = Control
  { _bpm   :: Int
  , _tempo :: Double
  }

makeLenses ''Control

-- Cutom monad ecapsulating a drum machine. Controls such as volume and
-- tempo are the state.
type SequenceR = ReaderT Control Beat

-- | Contert the drum maching into a Sequence to be played
runSequenceR :: Control -> SequenceR a -> Beat a
runSequenceR  = flip runReaderT

-- | Map a function on hits over a song.
beatMap :: (Hit -> Hit) -> Beat a -> Beat a
beatMap f (Beat (c,a)) = Beat $ (hmap f c, a)
  where
    hmap f (Prim h)      = Prim  (f h)
    hmap f (Chain b1 b2) = Chain (hmap f b1) (hmap f b2)
    hmap f (Par   b1 b2) = Par   (hmap f b1) (hmap f b2)
    hmap _ b             = b

instance Functor Beat where
  fmap = liftM

instance Applicative Beat where
  pure  = return
  (<*>) = ap

-- | This is basically a writer monad specialized to accumulating Commands
--   horizontally.
instance Monad Beat where
  return a = Beat (None, a)
  Beat (b, a) >>= k =
    let (Beat (b', a')) = k a
    in  Beat ((Chain b b'), a')

-------------------------------------------------------------------------------
-- | Unwarp a command.
execBeat :: Beat a -> Command
execBeat = fst . unBeat

-- | Wrap a command.
beat :: Command -> a -> Beat a
beat b a = Beat (b, a)

-- | Convenience function to wrap Commands with a = ().
song :: Command -> Song
song b = beat b ()

-- | Convert a hit to a 'Sequence ()'.
strike :: Hit -> Song
strike h = beat (Prim h) ()

-- | Play two Commands in parallel.
instance Monoid (Song) where
  mempty        = Beat (None, ())
  mappend b1 b2 = Beat (Par (execBeat b1) (execBeat b2), ())

-- | Loop a song forever.
orbit :: Beat a -> Beat a
orbit b = b >> orbit b

-- | Replicate a song n times.
clone :: Int -> Beat a -> Beat a
clone 1 b = b
clone n b = b >> clone (n-1) b

scaleBPM :: Double -> Beat a -> Beat a
scaleBPM x b = beat (Tempo x c) a
  where
    (c, a) = unBeat b
--------------------------------------------------------------------------------
