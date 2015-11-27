{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Core
-- Author      : Jeffrey Rosenbluth
-- Copyright   : (c) 2015, Jeffrey Rosenbluth
--
-- Core types and functions for 'drum'
-- Inspired and heavily influenced by: https://github.com/5outh/Bang
--    Copyright (c) 2014 Benjamin Kovach
--
--------------------------------------------------------------------------------

module Core
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
  , level
  , SequenceR
  , beat
  , song
  , runSequenceR
  , beatMap
  , execBeat
  , strike

  ) where

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid
import Data.Ratio
import GHC.Generics
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
   deriving (Show, Eq, Ord, Enum, Generic)

instance FromJSON Sound
instance ToJSON Sound

-- | A single drum strike, 'tone' is the instrument, dur the duration, and
--  vol the volume.
data Hit = Hit
    { _tone :: Sound
    , _dur  :: Rational
    , _vol  :: Rational
    } deriving (Show, Generic)

makeLenses ''Hit

instance FromJSON Hit
instance ToJSON Hit

-- | Constructor fo 'Hit'.
hit :: Sound -> Rational -> Rational -> Hit
hit = Hit

-- | The commands the comprixe a song. Prim for a single drum hit. Chain for
--   a sequence of hits. Par for making to sequences run in parallel. And BPM,
--   Vol, for tempo and volume.
data Command =
    Prim  Hit
  | Chain Command Command
  | Par   Command Command
  | Tempo Rational  Command
  | Level Rational  Command
  | None
  deriving (Show, Generic)

instance FromJSON Command
instance ToJSON Command

newtype Beat a = Beat {unBeat :: (Command, a)} deriving (Show, Generic)

type Song = Beat ()

instance FromJSON Song
instance ToJSON Song

data Control = Control
  { _bpm    :: Rational
  , _tempo  :: Rational
  , _level  :: Rational
  }

makeLenses ''Control

-- Cutom monad ecapsulating a drum machine. Controls such as volume and
-- tempo are the state.
type SequenceR = ReaderT Control Beat

-- | Contert the drum maching Rationalo a Sequence to be played
runSequenceR :: Control -> SequenceR a -> Beat a
runSequenceR  = flip runReaderT

-- | Map a function on hits over a song.
beatMap :: (Hit -> Hit) -> Beat a -> Beat a
beatMap f (Beat (c,a)) = Beat (hmap f c, a)
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
    in  Beat (Chain b b', a')

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
strike h = song (Prim h)

-- | Play two Commands in parallel.
instance Monoid (Song) where
  mempty        = Beat (None, ())
  mappend b1 b2 = Beat (Par (execBeat b1) (execBeat b2), ())
--------------------------------------------------------------------------------
